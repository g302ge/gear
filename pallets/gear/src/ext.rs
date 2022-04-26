// This file is part of Gear.

// Copyright (C) 2022 Gear Technologies Inc.
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

use alloc::collections::BTreeSet;
use common::lazy_pages;
use core_processor::{
    configs::{AllocationsConfig, BlockInfo},
    Ext, ProcessorExt,
};
use gear_backend_common::{ExtInfo, IntoExtInfo};
use gear_core::{
    costs::HostFnWeights,
    env::Ext as EnvExt,
    gas::{GasAllowanceCounter, GasAmount, GasCounter, ValueCounter},
    ids::{CodeId, MessageId, ProgramId},
    memory::{AllocationsContext, Memory, PageBuf, PageNumber, WasmPageNumber},
    message::{HandlePacket, MessageContext, ReplyPacket},
};
use sp_std::{boxed::Box, collections::btree_map::BTreeMap, vec, vec::Vec};

/// Ext with lazy pages support
pub struct LazyPagesExt {
    inner: Ext,
}

impl IntoExtInfo for LazyPagesExt {
    fn into_ext_info<F: FnMut(usize, &mut [u8]) -> Result<(), &'static str>>(
        self,
        mut get_page_data: F,
    ) -> Result<ExtInfo, (&'static str, GasAmount)> {
        // accessed pages are all pages except current lazy pages
        let allocations = self.inner.allocations_context.allocations().clone();
        let pages: BTreeSet<PageNumber> = allocations
            .iter()
            .flat_map(|p| p.to_gear_pages_iter())
            .collect();
        let mut accessed_pages = pages.clone();
        let lazy_pages_numbers = lazy_pages::get_lazy_pages_numbers();
        lazy_pages_numbers.into_iter().for_each(|p| {
            accessed_pages.remove(&p);
        });

        log::trace!("accessed pages numbers = {:?}", accessed_pages);

        let mut accessed_pages_data = BTreeMap::new();
        for page in accessed_pages {
            let mut buf = vec![0u8; PageNumber::size()];
            if let Err(err) = get_page_data(page.offset(), &mut buf) {
                return Err((err, self.into_gas_amount()));
            }
            accessed_pages_data.insert(page, buf);
        }

        let (outcome, context_store) = self.inner.message_context.drain();
        let (generated_dispatches, awakening) = outcome.drain();

        Ok(ExtInfo {
            gas_amount: self.inner.gas_counter.into(),
            allocations,
            pages_data: accessed_pages_data,
            generated_dispatches,
            awakening,
            context_store,
            trap_explanation: self.inner.error_explanation,
            exit_argument: self.inner.exit_argument,
            program_candidates_data: self.inner.program_candidates_data,
        })
    }

    fn into_gas_amount(self) -> gear_core::gas::GasAmount {
        self.inner.gas_counter.into()
    }
}

impl ProcessorExt for LazyPagesExt {
    fn new(
        gas_counter: GasCounter,
        gas_allowance_counter: GasAllowanceCounter,
        value_counter: ValueCounter,
        allocations_context: AllocationsContext,
        message_context: MessageContext,
        block_info: BlockInfo,
        config: AllocationsConfig,
        existential_deposit: u128,
        error_explanation: Option<&'static str>,
        exit_argument: Option<ProgramId>,
        origin: ProgramId,
        program_id: ProgramId,
        program_candidates_data: BTreeMap<CodeId, Vec<(ProgramId, MessageId)>>,
        host_fn_weights: HostFnWeights,
    ) -> Self {
        Self {
            inner: Ext {
                gas_counter,
                gas_allowance_counter,
                value_counter,
                allocations_context,
                message_context,
                block_info,
                config,
                existential_deposit,
                error_explanation,
                exit_argument,
                origin,
                program_id,
                program_candidates_data,
                host_fn_weights,
            },
        }
    }

    fn is_lazy_pages_enabled() -> bool {
        true
    }

    fn lazy_pages_protect_and_init_info(
        memory_pages: &BTreeSet<PageNumber>,
        prog_id: ProgramId,
        wasm_mem_begin_addr: u64,
    ) -> Result<(), &'static str> {
        lazy_pages::protect_pages_and_init_info(memory_pages, prog_id, wasm_mem_begin_addr)
    }

    fn lazy_pages_post_execution_actions(
        memory_pages: &mut BTreeMap<PageNumber, Box<PageBuf>>,
        wasm_mem_begin_addr: u64,
    ) -> Result<(), &'static str> {
        lazy_pages::post_execution_actions(memory_pages, wasm_mem_begin_addr)
    }
}

impl EnvExt for LazyPagesExt {
    fn alloc(
        &mut self,
        pages_num: WasmPageNumber,
        mem: &mut dyn Memory,
    ) -> Result<WasmPageNumber, &'static str> {
        // Greedily charge gas for allocations
        self.charge_gas(
            pages_num
                .0
                .saturating_mul(self.inner.config.alloc_cost as u32),
        )?;
        // Greedily charge gas for grow
        self.charge_gas(
            pages_num
                .0
                .saturating_mul(self.inner.config.mem_grow_cost as u32),
        )?;

        let old_mem_size = mem.size();

        // New pages allocation may change wasm memory buffer location.
        // So we remove protections from lazy-pages
        // and set protection back for new wasm memory buffer pages.
        // Also we correct lazy-pages info if need.
        let old_mem_addr = mem.get_wasm_memory_begin_addr();
        lazy_pages::remove_lazy_pages_prot(old_mem_addr)?;

        let result = self
            .inner
            .allocations_context
            .alloc(pages_num, mem)
            .map_err(|_e| "Allocation error");

        let page_number = self.inner.return_and_store_err(result)?;

        let new_mem_addr = mem.get_wasm_memory_begin_addr();
        lazy_pages::protect_lazy_pages_and_update_wasm_mem_addr(old_mem_addr, new_mem_addr)?;

        // Returns back greedily used gas for grow
        let new_mem_size = mem.size();
        let grow_pages_num = new_mem_size - old_mem_size;
        let mut gas_to_return_back = self
            .inner
            .config
            .mem_grow_cost
            .saturating_mul((pages_num - grow_pages_num).0 as u64);

        // Returns back greedily used gas for allocations
        let first_page = page_number;
        let last_page = first_page + pages_num - 1.into();
        let mut new_alloced_pages_num = WasmPageNumber(0);
        for page in first_page.0..=last_page.0 {
            if !self.inner.allocations_context.is_init_page(page.into()) {
                new_alloced_pages_num = new_alloced_pages_num + 1.into();
            }
        }
        gas_to_return_back = gas_to_return_back.saturating_add(
            self.inner
                .config
                .alloc_cost
                .saturating_mul((pages_num - new_alloced_pages_num).0 as u64),
        );

        self.refund_gas(gas_to_return_back as u32)?;

        Ok(page_number)
    }

    fn block_height(&mut self) -> Result<u32, &'static str> {
        self.inner.block_height()
    }

    fn block_timestamp(&mut self) -> Result<u64, &'static str> {
        self.inner.block_timestamp()
    }

    fn origin(&mut self) -> Result<ProgramId, &'static str> {
        self.inner.origin()
    }

    fn send_init(&mut self) -> Result<usize, &'static str> {
        self.inner.send_init()
    }

    fn send_push(&mut self, handle: usize, buffer: &[u8]) -> Result<(), &'static str> {
        self.inner.send_push(handle, buffer)
    }

    fn reply_push(&mut self, buffer: &[u8]) -> Result<(), &'static str> {
        self.inner.reply_push(buffer)
    }

    fn send_commit(&mut self, handle: usize, msg: HandlePacket) -> Result<MessageId, &'static str> {
        self.inner.send_commit(handle, msg)
    }

    fn reply_commit(&mut self, msg: ReplyPacket) -> Result<MessageId, &'static str> {
        self.inner.reply_commit(msg)
    }

    fn reply_to(&mut self) -> Result<Option<(MessageId, i32)>, &'static str> {
        self.inner.reply_to()
    }

    fn source(&mut self) -> Result<ProgramId, &'static str> {
        self.inner.source()
    }

    fn exit(&mut self, value_destination: ProgramId) -> Result<(), &'static str> {
        self.inner.exit(value_destination)
    }

    fn message_id(&mut self) -> Result<MessageId, &'static str> {
        self.inner.message_id()
    }

    fn program_id(&mut self) -> Result<ProgramId, &'static str> {
        self.inner.program_id()
    }

    fn free(&mut self, page: WasmPageNumber) -> Result<(), &'static str> {
        self.inner.free(page)
    }

    fn debug(&mut self, data: &str) -> Result<(), &'static str> {
        self.inner.debug(data)
    }

    fn msg(&mut self) -> &[u8] {
        self.inner.msg()
    }

    fn charge_gas(&mut self, val: u32) -> Result<(), &'static str> {
        self.inner.charge_gas(val)
    }

    fn refund_gas(&mut self, val: u32) -> Result<(), &'static str> {
        self.inner.refund_gas(val)
    }

    fn gas(&mut self, val: u32) -> Result<(), &'static str> {
        self.inner.gas(val)
    }

    fn gas_available(&mut self) -> Result<u64, &'static str> {
        self.inner.gas_available()
    }

    fn value(&mut self) -> Result<u128, &'static str> {
        self.inner.value()
    }

    fn leave(&mut self) -> Result<(), &'static str> {
        self.inner.leave()
    }

    fn wait(&mut self) -> Result<(), &'static str> {
        self.inner.wait()
    }

    fn wake(&mut self, waker_id: MessageId) -> Result<(), &'static str> {
        self.inner.wake(waker_id)
    }

    fn value_available(&mut self) -> Result<u128, &'static str> {
        self.inner.value_available()
    }

    fn create_program(
        &mut self,
        packet: gear_core::message::InitPacket,
    ) -> Result<ProgramId, &'static str> {
        self.inner.create_program(packet)
    }

    fn charge_gas_runtime(
        &mut self,
        costs: gear_core::costs::RuntimeCosts,
    ) -> Result<(), &'static str> {
        self.inner.charge_gas_runtime(costs)
    }
}
