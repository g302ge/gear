// This file is part of Gear.

// Copyright (C) 2021-2022 Gear Technologies Inc.
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

use crate::{
    common::{
        DispatchResult, DispatchResultKind, ExecutableActor, ExecutionContext, ExecutionError,
    },
    configs::ExecutionSettings,
    ext::ProcessorExt,
};
use alloc::{
    boxed::Box,
    collections::{BTreeMap, BTreeSet},
};
use gear_backend_common::{BackendReport, Environment, IntoExtInfo, TerminationReason};
use gear_core::{
    env::Ext as EnvExt,
    gas::{ChargeResult, GasAllowanceCounter, GasCounter, ValueCounter},
    memory::{AllocationsContext, PageBuf, WasmPageNumber},
    message::{ContextSettings, IncomingDispatch, MessageContext},
};

/// Execute wasm with dispatch and return dispatch result.
pub fn execute_wasm<A: ProcessorExt + EnvExt + IntoExtInfo + 'static, E: Environment<A>>(
    actor: ExecutableActor,
    dispatch: IncomingDispatch,
    context: ExecutionContext,
    settings: ExecutionSettings,
    msg_ctx_settings: ContextSettings,
) -> Result<DispatchResult, ExecutionError> {
    let ExecutableActor {
        mut program,
        balance,
        pages_data,
    } = actor;

    let program_id = program.id();
    let kind = dispatch.kind();

    log::debug!("Executing program {}", program_id);
    log::debug!("Executing dispatch {:?}", dispatch);

    // Creating gas counter.
    let mut gas_counter = GasCounter::new(dispatch.gas_limit());
    let mut gas_allowance_counter = GasAllowanceCounter::new(context.gas_allowance);

    // Creating value counter.
    let value_counter = ValueCounter::new(balance + dispatch.value());

    let static_pages = program.static_pages();

    let mem_size = if let Some(max_wasm_page) = program.get_pages().iter().next_back() {
        // Charging gas for loaded pages
        let amount = settings.load_page_cost() * program.get_pages().len() as u64;

        if gas_allowance_counter.charge(amount) != ChargeResult::Enough {
            return Err(ExecutionError {
                program_id,
                gas_amount: gas_counter.into(),
                reason: "",
                allowance_exceed: true,
            });
        };

        if gas_counter.charge(amount) != ChargeResult::Enough {
            return Err(ExecutionError {
                program_id,
                gas_amount: gas_counter.into(),
                reason: "Not enough gas to load memory.",
                allowance_exceed: false,
            });
        };

        // Charging gas for mem size
        let amount =
            settings.mem_grow_cost() * (max_wasm_page.0 as u64 + 1 - static_pages.0 as u64);

        if gas_allowance_counter.charge(amount) != ChargeResult::Enough {
            return Err(ExecutionError {
                program_id,
                gas_amount: gas_counter.into(),
                reason: "",
                allowance_exceed: true,
            });
        }

        if gas_counter.charge(amount) != ChargeResult::Enough {
            return Err(ExecutionError {
                program_id,
                gas_amount: gas_counter.into(),
                reason: "Not enough gas to grow memory size.",
                allowance_exceed: false,
            });
        }

        // +1 because pages numeration begins from 0
        *max_wasm_page + 1.into()
    } else {
        // Charging gas for initial pages
        let amount = settings.init_cost() * static_pages.0 as u64;

        if gas_allowance_counter.charge(amount) != ChargeResult::Enough {
            return Err(ExecutionError {
                program_id,
                gas_amount: gas_counter.into(),
                reason: "",
                allowance_exceed: true,
            });
        };

        if gas_counter.charge(amount) != ChargeResult::Enough {
            return Err(ExecutionError {
                program_id,
                gas_amount: gas_counter.into(),
                reason: "Not enough gas for initial memory.",
                allowance_exceed: false,
            });
        };

        static_pages
    };

    if mem_size < static_pages {
        log::error!(
            "Mem size less then static pages num: mem_size = {:?}, static_pages = {:?}",
            mem_size,
            static_pages
        );
        return Err(ExecutionError {
            program_id,
            gas_amount: gas_counter.into(),
            reason: "Mem size less then static pages num",
            allowance_exceed: false,
        });
    }

    // Getting wasm pages allocations.
    let allocations: BTreeSet<WasmPageNumber> = {
        let static_pages = program.static_pages().0;
        if program.get_pages().is_empty() {
            program
                .get_pages_mut()
                .extend((0..static_pages).map(|p| WasmPageNumber(p)));
        }
        program.get_pages().clone()
    };

    // Creating allocations context.
    let allocations_context =
        AllocationsContext::new(allocations, static_pages, settings.max_pages());

    // Creating message context.
    let message_context = MessageContext::new_with_settings(
        dispatch.message().clone(),
        program_id,
        dispatch.context().clone(),
        msg_ctx_settings,
    );

    // Creating externalities.
    let ext = A::new(
        gas_counter,
        gas_allowance_counter,
        value_counter,
        allocations_context,
        message_context,
        settings.block_info,
        settings.config,
        settings.existential_deposit,
        None,
        None,
        context.origin,
        program_id,
        Default::default(),
        settings.host_fn_weights,
    );

    // TODO: change later
    let mut pages_data = pages_data
        .into_iter()
        .map(|(page, data)| (page, Box::new(PageBuf::try_from(data).unwrap())))
        .collect();

    let mut env = E::new(ext, &program.raw_code(), &pages_data, mem_size).map_err(|err| {
        log::error!("Setup instance err = {:?}", err);
        ExecutionError {
            program_id,
            gas_amount: err.gas_amount,
            reason: err.reason,
            allowance_exceed: false,
        }
    })?;

    if A::is_lazy_pages_enabled() {
        // All program wasm pages, which has no data in actor, is supposed to be lazy page candidate.
        let lazy_pages = program
            .get_pages()
            .iter()
            .flat_map(|page| page.to_gear_pages_iter())
            .filter(|page| !pages_data.contains_key(page))
            .collect();
        if let Err(e) = A::lazy_pages_protect_and_init_info(
            &lazy_pages,
            program_id,
            env.get_wasm_memory_begin_addr(),
        ) {
            return Err(ExecutionError {
                program_id,
                gas_amount: env.into_gas_amount(),
                reason: e,
                allowance_exceed: false,
            });
        }
    }

    // Page which is right after stack last page
    let stack_end_page = env.get_stack_mem_end();
    log::trace!("Stack end page = {:?}", stack_end_page);

    // Running backend.
    let BackendReport { termination, info } =
        match env.execute(kind.into_entry(), |wasm_memory_addr| {
            // accessed lazy pages old data will be added to `initial_pages`
            // TODO: if post execution actions err is connected, with removing pages protections,
            // then we should panic here, because protected pages may cause UB later, during err handling,
            // if somebody will try to access this pages.
            if A::is_lazy_pages_enabled() {
                A::lazy_pages_post_execution_actions(&mut pages_data, wasm_memory_addr)
            } else {
                Ok(())
            }
        }) {
            Ok(report) => report,
            Err(e) => {
                return Err(ExecutionError {
                    program_id,
                    gas_amount: e.gas_amount,
                    reason: e.reason,
                    allowance_exceed: false,
                })
            }
        };

    log::trace!("term reason = {:?}", termination);

    // Parsing outcome.
    let kind = match termination {
        TerminationReason::Exit(value_dest) => DispatchResultKind::Exit(value_dest),
        TerminationReason::Leave | TerminationReason::Success => DispatchResultKind::Success,
        TerminationReason::Trap {
            explanation,
            description,
        } => {
            log::debug!(
                "💥 Trap during execution of {}\n❓ Description: {}\n📔 Explanation: {}",
                program_id,
                description.unwrap_or_else(|| "None".into()),
                explanation.unwrap_or("None"),
            );

            DispatchResultKind::Trap(explanation)
        }
        TerminationReason::Wait => DispatchResultKind::Wait,
        TerminationReason::GasAllowanceExceed => DispatchResultKind::GasAllowanceExceed,
    };

    // changed and new pages will be updated in storage
    let mut page_update = BTreeMap::new();
    for (page, new_data) in info.pages_data {
        // If there are stack memory pages, then
        // we ignore stack pages update, because they are unused after execution,
        // and for next program execution old data in stack it's just garbage.
        if let Some(stack_end_page) = stack_end_page {
            if page.0 < stack_end_page.to_gear_pages().0 {
                continue;
            }
        }

        if let Some(initial_data) = pages_data.get(&page) {
            if !new_data.eq(initial_data.as_ref()) {
                page_update.insert(page, new_data);
                log::trace!(
                    "Page {} has been changed - will be updated in storage",
                    page.0
                );
            }
        } else {
            // TODO: new allocated pages can also be protected and handled as lazy pages.
            // In this case @initial_pages_data will have data for these pages in post action func.
            page_update.insert(page, new_data);
            log::trace!("Page {} is a new page - will be upload to storage", page.0);
        };
    }

    // Getting new programs that are scheduled to be initialized (respected messages are in `generated_dispatches` collection)
    let program_candidates = info.program_candidates_data;

    log::debug!("after exec allocations = {:?}", info.allocations);

    // Output
    Ok(DispatchResult {
        kind,
        dispatch,
        program_id,
        context_store: info.context_store,
        generated_dispatches: info.generated_dispatches,
        awakening: info.awakening,
        program_candidates,
        gas_amount: info.gas_amount,
        page_update,
        allocations: info.allocations,
    })
}
