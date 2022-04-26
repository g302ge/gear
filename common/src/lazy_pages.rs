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

//! Lazy pages support runtime functions

use crate::Origin;
use core::convert::TryFrom;
use gear_core::ids::ProgramId;
use gear_core::memory::{PageBuf, PageNumber};
use gear_runtime_interface::gear_ri;
use sp_std::{boxed::Box, collections::{btree_map::BTreeMap, btree_set::BTreeSet}, vec::Vec};

fn mprotect_lazy_pages(addr: u64, protect: bool) -> Result<(), &'static str> {
    gear_ri::mprotect_lazy_pages(addr, protect).map_err(|_| "Cannot mprotect some pages")
}

/// Try to enable and initialize lazy pages env
pub fn try_to_enable_lazy_pages() -> bool {
    if cfg!(feature = "disable_lazy_pages") || !gear_ri::init_lazy_pages() {
        // TODO: lazy-pages must be disabled in validators in relay-chain,
        // but it can be fixed in future only.
        log::debug!("lazy-pages: disabled or unsupported");
        false
    } else {
        log::debug!("lazy-pages: enabled");
        true
    }
}

/// For every page which has no data in btree map
/// we download data from storage if there is data for this page in storage.
pub fn download_pages_data(
    prog_id: ProgramId,
    memory_pages: &BTreeSet<PageNumber>,
) -> Result<BTreeMap<PageNumber, Box<PageBuf>>, &'static str> {
    let prog_id_hash = prog_id.into_origin();
    let mut pages_data = BTreeMap::new();
    for page in memory_pages {
        if let Some(data) = crate::get_program_page_data(prog_id_hash, *page) {
            let page_data =
                PageBuf::try_from(data).map_err(|_| "Cannot convert vec to page data")?;
            pages_data.insert(*page, Box::new(page_data));
        }
    }
    Ok(pages_data)
}

/// Protect and save storage keys for pages which has no data
pub fn protect_pages_and_init_info(
    lazy_pages: &BTreeSet<PageNumber>,
    prog_id: ProgramId,
    wasm_mem_begin_addr: u64,
) -> Result<(), &'static str> {
    let prog_id_hash = prog_id.into_origin();

    gear_ri::reset_lazy_pages_info();

    gear_ri::set_wasm_mem_begin_addr(wasm_mem_begin_addr);

    lazy_pages.iter().for_each(|p| {
        crate::save_page_lazy_info(prog_id_hash, *p);
    });

    mprotect_lazy_pages(wasm_mem_begin_addr, true)
}

/// Lazy pages contract post execution actions
pub fn post_execution_actions(
    memory_pages: &mut BTreeMap<PageNumber, Box<PageBuf>>,
    wasm_mem_begin_addr: u64,
) -> Result<(), &'static str> {
    // Set data for accessed lazy pages. Data which was before execution.
    let released_pages = gear_ri::get_released_pages();
    for page in released_pages {
        let data = gear_ri::get_released_page_old_data(page)
            .map_err(|_| "Some of released pages has no data")?;
        let page_data =
            PageBuf::try_from(data).map_err(|_| "Cannot convert page data to page buff")?;
        if memory_pages
            .insert(page.into(), Box::new(page_data))
            .is_some()
        {
            return Err("RUNTIME ERROR: all released pages must have no data in @memory_pages arg");
        };
    }

    // Removes protections from lazy pages
    mprotect_lazy_pages(wasm_mem_begin_addr, false)
}

/// Remove lazy-pages protection, returns wasm memory begin addr
pub fn remove_lazy_pages_prot(mem_addr: u64) -> Result<(), &'static str> {
    mprotect_lazy_pages(mem_addr, false)
}

/// Protect lazy-pages and set new wasm mem addr if it has been changed
pub fn protect_lazy_pages_and_update_wasm_mem_addr(
    old_mem_addr: u64,
    new_mem_addr: u64,
) -> Result<(), &'static str> {
    if new_mem_addr != old_mem_addr {
        log::debug!(
            "backend executor has changed wasm mem buff: from {:#x} to {:#x}",
            old_mem_addr,
            new_mem_addr
        );
        gear_ri::set_wasm_mem_begin_addr(new_mem_addr);
    }
    mprotect_lazy_pages(new_mem_addr, true)
}

/// Returns list of current lazy pages numbers
pub fn get_lazy_pages_numbers() -> Vec<PageNumber> {
    gear_ri::get_wasm_lazy_pages_numbers()
        .iter()
        .map(|p| PageNumber(*p))
        .collect()
}
