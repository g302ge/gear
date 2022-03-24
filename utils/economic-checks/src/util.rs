// This file is part of Gear.

// Copyright (C) 2021 Gear Technologies Inc.
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

use codec::{Decode, Encode};
use common::{DAGBasedLedger, Dispatch, Origin as _};
use frame_support::{
    assert_ok,
    traits::{GenesisBuild, OffchainWorker, OnFinalize, OnIdle, OnInitialize},
    BasicExternalities,
};
use frame_system as system;
use gear_core::program::{CodeHash, ProgramId};
use gear_runtime::{
    AuraConfig, Call, Gear, GrandpaConfig, Runtime, Signature, SudoConfig, System,
    TransactionPaymentConfig, UncheckedExtrinsic, Usage,
};
use parking_lot::RwLock;
use primitive_types::H256;
use sp_consensus_aura::sr25519::AuthorityId as AuraId;
use sp_consensus_aura::{Slot, AURA_ENGINE_ID};
use sp_core::{
    offchain::{
        testing::{PoolState, TestOffchainExt, TestTransactionPoolExt},
        Duration, OffchainDbExt, OffchainWorkerExt, TransactionPoolExt,
    },
    Pair, Public,
};
use sp_finality_grandpa::AuthorityId as GrandpaId;
use sp_io::offchain;
use sp_runtime::{
    traits::{IdentifyAccount, Verify},
    AccountId32, Digest, DigestItem,
};

use sp_std::collections::btree_map::BTreeMap;
use std::sync::Arc;

type GasNodeKeyOf<T> = <<T as pallet_gear::Config>::GasHandler as DAGBasedLedger>::Key;
type GasBalanceOf<T> = <<T as pallet_gear::Config>::GasHandler as DAGBasedLedger>::Balance;

// Generate a crypto pair from seed.
pub fn get_from_seed<TPublic: Public>(seed: &str) -> <TPublic::Pair as Pair>::Public {
    TPublic::Pair::from_string(&format!("//{}", seed), None)
        .expect("static values are valid; qed")
        .public()
}

type AccountPublic = <Signature as Verify>::Signer;

// Generate an account ID from seed.
pub fn get_account_id_from_seed<TPublic: Public>(seed: &str) -> AccountId32
where
    AccountPublic: From<<TPublic::Pair as Pair>::Public>,
{
    AccountPublic::from(get_from_seed::<TPublic>(seed)).into_account()
}

// Generate an Aura authority key.
pub fn authority_keys_from_seed(s: &str) -> (AuraId, GrandpaId) {
    (get_from_seed::<AuraId>(s), get_from_seed::<GrandpaId>(s))
}

pub fn new_test_ext(
    balances: Vec<(impl Into<AccountId32>, u128)>,
    initial_authorities: Vec<(AuraId, GrandpaId)>,
    root_key: AccountId32,
) -> sp_io::TestExternalities {
    let mut t = system::GenesisConfig::default()
        .build_storage::<Runtime>()
        .unwrap();

    pallet_balances::GenesisConfig::<Runtime> {
        balances: balances
            .into_iter()
            .map(|(acc, balance)| (acc.into(), balance))
            .collect(),
    }
    .assimilate_storage(&mut t)
    .unwrap();

    AuraConfig {
        authorities: initial_authorities.iter().map(|x| (x.0.clone())).collect(),
    }
    .assimilate_storage(&mut t)
    .unwrap();

    BasicExternalities::execute_with_storage(&mut t, || {
        <GrandpaConfig as GenesisBuild<Runtime>>::build(&GrandpaConfig {
            authorities: initial_authorities
                .iter()
                .map(|x| (x.1.clone(), 1))
                .collect(),
        });
        // pallet_grandpa::Pallet::<Runtime>::initialize(
        //     &initial_authorities
        //         .iter()
        //         .map(|x| (x.1.clone(), 1))
        //         .collect(),
        // );

        <TransactionPaymentConfig as GenesisBuild<Runtime>>::build(
            &TransactionPaymentConfig::default(),
        );
    }); // .unwrap();

    SudoConfig {
        key: Some(root_key),
    }
    .assimilate_storage(&mut t)
    .unwrap();

    let mut ext: sp_io::TestExternalities = t.into(); //= sp_io::TestExternalities::new(t);
    ext.execute_with(|| System::set_block_number(1));
    ext
}

pub fn with_offchain_ext(
    balances: Vec<(impl Into<AccountId32>, u128)>,
    initial_authorities: Vec<(AuraId, GrandpaId)>,
    root_key: AccountId32,
) -> (sp_io::TestExternalities, Arc<RwLock<PoolState>>) {
    let mut ext = new_test_ext(balances, initial_authorities, root_key);
    let (offchain, _) = TestOffchainExt::new();
    let (pool, pool_state) = TestTransactionPoolExt::new();

    ext.register_extension(OffchainDbExt::new(offchain.clone()));
    ext.register_extension(OffchainWorkerExt::new(offchain));
    ext.register_extension(TransactionPoolExt::new(pool));

    (ext, pool_state)
}

#[allow(dead_code)]
pub(crate) fn run_to_block(n: u32, remaining_weight: Option<u64>) {
    // All blocks are to be authored by validator at index 0
    let slot = Slot::from(0);
    let pre_digest = Digest {
        logs: vec![DigestItem::PreRuntime(AURA_ENGINE_ID, slot.encode())],
    };

    while System::block_number() < n {
        System::on_finalize(System::block_number());
        let new_block_number = System::block_number() + 1;
        System::set_block_number(new_block_number);
        System::initialize(&new_block_number, &System::parent_hash(), &pre_digest);
        System::on_initialize(System::block_number());
        Gear::on_initialize(System::block_number());
        let remaining_weight =
            remaining_weight.unwrap_or_else(<Runtime as pallet_gear::Config>::BlockGasLimit::get);
        Gear::on_idle(System::block_number(), remaining_weight);
    }
}

pub(crate) fn run_to_block_with_ocw(
    n: u32,
    pool: Arc<RwLock<PoolState>>,
    remaining_weight: Option<u64>,
) {
    // All blocks are to be authored by validator at index 0
    let slot = Slot::from(0);
    let pre_digest = Digest {
        logs: vec![DigestItem::PreRuntime(AURA_ENGINE_ID, slot.encode())],
    };

    let now = System::block_number();
    for i in now + 1..=n {
        System::on_finalize(i - 1);
        System::set_block_number(i);
        System::initialize(&i, &System::parent_hash(), &pre_digest);
        System::on_initialize(i);
        Gear::on_initialize(i);
        let remaining_weight =
            remaining_weight.unwrap_or_else(<Runtime as pallet_gear::Config>::BlockGasLimit::get);
        Gear::on_idle(i, remaining_weight);
        process_tx_pool(pool.clone());
        increase_offchain_time(1_000);
        Usage::offchain_worker(i);
    }
}

fn increase_offchain_time(ms: u64) {
    offchain::sleep_until(offchain::timestamp().add(Duration::from_millis(ms)));
}

pub(crate) fn init_logger() {
    let _ = env_logger::Builder::from_default_env()
        .format_module_path(false)
        .format_level(true)
        .try_init();
}

pub(crate) fn generate_program_id(code: &[u8], salt: &[u8]) -> H256 {
    ProgramId::generate(CodeHash::generate(code), salt).into_origin()
}

pub(crate) fn process_tx_pool(pool: Arc<RwLock<PoolState>>) {
    let mut guard = pool.write();
    guard.transactions.iter().cloned().for_each(|bytes| {
        let tx = UncheckedExtrinsic::decode(&mut &bytes[..]).unwrap();
        if let Call::Usage(pallet_usage::Call::collect_waitlist_rent { payees_list }) = tx.function
        {
            log::debug!(
                "Sending collect_wait_list extrinsic with payees_list {:?}",
                payees_list
            );
            assert_ok!(Usage::collect_waitlist_rent(
                system::RawOrigin::None.into(),
                payees_list
            ));
        }
    });
    guard.transactions = vec![];
}

pub(crate) fn total_gas_in_wait_list() -> u64 {
    // Iterate through the wait list and record the respective gas nodes value limits
    // attributing the latter to the nearest `node_with_value` ID to avoid duplication
    let specified_value_by_node_id: BTreeMap<GasNodeKeyOf<Runtime>, GasBalanceOf<Runtime>> =
        frame_support::storage::PrefixIterator::<(u64, H256)>::new(
            common::STORAGE_WAITLIST_PREFIX.to_vec(),
            common::STORAGE_WAITLIST_PREFIX.to_vec(),
            |_, mut value| {
                let (dispatch, _) = <(Dispatch, u32)>::decode(&mut value)?;
                Ok(
                    <Runtime as pallet_gear::Config>::GasHandler::get_limit(dispatch.message.id)
                        .expect("Waitlisted messages must have associated gas"),
                )
            },
        )
        .map(|(gas, node_id)| (node_id, gas))
        .collect();

    specified_value_by_node_id
        .into_iter()
        .fold(0_u64, |acc, (_, val)| acc + val)
}
