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

use crate as pallet_gear_payment;
use frame_support::{
    construct_runtime, parameter_types,
    traits::{ConstU8, Currency, FindAuthor, OnUnbalanced},
    weights::{IdentityFee, Weight},
};
use frame_system as system;
use primitive_types::H256;
use sp_runtime::{
    testing::Header,
    traits::{BlakeTwo256, IdentityLookup},
    FixedPointNumber, FixedU128,
};
use sp_std::convert::{TryFrom, TryInto};

type UncheckedExtrinsic = frame_system::mocking::MockUncheckedExtrinsic<Test>;
type Block = frame_system::mocking::MockBlock<Test>;

pub const ALICE: u64 = 1;
pub const BOB: u64 = 2;
pub const BLOCK_AUTHOR: u64 = 255;

pub type CustomFeeMultiplier = FixedU128;

// Configure a mock runtime to test the pallet.
construct_runtime!(
    pub enum Test where
        Block = Block,
        NodeBlock = Block,
        UncheckedExtrinsic = UncheckedExtrinsic,
    {
        System: system::{Pallet, Call, Config, Storage, Event<T>},
        Gear: pallet_gear::{Pallet, Call, Storage, Event<T>},
        Gas: pallet_gas::{Pallet, Storage},
        Balances: pallet_balances::{Pallet, Call, Storage, Config<T>, Event<T>},
        Authorship: pallet_authorship::{Pallet, Storage},
        TransactionPayment: pallet_transaction_payment::{Pallet, Storage},
        Timestamp: pallet_timestamp::{Pallet, Call, Storage, Inherent},
        GearPayment: pallet_gear_payment::{Pallet, Storage},
        GearProgram: pallet_gear_program::{Pallet, Storage, Event<T>},
    }
);

impl pallet_balances::Config for Test {
    type MaxLocks = ();
    type MaxReserves = ();
    type ReserveIdentifier = [u8; 8];
    type Balance = u128;
    type DustRemoval = ();
    type Event = Event;
    type ExistentialDeposit = ExistentialDeposit;
    type AccountStore = System;
    type WeightInfo = ();
}

pub struct FixedBlockAuthor;

impl FindAuthor<u64> for FixedBlockAuthor {
    fn find_author<'a, I>(_digests: I) -> Option<u64>
    where
        I: 'a + IntoIterator<Item = (sp_runtime::ConsensusEngineId, &'a [u8])>,
    {
        Some(BLOCK_AUTHOR)
    }
}

impl pallet_authorship::Config for Test {
    type FindAuthor = FixedBlockAuthor;
    type UncleGenerations = ();
    type FilterUncle = ();
    type EventHandler = ();
}

parameter_types! {
    pub const MinimumPeriod: u64 = 500;
}

impl pallet_timestamp::Config for Test {
    type Moment = u64;
    type OnTimestampSet = ();
    type MinimumPeriod = MinimumPeriod;
    type WeightInfo = ();
}

parameter_types! {
    pub const BlockHashCount: u64 = 250;
    pub const SS58Prefix: u8 = 42;
    pub const ExistentialDeposit: u64 = 1;
}

impl system::Config for Test {
    type BaseCallFilter = frame_support::traits::Everything;
    type BlockWeights = ();
    type BlockLength = ();
    type DbWeight = ();
    type Origin = Origin;
    type Call = Call;
    type Index = u64;
    type BlockNumber = u64;
    type Hash = H256;
    type Hashing = BlakeTwo256;
    type AccountId = u64;
    type Lookup = IdentityLookup<Self::AccountId>;
    type Header = Header;
    type Event = Event;
    type BlockHashCount = BlockHashCount;
    type Version = ();
    type PalletInfo = PalletInfo;
    type AccountData = pallet_balances::AccountData<u128>;
    type OnNewAccount = ();
    type OnKilledAccount = ();
    type SystemWeightInfo = ();
    type SS58Prefix = SS58Prefix;
    type OnSetCode = ();
    type MaxConsumers = frame_support::traits::ConstU32<16>;
}

parameter_types! {
    pub const TransactionByteFee: u128 = 1;
    pub MinimumMultiplier: pallet_transaction_payment::Multiplier =
        pallet_transaction_payment::Multiplier::saturating_from_integer(1);
}

impl pallet_transaction_payment::Config for Test {
    type OnChargeTransaction = GearPayment;
    type TransactionByteFee = TransactionByteFee;
    type OperationalFeeMultiplier = ConstU8<5>;
    type WeightToFee = IdentityFee<u128>;
    type FeeMultiplierUpdate = pallet_gear_payment::ConstantFeeMultiplier<MinimumMultiplier>;
}

pub struct MockWeightInfo;
impl pallet_gear::WeightInfo for MockWeightInfo {
    fn submit_code(_c: u32) -> Weight {
        10_000_000 as Weight
    }
    fn submit_program(_c: u32, _p: u32) -> Weight {
        20_000_000 as Weight
    }
    fn send_message(_p: u32) -> Weight {
        30_000_000 as Weight
    }
    fn send_reply(_p: u32) -> Weight {
        40_000_000 as Weight
    }
    fn initial_allocation(_q: u32) -> Weight {
        0_u64
    }
    fn alloc_in_handle(_q: u32) -> Weight {
        0_u64
    }
    fn reinstrument(_c: u32) -> Weight {
        0_u64
    }
    fn alloc(_r: u32) -> Weight {
        0_u64
    }
    fn gas(_r: u32) -> Weight {
        0_u64
    }
    fn gr_gas_available(_r: u32) -> Weight {
        0_u64
    }
    fn gr_msg_id(_r: u32) -> Weight {
        0_u64
    }
    fn gr_origin(_r: u32) -> Weight {
        0_u64
    }
    fn gr_program_id(_r: u32) -> Weight {
        0_u64
    }
    fn gr_source(_r: u32) -> Weight {
        0_u64
    }
    fn gr_value(_r: u32) -> Weight {
        0_u64
    }
    fn gr_value_available(_r: u32) -> Weight {
        0_u64
    }
    fn gr_size(_r: u32) -> Weight {
        0_u64
    }
    fn gr_read(_r: u32) -> Weight {
        0_u64
    }
    fn gr_read_per_kb(_n: u32) -> Weight {
        0_u64
    }
    fn gr_block_height(_r: u32) -> Weight {
        0_u64
    }
    fn gr_block_timestamp(_r: u32) -> Weight {
        0_u64
    }
    fn gr_send_init(_r: u32) -> Weight {
        0_u64
    }
    fn gr_send_push(_r: u32) -> Weight {
        0_u64
    }
    fn gr_send_push_per_kb(_n: u32) -> Weight {
        0_u64
    }
    fn gr_send_commit(_r: u32) -> Weight {
        0_u64
    }
    fn gr_send_commit_per_kb(_n: u32) -> Weight {
        0_u64
    }
    fn gr_reply(_r: u32) -> Weight {
        0_u64
    }
    fn gr_reply_per_kb(_n: u32) -> Weight {
        0_u64
    }
    fn gr_reply_to(_r: u32) -> Weight {
        0_u64
    }
    fn gr_debug(_r: u32) -> Weight {
        0_u64
    }
    fn gr_exit_code(_r: u32) -> Weight {
        0_u64
    }
    fn gr_exit(_r: u32) -> Weight {
        0_u64
    }
    fn gr_leave(_r: u32) -> Weight {
        0_u64
    }
    fn gr_wait(_r: u32) -> Weight {
        0_u64
    }
    fn gr_wake(_r: u32) -> Weight {
        0_u64
    }
    fn gr_create_program_wgas(_r: u32) -> Weight {
        0_u64
    }

    fn instr_i64const(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64load(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64store(_r: u32) -> Weight {
        0_u64
    }
    fn instr_select(_r: u32) -> Weight {
        0_u64
    }
    fn instr_if(_r: u32) -> Weight {
        0_u64
    }
    fn instr_br(_r: u32) -> Weight {
        0_u64
    }
    fn instr_br_if(_r: u32) -> Weight {
        0_u64
    }
    fn instr_br_table(_r: u32) -> Weight {
        0_u64
    }
    fn instr_br_table_per_entry(_e: u32) -> Weight {
        0_u64
    }
    fn instr_call(_r: u32) -> Weight {
        0_u64
    }
    fn instr_call_indirect(_r: u32) -> Weight {
        0_u64
    }
    fn instr_call_indirect_per_param(_p: u32) -> Weight {
        0_u64
    }
    fn instr_local_get(_r: u32) -> Weight {
        0_u64
    }
    fn instr_local_set(_r: u32) -> Weight {
        0_u64
    }
    fn instr_local_tee(_r: u32) -> Weight {
        0_u64
    }
    fn instr_global_get(_r: u32) -> Weight {
        0_u64
    }
    fn instr_global_set(_r: u32) -> Weight {
        0_u64
    }
    fn instr_memory_current(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64clz(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64ctz(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64popcnt(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64eqz(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64extendsi32(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64extendui32(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i32wrapi64(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64eq(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64ne(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64lts(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64ltu(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64gts(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64gtu(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64les(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64leu(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64ges(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64geu(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64add(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64sub(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64mul(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64divs(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64divu(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64rems(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64remu(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64and(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64or(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64xor(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64shl(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64shrs(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64shru(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64rotl(_r: u32) -> Weight {
        0_u64
    }
    fn instr_i64rotr(_r: u32) -> Weight {
        0_u64
    }
}

pub struct GasConverter;
impl common::GasPrice for GasConverter {
    type Balance = u128;
}

parameter_types! {
    pub const BlockGasLimit: u64 = 500_000_000;
    pub const OutgoingLimit: u32 = 1024;
    pub const WaitListFeePerBlock: u64 = 1_000;
    pub MySchedule: pallet_gear::Schedule<Test> = <pallet_gear::Schedule<Test>>::default();
}

impl pallet_gear::Config for Test {
    type Event = Event;
    type Currency = Balances;
    type GasPrice = GasConverter;
    type GasHandler = Gas;
    type WeightInfo = MockWeightInfo;
    type Schedule = MySchedule;
    type BlockGasLimit = BlockGasLimit;
    type OutgoingLimit = OutgoingLimit;
    type DebugInfo = ();
    type WaitListFeePerBlock = WaitListFeePerBlock;
}

impl pallet_gear_program::Config for Test {
    type Event = Event;
    type WeightInfo = ();
    type Currency = Balances;
}

impl pallet_gas::Config for Test {}

pub struct MsgQueueInflationPenalty;

impl pallet_gear_payment::CustomFees<CustomFeeMultiplier, Call> for MsgQueueInflationPenalty {
    fn apply_custom_fee(call: &Call) -> Option<CustomFeeMultiplier> {
        match call {
            Call::Gear(pallet_gear::Call::submit_program { .. })
            | Call::Gear(pallet_gear::Call::send_message { .. })
            | Call::Gear(pallet_gear::Call::send_reply { .. }) => {
                Some(GearPayment::message_queue_size_to_fee())
            }
            _ => None,
        }
    }
}

type NegativeImbalance = <Balances as Currency<u64>>::NegativeImbalance;

pub struct DealWithFees;
impl OnUnbalanced<NegativeImbalance> for DealWithFees {
    fn on_unbalanceds<B>(mut fees_then_tips: impl Iterator<Item = NegativeImbalance>) {
        if let Some(fees) = fees_then_tips.next() {
            if let Some(author) = Authorship::author() {
                Balances::resolve_creating(&author, fees);
            }
            if let Some(tips) = fees_then_tips.next() {
                if let Some(author) = Authorship::author() {
                    Balances::resolve_creating(&author, tips);
                }
            }
        }
    }
}

impl pallet_gear_payment::Config for Test {
    type MessageQueueExtras = MsgQueueInflationPenalty;
    type FeeDepositor = DealWithFees;
    type Currency = Balances;
}

// Build genesis storage according to the mock runtime.
pub fn new_test_ext() -> sp_io::TestExternalities {
    let mut t = system::GenesisConfig::default()
        .build_storage::<Test>()
        .unwrap();

    pallet_balances::GenesisConfig::<Test> {
        balances: vec![
            (ALICE, 100_000_000_u128),
            (BOB, 2_u128),
            (BLOCK_AUTHOR, 1_u128),
        ],
    }
    .assimilate_storage(&mut t)
    .unwrap();

    let mut ext = sp_io::TestExternalities::new(t);
    ext.execute_with(|| System::set_block_number(1));
    ext
}
