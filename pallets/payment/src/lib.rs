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

#![cfg_attr(not(feature = "std"), no_std)]

use frame_support::{
    pallet_prelude::*,
    traits::{
        Currency, ExistenceRequirement, Imbalance, LockableCurrency, OnUnbalanced, WithdrawReasons,
    },
    weights::{DispatchInfo, GetDispatchInfo, Pays, Weight},
};
use pallet_transaction_payment::{
    FeeDetails, InclusionFee, Multiplier, MultiplierUpdate, OnChargeTransaction,
    RuntimeDispatchInfo,
};
use sp_runtime::{
    generic::{CheckedExtrinsic, UncheckedExtrinsic},
    traits::{
        Convert, DispatchInfoOf, Dispatchable, PostDispatchInfoOf, SaturatedConversion, Saturating,
        SignedExtension, Zero,
    },
    FixedPointNumber, FixedPointOperand, Perquintill,
};

pub use pallet::*;

type BalanceOf<T> =
    <<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;

type NegativeImbalanceOf<T> = <<T as Config>::Currency as Currency<
    <T as frame_system::Config>::AccountId,
>>::NegativeImbalance;
type PositiveImbalanceOf<T> = <<T as Config>::Currency as Currency<
    <T as frame_system::Config>::AccountId,
>>::PositiveImbalance;

type CallOf<T> = <T as frame_system::Config>::Call;

pub type TransactionPayment<T> = pallet_transaction_payment::Pallet<T>;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

/// Custom fee multiplier which remains constant regardless the network congestion
/// TODO: consider using Substrate's built-in `pallet_transaction_payment::TargetedFeeAdjustment`
/// to allow elastic fees based on network conditions (if that's more appropriate)
pub struct ConstantFeeMultiplier<M>(sp_std::marker::PhantomData<M>);

impl<M> MultiplierUpdate for ConstantFeeMultiplier<M>
where
    M: frame_support::traits::Get<Multiplier>,
{
    fn min() -> Multiplier {
        M::get()
    }
    fn target() -> Perquintill {
        Default::default()
    }
    fn variability() -> Multiplier {
        Default::default()
    }
}
impl<M> Convert<Multiplier, Multiplier> for ConstantFeeMultiplier<M>
where
    M: frame_support::traits::Get<Multiplier>,
{
    fn convert(previous: Multiplier) -> Multiplier {
        let min_multiplier = M::get();
        previous.max(min_multiplier)
    }
}

/// A trait to be able to customize fees for certain extrinsics types
pub trait CustomFees<Balance, Call> {
    fn apply_custom_fee(call: &Call) -> Option<Balance>;
}

impl<B, C> CustomFees<B, C> for () {
    fn apply_custom_fee(_call: &C) -> Option<B> {
        None
    }
}

/// A trait whose purpose is to extract the `Call` variant of an extrinsic
pub trait ExtractCall<Call> {
    fn extract_call(&self) -> Call;
}

/// Implementation for unchecked extrinsic.
impl<Address, Call, Signature, Extra> ExtractCall<Call>
    for UncheckedExtrinsic<Address, Call, Signature, Extra>
where
    Call: Dispatchable + Clone,
    Extra: SignedExtension,
{
    fn extract_call(&self) -> Call {
        self.function.clone()
    }
}

/// Implementation for checked extrinsic.
impl<Address, Call, Extra> ExtractCall<Call> for CheckedExtrinsic<Address, Call, Extra>
where
    Call: Dispatchable + Clone,
{
    fn extract_call(&self) -> Call {
        self.function.clone()
    }
}

macro_rules! balance {
    ($name:ident) => {
        BalanceOf::<T>::saturated_from($name.saturated_into::<u128>())
    };
}

impl<T: Config> Pallet<T> {
    /// Query the data that we know about the fee of a given `call`.
    ///
    /// The fee includes two components:
    /// - the usual fee that applies to all dispatchables and takes into account extrinsic weight;
    /// - optionally, additional fee that applies to extrinsics that push messages to the queue.
    pub fn query_info<
        Extrinsic: sp_runtime::traits::Extrinsic + GetDispatchInfo + ExtractCall<CallOf<T>>,
    >(
        unchecked_extrinsic: Extrinsic,
        len: u32,
    ) -> RuntimeDispatchInfo<BalanceOf<T>>
    where
        CallOf<T>: Dispatchable<Info = DispatchInfo>,
        <<T as pallet_transaction_payment::Config>::OnChargeTransaction as OnChargeTransaction<
            T,
        >>::Balance: FixedPointOperand,
    {
        // Check if additional fees apply to this call
        let DispatchInfo { pays_fee, .. } =
            <Extrinsic as GetDispatchInfo>::get_dispatch_info(&unchecked_extrinsic);

        let extra_fee = match pays_fee {
            Pays::Yes => {
                let call =
                    <Extrinsic as ExtractCall<CallOf<T>>>::extract_call(&unchecked_extrinsic);
                T::MessageQueueExtras::apply_custom_fee(&call).unwrap_or_default()
            }
            _ => BalanceOf::<T>::saturated_from(0_u32),
        };

        // Common fee part
        let RuntimeDispatchInfo {
            weight,
            class,
            partial_fee,
        } = TransactionPayment::<T>::query_info(unchecked_extrinsic, len);

        // Combine (converting fees values trouhg u128)
        let combined_fee = balance!(partial_fee).saturating_add(extra_fee);

        RuntimeDispatchInfo {
            weight,
            class,
            partial_fee: combined_fee,
        }
    }

    /// Query the detailed fee of a given `call`.
    /// Combines data about common and optional (extra) fees.
    pub fn query_fee_details<
        Extrinsic: sp_runtime::traits::Extrinsic + GetDispatchInfo + ExtractCall<CallOf<T>>,
    >(
        unchecked_extrinsic: Extrinsic,
        len: u32,
    ) -> FeeDetails<BalanceOf<T>>
    where
        CallOf<T>: Dispatchable<Info = DispatchInfo>,
        <<T as pallet_transaction_payment::Config>::OnChargeTransaction as OnChargeTransaction<
            T,
        >>::Balance: FixedPointOperand,
    {
        let call: CallOf<T> =
            <Extrinsic as ExtractCall<CallOf<T>>>::extract_call(&unchecked_extrinsic);
        let extra_fee = T::MessageQueueExtras::apply_custom_fee(&call).unwrap_or_default();

        let FeeDetails { inclusion_fee, tip } =
            TransactionPayment::<T>::query_fee_details(unchecked_extrinsic, len);

        let inclusion_fee = inclusion_fee.map(
            |InclusionFee {
                 base_fee,
                 len_fee,
                 adjusted_weight_fee,
             }| {
                // Combine the `adjusted_weight_fee` with `extra_fee`
                let combined_adjusted_weight_fee =
                    balance!(adjusted_weight_fee).saturating_add(extra_fee);
                InclusionFee {
                    base_fee: balance!(base_fee),
                    len_fee: balance!(len_fee),
                    adjusted_weight_fee: combined_adjusted_weight_fee,
                }
            },
        );
        FeeDetails {
            inclusion_fee,
            tip: balance!(tip),
        }
    }

    // Calculate extra fee based on Message Queue state
    pub fn message_queue_size_to_fee() -> BalanceOf<T> {
        // TODO: implement proper algorithm, constant for testing purpose
        BalanceOf::<T>::saturated_from(300_000_000_u128)
    }
}

pub struct LiquidityInfo<T: Config> {
    extra_fee: BalanceOf<T>,
    imbalance: Option<NegativeImbalanceOf<T>>,
}

impl<T: Config> Default for LiquidityInfo<T> {
    fn default() -> Self {
        LiquidityInfo {
            extra_fee: Default::default(),
            imbalance: None,
        }
    }
}

impl<T: Config> OnChargeTransaction<T> for Pallet<T>
where
    DispatchInfoOf<CallOf<T>>: Into<DispatchInfo> + Clone,
{
    type Balance = BalanceOf<T>;
    type LiquidityInfo = LiquidityInfo<T>;

    fn withdraw_fee(
        who: &T::AccountId,
        call: &CallOf<T>,
        _info: &DispatchInfoOf<CallOf<T>>,
        fee: Self::Balance,
        tip: Self::Balance,
    ) -> Result<Self::LiquidityInfo, TransactionValidityError> {
        if fee.is_zero() {
            return Ok(Default::default());
        }

        let extra_fee = T::MessageQueueExtras::apply_custom_fee(call).unwrap_or_default();
        let combined_fee = fee.saturating_add(extra_fee);

        let withdraw_reason = if tip.is_zero() {
            WithdrawReasons::TRANSACTION_PAYMENT
        } else {
            WithdrawReasons::TRANSACTION_PAYMENT | WithdrawReasons::TIP
        };

        match T::Currency::withdraw(
            who,
            combined_fee,
            withdraw_reason,
            ExistenceRequirement::AllowDeath,
        ) {
            Ok(imbalance) => Ok(LiquidityInfo {
                extra_fee,
                imbalance: Some(imbalance),
            }),
            Err(_) => Err(InvalidTransaction::Payment.into()),
        }
    }

    fn correct_and_deposit_fee(
        who: &T::AccountId,
        _dispatch_info: &DispatchInfoOf<CallOf<T>>,
        _post_info: &PostDispatchInfoOf<CallOf<T>>,
        corrected_fee: Self::Balance,
        tip: Self::Balance,
        already_withdrawn: Self::LiquidityInfo,
    ) -> Result<(), TransactionValidityError> {
        let LiquidityInfo {
            extra_fee,
            imbalance,
        } = already_withdrawn;
        if let Some(paid) = imbalance {
            // Calculate the amont to refund: `refund` = `paid` - (`corrected_fee` + `extra_fee`)
            let refund_amount = paid
                .peek()
                .saturating_sub(corrected_fee)
                .saturating_sub(extra_fee);

            // refund to the the account that paid the fees. If this fails, the
            // account might have dropped below the existential balance. In
            // that case we don't refund anything.
            let refund_imbalance = T::Currency::deposit_into_existing(who, refund_amount)
                .unwrap_or_else(|_| PositiveImbalanceOf::<T>::zero());
            // merge the imbalance caused by paying the fees and refunding parts of it again.
            let adjusted_paid = paid
                .offset(refund_imbalance)
                .same()
                .map_err(|_| TransactionValidityError::Invalid(InvalidTransaction::Payment))?;
            // Call someone else to handle the imbalance (fee and tip separately)
            let (tip, fee) = adjusted_paid.split(tip);
            T::FeeDepositor::on_unbalanceds(Some(fee).into_iter().chain(Some(tip)));
        }
        Ok(())
    }
}

#[frame_support::pallet]
pub mod pallet {
    use super::*;
    use frame_system::pallet_prelude::*;

    #[pallet::config]
    pub trait Config:
        frame_system::Config + pallet_transaction_payment::Config + pallet_authorship::Config
    {
        /// Additional fees to be applied based on the state of the Message Queue
        type MessageQueueExtras: CustomFees<BalanceOf<Self>, CallOf<Self>>;

        /// Type used to actually deposit collected fees
        type FeeDepositor: OnUnbalanced<
            <Self::Currency as Currency<Self::AccountId>>::NegativeImbalance,
        >;

        type Currency: LockableCurrency<Self::AccountId>;
    }

    #[pallet::pallet]
    #[pallet::generate_store(pub(super) trait Store)]
    #[pallet::without_storage_info]
    pub struct Pallet<T>(_);

    #[pallet::type_value]
    pub fn FeeMultiplierOnEmpty() -> Multiplier {
        Multiplier::saturating_from_integer(1)
    }

    #[pallet::storage]
    #[pallet::getter(fn fee_multiplier)]
    pub type FeeMultiplier<T: Config> =
        StorageValue<_, Multiplier, ValueQuery, FeeMultiplierOnEmpty>;

    #[pallet::hooks]
    impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {
        /// Initialization
        fn on_initialize(_bn: BlockNumberFor<T>) -> Weight {
            0_u64
        }

        /// Finalization
        fn on_finalize(_bn: BlockNumberFor<T>) {
            // Update multiplier
            <FeeMultiplier<T>>::mutate(|fm| {
                *fm = Multiplier::saturating_from_integer(10); // TODO: implement proper algorithm
            });
        }
    }
}
