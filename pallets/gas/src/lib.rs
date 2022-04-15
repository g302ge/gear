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

pub use self::imbalances::{NegativeImbalance, PositiveImbalance};
use common::{DAGBasedLedger, Origin};
use frame_support::{dispatch::DispatchError, pallet_prelude::*, traits::Imbalance};
pub use pallet::*;
pub use primitive_types::H256;
use scale_info::TypeInfo;
use sp_std::convert::TryInto;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

#[derive(Clone, Decode, Debug, Encode, MaxEncodedLen, TypeInfo)]
pub enum ValueType {
    External { id: H256, value: u64 },
    SpecifiedLocal { parent: H256, value: u64 },
    UnspecifiedLocal { parent: H256 },
}

#[allow(clippy::derivable_impls)]
// this cannot be derived, despite clippy is saying that!!
impl Default for ValueType {
    fn default() -> Self {
        ValueType::External {
            id: H256::default(),
            value: 0,
        }
    }
}

#[derive(Clone, Default, Decode, Debug, Encode, MaxEncodedLen, TypeInfo)]
pub struct ValueNode {
    pub spec_refs: u32,
    pub unspec_refs: u32,
    pub inner: ValueType,
    pub consumed: bool,
}

impl ValueNode {
    pub fn new(origin: H256, value: u64) -> Self {
        Self {
            inner: ValueType::External { id: origin, value },
            spec_refs: 0,
            unspec_refs: 0,
            consumed: false,
        }
    }

    pub fn inner_value(&self) -> Option<u64> {
        match self.inner {
            ValueType::External { value, .. } => Some(value),
            ValueType::SpecifiedLocal { value, .. } => Some(value),
            ValueType::UnspecifiedLocal { .. } => None,
        }
    }

    pub fn inner_value_mut(&mut self) -> Option<&mut u64> {
        match self.inner {
            ValueType::External { ref mut value, .. } => Some(value),
            ValueType::SpecifiedLocal { ref mut value, .. } => Some(value),
            ValueType::UnspecifiedLocal { .. } => None,
        }
    }

    pub fn parent(&self) -> Option<H256> {
        match self.inner {
            ValueType::External { .. } => None,
            ValueType::SpecifiedLocal { parent, .. } => Some(parent),
            ValueType::UnspecifiedLocal { parent } => Some(parent),
        }
    }

    pub fn refs(&self) -> u32 {
        self.spec_refs.saturating_add(self.unspec_refs)
    }
}

pub type ConsumeResult<T> = Option<(NegativeImbalance<T>, H256)>;

#[frame_support::pallet]
pub mod pallet {
    use super::*;
    use frame_system::pallet_prelude::*;

    #[pallet::config]
    pub trait Config: frame_system::Config {}

    #[pallet::pallet]
    #[pallet::generate_store(pub(super) trait Store)]
    pub struct Pallet<T>(_);

    // Gas pallet error.
    #[pallet::error]
    pub enum Error<T> {
        /// Gas (gas tree) has already been created for the provided key.
        GasTreeAlreadyExists,

        /// Account doesn't have enough funds to complete operation.
        InsufficientBalance,

        /// Value node doesn't exist for a key
        NodeNotFound,
    }

    #[pallet::storage]
    #[pallet::getter(fn total_issuance)]
    pub type TotalIssuance<T> = StorageValue<_, u64, ValueQuery, GetDefault>;

    #[pallet::storage]
    #[pallet::getter(fn get_node)]
    pub type GasTree<T> = StorageMap<_, Identity, H256, ValueNode>;

    #[pallet::hooks]
    impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {
        /// Initialization
        fn on_initialize(_bn: BlockNumberFor<T>) -> Weight {
            0_u64
        }

        /// Finalization
        fn on_finalize(_bn: BlockNumberFor<T>) {}
    }

    impl<T: Config> Pallet<T>
    where
        T::AccountId: Origin,
    {
        /// Check if a node is consumed and does not have any child nodes so it can be deleted.
        /// If the node's type is `ValueType::External`, the locked value is released to the owner.
        /// Otherwise this function is called for the parent node to propagate the process furter.
        pub(super) fn check_consumed(key: H256) -> ConsumeResult<T> {
            let mut delete_current_node = false;
            let res = Self::get_node(key).and_then(|current_node| match current_node.inner {
                ValueType::SpecifiedLocal { parent, .. }
                | ValueType::UnspecifiedLocal { parent } => {
                    if current_node.consumed && current_node.refs() == 0 {
                        let mut parent_node =
                            Self::get_node(parent).expect("Parent node must exist for any node");
                        assert!(
                            parent_node.refs() > 0,
                            "parent node must contain ref to its child node"
                        );

                        if let ValueType::SpecifiedLocal { .. } = current_node.inner {
                            parent_node.spec_refs = parent_node.spec_refs.saturating_sub(1);
                        } else {
                            parent_node.unspec_refs = parent_node.unspec_refs.saturating_sub(1);
                        }

                        GasTree::<T>::mutate(parent, |node| {
                            *node = Some(parent_node);
                        });

                        if let ValueType::SpecifiedLocal {
                            value: self_value, ..
                        } = current_node.inner
                        {
                            // this is specified, so it needs to get to the first specified parent also
                            // going up until external or specified parent is found

                            // NOTE: intentional expect. Every node of type `ValueType::SpecifiedLocal` must
                            // have a parent with a concrete value by definiton of how the tree is constructed.
                            // Therefore, given a valid `parent` key we can take it as an invariant there is
                            // a corresponding node in storage, hence safe to unwrap.
                            // In case this invariant doesn't hold we must stop immediately due to likely
                            // storage corruption.
                            let (parent_key, mut parent_node) = Self::node_with_value(parent)
                                .expect("Upstream node with value always exists for this key");
                            let parent_val = parent_node
                                .inner_value_mut()
                                .expect("Querying parent with value");

                            *parent_val = parent_val.saturating_add(self_value);

                            GasTree::<T>::mutate(parent_key, |value| {
                                *value = Some(parent_node);
                            });
                        }

                        delete_current_node = true;
                        Self::check_consumed(parent)
                    } else {
                        None
                    }
                }
                ValueType::External { id, value } => {
                    if current_node.refs() == 0 && current_node.consumed {
                        delete_current_node = true;
                        Some((NegativeImbalance::new(value), id))
                    } else {
                        None
                    }
                }
            });

            if delete_current_node {
                GasTree::<T>::remove(key);
            }

            res
        }

        /// Returns the AccountId (as Origin) of the value tree creator
        pub(crate) fn node_root_origin(node: &ValueNode) -> H256 {
            match node.inner {
                ValueType::External { id, .. } => id,
                ValueType::SpecifiedLocal { parent, .. }
                | ValueType::UnspecifiedLocal { parent } => {
                    // NOTE: intentional expect. Every node of type `ValueType::SpecifiedLocal`
                    // or `ValueType::UnspecifiedLocal` is guaranted to have a parent.
                    // Given a valid `parent` key we can take it as an invariant there is
                    // a corresponding node in storage, hence safe to unwrap.
                    // In case this invariant doesn't hold we must stop immediately due to likely
                    // storage corruption.
                    Self::node_root_origin(&Self::get_node(parent).expect("Parent should exist"))
                }
            }
        }

        /// The first node from the node corresponding to `key` along the upstream path, including
        /// the node corresponding to `key`, that holds a concrete value. In case the `key` or any
        /// of the node's ancestors keys do not have an associated node in storage, returns `None`.
        pub(crate) fn node_with_value(key: H256) -> Option<(H256, ValueNode)> {
            Self::get_node(key).and_then(|node| {
                if let ValueType::UnspecifiedLocal { parent } = node.inner {
                    Self::node_with_value(parent)
                } else {
                    Some((key, node))
                }
            })
        }
    }
}

impl<T: Config> DAGBasedLedger for Pallet<T>
where
    T::AccountId: Origin,
{
    type ExternalOrigin = H256;
    type Key = H256;
    type Balance = u64;
    type PositiveImbalance = PositiveImbalance<T>;
    type NegativeImbalance = NegativeImbalance<T>;

    fn total_supply() -> u64 {
        Self::total_issuance()
    }

    /// Releases in circulation a certain amount of newly created gas
    fn create(origin: H256, key: H256, amount: u64) -> Result<PositiveImbalance<T>, DispatchError> {
        ensure!(
            !GasTree::<T>::contains_key(key),
            Error::<T>::GasTreeAlreadyExists
        );

        let node = ValueNode::new(origin, amount);

        // Save value node to storage
        GasTree::<T>::insert(key, node);

        Ok(PositiveImbalance::new(amount))
    }

    fn get_origin(key: H256) -> Option<H256> {
        Self::get_node(key).map(|node| Self::node_root_origin(&node))
    }

    fn get_limit(key: H256) -> Option<(u64, H256)> {
        Self::node_with_value(key).map(|(k, node)| {
            let v = node
                .inner_value()
                .expect("The node here is either external or specified, hence the inner value");
            (v, k)
        })
    }

    fn consume(key: H256) -> ConsumeResult<T> {
        let mut delete_current_node = false;
        let mut consume_parent_node = false;
        Self::get_node(key).and_then(|mut node| {
            match node.inner {
                ValueType::UnspecifiedLocal { parent }
                | ValueType::SpecifiedLocal { parent, .. } => {
                    let mut parent_node =
                        Self::get_node(parent).expect("Parent node must exist for any node");
                    assert!(
                        parent_node.refs() > 0,
                        "parent node must contain ref to its child node"
                    );

                    if node.refs() == 0 {
                        delete_current_node = true;

                        if let ValueType::SpecifiedLocal { .. } = node.inner {
                            parent_node.spec_refs = parent_node.spec_refs.saturating_sub(1);
                        } else {
                            parent_node.unspec_refs = parent_node.unspec_refs.saturating_sub(1);
                        }
                    }

                    if parent_node.refs() == 0 {
                        consume_parent_node = true;
                    }

                    if delete_current_node {
                        // Update parent node
                        GasTree::<T>::mutate(parent, |value| {
                            *value = Some(parent_node);
                        });
                    }

                    // Upstream value to the first node that limits value
                    if let ValueType::SpecifiedLocal {
                        value: self_value, ..
                    } = node.inner
                    {
                        if node.unspec_refs == 0 {
                            // NOTE: intentional expect. Every node of type `ValueType::SpecifiedLocal` must
                            // have a parent with a concrete value by definiton of how the tree is constructed.
                            // Therefore, given a valid `parent` key we can take it as an invariant there is
                            // a corresponding node in storage, hence safe to unwrap.
                            // In case this invariant doesn't hold we must stop immediately due to likely
                            // storage corruption.
                            let (parent_key, mut parent_node) = Self::node_with_value(parent)
                                .expect("Upstream node with value always exists for this key");
                            let parent_val = parent_node
                                .inner_value_mut()
                                .expect("Querying parent with value");
                            *parent_val = parent_val.saturating_add(self_value);

                            GasTree::<T>::mutate(parent_key, |value| {
                                *value = Some(parent_node);
                            });
                        }
                    }

                    if delete_current_node {
                        GasTree::<T>::remove(key);
                    } else {
                        node.consumed = true;

                        if node.unspec_refs == 0 {
                            if let Some(inner_value) = node.inner_value_mut() {
                                *inner_value = 0
                            };

                            // Save current node
                            GasTree::<T>::mutate(key, |value| {
                                *value = Some(node);
                            });
                        }
                    }

                    // now check if the parent node can be consumed as well
                    if consume_parent_node {
                        Self::check_consumed(parent)
                    } else {
                        None
                    }
                }
                ValueType::External { id, value } => {
                    node.consumed = true;

                    if node.refs() == 0 {
                        // Delete current node
                        GasTree::<T>::remove(id);

                        Some((NegativeImbalance::new(value), id))
                    } else {
                        // Save current node
                        GasTree::<T>::mutate(key, |n| {
                            *n = Some(node);
                        });

                        None
                    }
                }
            }
        })
    }

    fn spend(key: H256, amount: u64) -> Result<NegativeImbalance<T>, DispatchError> {
        Self::get_node(key).ok_or(Error::<T>::NodeNotFound)?;
        // NOTE: intentional expect. The fact a node exists in storage for the provided `key`
        // has been just checked above.
        let (key, mut node) = Self::node_with_value(key)
            .expect("Upstream node with value always exists for this key");

        ensure!(
            node.inner_value().expect("Querying node with value") >= amount,
            Error::<T>::InsufficientBalance
        );
        *node.inner_value_mut().expect("Querying node with value") -= amount;

        // Save current node
        GasTree::<T>::mutate(key, |value| {
            *value = Some(node);
        });

        Ok(NegativeImbalance::new(amount))
    }

    fn split(key: H256, new_node_key: H256) -> DispatchResult {
        let mut node = Self::get_node(key).ok_or(Error::<T>::NodeNotFound)?;

        node.unspec_refs = node.unspec_refs.saturating_add(1);

        let new_node = ValueNode {
            inner: ValueType::UnspecifiedLocal { parent: key },
            spec_refs: 0,
            unspec_refs: 0,
            consumed: false,
        };

        // Save new node
        GasTree::<T>::insert(new_node_key, new_node);
        // Update current node
        GasTree::<T>::mutate(key, |value| {
            *value = Some(node);
        });

        Ok(())
    }

    fn split_with_value(key: H256, new_node_key: H256, amount: u64) -> DispatchResult {
        let mut node = Self::get_node(key).ok_or(Error::<T>::NodeNotFound)?;
        // NOTE: intentional expect. The fact a node exists in storage for the provided `key`
        // has been just checked above.
        let (_, node_with_value) = Self::node_with_value(key)
            .expect("Upstream node with value always exists for this key");
        ensure!(
            node_with_value
                .inner_value()
                .expect("Querying node with value")
                >= amount,
            Error::<T>::InsufficientBalance
        );

        node.spec_refs = node.spec_refs.saturating_add(1);

        let new_node = ValueNode {
            inner: ValueType::SpecifiedLocal {
                value: amount,
                parent: key,
            },
            spec_refs: 0,
            unspec_refs: 0,
            consumed: false,
        };

        // Save new node
        GasTree::<T>::insert(new_node_key, new_node);
        // Update current node
        GasTree::<T>::mutate(key, |value| {
            *value = Some(node);
        });

        // re-querying it since it might be the same node we already updated above.. :(
        // NOTE: intentional expect. The fact a node exists in storage for the provided `key`
        // has been explicitly checked earlier.
        let (node_key_with_value, mut node_with_value) = Self::node_with_value(key)
            .expect("Upstream node with value always exists for this key");
        ensure!(
            node_with_value
                .inner_value()
                .expect("Querying node with value")
                >= amount,
            Error::<T>::InsufficientBalance
        );
        *node_with_value
            .inner_value_mut()
            .expect("Querying node with value") -= amount;
        GasTree::<T>::mutate(node_key_with_value, |value| {
            *value = Some(node_with_value);
        });

        Ok(())
    }
}

// Wrapping the imbalances in a private module to ensure privacy of the inner members.
mod imbalances {
    use super::{Config, Imbalance};
    use frame_support::{
        traits::{SameOrOther, TryDrop},
        RuntimeDebug,
    };
    use sp_runtime::traits::Zero;
    use sp_std::{marker::PhantomData, mem};

    /// Opaque, move-only struct with private field to denote that value has been created
    /// without any equal and opposite accounting
    #[derive(RuntimeDebug, PartialEq, Eq)]
    pub struct PositiveImbalance<T: Config>(u64, PhantomData<T>);

    impl<T: Config> PositiveImbalance<T> {
        /// Create a new positive imbalance from value amount.
        pub fn new(amount: u64) -> Self {
            PositiveImbalance(amount, PhantomData)
        }
    }

    /// Opaque, move-only struct with private field to denote that value has been destroyed
    /// without any equal and opposite accounting.
    #[derive(RuntimeDebug, PartialEq, Eq)]
    pub struct NegativeImbalance<T: Config>(u64, PhantomData<T>);

    impl<T: Config> NegativeImbalance<T> {
        /// Create a new negative imbalance from value amount.
        pub fn new(amount: u64) -> Self {
            NegativeImbalance(amount, PhantomData)
        }
    }

    impl<T: Config> TryDrop for PositiveImbalance<T> {
        fn try_drop(self) -> Result<(), Self> {
            self.drop_zero()
        }
    }

    impl<T: Config> Default for PositiveImbalance<T> {
        fn default() -> Self {
            Self::zero()
        }
    }

    impl<T: Config> Imbalance<u64> for PositiveImbalance<T> {
        type Opposite = NegativeImbalance<T>;

        fn zero() -> Self {
            Self(Zero::zero(), PhantomData)
        }

        fn drop_zero(self) -> Result<(), Self> {
            if self.0.is_zero() {
                Ok(())
            } else {
                Err(self)
            }
        }

        fn split(self, amount: u64) -> (Self, Self) {
            let first = self.0.min(amount);
            let second = self.0 - first;

            mem::forget(self);
            (Self(first, PhantomData), Self(second, PhantomData))
        }

        fn merge(mut self, other: Self) -> Self {
            self.0 = self.0.saturating_add(other.0);
            mem::forget(other);

            self
        }

        fn subsume(&mut self, other: Self) {
            self.0 = self.0.saturating_add(other.0);
            mem::forget(other);
        }

        #[allow(clippy::comparison_chain)]
        fn offset(self, other: Self::Opposite) -> SameOrOther<Self, Self::Opposite> {
            let (a, b) = (self.0, other.0);
            mem::forget((self, other));

            if a > b {
                SameOrOther::Same(Self(a - b, PhantomData))
            } else if b > a {
                SameOrOther::Other(NegativeImbalance::new(b - a))
            } else {
                SameOrOther::None
            }
        }

        fn peek(&self) -> u64 {
            self.0
        }
    }

    impl<T: Config> TryDrop for NegativeImbalance<T> {
        fn try_drop(self) -> Result<(), Self> {
            self.drop_zero()
        }
    }

    impl<T: Config> Default for NegativeImbalance<T> {
        fn default() -> Self {
            Self::zero()
        }
    }

    impl<T: Config> Imbalance<u64> for NegativeImbalance<T> {
        type Opposite = PositiveImbalance<T>;

        fn zero() -> Self {
            Self(Zero::zero(), PhantomData)
        }

        fn drop_zero(self) -> Result<(), Self> {
            if self.0.is_zero() {
                Ok(())
            } else {
                Err(self)
            }
        }

        fn split(self, amount: u64) -> (Self, Self) {
            let first = self.0.min(amount);
            let second = self.0 - first;

            mem::forget(self);
            (Self(first, PhantomData), Self(second, PhantomData))
        }

        fn merge(mut self, other: Self) -> Self {
            self.0 = self.0.saturating_add(other.0);
            mem::forget(other);

            self
        }

        fn subsume(&mut self, other: Self) {
            self.0 = self.0.saturating_add(other.0);
            mem::forget(other);
        }

        #[allow(clippy::comparison_chain)]
        fn offset(self, other: Self::Opposite) -> SameOrOther<Self, Self::Opposite> {
            let (a, b) = (self.0, other.0);
            mem::forget((self, other));

            if a > b {
                SameOrOther::Same(Self(a - b, PhantomData))
            } else if b > a {
                SameOrOther::Other(PositiveImbalance::new(b - a))
            } else {
                SameOrOther::None
            }
        }

        fn peek(&self) -> u64 {
            self.0
        }
    }

    impl<T: Config> Drop for PositiveImbalance<T> {
        /// Basic drop handler will just square up the total issuance.
        fn drop(&mut self) {
            <super::TotalIssuance<T>>::mutate(|v| *v = v.saturating_add(self.0));
        }
    }

    impl<T: Config> Drop for NegativeImbalance<T> {
        /// Basic drop handler will just square up the total issuance.
        fn drop(&mut self) {
            <super::TotalIssuance<T>>::mutate(|v| {
                if self.0 > *v {
                    log::warn!(
                        "Unaccounted gas detected: burnt {:?}, known total supply was {:?}.",
                        self.0,
                        *v
                    )
                }
                *v = v.saturating_sub(self.0)
            });
        }
    }
}
