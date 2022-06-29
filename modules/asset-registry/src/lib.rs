//! # Asset Registry Module
//!
//! Local and foreign assets management. The foreign assets can be updated without runtime upgrade.

#![cfg_attr(not(feature = "std"), no_std)]
#![allow(clippy::unused_unit)]

use frame_support::{
	assert_ok,
	dispatch::DispatchResult,
	ensure,
	pallet_prelude::*,
	traits::{Currency, EnsureOrigin},
	transactional,
	RuntimeDebug,
};
use primitives::{
	currency::{CurrencyIdType, TokenInfo, AssetMetadata, AssetIds},
	CurrencyId,
};
use scale_info::{prelude::format};
use sp_std::{boxed::Box, vec::Vec};


mod mock;
mod tests;
#[cfg(feature = "runtime-benchmarks")]
mod benchmarking;
pub mod weights;

pub use pallet::*;
pub use weights::WeightInfo;

/// Type alias for currency balance.
pub type BalanceOf<T> = <<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;

#[frame_support::pallet]
pub mod pallet {
	use super::*;
	use frame_support::pallet_prelude::*;
	use frame_system::pallet_prelude::*;

	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	pub struct Pallet<T>(_);

	#[pallet::config]
	pub trait Config: frame_system::Config {
		/// The overarching event type.
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;

		/// Currency type for withdraw and balance storage.
		type Currency: Currency<Self::AccountId>;

		/// Required origin for registering asset.
		type RegisterOrigin: EnsureOrigin<Self::Origin>;

		/// Weight information for the extrinsics in this module.
		type WeightInfo: WeightInfo;
	}

	#[pallet::error]
	pub enum Error<T> {
		/// AssetId not exists
		AssetIdNotExists,
		/// AssetId exists
		AssetIdExisted,
	}

	#[pallet::event]
	#[pallet::generate_deposit(fn deposit_event)]
	pub enum Event<T: Config> {
		/// The asset registered.
		AssetRegistered {
			asset_id: AssetIds,
			metadata: AssetMetadata<BalanceOf<T>>,
		},
		/// The asset updated.
		AssetUpdated {
			asset_id: AssetIds,
			metadata: AssetMetadata<BalanceOf<T>>,
		},
	}

	/// The storages for AssetMetadatas.
	///
	/// AssetMetadatas: map AssetIds => Option<AssetMetadata>
	#[pallet::storage]
	#[pallet::getter(fn asset_metadatas)]
	pub type AssetMetadatas<T: Config> =
		StorageMap<_, Twox64Concat, AssetIds, AssetMetadata<BalanceOf<T>>, OptionQuery>;

	#[pallet::genesis_config]
	pub struct GenesisConfig<T: Config> {
		pub assets: Vec<(CurrencyId, BalanceOf<T>)>,
	}

	#[cfg(feature = "std")]
	impl<T: Config> Default for GenesisConfig<T> {
		fn default() -> Self {
			GenesisConfig {
				assets: Default::default(),
			}
		}
	}

	#[pallet::genesis_build]
	impl<T: Config> GenesisBuild<T> for GenesisConfig<T> {
		fn build(&self) {
			self.assets.iter().for_each(|(asset, ed)| {
				assert_ok!(Pallet::<T>::do_register_native_asset(
					*asset,
					&AssetMetadata {
						name: asset.name().unwrap().as_bytes().to_vec(),
						symbol: asset.symbol().unwrap().as_bytes().to_vec(),
						decimals: asset.decimals().unwrap(),
						minimal_balance: *ed,
					}
				));
			});
		}
	}

	#[pallet::call]
	impl<T: Config> Pallet<T> {

		#[pallet::weight(T::WeightInfo::register_native_asset())]
		#[transactional]
		pub fn register_native_asset(
			origin: OriginFor<T>,
			currency_id: CurrencyId,
			metadata: Box<AssetMetadata<BalanceOf<T>>>,
		) -> DispatchResult {
			T::RegisterOrigin::ensure_origin(origin)?;

			Self::do_register_native_asset(currency_id, &metadata)?;

			Self::deposit_event(Event::<T>::AssetRegistered {
				asset_id: AssetIds::NativeAssetId(currency_id),
				metadata: *metadata,
			});
			Ok(())
		}

		#[pallet::weight(T::WeightInfo::update_native_asset())]
		#[transactional]
		pub fn update_native_asset(
			origin: OriginFor<T>,
			currency_id: CurrencyId,
			metadata: Box<AssetMetadata<BalanceOf<T>>>,
		) -> DispatchResult {
			T::RegisterOrigin::ensure_origin(origin)?;

			Self::do_update_native_asset(currency_id, &metadata)?;

			Self::deposit_event(Event::<T>::AssetUpdated {
				asset_id: AssetIds::NativeAssetId(currency_id),
				metadata: *metadata,
			});
			Ok(())
		}
	}
}

impl<T: Config> Pallet<T> {

	fn do_register_native_asset(asset: CurrencyId, metadata: &AssetMetadata<BalanceOf<T>>) -> DispatchResult {
		AssetMetadatas::<T>::try_mutate(
			AssetIds::NativeAssetId(asset),
			|maybe_asset_metadatas| -> DispatchResult {
				ensure!(maybe_asset_metadatas.is_none(), Error::<T>::AssetIdExisted);

				*maybe_asset_metadatas = Some(metadata.clone());
				Ok(())
			},
		)?;

		Ok(())
	}

	fn do_update_native_asset(currency_id: CurrencyId, metadata: &AssetMetadata<BalanceOf<T>>) -> DispatchResult {
		AssetMetadatas::<T>::try_mutate(
			AssetIds::NativeAssetId(currency_id),
			|maybe_asset_metadatas| -> DispatchResult {
				ensure!(maybe_asset_metadatas.is_some(), Error::<T>::AssetIdNotExists);

				*maybe_asset_metadatas = Some(metadata.clone());
				Ok(())
			},
		)
	}
}

