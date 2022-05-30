//! # Migration pallet
#![cfg_attr(not(feature = "std"), no_std)]

use codec::{Decode, Encode, MaxEncodedLen};
use frame_support::{traits::{Currency, StoredMap, Contains}};
use sp_runtime::AccountId32;
use sp_runtime::{traits::{SaturatedConversion, Zero, StaticLookup}, RuntimeDebug};
use sp_std::{num::ParseIntError};
use serde_json::{Value};
use serde::{Deserialize};
use scale_info::prelude::string::String;
use sp_std::convert::TryInto;
use sp_std::vec::Vec;

pub use pallet::*;
use scale_info::TypeInfo;
use pallet_identity::{Registration, Data, IdentityInfo};
use pallet_balances::AccountData;

pub mod migration;
pub use migration::migrate;

#[cfg(test)]
pub mod mock;
#[cfg(test)]
mod tests;


type BalanceOf<T> = <<T as pallet_identity::Config>::Currency as Currency<
	<T as frame_system::Config>::AccountId,
>>::Balance;

#[derive(Encode, Decode, PartialEq, Clone, TypeInfo)]
pub enum MigrationStatus {
	Inactive,
	Ongoing,
	Complete,
}

#[derive(Encode, Decode, Clone, RuntimeDebug, PartialEq, TypeInfo, MaxEncodedLen)]
pub enum StorageVersion {
	V1Initial,
	V2Imported,
}
impl Default for StorageVersion {
	fn default() -> Self {Self::V1Initial}
}

// Balances structs
#[derive(Deserialize)]
struct AccountItem {
	pub account_id: String,
	pub free_balance: String,
	pub reserved_balance: String,
}

// Identity structs
struct IdentityItem<T: Config> {
	account_id: T::AccountId,
	registration: Registration<BalanceOf<T>, T::MaxRegistrars, T::MaxAdditionalFields>
}

#[frame_support::pallet]
pub mod pallet {
	use frame_support::pallet_prelude::*;
	use frame_support::transactional;
	use frame_system::pallet_prelude::*;
	use sp_std::vec::Vec;

	use super::*;

	#[pallet::pallet]
	#[pallet::generate_store(pub (super) trait Store)]
	pub struct Pallet<T>(_);

	#[pallet::config]
	pub trait Config:
		frame_system::Config
		+ pallet_identity::Config
		+ pallet_balances::Config
		+ orml_tokens::Config
	{
		type Event: From<Event<Self>>
			+ IsType<<Self as frame_system::Config>::Event>
			+ Into<<Self as frame_system::Config>::Event>;
		type ModuleAccounts: Contains<Self::AccountId>;

		#[pallet::constant]
		type ProtocolTokenId: Get<Self::CurrencyId>;
		#[pallet::constant]
		type PaymentTokenId: Get<Self::CurrencyId>;
	}

	#[pallet::hooks]
	impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {
		fn on_runtime_upgrade() -> frame_support::weights::Weight {
			migrate::<T>()
		}
	}

	#[pallet::storage]
	pub(super) type BalancesVersion<T: Config> = StorageValue<_, StorageVersion, ValueQuery, GetDefault>;

	#[pallet::storage]
	pub(super) type IdentityVersion<T: Config> = StorageValue<_, StorageVersion, ValueQuery, GetDefault>;

	#[pallet::storage]
	pub(super) type TokensVersion<T: Config> = StorageValue<_, StorageVersion, ValueQuery, GetDefault>;

	#[pallet::storage]
	pub(super) type MigrationVersion<T: Config> = StorageValue<_, StorageVersion, ValueQuery, GetDefault>;

	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		MigratedBalances(u32),
		MigratedIdentities(u32),
		TokensAirDropped(u32)
	}

	#[pallet::error]
	pub enum Error<T> {
		GuruMeditation,
	}

	#[pallet::call]
	impl<T: Config> Pallet<T> {

		/// Set Tokens (GAME & PLAY) balances with the same amount as native currency has
		#[pallet::weight(5_000_000)]
		pub fn tokens_airdrop(origin: OriginFor<T>) -> DispatchResult {
			ensure_root(origin.clone())?;

			if <TokensVersion<T>>::get() == StorageVersion::V2Imported {
				return Ok(())
			}
			let mut accounts: u32 = 0;

			for (acc_id, _acc_info) in <frame_system::Account::<T>>::iter() {		
				if T::ModuleAccounts::contains(&acc_id) {
					// Skip system (module) accounts: Treasuries, etc.
					continue;
				}		
				// let bal: u128 = acc_info.data.free.saturated_into();	// --> no field `free` on type `AccountData`
				let bal: u128 = <T as pallet_balances::Config>::AccountStore::get(&acc_id).free.saturated_into();
				let balance: <T as orml_tokens::Config>::Balance = bal.saturated_into();
				
				let lookup: <T::Lookup as StaticLookup>::Source = T::Lookup::unlookup(acc_id.clone());
				orml_tokens::Pallet::<T>::set_balance(
					origin.clone(), lookup.clone(), T::ProtocolTokenId::get(),
					balance, Zero::zero()
				);
				orml_tokens::Pallet::<T>::set_balance(
					origin.clone(), lookup.clone(), T::PaymentTokenId::get(),
					balance, Zero::zero()
				);
				accounts += 1;
			}
			<TokensVersion<T>>::set(StorageVersion::V2Imported);

			Self::deposit_event(Event::<T>::TokensAirDropped(accounts));

			Ok(())
		}

		/// Migrates to `Balances` storage from another chain.
		/// Storage: `TotalIssuance`, `AccountStore`
		#[pallet::weight(5_000_000)]
		#[transactional]
		pub fn migrate_balances(
			origin: OriginFor<T>,
			data: Vec<u8>,
		) -> DispatchResult {
			ensure_root(origin)?;
			
			if <BalancesVersion<T>>::get() == StorageVersion::V2Imported {
				return Ok(())
			}

			let data_str = sp_std::str::from_utf8(&data).unwrap();
			let parsed: Vec<AccountItem> = serde_json::from_str(data_str).unwrap();

			for acc in &parsed {
				let account_id = Self::get_account(acc);
				let balance = Self::get_balance(&acc);
				<T as pallet_balances::Config>::AccountStore::insert(&account_id, AccountData { free: balance, ..Default::default() }).unwrap();
				pallet_balances::TotalIssuance::<T>::mutate(|t| *t += balance);
			}
			<BalancesVersion<T>>::set(StorageVersion::V2Imported);

			Self::deposit_event(Event::<T>::MigratedBalances(parsed.len() as u32));

			Ok(())
		}

		/// Migrates to `Identity` storage from another chain.
		/// Storage: `IdentityOf`
		/// `IdentityOf` before using, privacy level should be patched: pub(super) -> pub
		#[pallet::weight(5_000_000)]
		#[transactional]
		pub fn migrate_identities(
			origin: OriginFor<T>,
			data: Vec<u8>,
		) -> DispatchResult {
			ensure_root(origin)?;

			if <IdentityVersion<T>>::get() == StorageVersion::V2Imported {
				return Ok(())
			}

			let data_str = sp_std::str::from_utf8(&data).unwrap();
			let parsed: Vec<Value> = serde_json::from_str(data_str).unwrap();

			for i in &parsed {
				let r = &i["identity"];
				let f = &r["info"];
				let acc_id = i["account_id"].as_str().unwrap();
				let item: IdentityItem<T> = IdentityItem {
					account_id: Self::get_account_id(acc_id),
					registration: Registration {
						judgements: BoundedVec::default(),
						deposit: r["deposit"].as_u64().ok_or(0).unwrap().saturated_into::<BalanceOf<T>>(),
						info: IdentityInfo {
							additional: BoundedVec::default(),
							display: Self::parse_identity_field(&f["display"]),
							pgp_fingerprint: Option::None,
							legal: Self::parse_identity_field(&f["legal"]),
							web: Self::parse_identity_field(&f["web"]),
							riot: Self::parse_identity_field(&f["riot"]),
							email: Self::parse_identity_field(&f["email"]),
							image: Self::parse_identity_field(&f["image"]),
							twitter: Self::parse_identity_field(&f["twitter"]),
						}
					}
				};
				pallet_identity::IdentityOf::<T>::insert(&item.account_id, item.registration);
			}

			<IdentityVersion<T>>::set(StorageVersion::V2Imported);
			
			Self::deposit_event(Event::<T>::MigratedIdentities(parsed.len() as u32));

			Ok(())

		}
	}
}

impl<T: Config> Pallet<T> {
	// Balances helpers
	fn get_balance(item: &AccountItem) -> <T as pallet_balances::Config>::Balance {
		let free: u128 = item.free_balance.parse().unwrap();
		let reserved: u128 = item.reserved_balance.parse().unwrap();
		(free + reserved).saturated_into()
	}

	fn get_account(item: &AccountItem) -> T::AccountId {
		let mut array = [0; 32];
		let mut decoded = bs58::decode(&item.account_id).into_vec().unwrap();
		let cut_address_vec:Vec<u8> = decoded.drain(1..33).collect();
		let bytes = &cut_address_vec[..array.len()];
		array.copy_from_slice(bytes);
		let account32: AccountId32 = array.into();
		let mut to32 = AccountId32::as_ref(&account32);
		let to_address: T::AccountId = T::AccountId::decode(&mut to32).unwrap_or_default();
		to_address
	}

	// Identity helpers
	pub fn decode_hex(s: &str) -> Result<Vec<u8>, ParseIntError> {
		(0..s.len())
			.step_by(2)
			.map(|i| u8::from_str_radix(&s[i..i + 2], 16))
			.collect()
	}
	
	pub fn get_account_id(s: &str) -> T::AccountId {
		let mut array = [0; 32];
		let mut decoded = bs58::decode(s).into_vec().unwrap();
		let cut_address_vec:Vec<u8> = decoded.drain(1..33).collect();
		let bytes = &cut_address_vec[..array.len()];
		array.copy_from_slice(bytes);
		let account32: AccountId32 = array.into();
		let mut to32 = AccountId32::as_ref(&account32);
		let to_address: T::AccountId = T::AccountId::decode(&mut to32).unwrap_or_default();
		to_address
	}
	
	pub fn parse_identity_field(f: &Value) -> Data {
		match &f["raw"] {
			Value::String(s) => {
				Data::Raw(Self::decode_hex(&s[2..]).unwrap().try_into().unwrap())
			},
			_ => {
				Data::None
			}
		}
	}

}
