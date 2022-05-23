//           _______________________________ ________
//           \____    /\_   _____/\______   \\_____  \
//             /     /  |    __)_  |       _/ /   |   \
//            /     /_  |        \ |    |   \/    |    \
//           /_______ \/_______  / |____|_  /\_______  /
//                   \/        \/         \/         \/
//           Z  E  R  O  .  I  O     N  E  T  W  O  R  K
//           © C O P Y R I O T   2 0 7 5 @ Z E R O . I O

// This file is part of ZERO Network.
// Copyright (C) 2010-2020 ZERO Labs.
// SPDX-License-Identifier: Apache-2.0

//! DROP
//! Loot token from a treasury

#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;

// TODO:
// #[cfg(test)]
// pub mod mock;

// TODO:
// #[cfg(test)]
// mod tests;

// TODO:
// #[cfg(feature = "runtime-benchmarks")]
// mod benchmarking;

#[frame_support::pallet]
pub mod pallet {

	use super::*;

	use codec::{ HasCompact };
	use scale_info::TypeInfo;
	use sp_std::vec::Vec;

	use sp_runtime::traits::{
		AtLeast32BitUnsigned,
		CheckedAdd,
		CheckedSub,
		Zero
	};

	use frame_support::{
		dispatch::DispatchResult,
		// transactional,
		pallet_prelude::*,
		traits::Randomness,
		traits::Get,
		PalletId,
	};
	use frame_system::{
		ensure_signed,
		pallet_prelude::{ BlockNumberFor, OriginFor },
		WeightInfo,
	};

	use orml_traits::{
		MultiCurrency,
		MultiReservableCurrency
	};



	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	pub struct Pallet<T>(_);



	#[pallet::config]
	pub trait Config: frame_system::Config {

		type Event: From<Event<Self>>
			+ IsType<<Self as frame_system::Config>::Event>
			+ Into<<Self as frame_system::Config>::Event>;
		type Balance: Member
			+ Parameter
			+ AtLeast32BitUnsigned
			+ Default
			+ CheckedAdd
			+ CheckedSub
			+ Copy
			+ Zero
			+ MaybeSerializeDeserialize
			+ MaxEncodedLen
			+ TypeInfo;
		type CurrencyId: Member
			+ Parameter
			+ Default
			+ Copy
			+ HasCompact
			+ MaybeSerializeDeserialize
			+ MaxEncodedLen
			+ TypeInfo;
		type Currency: MultiCurrency<Self::AccountId, CurrencyId = Self::CurrencyId, Balance = Self::Balance>
			+ MultiReservableCurrency<Self::AccountId>;
		type Randomness: Randomness<Self::Hash, Self::BlockNumber>;
		type ForceOrigin: EnsureOrigin<Self::Origin>;
		type WeightInfo: WeightInfo;

		type GameDAOTreasury: Get<Self::AccountId>;

		/// The treasury's pallet id, used for deriving its sovereign account ID.
		#[pallet::constant]
		type PalletId: Get<PalletId>;

		/// The Network's tx currency
		#[pallet::constant]
		type NetworkTokenId: Get<Self::CurrencyId>;
		/// The Network's payment Currency
		#[pallet::constant]
		type PaymentTokenId: Get<Self::CurrencyId>;
		/// The Network's Governance Token
		#[pallet::constant]
		type ProtocolTokenId: Get<Self::CurrencyId>;

		//

		/// Max Drops per block
		#[pallet::constant]
		type MaxDropsPerBlock: Get<u32>;

		// the typical drop interval is configured in the runtime.
		// on abuse, this interval gets a multiplier...

		/// Min Drop Interval
		#[pallet::constant]
		type MinDropInterval: Get<Self::BlockNumber>;

		/// Account Drop Interval
		#[pallet::constant]
		type AccountDropInterval: Get<Self::BlockNumber>;

		#[pallet::constant]
		type DropAmount: Get<Self::Balance>;

	}

	// last drop block of an AccountID
	// or list of accounts received a drop on block x
	#[pallet::storage]
	pub(super) type DropHistory<T: Config> = StorageMap<_, Blake2_128Concat, T::BlockNumber, Vec<T::AccountId>, ValueQuery>;



	/// drop queue for next execution
	#[pallet::storage]
	pub(super) type DropQueue<T: Config> = StorageValue<_, Vec<T::AccountId>, ValueQuery>;
	/// drop queue count
	#[pallet::storage]
	pub(super) type DropQueueCount<T: Config> = StorageValue<_, u32, ValueQuery>;



	/// drop interval
	#[pallet::storage]
	#[pallet::getter(fn drop_interval)]
	pub(super) type DropInterval<T: Config> = StorageValue<_, T::BlockNumber, ValueQuery>;

	/// drop interval multiplier
	#[pallet::storage]
	#[pallet::getter(fn drop_mul)]
	pub(super) type DropMul<T: Config> = StorageValue<_, u64, ValueQuery>;

	/// next drop block
	#[pallet::storage]
	#[pallet::getter(fn next_drop_block)]
	pub(super) type NextDropBlock<T: Config> = StorageValue<_, T::BlockNumber, ValueQuery>;



	/// total rounds
	#[pallet::storage]
	#[pallet::getter(fn total_rounds)]
	pub(super) type TotalRounds<T: Config> = StorageValue<_, u64, ValueQuery>;

	/// total drops
	#[pallet::storage]
	#[pallet::getter(fn total_drops)]
	pub(super) type TotalDrops<T: Config> = StorageValue<_, u64, ValueQuery>;



	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		NextDrop { block: T::BlockNumber },
		LootDrop { from: T::AccountId, to: T::AccountId },
		LootAccept { account: T::AccountId },
		LootReject { account: T::AccountId },
	}



	#[pallet::error]
	pub enum Error<T> {
		/// Guru Meditation at 0x1007 DROP
		GuruMeditation,
		/// Too Many Requests
		TooManyRequests,
		/// Leech attempt detected
		LeechDetected,
		/// Drop Queue Full, try again later
		DropQueueFull,
	}



	#[pallet::call]
	impl<T: Config> Pallet<T> {

		#[pallet::weight((100_000, DispatchClass::Normal, Pays::No))]
		pub fn loot(
			origin: OriginFor<T>,
		) -> DispatchResult {

			ensure!( DropQueueCount::<T>::get() < T::MaxDropsPerBlock::get(), Error::<T>::DropQueueFull );
			let sender = ensure_signed(origin)?;

			// more efficient:
			// block history is sorted by blocknumber -> vec(accountid)
			// so we can simply search in the more recent blocks and
			// eventually truncate all older ones to be more lightweight:
			//
			// get `now`
			// and iterate from `now - AccountDropInterval` to `now`
			// to get all recent leechers
			// so the max number of searches would be `AccountDropInterval * MaxDropsPerBlock`

			// let drop_history = DropHistory::<T>::get();

			// match drop_history.binary_search(&sender) {

			// 	Ok(_) => {

			// 		Err(Error::<T>::MemberExists.into()),

			// 	}

			// 	// no result, just process
			// 	Err(index) => {
			// 		drop_history.insert(index, sender.clone());
			// 		DropHistory::<T>::insert(&org_id, members.clone());

			// 		// counter
			// 		let count = members.len();
			// 		OrgMemberCount::<T>::insert(&org_id, count as u64);

			// 		let mut memberships = Memberships::<T>::get(&account_id);
			// 		memberships.push(org_id.clone());
			// 		Memberships::<T>::insert(&account_id, memberships);

			// 		Ok(())
			// 	}
			// }


			// let last_drop = DropHistory::<T>::get(&sender);
			// let now = <frame_system::Pallet<T>>::block_number();
			// let last_drop_diff = now - last_drop;
			// ensure!( last_drop_diff > T::AccountDropInterval::get(), Error::<T>::LeechDetected );

			// TODO: check if there is an identity for sender?

			Self::add_to_drop_queue( sender );
			Ok(())

		}
	}



	#[pallet::hooks]
	impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {

		fn on_initialize( now: BlockNumberFor<T> ) -> Weight {

			let next = NextDropBlock::<T>::get();
			if now == next  {
				Self::update_drop_block();
			}
			Self::process_drop_queue();
			0

		}

		fn on_finalize( now: T::BlockNumber ) {

			// if requests per block is too high, reject them

			// reset count

		}

	}

	impl<T: Config> Pallet<T> {

		fn add_to_drop_queue(
			account: T::AccountId
		) -> DispatchResult {
			DropQueue::<T>::mutate( |looters| looters.push(account.clone()));
			DropQueueCount::<T>::mutate( |looters| *looters += 1);
			Self::deposit_event(Event::LootAccept {
				account: account,
			});
			Ok(())
		}

		fn process_drop_queue() {
			let treasury = T::GameDAOTreasury::get();
			let to_process = DropQueueCount::<T>::get();
			let loot = T::DropAmount::get();
			let looters = DropQueue::<T>::get();
			let mut flush_looters = looters.clone();

			for looter in &looters {
				let transfer_zero = T::Currency::transfer( T::NetworkTokenId::get(), &treasury, &looter, loot );
				let transfer_game = T::Currency::transfer( T::ProtocolTokenId::get(), &treasury, &looter, loot );
				let transfer_play = T::Currency::transfer( T::PaymentTokenId::get(), &treasury, &looter, loot );

				match flush_looters.binary_search(looter) {
					Ok(index) => {
						flush_looters.remove(index);
					}
					Err(_) => {}
				}

				Self::deposit_event(Event::LootDrop {
					from: treasury.clone(),
					to: looter.clone(),
				});
			}
			DropQueue::<T>::put(flush_looters);
			DropQueueCount::<T>::put(0);
		}

		// fn drop(
		// 	account: T::AccountId
		// ) -> DispatchResult {
		// 	Self::update_drop_count();
		// 	Self::deposit_event(Event::<T>::LootDrop {
		// 		account: account,
		// 	});
		// 	Ok(())
		// }

		fn update_drop_block() -> DispatchResult {
			let block = NextDropBlock::<T>::get();
			let interval = DropInterval::<T>::get();
			let next_block = block + interval;
			NextDropBlock::<T>::put(next_block);
			Self::deposit_event(Event::NextDrop {
				block: next_block,
			});
			Ok(())
		}

		fn update_drop_count() -> u64 {
			let count = TotalDrops::<T>::get();
			TotalDrops::<T>::put(count.wrapping_add(1));
			count
		}

	}

}