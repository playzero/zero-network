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
		pallet_prelude::{ OriginFor },
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

	}

	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		Drop { sender_id: T::AccountId },
	}

	#[pallet::error]
	pub enum Error<T> {
		/// Guru Meditation at 0x1007 DROP
		GuruMeditation,
		/// Too Many Requests
		TooManyRequests,
	}

	#[pallet::call]
	impl<T: Config> Pallet<T> {

		#[pallet::weight((100_000, DispatchClass::Normal, Pays::No))]
		// #[transactional]
		pub fn apply(
			origin: OriginFor<T>,
		) -> DispatchResult {
			let who = ensure_signed(origin)?;
			Self::add_to_next_airdrop( who.clone() );
			Self::deposit_event( Event::<T>::Drop { sender_id: who });
			Ok(())
		}

		#[pallet::hooks]
		impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {
			fn on_finalize(block_number: T::BlockNumber) {
				// process airdrops every n blocks to debounce and dedupe
			}
		}

	}

	impl<T: Config> Pallet<T> {
		// TODO: DISCUSSION
		// withdrawal proposals are accepted
		// when the number of approvals is higher
		// than the number of rejections
		// accepted / denied >= 1
		fn unlock_balance(proposal: &Proposal<T::Hash, T::BlockNumber>, _supported_count: u64) -> Result<(), Error<T>> {

			Self::deposit_event(Event::<T>::WithdrawalGranted {
				proposal_id: proposal.proposal_id,
				campaign_id: proposal.context_id,
				org_id
			});
			Ok(())
		}

}