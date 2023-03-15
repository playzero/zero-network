pub mod time {
	use primitives::{
		currency::ZERO,
		cent,
		Balance, BlockNumber, Moment,
	};

	/// This determines the average expected block time that we are targeting.
	/// Blocks will be produced at a minimum duration defined by `SLOT_DURATION`.
	/// `SLOT_DURATION` is picked up by `pallet_timestamp` which is in turn picked
	/// up by `pallet_aura` to implement `fn slot_duration()`.
	///
	/// Change this to adjust the block time.
	pub const SECS_PER_BLOCK: Moment = 12;
	pub const MILLISECS_PER_BLOCK: Moment = SECS_PER_BLOCK * 1000;

	// These time units are defined in number of blocks.
	pub const MINUTES: BlockNumber = 60 / (SECS_PER_BLOCK as BlockNumber);
	pub const HOURS: BlockNumber = MINUTES * 60;
	pub const DAYS: BlockNumber = HOURS * 24;

	// NOTE: Currently it is not possible to change the slot duration after the chain has started.
	//       Attempting to do so will brick block production.
	pub const SLOT_DURATION: Moment = MILLISECS_PER_BLOCK;

	pub fn deposit(items: u32, bytes: u32) -> Balance {
		items as Balance * 15 * cent(ZERO) + (bytes as Balance) * 6 * cent(ZERO)
	}
}

pub mod fee {
	use frame_support::weights::{
		constants::{ExtrinsicBaseWeight, WEIGHT_REF_TIME_PER_SECOND},
		WeightToFeePolynomial,
		WeightToFeeCoefficients,
		WeightToFeeCoefficient
	};
	use smallvec::smallvec;
	use sp_runtime::Perbill;
	use primitives::{
		currency::{TokenSymbol, DOT, ZERO, GAME, PLAY},
		Balance,
		cent
	};

	pub fn base_tx_in_token(token: TokenSymbol) -> Balance {
		match token {
			TokenSymbol::DOT => cent(DOT) * 30 / 10000,
			// ZERO:DOT = 6:1
			TokenSymbol::ZERO => cent(ZERO) * 18 / 1000,
			// GAME:DOT = 6:1
			TokenSymbol::GAME => cent(GAME) * 18 / 1000,
			// PLAY:DOT = 6:1
			TokenSymbol::PLAY => cent(PLAY) * 18 / 1000,
		}
	}

	pub fn base_tx_per_second() -> u128 {
		let base_weight = Balance::from(ExtrinsicBaseWeight::get().ref_time());
	    (WEIGHT_REF_TIME_PER_SECOND as u128) / base_weight
	}

	pub fn dot_per_second() -> u128 {
	    base_tx_per_second() * base_tx_in_token(TokenSymbol::DOT)
	}

	pub fn zero_per_second() -> u128 {
		base_tx_per_second() * base_tx_in_token(TokenSymbol::ZERO)
	}

	pub fn game_per_second() -> u128 {
		base_tx_per_second() * base_tx_in_token(TokenSymbol::GAME)
	}

	pub fn play_per_second() -> u128 {
		base_tx_per_second() * base_tx_in_token(TokenSymbol::PLAY)
	}

	/// Handles converting a weight scalar to a fee value, based on the scale and granularity of the
	/// node's balance type.
	///
	/// This should typically create a mapping between the following ranges:
	///   - `[0, MAXIMUM_BLOCK_WEIGHT]`
	///   - `[Balance::min, Balance::max]`
	///
	/// Yet, it can be used for any other sort of change to weight-fee. Some examples being:
	///   - Setting it to `0` will essentially disable the weight fee.
	///   - Setting it to `1` will cause the literal `#[weight = x]` values to be charged.
	pub struct WeightToFee;
	impl WeightToFeePolynomial for WeightToFee {
		type Balance = Balance;
		fn polynomial() -> WeightToFeeCoefficients<Self::Balance> {
			let p = base_tx_in_token(TokenSymbol::ZERO);
			let q = Balance::from(ExtrinsicBaseWeight::get().ref_time());
			smallvec![WeightToFeeCoefficient {
				degree: 1,
				negative: false,
				coeff_frac: Perbill::from_rational(p % q, q),
				coeff_integer: p / q,
			}]
		}
	}
}

pub mod parachains {
	pub mod acala {
		pub const ID: u32 = 2000;
	}
}
