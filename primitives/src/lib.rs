//
//           _______________________________ ________
//           \____    /\_   _____/\______   \\_____  \
//             /     /  |    __)_  |       _/ /   |   \
//            /     /_  |        \ |    |   \/    |    \
//           /_______ \/_______  / |____|_  /\_______  /
//                   \/        \/         \/         \/
//           Z  E  R  O  .  I  O     N  E  T  W  O  R  K
//           © C O P Y R I O T   2 0 7 5 @ Z E R O . I O

// This file is part of ZERO Network.
// Copyright (C) 2010-2020 ZERO Collective.
// SPDX-License-Identifier: Apache-2.0

//! Low-level types used throughout the Substrate code.

// #![warn(missing_docs)]

#![cfg_attr(not(feature = "std"), no_std)]

// pub mod currency;

use codec::{Decode, Encode};
use sp_runtime::{
	generic,
	traits::{BlakeTwo256, IdentifyAccount, Verify},
	MultiSignature, OpaqueExtrinsic, RuntimeDebug,
};

#[cfg(feature = "std")]
use serde::{Deserialize, Serialize};

/// An index to a block.
pub type BlockNumber = u32;

/// Alias to the public key used for this chain, actually a `MultiSigner`. Like
/// the signature, this also isn't a fixed size when encoded, as different
/// cryptos have different size public keys.
pub type AccountPublic = <Signature as Verify>::Signer;

/// Alias to the opaque account ID type for this chain, actually a
/// `AccountId32`. This is always 32 bytes.
pub type AccountId = <AccountPublic as IdentifyAccount>::AccountId;

/// The type for looking up accounts. We don't expect more than 4 billion of
/// them.
pub type AccountIndex = u32;

/// The address format for describing accounts.
pub type Address = sp_runtime::MultiAddress<AccountId, AccountIndex>;

/// Index of a transaction in the chain. 32-bit should be plenty.
pub type Nonce = u32;

/// A hash of some data used by the chain.
pub type Hash = sp_core::H256;

/// An instant or duration in time.
pub type Moment = u64;

/// Counter for the number of eras that have passed.
pub type EraIndex = u32;

/// Balance of an account.
pub type Balance = u128;

/// Signed version of Balance
pub type Amount = i128;

/// Index of a transaction in the chain.
pub type Index = u32;

/// Header type.
pub type Header = generic::Header<BlockNumber, BlakeTwo256>;

/// Block type.
pub type Block = generic::Block<Header, UncheckedExtrinsic>;

/// Block ID.
pub type BlockId = generic::BlockId<Block>;

/// Opaque, encoded, unchecked extrinsic.
pub use sp_runtime::OpaqueExtrinsic as UncheckedExtrinsic;

/// Alias to 512-bit hash when used in the context of a transaction signature on the chain.
pub type Signature = MultiSignature;

/// A timestamp: milliseconds since the unix epoch.
/// `u64` is enough to represent a duration of half a billion years, when the
/// time scale is milliseconds.
pub type Timestamp = u64;

/// Currency Id type
pub type CurrencyId = u32;
/// Group collection id type
pub type GroupCollectionId = u64;
/// AssetId for all NFT and FT
pub type AssetId = u64;
/// RealmId
pub type RealmId = u64;
/// NFT Balance
pub type NFTBalance = u128;

#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, RuntimeDebug, PartialOrd, Ord)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum TokenSymbol {
	ZERO = 0,
	PLAY = 1,
	GAME = 2,
	DOT = 3,
	KSM = 4,
	DAI = 5,
	EUR = 6,
	HYPE = 7,
	FUEL = 8,
}

#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, RuntimeDebug, PartialOrd, Ord)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum AirDropCurrencyId {
	ZERO = 0,
	PLAY = 1,
	GAME = 2,
}

// bodies

#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, RuntimeDebug, PartialOrd, Ord)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum BodyType {
	INDIVIDUAL = 0, // individual address
	COMPANY = 1,    // ...with a legal body
	DAO = 2,        // ...governed by a dao
}

#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, RuntimeDebug, PartialOrd, Ord)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum BodyState {
	INACTIVE = 0,
	ACTIVE = 1,
	LOCKED = 2,
}

// roles

#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, RuntimeDebug, PartialOrd, Ord)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum AuthoritysOriginId {
	Root,
	ZeroTreasury,
	GameDAOTreasury,
	SenseNet,
}

//
//	s e n s e
//

#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, RuntimeDebug, PartialOrd, Ord)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum SenseProps {
	XP = 0,
	REP = 1,
	TRUST = 2,
}

//
//	g o v e r n a n c e
//

#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum ProposalType {
	PROPOSAL = 0,
	TREASURY = 1,
	MEMBERSHIP = 2,
}

#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum VotingType {
	WEIGHTED = 0,
	DEMOCRATIC = 1,
	QUADRATIC = 2,
	CONVICTION = 3,
}

#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum ProposalState {
	LOCK = 0,
	OPEN = 1,
	ACK = 2,
	NACK = 3,
	TERM = 4,
	DONE = 5,
}

//
//
//

// / App-specific crypto used for reporting equivocation/misbehavior in BABE and
// / GRANDPA. Any rewards for misbehavior reporting will be paid out to this
// / account.

// pub mod report {
// 	use super::{Signature, Verify};
// 	use frame_system::offchain::AppCrypto;
// 	use sp_core::crypto::{key_types, KeyTypeId};

// 	/// Key type for the reporting module. Used for reporting BABE and GRANDPA
// 	/// equivocations.
// 	pub const KEY_TYPE: KeyTypeId = key_types::REPORTING;

// 	mod app {
// 		use sp_application_crypto::{app_crypto, sr25519};
// 		app_crypto!(sr25519, super::KEY_TYPE);
// 	}

// 	/// Identity of the equivocation/misbehavior reporter.
// 	pub type ReporterId = app::Public;

// 	/// An `AppCrypto` type to allow submitting signed transactions using the reporting
// 	/// application key as signer.
// 	pub struct ReporterAppCrypto;

// 	impl AppCrypto<<Signature as Verify>::Signer, Signature> for ReporterAppCrypto {
// 		type RuntimeAppPublic = ReporterId;
// 		type GenericSignature = sp_core::sr25519::Signature;
// 		type GenericPublic = sp_core::sr25519::Public;
// 	}
// }
