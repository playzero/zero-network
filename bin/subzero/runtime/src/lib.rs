#![cfg_attr(not(feature = "std"), no_std)]
// `construct_runtime!` does a lot of recursion and requires us to increase the limit to 256.
#![recursion_limit = "256"]

// Make the WASM binary available.
#[cfg(feature = "std")]
include!(concat!(env!("OUT_DIR"), "/wasm_binary.rs"));

mod weights;
pub mod xcm_config;
pub mod constants;

use codec::{Decode, Encode, MaxEncodedLen};
use cumulus_pallet_parachain_system::RelayNumberStrictlyIncreases;
use sp_api::impl_runtime_apis;
use sp_core::{crypto::KeyTypeId, OpaqueMetadata};
use sp_runtime::{
	create_runtime_str, generic, impl_opaque_keys,
	traits::{AccountIdConversion, AccountIdLookup, BlakeTwo256, Block as BlockT, Convert, IdentifyAccount, Verify},
	transaction_validity::{TransactionSource, TransactionValidity},
	ApplyExtrinsicResult, MultiSignature, Percent
};

use sp_std::{collections::btree_set::BTreeSet, prelude::*};
#[cfg(feature = "std")]
use sp_version::NativeVersion;
use sp_version::RuntimeVersion;
use static_assertions::const_assert;

use frame_support::{
	construct_runtime, parameter_types,
	pallet_prelude::RuntimeDebug,
	traits::{
		tokens::nonfungibles::*,
		AsEnsureOriginWithArg, ConstU32, Contains, EitherOfDiverse,
		EnsureOrigin, EnsureOriginWithArg, EqualPrivilegeOnly, InstanceFilter,
		LockIdentifier, U128CurrencyToVote
	},
	weights::{
		constants::WEIGHT_REF_TIME_PER_SECOND, ConstantMultiplier, Weight,
	},
	BoundedVec, PalletId,
	dispatch::DispatchClass,
};
use frame_system::{
	limits::{BlockLength, BlockWeights},
	EnsureRoot, EnsureSigned
};
pub use sp_consensus_aura::sr25519::AuthorityId as AuraId;
pub use sp_runtime::{DispatchError, MultiAddress, Perbill, Permill};

#[cfg(any(feature = "std", test))]
pub use sp_runtime::BuildStorage;

// Polkadot imports
use polkadot_runtime_common::{BlockHashCount, SlowAdjustingFeeUpdate};

use weights::{BlockExecutionWeight, ExtrinsicBaseWeight, RocksDbWeight};

// XCM Imports
use xcm::latest::prelude::BodyId;

pub use constants::{fee::*, time::*};
pub use primitives::{
	currency::{ZERO, PLAY, GAME, DOT, AssetIds, CurrencyId, CustomMetadata, ForeignAssetId, TokenSymbol},
	dollar, cent, millicent,
	Amount, ReserveIdentifier
};

use orml_asset_registry::SequentialId;
use orml_currencies::BasicCurrencyAdapter;
use orml_traits::{parameter_type_with_key, GetByKey};

use pallet_rmrk_core::{CollectionInfoOf, InstanceInfoOf, PropertyInfoOf, ResourceInfoOf};
use pallet_rmrk_equip::{BaseInfoOf, BoundedThemeOf, PartTypeOf};
use rmrk_traits::{
	primitives::{BaseId, CollectionId, NftId, ResourceId},
	NftChild,
};

/// Alias to 512-bit hash when used in the context of a transaction signature on the chain.
pub type Signature = MultiSignature;

/// Some way of identifying an account on the chain. We intentionally make it equivalent
/// to the public key of our transaction signing scheme.
pub type AccountId = <<Signature as Verify>::Signer as IdentifyAccount>::AccountId;

/// Balance of an account.
pub type Balance = u128;

/// Index of a transaction in the chain.
pub type Index = u32;

/// A hash of some data used by the chain.
pub type Hash = sp_core::H256;

/// An index to a block.
pub type BlockNumber = u32;

/// The address format for describing accounts.
pub type Address = MultiAddress<AccountId, ()>;

/// Block header type as expected by this runtime.
pub type Header = generic::Header<BlockNumber, BlakeTwo256>;

/// Block type as expected by this runtime.
pub type Block = generic::Block<Header, UncheckedExtrinsic>;

/// A Block signed with a Justification
pub type SignedBlock = generic::SignedBlock<Block>;

/// BlockId type as expected by this runtime.
pub type BlockId = generic::BlockId<Block>;

/// The SignedExtension to the basic transaction logic.
pub type SignedExtra = (
	frame_system::CheckNonZeroSender<Runtime>,
	frame_system::CheckSpecVersion<Runtime>,
	frame_system::CheckTxVersion<Runtime>,
	frame_system::CheckGenesis<Runtime>,
	frame_system::CheckEra<Runtime>,
	frame_system::CheckNonce<Runtime>,
	frame_system::CheckWeight<Runtime>,
	pallet_transaction_payment::ChargeTransactionPayment<Runtime>,
);

/// Unchecked extrinsic type as expected by this runtime.
pub type UncheckedExtrinsic = generic::UncheckedExtrinsic<Address, RuntimeCall, Signature, SignedExtra>;

/// Extrinsic type that has already been checked.
pub type CheckedExtrinsic = generic::CheckedExtrinsic<AccountId, RuntimeCall, SignedExtra>;

/// Executive: handles dispatch to the various modules.
pub type Executive = frame_executive::Executive<
	Runtime,
	Block,
	frame_system::ChainContext<Runtime>,
	Runtime,
	AllPalletsWithSystem,
>;

/// Opaque types. These are used by the CLI to instantiate machinery that don't need to know
/// the specifics of the runtime. They can then be made to be agnostic over specific formats
/// of data like extrinsics, allowing for them to continue syncing the network through upgrades
/// to even the core data structures.
pub mod opaque {
	use super::*;
	use sp_runtime::{generic, traits::BlakeTwo256};

	pub use sp_runtime::OpaqueExtrinsic as UncheckedExtrinsic;
	/// Opaque block header type.
	pub type Header = generic::Header<BlockNumber, BlakeTwo256>;
	/// Opaque block type.
	pub type Block = generic::Block<Header, UncheckedExtrinsic>;
	/// Opaque block identifier type.
	pub type BlockId = generic::BlockId<Block>;
}

impl_opaque_keys! {
	pub struct SessionKeys {
		pub aura: Aura,
	}
}

#[sp_version::runtime_version]
pub const VERSION: RuntimeVersion = RuntimeVersion {
	spec_name: create_runtime_str!("subzero"),
	impl_name: create_runtime_str!("live"),
	authoring_version: 75,
	spec_version: 63,
	impl_version: 0,
	apis: RUNTIME_API_VERSIONS,
	transaction_version: 1,
	state_version: 1,
};

// Unit = the base number of indivisible units for balances
pub const UNIT: Balance = 1_000_000_000_000;
pub const MILLIUNIT: Balance = 1_000_000_000;
pub const MICROUNIT: Balance = 1_000_000;

/// The existential deposit. Set to 1/10 of the Connected Relay Chain.
pub const EXISTENTIAL_DEPOSIT: Balance = MILLIUNIT;

/// We assume that ~5% of the block weight is consumed by `on_initialize` handlers. This is
/// used to limit the maximal weight of a single extrinsic.
const AVERAGE_ON_INITIALIZE_RATIO: Perbill = Perbill::from_percent(5);

/// We allow `Normal` extrinsics to fill up the block up to 75%, the rest can be used by
/// `Operational` extrinsics.
const NORMAL_DISPATCH_RATIO: Perbill = Perbill::from_percent(75);

const MAX_POV_SIZE: u64 = 5 * 1024 * 1024;

/// We allow for 0.5 of a second of compute with a 12 second average block time.
const MAXIMUM_BLOCK_WEIGHT: Weight = Weight::from_parts(WEIGHT_REF_TIME_PER_SECOND / 2u64, MAX_POV_SIZE);

/// The version information used to identify this runtime when compiled natively.
#[cfg(feature = "std")]
pub fn native_version() -> NativeVersion {
	NativeVersion {
		runtime_version: VERSION,
		can_author_with: Default::default(),
	}
}

parameter_types! {
	pub const Version: RuntimeVersion = VERSION;

	// This part is copied from Substrate's `bin/node/runtime/src/lib.rs`.
	//  The `RuntimeBlockLength` and `RuntimeBlockWeights` exist here because the
	// `DeletionWeightLimit` and `DeletionQueueDepth` depend on those to parameterize
	// the lazy contract deletion.
	pub RuntimeBlockLength: BlockLength =
		BlockLength::max_with_normal_ratio(5 * 1024 * 1024, NORMAL_DISPATCH_RATIO);
	pub RuntimeBlockWeights: BlockWeights = BlockWeights::builder()
		.base_block(BlockExecutionWeight::get())
		.for_class(DispatchClass::all(), |weights| {
			weights.base_extrinsic = ExtrinsicBaseWeight::get();
		})
		.for_class(DispatchClass::Normal, |weights| {
			weights.max_total = Some(NORMAL_DISPATCH_RATIO * MAXIMUM_BLOCK_WEIGHT);
		})
		.for_class(DispatchClass::Operational, |weights| {
			weights.max_total = Some(MAXIMUM_BLOCK_WEIGHT);
			// Operational transactions have some extra reserved space, so that they
			// are included even if block reached `MAXIMUM_BLOCK_WEIGHT`.
			weights.reserved = Some(
				MAXIMUM_BLOCK_WEIGHT - NORMAL_DISPATCH_RATIO * MAXIMUM_BLOCK_WEIGHT
			);
		})
		.avg_block_initialization(AVERAGE_ON_INITIALIZE_RATIO)
		.build_or_panic();
	pub const SS58Prefix: u16 = 25;
}

pub struct BaseFilter;
impl Contains<RuntimeCall> for BaseFilter {
	fn contains(call: &RuntimeCall) -> bool {
		// Disable direct calls to pallet_uniques
		!matches!(
			call,
			RuntimeCall::Uniques(pallet_uniques::Call::approve_transfer { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::burn { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::cancel_approval { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::clear_collection_metadata { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::clear_metadata { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::create { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::destroy { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::force_item_status { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::force_create { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::freeze_collection { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::mint { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::redeposit { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::set_collection_metadata { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::thaw_collection { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::transfer { .. }) |
			RuntimeCall::Uniques(pallet_uniques::Call::transfer_ownership { .. })
		)
	}
}

// Configure FRAME pallets to include in runtime.

impl frame_system::Config for Runtime {
	/// The identifier used to distinguish between accounts.
	type AccountId = AccountId;
	/// The aggregated dispatch type that is available for extrinsics.
	type RuntimeCall = RuntimeCall;
	/// The lookup mechanism to get account ID from whatever is passed in dispatchers.
	type Lookup = AccountIdLookup<AccountId, ()>;
	/// The index type for storing how many extrinsics an account has signed.
	type Index = Index;
	/// The index type for blocks.
	type BlockNumber = BlockNumber;
	/// The type for hashing blocks and tries.
	type Hash = Hash;
	/// The hashing algorithm used.
	type Hashing = BlakeTwo256;
	/// The header type.
	type Header = generic::Header<BlockNumber, BlakeTwo256>;
	/// The ubiquitous event type.
	type RuntimeEvent = RuntimeEvent;
	/// The ubiquitous origin type.
	type RuntimeOrigin = RuntimeOrigin;
	/// Maximum number of block number to block hash mappings to keep (oldest pruned first).
	type BlockHashCount = BlockHashCount;
	/// Runtime version.
	type Version = Version;
	/// Converts a module to an index of this module in the runtime.
	type PalletInfo = PalletInfo;
	/// The data to be stored in an account.
	type AccountData = pallet_balances::AccountData<Balance>;
	/// What to do if a new account is created.
	type OnNewAccount = ();
	/// What to do if an account is fully reaped from the system.
	type OnKilledAccount = ();
	/// The weight of database operations that the runtime can invoke.
	type DbWeight = RocksDbWeight;
	/// The basic call filter to use in dispatchable.
	type BaseCallFilter = BaseFilter;
	/// Weight information for the extrinsics of this pallet.
	type SystemWeightInfo = ();
	/// Block & extrinsics weights: base values and limits.
	type BlockWeights = RuntimeBlockWeights;
	/// The maximum length of a block (in bytes).
	type BlockLength = RuntimeBlockLength;
	/// This is used as an identifier of the chain. 42 is the generic substrate prefix.
	type SS58Prefix = SS58Prefix;
	/// The action to take on a Runtime Upgrade
	type OnSetCode = cumulus_pallet_parachain_system::ParachainSetCode<Self>;
	type MaxConsumers = ConstU32<16>;
}

parameter_types! {
	pub const MinimumPeriod: u64 = SLOT_DURATION / 2;
}

impl pallet_timestamp::Config for Runtime {
	/// A timestamp: milliseconds since the unix epoch.
	type Moment = u64;
	type OnTimestampSet = ();
	type MinimumPeriod = MinimumPeriod;
	type WeightInfo = ();
}

parameter_types! {
	pub const UncleGenerations: u32 = 0;
}

impl pallet_authorship::Config for Runtime {
	type FindAuthor = pallet_session::FindAccountFromAuthorIndex<Self, Aura>;
	type UncleGenerations = UncleGenerations;
	type FilterUncle = ();
	type EventHandler = (CollatorSelection,);
}

parameter_types! {
	pub ExistentialDeposit: Balance = ExistentialDeposits::get(&ZERO);
	pub const MaxLocks: u32 = 50;
	pub const MaxReserves: u32 = ReserveIdentifier::Count as u32;
}

impl pallet_balances::Config for Runtime {
	type MaxLocks = MaxLocks;
	/// The type for recording an account's balance.
	type Balance = Balance;
	/// The ubiquitous event type.
	type RuntimeEvent = RuntimeEvent;
	type DustRemoval = ();
	type ExistentialDeposit = ExistentialDeposit;
	type AccountStore = System;
	type WeightInfo = pallet_balances::weights::SubstrateWeight<Runtime>;
	type MaxReserves = MaxReserves;
	type ReserveIdentifier = ReserveIdentifier;
}

parameter_types! {
	/// Relay Chain `TransactionByteFee` / 10
	pub const TransactionByteFee: Balance = 10 * MICROUNIT;
	pub const OperationalFeeMultiplier: u8 = 5;
}

impl pallet_transaction_payment::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type OnChargeTransaction = pallet_transaction_payment::CurrencyAdapter<Balances, ()>;
	type WeightToFee = WeightToFee;
	type LengthToFee = ConstantMultiplier<Balance, TransactionByteFee>;
	type FeeMultiplierUpdate = SlowAdjustingFeeUpdate<Self>;
	type OperationalFeeMultiplier = OperationalFeeMultiplier;
}

parameter_types! {
	// One storage item; key size is 32; value is size 4+4+16+32 bytes = 56 bytes.
	pub MultisigDepositBase: Balance = deposit(1, 88);
	// Additional storage item size of 32 bytes.
	pub MultisigDepositFactor: Balance = deposit(0, 32);
}

impl pallet_multisig::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type RuntimeCall = RuntimeCall;
	type Currency = Balances;
	type DepositBase = MultisigDepositBase;
	type DepositFactor = MultisigDepositFactor;
	type MaxSignatories = ConstU32<100>;
	type WeightInfo = pallet_multisig::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
	pub MaximumSchedulerWeight: Weight = Perbill::from_percent(80) *
		RuntimeBlockWeights::get().max_block;
	// Retry a scheduled item every 10 blocks (1 minute) until the preimage exists.
	pub const NoPreimagePostponement: Option<u32> = Some(10);
}

impl pallet_scheduler::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type RuntimeOrigin = RuntimeOrigin;
	type PalletsOrigin = OriginCaller;
	type RuntimeCall = RuntimeCall;
	type MaximumWeight = MaximumSchedulerWeight;
	type ScheduleOrigin = EnsureRoot<AccountId>;
	type MaxScheduledPerBlock = ConstU32<50>;
	type WeightInfo = pallet_scheduler::weights::SubstrateWeight<Runtime>;
	type OriginPrivilegeCmp = EqualPrivilegeOnly;
	type Preimages = Preimage;
}

parameter_types! {
	pub PreimageBaseDeposit: Balance = 1 * dollar(ZERO);
	// One cent: $10,000 / MB
	pub PreimageByteDeposit: Balance = 1 * cent(ZERO);
}

impl pallet_preimage::Config for Runtime {
	type WeightInfo = pallet_preimage::weights::SubstrateWeight<Runtime>;
	type RuntimeEvent = RuntimeEvent;
	type Currency = Balances;
	type ManagerOrigin = EnsureRoot<AccountId>;
	type BaseDeposit = PreimageBaseDeposit;
	type ByteDeposit = PreimageByteDeposit;
}

parameter_types! {
	// One storage item; key size 32, value size 8; .
	pub ProxyDepositBase: Balance = deposit(1, 8);
	// Additional storage item size of 33 bytes.
	pub ProxyDepositFactor: Balance = deposit(0, 33);
	pub AnnouncementDepositBase: Balance = deposit(1, 8);
	pub AnnouncementDepositFactor: Balance = deposit(0, 66);
}

#[derive(Copy, Clone, Eq, PartialEq, Ord,PartialOrd, Encode, Decode, RuntimeDebug, MaxEncodedLen, scale_info::TypeInfo)]
pub enum ProxyType {
	Any,
	NonTransfer,
	Governance,
}
impl Default for ProxyType {
	fn default() -> Self {
		Self::Any
	}
}
impl InstanceFilter<RuntimeCall> for ProxyType {
	fn filter(&self, c: &RuntimeCall) -> bool {
		match self {
			ProxyType::Any => true,
			ProxyType::NonTransfer => !matches!(
				c,
				RuntimeCall::Balances(..) |
				RuntimeCall::Uniques(..) |
				RuntimeCall::Currencies(..) |
				RuntimeCall::Tokens(..)
			),
			ProxyType::Governance => matches!(
				c,
				RuntimeCall::Democracy(..) |
				RuntimeCall::Council(..) |
				RuntimeCall::TechnicalCommittee(..) |
				RuntimeCall::Treasury(..) |
				RuntimeCall::Bounties(..) | RuntimeCall::ChildBounties(..) |
				RuntimeCall::Utility(..) |
				RuntimeCall::Elections(..)
			)
		}
	}
	fn is_superset(&self, o: &Self) -> bool {
		match (self, o) {
			(x, y) if x == y => true,
			(ProxyType::Any, _) => true,
			(_, ProxyType::Any) => false,
			(ProxyType::NonTransfer, _) => true,
			_ => false,
		}
	}
}

impl pallet_proxy::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type RuntimeCall = RuntimeCall;
	type Currency = Balances;
	type ProxyType = ProxyType;
	type ProxyDepositBase = ProxyDepositBase;
	type ProxyDepositFactor = ProxyDepositFactor;
	type MaxProxies = ConstU32<32>;
	type WeightInfo = pallet_proxy::weights::SubstrateWeight<Runtime>;
	type MaxPending = ConstU32<32>;
	type CallHasher = BlakeTwo256;
	type AnnouncementDepositBase = AnnouncementDepositBase;
	type AnnouncementDepositFactor = AnnouncementDepositFactor;
}

parameter_types! {
	pub const LaunchPeriod: BlockNumber = 28 * 24 * 60 * MINUTES;
	pub const VotingPeriod: BlockNumber = 28 * 24 * 60 * MINUTES;
	pub const FastTrackVotingPeriod: BlockNumber = 3 * 24 * 60 * MINUTES;
	pub MinimumDeposit: Balance = 100 * dollar(ZERO);
	pub const EnactmentPeriod: BlockNumber = 30 * 24 * 60 * MINUTES;
	pub const CooloffPeriod: BlockNumber = 28 * 24 * 60 * MINUTES;
	pub const MaxProposals: u32 = 100;
}

impl pallet_democracy::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type Currency = Balances;
	type EnactmentPeriod = EnactmentPeriod;
	type LaunchPeriod = LaunchPeriod;
	type VotingPeriod = VotingPeriod;
	type VoteLockingPeriod = EnactmentPeriod; // Same as EnactmentPeriod
	type MinimumDeposit = MinimumDeposit;
	/// A straight majority of the council can decide what their next motion is.
	type ExternalOrigin =
		pallet_collective::EnsureProportionAtLeast<AccountId, CouncilCollective, 1, 2>;
	/// A super-majority can have the next scheduled referendum be a straight majority-carries vote.
	type ExternalMajorityOrigin =
		pallet_collective::EnsureProportionAtLeast<AccountId, CouncilCollective, 3, 4>;
	/// A unanimous council can have the next scheduled referendum be a straight default-carries
	/// (NTB) vote.
	type ExternalDefaultOrigin =
		pallet_collective::EnsureProportionAtLeast<AccountId, CouncilCollective, 1, 1>;
	/// Two thirds of the technical committee can have an ExternalMajority/ExternalDefault vote
	/// be tabled immediately and with a shorter voting/enactment period.
	type FastTrackOrigin =
		pallet_collective::EnsureProportionAtLeast<AccountId, TechnicalCollective, 2, 3>;
	type InstantOrigin =
		pallet_collective::EnsureProportionAtLeast<AccountId, TechnicalCollective, 1, 1>;
	type InstantAllowed = frame_support::traits::ConstBool<true>;
	type FastTrackVotingPeriod = FastTrackVotingPeriod;
	// To cancel a proposal which has been passed, 2/3 of the council must agree to it.
	type CancellationOrigin =
		pallet_collective::EnsureProportionAtLeast<AccountId, CouncilCollective, 2, 3>;
	// To cancel a proposal before it has been passed, the technical committee must be unanimous or
	// Root must agree.
	type CancelProposalOrigin = EitherOfDiverse<
		EnsureRoot<AccountId>,
		pallet_collective::EnsureProportionAtLeast<AccountId, TechnicalCollective, 1, 1>,
	>;
	type BlacklistOrigin = EnsureRoot<AccountId>;
	// Any single technical committee member may veto a coming council proposal, however they can
	// only do it once and it lasts only for the cool-off period.
	type VetoOrigin = pallet_collective::EnsureMember<AccountId, TechnicalCollective>;
	type CooloffPeriod = CooloffPeriod;
	type Slash = Treasury;
	type Scheduler = Scheduler;
	type PalletsOrigin = OriginCaller;
	type MaxVotes = ConstU32<100>;
	type WeightInfo = pallet_democracy::weights::SubstrateWeight<Runtime>;
	type MaxProposals = MaxProposals;
	type Preimages = Preimage;
	type MaxDeposits = ConstU32<100>;
	type MaxBlacklisted = ConstU32<100>;
}

impl pallet_sudo::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type RuntimeCall = RuntimeCall;
}

parameter_types! {
	pub BountyValueMinimum: Balance = 5 * dollar(ZERO);
	pub BountyDepositBase: Balance = 1 * dollar(ZERO);
	pub const CuratorDepositMultiplier: Permill = Permill::from_percent(50);
	pub CuratorDepositMin: Balance = 1 * dollar(ZERO);
	pub CuratorDepositMax: Balance = 100 * dollar(ZERO);
	pub const BountyDepositPayoutDelay: BlockNumber = 1 * DAYS;
	pub const BountyUpdatePeriod: BlockNumber = 14 * DAYS;
}

impl pallet_bounties::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type BountyDepositBase = BountyDepositBase;
	type BountyDepositPayoutDelay = BountyDepositPayoutDelay;
	type BountyUpdatePeriod = BountyUpdatePeriod;
	type CuratorDepositMultiplier = CuratorDepositMultiplier;
	type CuratorDepositMin = CuratorDepositMin;
	type CuratorDepositMax = CuratorDepositMax;
	type BountyValueMinimum = BountyValueMinimum;
	type DataDepositPerByte = DataDepositPerByte;
	type MaximumReasonLength = MaximumReasonLength;
	type WeightInfo = pallet_bounties::weights::SubstrateWeight<Runtime>;
	type ChildBountyManager = ChildBounties;
}

parameter_types! {
	pub ChildBountyValueMinimum: Balance = 1 * dollar(ZERO);
}

impl pallet_child_bounties::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type MaxActiveChildBountyCount = ConstU32<5>;
	type ChildBountyValueMinimum = ChildBountyValueMinimum;
	type WeightInfo = pallet_child_bounties::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
	pub const CouncilMotionDuration: BlockNumber = 5 * DAYS;
	pub const CouncilMaxMembers: u32 = 100;
}

type EnsureRootOrHalfCouncil = EitherOfDiverse<
	EnsureRoot<AccountId>,
	pallet_collective::EnsureProportionMoreThan<AccountId, CouncilCollective, 1, 2>,
>;

type EnsureRootOrThreeFourthsCouncil = EitherOfDiverse<
	EnsureRoot<AccountId>,
	pallet_collective::EnsureProportionMoreThan<AccountId, CouncilCollective, 3, 4>,
>;

parameter_types! {
	pub const ResourceSymbolLimit: u32 = 10;
	pub const PartsLimit: u32 = 25;
	pub const MaxPriorities: u32 = 25;
	pub const CollectionSymbolLimit: u32 = 100;
	pub const MaxResourcesOnMint: u32 = 100;
	pub const NestingBudget: u32 = 20;
}

impl pallet_rmrk_core::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type ProtocolOrigin = frame_system::EnsureRoot<AccountId>;
	type ResourceSymbolLimit = ResourceSymbolLimit;
	type PartsLimit = PartsLimit;
	type MaxPriorities = MaxPriorities;
	type CollectionSymbolLimit = CollectionSymbolLimit;
	type MaxResourcesOnMint = MaxResourcesOnMint;
	type NestingBudget = NestingBudget;
	type WeightInfo = pallet_rmrk_core::weights::SubstrateWeight<Runtime>;
	type TransferHooks = ();
}

parameter_types! {
	pub MinimumOfferAmount: Balance = cent(ZERO) / 10;
}

impl pallet_rmrk_market::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type ProtocolOrigin = frame_system::EnsureRoot<AccountId>;
	type Currency = Balances;
	type MinimumOfferAmount = MinimumOfferAmount;
	type WeightInfo = pallet_rmrk_market::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
	pub CollectionDeposit: Balance = cent(ZERO) * 10;
	pub ItemDeposit: Balance = dollar(ZERO);
	pub const KeyLimit: u32 = 32;
	pub const ValueLimit: u32 = 256;
	pub UniquesMetadataDepositBase: Balance = cent(ZERO) * 10;
	pub UniquesAttributeDepositBase: Balance = cent(ZERO) * 10;
	pub UniquesDepositPerByte: Balance = cent(ZERO);
	pub const UniquesStringLimit: u32 = 128;
	pub const MaxPropertiesPerTheme: u32 = 100;
	pub const MaxCollectionsEquippablePerPart: u32 = 100;
}

impl pallet_rmrk_equip::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type MaxPropertiesPerTheme = MaxPropertiesPerTheme;
	type MaxCollectionsEquippablePerPart = MaxCollectionsEquippablePerPart;
	type WeightInfo = pallet_rmrk_equip::weights::SubstrateWeight<Runtime>;
}

impl pallet_uniques::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type CollectionId = u32;
	type ItemId = u32;
	type Currency = Balances;
	type ForceOrigin = EnsureRoot<AccountId>;
	type CreateOrigin = AsEnsureOriginWithArg<EnsureSigned<AccountId>>;
	type Locker = pallet_rmrk_core::Pallet<Runtime>;
	type CollectionDeposit = CollectionDeposit;
	type ItemDeposit = ItemDeposit;
	type MetadataDepositBase = UniquesMetadataDepositBase;
	type AttributeDepositBase = UniquesAttributeDepositBase;
	type DepositPerByte = UniquesDepositPerByte;
	type StringLimit = UniquesStringLimit;
	type KeyLimit = KeyLimit;
	type ValueLimit = ValueLimit;
	type WeightInfo = pallet_uniques::weights::SubstrateWeight<Runtime>;
}

impl pallet_utility::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type RuntimeCall = RuntimeCall;
	type PalletsOrigin = OriginCaller;
	type WeightInfo = ();
}

type CouncilCollective = pallet_collective::Instance1;
impl pallet_collective::Config<CouncilCollective> for Runtime {
	type RuntimeOrigin = RuntimeOrigin;
	type Proposal = RuntimeCall;
	type RuntimeEvent = RuntimeEvent;
	type MotionDuration = CouncilMotionDuration;
	type MaxProposals = ConstU32<100>;
	type MaxMembers = CouncilMaxMembers;
	type DefaultVote = pallet_collective::PrimeDefaultVote;
	type WeightInfo = pallet_collective::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
	pub const TechnicalMotionDuration: BlockNumber = 5 * DAYS;
	pub const TechnicalMaxMembers: u32 = 100;
}

type TechnicalCollective = pallet_collective::Instance2;
impl pallet_collective::Config<TechnicalCollective> for Runtime {
	type RuntimeOrigin = RuntimeOrigin;
	type Proposal = RuntimeCall;
	type RuntimeEvent = RuntimeEvent;
	type MotionDuration = TechnicalMotionDuration;
	type MaxProposals = ConstU32<100>;
	type MaxMembers = TechnicalMaxMembers;
	type DefaultVote = pallet_collective::PrimeDefaultVote;
	type WeightInfo = pallet_collective::weights::SubstrateWeight<Runtime>;
}

impl pallet_membership::Config<pallet_membership::Instance1> for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type AddOrigin = EnsureRootOrHalfCouncil;
	type RemoveOrigin = EnsureRootOrHalfCouncil;
	type SwapOrigin = EnsureRootOrHalfCouncil;
	type ResetOrigin = EnsureRootOrHalfCouncil;
	type PrimeOrigin = EnsureRootOrHalfCouncil;
	type MembershipInitialized = TechnicalCommittee;
	type MembershipChanged = TechnicalCommittee;
	type MaxMembers = TechnicalMaxMembers;
	type WeightInfo = pallet_membership::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
	pub CandidacyBond: Balance = 10 * dollar(ZERO);
	// 1 storage item created, key size is 32 bytes, value size is 16+16.
	pub VotingBondBase: Balance = deposit(1, 64);
	// additional data per vote is 32 bytes (account id).
	pub VotingBondFactor: Balance = deposit(0, 32);
	pub const TermDuration: BlockNumber = 7 * DAYS;
	pub const DesiredMembers: u32 = 13;
	pub const ElectionsPhragmenPalletId: LockIdentifier = *b"phrelect";
}

// Make sure that there are no more than `MaxMembers` members elected via elections-phragmen.
const_assert!(DesiredMembers::get() <= CouncilMaxMembers::get());

impl pallet_elections_phragmen::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type PalletId = ElectionsPhragmenPalletId;
	type Currency = Balances;
	type ChangeMembers = Council;
	// NOTE: this implies that council's genesis members cannot be set directly and must come from
	// this module.
	type InitializeMembers = Council;
	type CurrencyToVote = U128CurrencyToVote;
	type CandidacyBond = CandidacyBond;
	type VotingBondBase = VotingBondBase;
	type VotingBondFactor = VotingBondFactor;
	type LoserCandidate = Treasury;
	type KickedMember = Treasury;
	type DesiredMembers = DesiredMembers;
	type DesiredRunnersUp = ConstU32<7>;
	type TermDuration = TermDuration;
	type MaxVoters = ConstU32<{10 * 1000}>;
	type MaxCandidates = ConstU32<1000>;
	type WeightInfo = pallet_elections_phragmen::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
	pub const TipCountdown: BlockNumber = 1 * DAYS;
	pub const TipFindersFee: Percent = Percent::from_percent(20);
	pub TipReportDepositBase: Balance = 1 * dollar(ZERO);
}

impl pallet_tips::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type DataDepositPerByte = DataDepositPerByte;
	type MaximumReasonLength = MaximumReasonLength;
	type Tippers = Elections;
	type TipCountdown = TipCountdown;
	type TipFindersFee = TipFindersFee;
	type TipReportDepositBase = TipReportDepositBase;
	type WeightInfo = pallet_tips::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
	pub const ProposalBond: Permill = Permill::from_percent(5);
	pub ProposalBondMinimum: Balance = 1 * dollar(ZERO);
	pub const SpendPeriod: BlockNumber = 1 * DAYS;
	pub const Burn: Permill = Permill::from_percent(50);
	pub DataDepositPerByte: Balance = 1 * cent(ZERO);
	pub const MaximumReasonLength: u32 = 300;
}

impl pallet_treasury::Config for Runtime {
	type PalletId = TreasuryPalletId;
	type Currency = Balances;
	type ApproveOrigin = EitherOfDiverse<
		EnsureRoot<AccountId>,
		pallet_collective::EnsureProportionAtLeast<AccountId, CouncilCollective, 3, 5>,
	>;
	type RejectOrigin = EitherOfDiverse<
		EnsureRoot<AccountId>,
		pallet_collective::EnsureProportionMoreThan<AccountId, CouncilCollective, 1, 2>,
	>;
	type RuntimeEvent = RuntimeEvent;
	type OnSlash = ();
	type ProposalBond = ProposalBond;
	type ProposalBondMinimum = ProposalBondMinimum;
	type ProposalBondMaximum = ();
	type SpendPeriod = SpendPeriod;
	type Burn = Burn;
	type BurnDestination = ();
	type SpendFunds = Bounties;
	type WeightInfo = pallet_treasury::weights::SubstrateWeight<Runtime>;
	type MaxApprovals = ConstU32<100>;
	type SpendOrigin = frame_support::traits::NeverEnsureOrigin<u128>;
}

parameter_types! {
	pub BasicDeposit: Balance = 10 * dollar(ZERO);		// 258 bytes on-chain
	pub FieldDeposit: Balance = 250 * cent(ZERO);		// 	66 bytes on-chain
	pub SubAccountDeposit: Balance = 2 * dollar(ZERO);  // 	53 bytes on-chain
}

impl pallet_identity::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type Currency = Balances;
	type BasicDeposit = BasicDeposit;
	type FieldDeposit = FieldDeposit;
	type SubAccountDeposit = SubAccountDeposit;
	type MaxSubAccounts = ConstU32<100>;
	type MaxAdditionalFields = ConstU32<100>;
	type MaxRegistrars = ConstU32<20>;
	type Slashed = Treasury;
	type ForceOrigin = EnsureRootOrHalfCouncil;
	type RegistrarOrigin = EnsureRootOrHalfCouncil;
	type WeightInfo = pallet_identity::weights::SubstrateWeight<Runtime>;
}

parameter_types! {
	pub const ReservedXcmpWeight: Weight = MAXIMUM_BLOCK_WEIGHT.saturating_div(4);
	pub const ReservedDmpWeight: Weight = MAXIMUM_BLOCK_WEIGHT.saturating_div(4);
}

impl cumulus_pallet_parachain_system::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type OnSystemEvent = ();
	type SelfParaId = parachain_info::Pallet<Runtime>;
	type OutboundXcmpMessageSource = XcmpQueue;
	type DmpMessageHandler = DmpQueue;
	type ReservedDmpWeight = ReservedDmpWeight;
	type XcmpMessageHandler = XcmpQueue;
	type ReservedXcmpWeight = ReservedXcmpWeight;
	type CheckAssociatedRelayNumber = RelayNumberStrictlyIncreases;
}

impl parachain_info::Config for Runtime {}

impl cumulus_pallet_aura_ext::Config for Runtime {}

parameter_types! {
	pub const Period: u32 = 6 * HOURS;
	pub const Offset: u32 = 0;
	pub const MaxAuthorities: u32 = 100_000;
}

impl pallet_session::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type ValidatorId = <Self as frame_system::Config>::AccountId;
	// we don't have stash and controller, thus we don't need the convert as well.
	type ValidatorIdOf = pallet_collator_selection::IdentityCollator;
	type ShouldEndSession = pallet_session::PeriodicSessions<Period, Offset>;
	type NextSessionRotation = pallet_session::PeriodicSessions<Period, Offset>;
	type SessionManager = CollatorSelection;
	// Essentially just Aura, but lets be pedantic.
	type SessionHandler = <SessionKeys as sp_runtime::traits::OpaqueKeys>::KeyTypeIdProviders;
	type Keys = SessionKeys;
	type WeightInfo = ();
}

impl pallet_aura::Config for Runtime {
	type AuthorityId = AuraId;
	type DisabledValidators = ();
	type MaxAuthorities = MaxAuthorities;
}

parameter_types! {
	pub const PotId: PalletId = PalletId(*b"PotStake");
	pub const MaxCandidates: u32 = 1000;
	pub const MinCandidates: u32 = 5;
	pub const SessionLength: BlockNumber = 6 * HOURS;
	pub const MaxInvulnerables: u32 = 100;
	pub const ExecutiveBody: BodyId = BodyId::Executive;
}

// We allow root only to execute privileged collator selection operations.
pub type CollatorSelectionUpdateOrigin = EnsureRoot<AccountId>;

impl pallet_collator_selection::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type Currency = Balances;
	type UpdateOrigin = CollatorSelectionUpdateOrigin;
	type PotId = PotId;
	type MaxCandidates = MaxCandidates;
	type MinCandidates = MinCandidates;
	type MaxInvulnerables = MaxInvulnerables;
	// should be a multiple of session or things will get inconsistent
	type KickThreshold = Period;
	type ValidatorId = <Self as frame_system::Config>::AccountId;
	type ValidatorIdOf = pallet_collator_selection::IdentityCollator;
	type ValidatorRegistration = Session;
	type WeightInfo = ();
}

// Pallet accounts of runtime
parameter_types! {
	pub const TreasuryPalletId: PalletId = PalletId(*b"zr/zrtrs");
	pub const ControlPalletId: PalletId = PalletId(*b"gd/cntrl");
	pub TreasuryAccountId: AccountId = TreasuryPalletId::get().into_account_truncating();
	pub Game3FoundationTreasuryAccountId: AccountId = PalletId(*b"gd/g3trs").into_account_truncating();
	pub GameDAOTreasuryAccountId: AccountId = PalletId(*b"gd/gdtrs").into_account_truncating();
}

pub fn get_all_module_accounts() -> Vec<AccountId> {
	vec![
		TreasuryAccountId::get(),
		ControlPalletId::get().into_account_truncating(),
		Game3FoundationTreasuryAccountId::get(),
		GameDAOTreasuryAccountId::get(),
	]
}

parameter_type_with_key! {
	pub ExistentialDeposits: |currency_id: CurrencyId| -> Balance {
		match currency_id {
			CurrencyId::Token(symbol) => match symbol {
				TokenSymbol::ZERO => cent(*currency_id),
				TokenSymbol::PLAY => 10 * cent(*currency_id),
				TokenSymbol::GAME => 10 * cent(*currency_id),
				TokenSymbol::DOT => cent(*currency_id),
			},
			CurrencyId::ForeignAsset(id) => {
				AssetRegistry::metadata(&id)
					.map_or(Balance::max_value(), |metadata| metadata.existential_deposit)
			},
		}
	};
}

/// Allow asset registration only from root origin
pub struct AssetAuthority;
impl EnsureOriginWithArg<RuntimeOrigin, Option<u32>> for AssetAuthority {
	type Success = ();

	fn try_origin(origin: RuntimeOrigin, _asset_id: &Option<u32>) -> Result<Self::Success, RuntimeOrigin> {
		EnsureRoot::try_origin(origin)
	}

	#[cfg(feature = "runtime-benchmarks")]
	fn successful_origin(_asset_id: &Option<u32>) -> RuntimeOrigin {
		EnsureRoot::successful_origin()
	}
}

impl orml_asset_registry::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type Balance = Balance;
	type CustomMetadata = CustomMetadata;
	type AssetProcessor = SequentialId<Runtime>;
	type AssetId = ForeignAssetId;
	type AuthorityOrigin = AssetAuthority;
	type WeightInfo = ();
}

pub struct DustRemovalWhitelist;
impl Contains<AccountId> for DustRemovalWhitelist {
	fn contains(a: &AccountId) -> bool {
		get_all_module_accounts().contains(a)
	}
}

parameter_types! {
	pub const GetNativeCurrencyId: CurrencyId = ZERO;
	pub const GetStableCurrencyId: CurrencyId = PLAY;
	pub const GetProtocolCurrencyId: CurrencyId = GAME;
	pub const StringLimit: u32 = 64;
}

impl orml_tokens::Config for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type Balance = Balance;
	type Amount = Amount;
	type CurrencyId = CurrencyId;
	type WeightInfo = ();
	type ExistentialDeposits = ExistentialDeposits;
	type CurrencyHooks = ();
	type MaxLocks = MaxLocks;
	type MaxReserves = MaxReserves;
	type ReserveIdentifier = ReserveIdentifier;
	type DustRemovalWhitelist = DustRemovalWhitelist;
}

impl orml_currencies::Config for Runtime {
	type MultiCurrency = Tokens;
	type NativeCurrency = BasicCurrencyAdapter<Runtime, Balances, Amount, BlockNumber>;
	type GetNativeCurrencyId = GetNativeCurrencyId;
	type WeightInfo = ();
}

// Create the runtime by composing the FRAME pallets that were previously configured.
construct_runtime!(
	pub enum Runtime where
		Block = Block,
		NodeBlock = opaque::Block,
		UncheckedExtrinsic = UncheckedExtrinsic,
	{
		// System support stuff.
		System: frame_system::{Pallet, Call, Config, Storage, Event<T>} = 0,
		ParachainSystem: cumulus_pallet_parachain_system::{
			Pallet, Call, Config, Storage, Inherent, Event<T>,
		} = 1,
		Timestamp: pallet_timestamp::{Pallet, Call, Storage, Inherent} = 2,
		Utility: pallet_utility::{Pallet, Call, Storage, Event} = 3,
		Multisig: pallet_multisig = 4,
		ParachainInfo: parachain_info::{Pallet, Storage, Config} = 5,
		Council: pallet_collective::<Instance1> = 6,
		TechnicalCommittee: pallet_collective::<Instance2> = 7,
		Identity: pallet_identity = 8,
		Bounties: pallet_bounties = 9,
		ChildBounties: pallet_child_bounties = 10,
		Scheduler: pallet_scheduler = 11,
		Preimage: pallet_preimage = 12,
		Sudo: pallet_sudo = 13,
		Proxy: pallet_proxy = 14,
		Democracy: pallet_democracy = 15,
		Elections: pallet_elections_phragmen::{Pallet, Call, Storage, Config<T>, Event<T>} = 16,
		TechnicalMembership: pallet_membership::<Instance1> = 17,

		// Monetary stuff.
		Treasury: pallet_treasury = 20,
		Tips: pallet_tips = 21,
		Balances: pallet_balances::{Pallet, Call, Storage, Config<T>, Event<T>} = 22,
		TransactionPayment: pallet_transaction_payment::{Pallet, Storage, Event<T>} = 23,

		// NFT
		RmrkEquip: pallet_rmrk_equip::{Pallet, Call, Event<T>, Storage} = 30,
		RmrkCore: pallet_rmrk_core::{Pallet, Call, Event<T>, Storage} = 31,
		RmrkMarket: pallet_rmrk_market::{Pallet, Call, Storage, Event<T>} = 32,
		Uniques: pallet_uniques::{Pallet, Call, Storage, Event<T>} = 33,

		// Collator support. The order of these 4 are important and shall not change.
		Authorship: pallet_authorship::{Pallet, Call, Storage} = 40,
		CollatorSelection: pallet_collator_selection::{Pallet, Call, Storage, Event<T>, Config<T>} = 41,
		Session: pallet_session::{Pallet, Call, Storage, Event, Config<T>} = 42,
		Aura: pallet_aura::{Pallet, Storage, Config<T>} = 43,
		AuraExt: cumulus_pallet_aura_ext::{Pallet, Storage, Config} = 44,

		// XCM helpers.
		XcmpQueue: cumulus_pallet_xcmp_queue::{Pallet, Call, Storage, Event<T>} = 50,
		PolkadotXcm: pallet_xcm::{Pallet, Call, Event<T>, Origin, Config} = 51,
		CumulusXcm: cumulus_pallet_xcm::{Pallet, Event<T>, Origin} = 52,
		DmpQueue: cumulus_pallet_dmp_queue::{Pallet, Call, Storage, Event<T>} = 53,

		// ORML:
		AssetRegistry: orml_asset_registry::{Pallet, Storage, Call, Event<T>, Config<T>} = 60,
		Currencies: orml_currencies::{Pallet, Call} = 61,
		OrmlXcm: orml_xcm::{Pallet, Call, Event<T>} = 62,
		Tokens: orml_tokens::{Pallet, Storage, Call, Event<T>, Config<T>} = 63,
		UnknownTokens: orml_unknown_tokens::{Pallet, Storage, Event} = 64,
		XTokens: orml_xtokens::{Pallet, Storage, Call, Event<T>} = 65,
	}
);

#[cfg(feature = "runtime-benchmarks")]
#[macro_use]
extern crate frame_benchmarking;

#[cfg(feature = "runtime-benchmarks")]
mod benches {
	define_benchmarks!(
		[frame_system, SystemBench::<Runtime>]
		[pallet_balances, Balances]
		[pallet_session, SessionBench::<Runtime>]
		[pallet_timestamp, Timestamp]
		[pallet_collator_selection, CollatorSelection]
		[cumulus_pallet_xcmp_queue, XcmpQueue]
	);
}

fn option_filter_keys_to_set<StringLimit: frame_support::traits::Get<u32>>(
	filter_keys: Option<Vec<pallet_rmrk_rpc_runtime_api::PropertyKey>>,
) -> pallet_rmrk_rpc_runtime_api::Result<Option<BTreeSet<BoundedVec<u8, StringLimit>>>> {
	match filter_keys {
		Some(filter_keys) => {
			let tree = filter_keys
				.into_iter()
				.map(|filter_keys| -> pallet_rmrk_rpc_runtime_api::Result<BoundedVec<u8, StringLimit>> {
					filter_keys
						.try_into()
						.map_err(|_| DispatchError::Other("Can't read filter key"))
				})
				.collect::<pallet_rmrk_rpc_runtime_api::Result<BTreeSet<_>>>()?;
			Ok(Some(tree))
		},
		None => Ok(None),
	}
}

impl_runtime_apis! {
	impl sp_consensus_aura::AuraApi<Block, AuraId> for Runtime {
		fn slot_duration() -> sp_consensus_aura::SlotDuration {
			sp_consensus_aura::SlotDuration::from_millis(Aura::slot_duration())
		}

		fn authorities() -> Vec<AuraId> {
			Aura::authorities().into_inner()
		}
	}

	impl sp_api::Core<Block> for Runtime {
		fn version() -> RuntimeVersion {
			VERSION
		}

		fn execute_block(block: Block) {
			Executive::execute_block(block)
		}

		fn initialize_block(header: &<Block as BlockT>::Header) {
			Executive::initialize_block(header)
		}
	}

	impl sp_api::Metadata<Block> for Runtime {
		fn metadata() -> OpaqueMetadata {
			OpaqueMetadata::new(Runtime::metadata().into())
		}
	}

	impl sp_block_builder::BlockBuilder<Block> for Runtime {
		fn apply_extrinsic(extrinsic: <Block as BlockT>::Extrinsic) -> ApplyExtrinsicResult {
			Executive::apply_extrinsic(extrinsic)
		}

		fn finalize_block() -> <Block as BlockT>::Header {
			Executive::finalize_block()
		}

		fn inherent_extrinsics(data: sp_inherents::InherentData) -> Vec<<Block as BlockT>::Extrinsic> {
			data.create_extrinsics()
		}

		fn check_inherents(
			block: Block,
			data: sp_inherents::InherentData,
		) -> sp_inherents::CheckInherentsResult {
			data.check_extrinsics(&block)
		}
	}

	impl sp_transaction_pool::runtime_api::TaggedTransactionQueue<Block> for Runtime {
		fn validate_transaction(
			source: TransactionSource,
			tx: <Block as BlockT>::Extrinsic,
			block_hash: <Block as BlockT>::Hash,
		) -> TransactionValidity {
			Executive::validate_transaction(source, tx, block_hash)
		}
	}

	impl sp_offchain::OffchainWorkerApi<Block> for Runtime {
		fn offchain_worker(header: &<Block as BlockT>::Header) {
			Executive::offchain_worker(header)
		}
	}

	impl sp_session::SessionKeys<Block> for Runtime {
		fn generate_session_keys(seed: Option<Vec<u8>>) -> Vec<u8> {
			SessionKeys::generate(seed)
		}

		fn decode_session_keys(
			encoded: Vec<u8>,
		) -> Option<Vec<(Vec<u8>, KeyTypeId)>> {
			SessionKeys::decode_into_raw_public_keys(&encoded)
		}
	}

	impl frame_system_rpc_runtime_api::AccountNonceApi<Block, AccountId, Index> for Runtime {
		fn account_nonce(account: AccountId) -> Index {
			System::account_nonce(account)
		}
	}

	impl pallet_transaction_payment_rpc_runtime_api::TransactionPaymentApi<Block, Balance> for Runtime {
		fn query_info(
			uxt: <Block as BlockT>::Extrinsic,
			len: u32,
		) -> pallet_transaction_payment_rpc_runtime_api::RuntimeDispatchInfo<Balance> {
			TransactionPayment::query_info(uxt, len)
		}
		fn query_fee_details(
			uxt: <Block as BlockT>::Extrinsic,
			len: u32,
		) -> pallet_transaction_payment::FeeDetails<Balance> {
			TransactionPayment::query_fee_details(uxt, len)
		}
	}

	impl cumulus_primitives_core::CollectCollationInfo<Block> for Runtime {
		fn collect_collation_info(header: &<Block as BlockT>::Header) -> cumulus_primitives_core::CollationInfo {
			ParachainSystem::collect_collation_info(header)
		}
	}

	#[cfg(feature = "try-runtime")]
	impl frame_try_runtime::TryRuntime<Block> for Runtime {
		fn on_runtime_upgrade() -> (Weight, Weight) {
			log::info!("try-runtime::on_runtime_upgrade parachain-subzero.");
			let weight = Executive::try_runtime_upgrade().unwrap();
			(weight, RuntimeBlockWeights::get().max_block)
		}

		fn execute_block_no_check(block: Block) -> Weight {
			Executive::execute_block_no_check(block)
		}
	}

	#[cfg(feature = "runtime-benchmarks")]
	impl frame_benchmarking::Benchmark<Block> for Runtime {
		fn benchmark_metadata(extra: bool) -> (
			Vec<frame_benchmarking::BenchmarkList>,
			Vec<frame_support::traits::StorageInfo>,
		) {
			use frame_benchmarking::{Benchmarking, BenchmarkList};
			use frame_support::traits::StorageInfoTrait;
			use frame_system_benchmarking::Pallet as SystemBench;
			use cumulus_pallet_session_benchmarking::Pallet as SessionBench;

			let mut list = Vec::<BenchmarkList>::new();
			list_benchmarks!(list, extra);

			let storage_info = AllPalletsWithSystem::storage_info();
			return (list, storage_info)
		}

		fn dispatch_benchmark(
			config: frame_benchmarking::BenchmarkConfig
		) -> Result<Vec<frame_benchmarking::BenchmarkBatch>, sp_runtime::RuntimeString> {
			use frame_benchmarking::{Benchmarking, BenchmarkBatch, TrackedStorageKey};

			use frame_system_benchmarking::Pallet as SystemBench;
			impl frame_system_benchmarking::Config for Runtime {}

			use cumulus_pallet_session_benchmarking::Pallet as SessionBench;
			impl cumulus_pallet_session_benchmarking::Config for Runtime {}

			let whitelist: Vec<TrackedStorageKey> = vec![
				// Block Number
				hex_literal::hex!("26aa394eea5630e07c48ae0c9558cef702a5c1b19ab7a04f536c519aca4983ac").to_vec().into(),
				// Total Issuance
				hex_literal::hex!("c2261276cc9d1f8598ea4b6a74b15c2f57c875e4cff74148e4628f264b974c80").to_vec().into(),
				// Execution Phase
				hex_literal::hex!("26aa394eea5630e07c48ae0c9558cef7ff553b5a9862a516939d82b3d3d8661a").to_vec().into(),
				// Event Count
				hex_literal::hex!("26aa394eea5630e07c48ae0c9558cef70a98fdbe9ce6c55837576c60c7af3850").to_vec().into(),
				// System Events
				hex_literal::hex!("26aa394eea5630e07c48ae0c9558cef780d41e5e16056765bc8461851072c9d7").to_vec().into(),
			];

			let mut batches = Vec::<BenchmarkBatch>::new();
			let params = (&config, &whitelist);
			add_benchmarks!(params, batches);

			if batches.is_empty() { return Err("Benchmark not found for this pallet.".into()) }
			Ok(batches)
		}
	}

	impl pallet_rmrk_rpc_runtime_api::RmrkApi<
		Block,
		AccountId,
		CollectionInfoOf<Runtime>,
		InstanceInfoOf<Runtime>,
		ResourceInfoOf<Runtime>,
		PropertyInfoOf<Runtime>,
		BaseInfoOf<Runtime>,
		PartTypeOf<Runtime>,
		BoundedThemeOf<Runtime>
	> for Runtime
	{
		fn collection_by_id(id: CollectionId) -> pallet_rmrk_rpc_runtime_api::Result<Option<CollectionInfoOf<Runtime>>> {
			Ok(RmrkCore::collections(id))
		}

		fn nft_by_id(collection_id: CollectionId, nft_id: NftId) -> pallet_rmrk_rpc_runtime_api::Result<Option<InstanceInfoOf<Runtime>>> {
			Ok(RmrkCore::nfts(collection_id, nft_id))
		}

		fn account_tokens(account_id: AccountId, collection_id: CollectionId) -> pallet_rmrk_rpc_runtime_api::Result<Vec<NftId>> {
			Ok(Uniques::owned_in_collection(&collection_id, &account_id).collect())
		}

		fn nft_children(collection_id: CollectionId, nft_id: NftId) -> pallet_rmrk_rpc_runtime_api::Result<Vec<NftChild<CollectionId, NftId>>> {
			let children = RmrkCore::iterate_nft_children(collection_id, nft_id).collect();

			Ok(children)
		}

		fn collection_properties(
			collection_id: CollectionId,
			filter_keys: Option<Vec<pallet_rmrk_rpc_runtime_api::PropertyKey>>
		) -> pallet_rmrk_rpc_runtime_api::Result<Vec<PropertyInfoOf<Runtime>>> {
			let nft_id = None;

			let filter_keys = option_filter_keys_to_set::<<Self as pallet_uniques::Config>::KeyLimit>(
				filter_keys
			)?;

			Ok(RmrkCore::query_properties(collection_id, nft_id, filter_keys).collect())
		}

		fn nft_properties(
			collection_id: CollectionId,
			nft_id: NftId,
			filter_keys: Option<Vec<pallet_rmrk_rpc_runtime_api::PropertyKey>>
		) -> pallet_rmrk_rpc_runtime_api::Result<Vec<PropertyInfoOf<Runtime>>> {
			let filter_keys = option_filter_keys_to_set::<<Self as pallet_uniques::Config>::KeyLimit>(
				filter_keys
			)?;

			Ok(RmrkCore::query_properties(collection_id, Some(nft_id), filter_keys).collect())
		}

		fn nft_resources(collection_id: CollectionId, nft_id: NftId) -> pallet_rmrk_rpc_runtime_api::Result<Vec<ResourceInfoOf<Runtime>>> {
			Ok(RmrkCore::iterate_resources(collection_id, nft_id).collect())
		}

		fn nft_resource_priority(collection_id: CollectionId, nft_id: NftId, resource_id: ResourceId) -> pallet_rmrk_rpc_runtime_api::Result<Option<u32>> {
			let priority = RmrkCore::priorities((collection_id, nft_id, resource_id));

			Ok(priority)
		}

		fn base(base_id: BaseId) -> pallet_rmrk_rpc_runtime_api::Result<Option<BaseInfoOf<Runtime>>> {
			Ok(RmrkEquip::bases(base_id))
		}

		fn base_parts(base_id: BaseId) -> pallet_rmrk_rpc_runtime_api::Result<Vec<PartTypeOf<Runtime>>> {
			Ok(RmrkEquip::iterate_part_types(base_id).collect())
		}

		fn theme_names(base_id: BaseId) -> pallet_rmrk_rpc_runtime_api::Result<Vec<pallet_rmrk_rpc_runtime_api::ThemeName>> {
			let names = RmrkEquip::iterate_theme_names(base_id)
				.map(|name| name.into())
				.collect();

			Ok(names)
		}

		fn theme(
			base_id: BaseId,
			theme_name: pallet_rmrk_rpc_runtime_api::ThemeName,
			filter_keys: Option<Vec<pallet_rmrk_rpc_runtime_api::PropertyKey>>
		) -> pallet_rmrk_rpc_runtime_api::Result<Option<BoundedThemeOf<Runtime>>> {
			use pallet_rmrk_equip::StringLimitOf;

			let theme_name: StringLimitOf<Self> = theme_name.try_into()
				.map_err(|_| DispatchError::Other("Can't read theme_name"))?;

			let filter_keys = option_filter_keys_to_set::<<Self as pallet_uniques::Config>::StringLimit>(
				filter_keys
			)?;

			let theme = RmrkEquip::get_theme(base_id, theme_name, filter_keys)?;
			Ok(theme)
		}
	}
}

struct CheckInherents;

impl cumulus_pallet_parachain_system::CheckInherents<Block> for CheckInherents {
	fn check_inherents(
		block: &Block,
		relay_state_proof: &cumulus_pallet_parachain_system::RelayChainStateProof,
	) -> sp_inherents::CheckInherentsResult {
		let relay_chain_slot = relay_state_proof
			.read_slot()
			.expect("Could not read the relay chain slot from the proof");

		let inherent_data =
			cumulus_primitives_timestamp::InherentDataProvider::from_relay_chain_slot_and_duration(
				relay_chain_slot,
				sp_std::time::Duration::from_secs(6),
			)
			.create_inherent_data()
			.expect("Could not create the timestamp inherent data");

		inherent_data.check_extrinsics(block)
	}
}

cumulus_pallet_parachain_system::register_validate_block! {
	Runtime = Runtime,
	BlockExecutor = cumulus_pallet_aura_ext::BlockExecutor::<Runtime, Executive>,
	CheckInherents = CheckInherents,
}
