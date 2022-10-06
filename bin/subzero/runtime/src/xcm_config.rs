use super::{
	constants::{fee::*, parachains},
	AccountId, AssetRegistry, Balance, Call, Convert,
	CurrencyId::{self, ForeignAsset}, Currencies,
	EnsureRootOrHalfCouncil, EnsureRootOrThreeFourthsCouncil,
	Event, Origin, ParachainInfo, ParachainSystem, PolkadotXcm,
	Runtime, TreasuryAccountId, UnknownTokens, XcmpQueue,
	ZERO, PLAY, GAME, DOT,
};
use codec::{Decode, Encode};
use cumulus_primitives_core::ParaId;
use frame_support::{
	match_types,
	parameter_types,
	traits::{Everything, Get, Nothing},
	weights::Weight,
};
use sp_runtime::{
	traits::ConstU32,
	WeakBoundedVec
};
use pallet_xcm::XcmPassthrough;
use polkadot_parachain::primitives::Sibling;
use xcm::latest::prelude::*;
use xcm_builder::{
	AccountId32Aliases, AllowKnownQueryResponses, AllowTopLevelPaidExecutionFrom,
	AllowSubscriptionsFrom, AllowUnpaidExecutionFrom, EnsureXcmOrigin, FixedRateOfFungible, FixedWeightBounds,
	LocationInverter, NativeAsset, ParentIsPreset, RelayChainAsNative,
	SiblingParachainAsNative, SiblingParachainConvertsVia, SignedAccountId32AsNative,
	SignedToAccountId32, SovereignSignedViaLocation, TakeRevenue, TakeWeightCredit,
};
use xcm_executor::XcmExecutor;
use orml_asset_registry::{AssetRegistryTrader, FixedRateAssetRegistryTrader};
use orml_traits::{location::AbsoluteReserveProvider, FixedConversionRateProvider, MultiCurrency, parameter_type_with_key};
use orml_xcm_support::{DepositToAlternative, IsNativeConcrete, MultiCurrencyAdapter, MultiNativeAsset};


parameter_types! {
	pub const DotLocation: MultiLocation = MultiLocation::parent();
	pub const RelayNetwork: NetworkId = NetworkId::Polkadot;
	pub RelayChainOrigin: Origin = cumulus_pallet_xcm::Origin::Relay.into();
	pub Ancestry: MultiLocation = Parachain(ParachainInfo::parachain_id().into()).into();
}

/// Type for specifying how a `MultiLocation` can be converted into an `AccountId`. This is used
/// when determining ownership of accounts for asset transacting and when attempting to use XCM
/// `Transact` in order to determine the dispatch Origin.
pub type LocationToAccountId = (
	// The parent (Relay-chain) origin converts to the default `AccountId`.
	ParentIsPreset<AccountId>,
	// Sibling parachain origins convert to AccountId via the `ParaId::into`.
	SiblingParachainConvertsVia<Sibling, AccountId>,
	// Straight up local `AccountId32` origins just alias directly to `AccountId`.
	AccountId32Aliases<RelayNetwork, AccountId>,
);

/// This is the type we use to convert an (incoming) XCM origin into a local `Origin` instance,
/// ready for dispatching a transaction with Xcm's `Transact`. There is an `OriginKind` which can
/// biases the kind of local `Origin` it will become.
pub type XcmOriginToCallOrigin = (
	// Sovereign account converter; this attempts to derive an `AccountId` from the origin location
	// using `LocationToAccountId` and then turn that into the usual `Signed` origin. Useful for
	// foreign chains who want to have a local sovereign account on this chain which they control.
	SovereignSignedViaLocation<LocationToAccountId, Origin>,
	// Native converter for Relay-chain (Parent) location; will converts to a `Relay` origin when
	// recognized.
	RelayChainAsNative<RelayChainOrigin, Origin>,
	// Native converter for sibling Parachains; will convert to a `SiblingPara` origin when
	// recognized.
	SiblingParachainAsNative<cumulus_pallet_xcm::Origin, Origin>,
	// Native signed account converter; this just converts an `AccountId32` origin into a normal
	// `Origin::Signed` origin of the same 32-byte value.
	SignedAccountId32AsNative<RelayNetwork, Origin>,
	// Xcm origins can be represented natively under the Xcm pallet's Xcm origin.
	XcmPassthrough<Origin>,
);

match_types! {
	pub type ParentOrParentsExecutivePlurality: impl Contains<MultiLocation> = {
		MultiLocation { parents: 1, interior: Here } |
		MultiLocation { parents: 1, interior: X1(Plurality { id: BodyId::Executive, .. }) }
	};
}

match_types! {
	pub type SpecParachain: impl Contains<MultiLocation> = {
		MultiLocation { parents: 1, interior: X1(Parachain(parachains::acala::ID)) }
	};
}

pub type Barrier = (
	TakeWeightCredit,
	AllowTopLevelPaidExecutionFrom<Everything>,
	AllowKnownQueryResponses<PolkadotXcm>,
	AllowUnpaidExecutionFrom<ParentOrParentsExecutivePlurality>,
	AllowUnpaidExecutionFrom<SpecParachain>,
	AllowSubscriptionsFrom<Everything>
);

parameter_types! {
	pub UnitWeightCost: Weight = 200_000_000;
	pub const MaxInstructions: u32 = 100;
	pub DotPerSecond: (AssetId, u128) = (
		MultiLocation::parent().into(),
		dot_per_second()
	);
	pub ZeroPerSecond: (AssetId, u128) = (
		native_currency_location(
			ParachainInfo::get().into(),
			ZERO
		).into(),
		zero_per_second()
	);
	pub PlayPerSecond: (AssetId, u128) = (
		native_currency_location(
			ParachainInfo::get().into(),
			PLAY
		).into(),
		play_per_second()
	);
	pub GamePerSecond: (AssetId, u128) = (
		native_currency_location(
			ParachainInfo::get().into(),
			GAME
		).into(),
		game_per_second()
	);
}

pub struct ToAuthor;
impl TakeRevenue for ToAuthor {
	fn take_revenue(revenue: MultiAsset) {
		if let MultiAsset {
			id: Concrete(location),
			fun: Fungible(amount),
		} = revenue
		{
			if let Some(currency_id) = CurrencyIdConvert::convert(location) {
				if let Some(author) = pallet_authorship::Pallet::<Runtime>::author() {
					let _ = Currencies::deposit(currency_id, &author, amount);
				} else {
					let _ = Currencies::deposit(currency_id, &TreasuryAccountId::get(), amount);
				}
			}
		}
	}
}

pub struct ChainFixedConversionRateProvider;
impl FixedConversionRateProvider for ChainFixedConversionRateProvider {
	fn get_fee_per_second(location: &MultiLocation) -> Option<u128> {
		let metadata = AssetRegistry::fetch_metadata_by_location(location)?;
		Some(metadata.additional.fee_per_second)
	}
}

pub type Trader = (
	FixedRateOfFungible<DotPerSecond, ToAuthor>,
	FixedRateOfFungible<PlayPerSecond, ToAuthor>,
	FixedRateOfFungible<GamePerSecond, ToAuthor>,
	FixedRateOfFungible<ZeroPerSecond, ToAuthor>,
	AssetRegistryTrader<FixedRateAssetRegistryTrader<ChainFixedConversionRateProvider>, ToAuthor>,
);

pub struct XcmConfig;
impl xcm_executor::Config for XcmConfig {
	type Call = Call;
	type XcmSender = XcmRouter;
	// How to withdraw and deposit an asset.
	type AssetTransactor = LocalAssetTransactor;
	type OriginConverter = XcmOriginToCallOrigin;
	type IsReserve = MultiNativeAsset<AbsoluteReserveProvider>;
	type IsTeleporter = NativeAsset;
	type LocationInverter = LocationInverter<Ancestry>;
	type Barrier = Barrier;
	type Weigher = FixedWeightBounds<UnitWeightCost, Call, MaxInstructions>;
	type Trader = Trader;
	type ResponseHandler = PolkadotXcm;
	type AssetTrap = PolkadotXcm;
	type AssetClaims = PolkadotXcm;
	type SubscriptionService = PolkadotXcm;
}

/// No local origins on this chain are allowed to dispatch XCM sends/executions.
pub type LocalOriginToLocation = (SignedToAccountId32<Origin, AccountId, RelayNetwork>,);

/// The means for routing XCM messages which are not for local execution into the right message
/// queues.
pub type XcmRouter = (
	// Two routers - use UMP to communicate with the relay chain:
	cumulus_primitives_utility::ParentAsUmp<ParachainSystem, PolkadotXcm>,
	// ..and XCMP to communicate with the sibling chains.
	XcmpQueue,
);

impl pallet_xcm::Config for Runtime {
	type Event = Event;
	type Call = Call;
	type Origin = Origin;
	type SendXcmOrigin = EnsureXcmOrigin<Origin, LocalOriginToLocation>;
	type XcmRouter = XcmRouter;
	type ExecuteXcmOrigin = EnsureXcmOrigin<Origin, LocalOriginToLocation>;
	type XcmExecuteFilter = Nothing;
	type XcmExecutor = XcmExecutor<XcmConfig>;
	type XcmTeleportFilter = Everything;
	type XcmReserveTransferFilter = Everything;
	type Weigher = FixedWeightBounds<UnitWeightCost, Call, MaxInstructions>;
	type LocationInverter = LocationInverter<Ancestry>;
	type AdvertisedXcmVersion = pallet_xcm::CurrentXcmVersion;
	const VERSION_DISCOVERY_QUEUE_SIZE: u32 = 100;
}

impl cumulus_pallet_xcm::Config for Runtime {
	type Event = Event;
	type XcmExecutor = XcmExecutor<XcmConfig>;
}

impl cumulus_pallet_xcmp_queue::Config for Runtime {
	type Event = Event;
	type XcmExecutor = XcmExecutor<XcmConfig>;
	type ChannelInfo = ParachainSystem;
	type VersionWrapper = PolkadotXcm;
	type ExecuteOverweightOrigin = EnsureRootOrHalfCouncil;
	type ControllerOrigin = EnsureRootOrHalfCouncil;
	type ControllerOriginConverter = XcmOriginToCallOrigin;
	type WeightInfo = ();
}

impl cumulus_pallet_dmp_queue::Config for Runtime {
	type Event = Event;
	type XcmExecutor = XcmExecutor<XcmConfig>;
	type ExecuteOverweightOrigin = EnsureRootOrHalfCouncil;
}

pub type LocalAssetTransactor = MultiCurrencyAdapter<
	Currencies,
	UnknownTokens,
	IsNativeConcrete<CurrencyId, CurrencyIdConvert>,
	AccountId,
	LocationToAccountId,
	CurrencyId,
	CurrencyIdConvert,
	DepositToAlternative<TreasuryAccountId, Currencies, CurrencyId, AccountId, Balance>,
>;

pub fn native_currency_location(para_id: u32, id: CurrencyId) -> MultiLocation {
	MultiLocation::new(
		1,
		X2(
			Parachain(para_id), GeneralKey(
				WeakBoundedVec::<u8, ConstU32<32>>::force_from(id.encode(), None)
			)
		)
	)
}

pub struct CurrencyIdConvert;
impl Convert<CurrencyId, Option<MultiLocation>> for CurrencyIdConvert {
	fn convert(id: CurrencyId) -> Option<MultiLocation> {
		match id {
			DOT => Some(MultiLocation::parent()),
			ZERO | GAME | PLAY
				=> Some(native_currency_location(ParachainInfo::get().into(), id)),
			ForeignAsset(id)
				=> AssetRegistry::multilocation(&id).unwrap_or_default(),
		}
	}
}
impl Convert<MultiLocation, Option<CurrencyId>> for CurrencyIdConvert {
	fn convert(location: MultiLocation) -> Option<CurrencyId> {

		fn decode_currency_id(key: WeakBoundedVec::<u8, ConstU32<32>>) -> Option<CurrencyId> {
			let key = &key.into_inner()[..];
			if let Ok(currency_id) = CurrencyId::decode(&mut &*key) {
				// check `currency_id` is cross-chain asset
				match currency_id {
					ZERO | GAME | PLAY => Some(currency_id),
					_ => None,
				}
			} else {
				None
			}
		}

		match location.clone() {
			x if x == MultiLocation::parent() => Some(DOT),
			MultiLocation {
				parents: 1,
				interior: X2(Parachain(id), GeneralKey(key)),
			} if ParaId::from(id) == ParachainInfo::get() => decode_currency_id(key),
			MultiLocation {
				parents: 0,
				interior: X1(GeneralKey(key)),
			} => decode_currency_id(key),
			_ => None,
		}
		.or_else(|| AssetRegistry::location_to_asset_id(&location)
		.map(|id| CurrencyId::ForeignAsset(id)))
	}
}
impl Convert<MultiAsset, Option<CurrencyId>> for CurrencyIdConvert {
	fn convert(asset: MultiAsset) -> Option<CurrencyId> {
		if let MultiAsset {
			id: Concrete(location), ..
		} = asset
		{
			Self::convert(location)
		} else {
			None
		}
	}
}

parameter_types! {
	pub SelfLocation: MultiLocation = MultiLocation::new(1, X1(Parachain(ParachainInfo::get().into())));
}

pub struct AccountIdToMultiLocation;
impl Convert<AccountId, MultiLocation> for AccountIdToMultiLocation {
	fn convert(account: AccountId) -> MultiLocation {
		X1(AccountId32 {
			network: NetworkId::Any,
			id: account.into(),
		})
		.into()
	}
}

parameter_types! {
	pub const MaxAssetsForTransfer: usize = 2;
}

parameter_type_with_key! {
	pub ParachainMinFee: |location: MultiLocation| -> Option<u128> {
		match (location.parents, location.first_interior()) {
			(1, Some(Parachain(parachains::acala::ID))) => Some(200_000_000),
			_ => None,
		}
	};
}

impl orml_xtokens::Config for Runtime {
	type Event = Event;
	type Balance = Balance;
	type CurrencyId = CurrencyId;
	type CurrencyIdConvert = CurrencyIdConvert;
	type AccountIdToMultiLocation = AccountIdToMultiLocation;
	type SelfLocation = SelfLocation;
	type XcmExecutor = XcmExecutor<XcmConfig>;
	type Weigher = FixedWeightBounds<UnitWeightCost, Call, MaxInstructions>;
	type BaseXcmWeight = UnitWeightCost;
	type LocationInverter = <XcmConfig as xcm_executor::Config>::LocationInverter;
	type MaxAssetsForTransfer = MaxAssetsForTransfer;
	type MinXcmFee = ParachainMinFee;
	type MultiLocationsFilter = Everything;
	type ReserveProvider = AbsoluteReserveProvider;
}

impl orml_unknown_tokens::Config for Runtime {
	type Event = Event;
}

impl orml_xcm::Config for Runtime {
	type Event = Event;
	type SovereignOrigin = EnsureRootOrThreeFourthsCouncil;
}
