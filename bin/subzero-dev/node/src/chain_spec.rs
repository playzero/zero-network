use cumulus_primitives_core::ParaId;
use subzero_dev_runtime::{AccountId, AuraId, SS58Prefix, Signature, SudoConfig};
use sc_chain_spec::{ChainSpecExtension, ChainSpecGroup, Properties};
use sc_service::ChainType;
use serde::{Deserialize, Serialize};
use sp_core::{sr25519, Pair, Public, crypto::UncheckedInto};
use sp_runtime::{BoundedVec, traits::{IdentifyAccount, Verify, Zero}};
use sp_std::collections::btree_map::BTreeMap;
use hex_literal::hex;

use gamedao_control::types::{OrgType, AccessModel, FeeModel};
use orml_tokens;

use primitives::{
	currency::{ZERO, PLAY, GAME, DOT, TokenInfo, CurrencyId},
	cent, dollar, Balance
};

/// Specialized `ChainSpec` for the normal parachain runtime.
pub type ChainSpec =
	sc_service::GenericChainSpec<subzero_dev_runtime::GenesisConfig, Extensions>;

/// The default XCM version to set in genesis config.
const SAFE_XCM_VERSION: u32 = xcm::prelude::XCM_VERSION;
const DEFAULT_PARA_ID: u32 = 2000;

/// Helper function to generate a crypto pair from seed
pub fn get_from_seed<TPublic: Public>(seed: &str) -> <TPublic::Pair as Pair>::Public {
	TPublic::Pair::from_string(&format!("//{}", seed), None)
		.expect("static values are valid; qed")
		.public()
}

/// The extensions for the [`ChainSpec`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ChainSpecGroup, ChainSpecExtension)]
#[serde(deny_unknown_fields)]
pub struct Extensions {
	/// The relay chain of the Parachain.
	pub relay_chain: String,
	/// The id of the Parachain.
	pub para_id: u32,
}

impl Extensions {
	/// Try to get the extension from the given `ChainSpec`.
	pub fn try_get(chain_spec: &dyn sc_service::ChainSpec) -> Option<&Self> {
		sc_chain_spec::get_extension(chain_spec.extensions())
	}
}

type AccountPublic = <Signature as Verify>::Signer;

/// Generate collator keys from seed.
///
/// This function's return type must always match the session keys of the chain in tuple format.
pub fn get_collator_keys_from_seed(seed: &str) -> AuraId {
	get_from_seed::<AuraId>(seed)
}

/// Helper function to generate an account ID from seed
pub fn get_account_id_from_seed<TPublic: Public>(seed: &str) -> AccountId
where
	AccountPublic: From<<TPublic::Pair as Pair>::Public>,
{
	AccountPublic::from(get_from_seed::<TPublic>(seed)).into_account()
}

/// Generate the session keys from individual elements.
///
/// The input must be a tuple of individual keys (a single arg for now since we have just one key).
pub fn subzero_session_keys(keys: AuraId) -> subzero_dev_runtime::SessionKeys {
	subzero_dev_runtime::SessionKeys { aura: keys }
}

/// Give your currencies a unit name and decimal places
pub fn get_properties() -> Properties {
	let mut properties = Properties::new();
	let mut token_symbol: Vec<String> = vec![];
	let mut token_decimals: Vec<u32> = vec![];
	[ZERO, PLAY, GAME, DOT].iter().for_each(|token| {
		token_symbol.push(token.symbol().unwrap().to_string());
		token_decimals.push(token.decimals().unwrap() as u32);
	});
	properties.insert("tokenSymbol".into(), token_symbol.into());
	properties.insert("tokenDecimals".into(), token_decimals.into());
	properties.insert("ss58Format".into(), SS58Prefix::get().into());
	properties
}

pub fn development_config() -> ChainSpec {
	// Give your base currency a unit name and decimal places
	let properties = get_properties();
	ChainSpec::from_genesis(
		// Name
		"Development",
		// ID
		"dev",
		ChainType::Development,
		move || {
			testnet_genesis(
				// initial collators.
				vec![
					(
						get_account_id_from_seed::<sr25519::Public>("Alice"),
						get_collator_keys_from_seed("Alice"),
					),
					(
						get_account_id_from_seed::<sr25519::Public>("Bob"),
						get_collator_keys_from_seed("Bob"),
					),
				],
				vec![
					get_account_id_from_seed::<sr25519::Public>("Alice"),
					get_account_id_from_seed::<sr25519::Public>("Bob"),
					get_account_id_from_seed::<sr25519::Public>("Charlie"),
					get_account_id_from_seed::<sr25519::Public>("Dave"),
					get_account_id_from_seed::<sr25519::Public>("Eve"),
					get_account_id_from_seed::<sr25519::Public>("Ferdie"),
					get_account_id_from_seed::<sr25519::Public>("Alice//stash"),
					get_account_id_from_seed::<sr25519::Public>("Bob//stash"),
					get_account_id_from_seed::<sr25519::Public>("Charlie//stash"),
					get_account_id_from_seed::<sr25519::Public>("Dave//stash"),
					get_account_id_from_seed::<sr25519::Public>("Eve//stash"),
					get_account_id_from_seed::<sr25519::Public>("Ferdie//stash"),
				],
				get_account_id_from_seed::<sr25519::Public>("Alice"),
				DEFAULT_PARA_ID.into(),
			)
		},
		Vec::new(),
		None,
		None,
		None,
		Some(properties),
		Extensions {
			relay_chain: "rococo-local".into(), // You MUST set this to the correct network!
			para_id: DEFAULT_PARA_ID,
		},
	)
}

pub fn local_testnet_config() -> ChainSpec {
	let properties = get_properties();

	ChainSpec::from_genesis(
		// Name
		"Local Testnet",
		// ID
		"local_testnet",
		ChainType::Local,
		move || {
			testnet_genesis(
				// initial collators.
				vec![
					(
						get_account_id_from_seed::<sr25519::Public>("Alice"),
						get_collator_keys_from_seed("Alice"),
					),
					(
						get_account_id_from_seed::<sr25519::Public>("Bob"),
						get_collator_keys_from_seed("Bob"),
					),
				],
				vec![
					get_account_id_from_seed::<sr25519::Public>("Alice"),
					get_account_id_from_seed::<sr25519::Public>("Bob"),
					get_account_id_from_seed::<sr25519::Public>("Charlie"),
					get_account_id_from_seed::<sr25519::Public>("Dave"),
					get_account_id_from_seed::<sr25519::Public>("Eve"),
					get_account_id_from_seed::<sr25519::Public>("Ferdie"),
					get_account_id_from_seed::<sr25519::Public>("Alice//stash"),
					get_account_id_from_seed::<sr25519::Public>("Bob//stash"),
					get_account_id_from_seed::<sr25519::Public>("Charlie//stash"),
					get_account_id_from_seed::<sr25519::Public>("Dave//stash"),
					get_account_id_from_seed::<sr25519::Public>("Eve//stash"),
					get_account_id_from_seed::<sr25519::Public>("Ferdie//stash"),
				],
				get_account_id_from_seed::<sr25519::Public>("Alice"),
				DEFAULT_PARA_ID.into(),
			)
		},
		// Bootnodes
		Vec::new(),
		// Telemetry
		None,
		// Protocol ID
		Some("subzero-local"),
		// Fork ID
		None,
		// Properties
		Some(properties),
		// Extensions
		Extensions {
			relay_chain: "rococo-local".into(), // You MUST set this to the correct network!
			para_id: DEFAULT_PARA_ID,
		},
	)
}

fn testnet_genesis(
	invulnerables: Vec<(AccountId, AuraId)>,
	endowed_accounts: Vec<AccountId>,
	root_key: AccountId,
	id: ParaId,
) -> subzero_dev_runtime::GenesisConfig {
	subzero_dev_runtime::GenesisConfig {
		system: subzero_dev_runtime::SystemConfig {
			code: subzero_dev_runtime::WASM_BINARY
				.expect("WASM binary was not build, please build it!")
				.to_vec(),
		},
		sudo: SudoConfig { key: Some(root_key) },
		balances: subzero_dev_runtime::BalancesConfig {
			balances: endowed_accounts.iter().cloned().map(|k| (k, 1 << 60)).collect(),
		},
		parachain_info: subzero_dev_runtime::ParachainInfoConfig { parachain_id: id },
		collator_selection: subzero_dev_runtime::CollatorSelectionConfig {
			invulnerables: invulnerables.iter().cloned().map(|(acc, _)| acc).collect(),
			candidacy_bond: cent(ZERO) * 16,
			..Default::default()
		},
		session: subzero_dev_runtime::SessionConfig {
			keys: invulnerables
				.into_iter()
				.map(|(acc, aura)| {
					(
						acc.clone(),                 // account id
						acc,                         // validator id
						subzero_session_keys(aura), // session keys
					)
				})
				.collect(),
		},
		// no need to pass anything to aura, in fact it will panic if we do. Session will take care
		// of this.
		aura: Default::default(),
		aura_ext: Default::default(),
		parachain_system: Default::default(),
		polkadot_xcm: subzero_dev_runtime::PolkadotXcmConfig {
			safe_xcm_version: Some(SAFE_XCM_VERSION),
		},
		transaction_payment: Default::default(),
		council: Default::default(),
		treasury: Default::default(),
		tokens: Default::default(),
		democracy: Default::default(),
		technical_committee: Default::default(),
		elections: Default::default(),
		technical_membership: Default::default(),
		control: Default::default(),
		asset_registry: Default::default(),
	}
}

pub fn development_subzero_config() -> ChainSpec {
	let properties = get_properties();

	ChainSpec::from_genesis(
		// Name
		"Subzero Development",
		// ID
		"subzero-dev",
		ChainType::Local,
		// SECRET="..."
		// docker run --rm parity/subkey inspect "$SECRET//subzero//root" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//1//collator" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//1//aura" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//2//collator" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//2//aura" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//zero-treasury" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//gamedao-treasury" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//game3-treasury" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//prime" 
		move || {
			subzero_genesis(
				// Initial collators
				vec![
					(
						// 5FyCQF345qawarNwSoUgg5ankQt1g4J4wHwryMsEiUHi592W
						hex!["acbd8ce2c37263c4964f4babcc6eaedd5276006c448e046290d366318c3b0016"].into(),
						hex!["d081d0b6ee6fe89fca7ed9ee7a927da7462389798a2ed1a62caa456de487f574"].unchecked_into(),
					),
					(
						// 5FP7zjy4uZ46uGSTrXBhkk6L7RkKtS8emD9BLAFAXBDS5nD2
						hex!["92c0609c5bc85053246436c99e5474163e2f73f59adef926bc0897502eca1945"].into(),
						hex!["d008cd73618b8bce15531f693516635800ec1ee69f19e43f25f32129c5d5fd39"].unchecked_into(),
					),
				],
				vec![
					// Faucet Bot
					// 5Ef24TjfGNPnxrALTsPN3wJ9XoTTY4coMvryNed4MkZmqKaL
					hex!["72a416a9270487e9fa301883780fe22edfac394bcf018b5da4444dff38937574"].into(),
					// Org Prime
					// 5GWchgf4qbaJZTBbmKTGWDCVPJt1XPLQfxaKsDe3Y3LqyHbT
					hex!["c4b3f61d4f896a660d35bdf6a78c54cf2aa2af48ba0945f568dc67674247e136"].into(),
					// Root
					// 5CwWu8QwK8hdHcC5JHWWVSbxAFcLD4eUPsWY1BEy48LxjjMf
					hex!["26c0a10c263cf2583936ff9714a645de855e39090765a0a23fe8a2001ab83016"].into(),
				],
				// Root
				// 5CwWu8QwK8hdHcC5JHWWVSbxAFcLD4eUPsWY1BEy48LxjjMf
				hex!["26c0a10c263cf2583936ff9714a645de855e39090765a0a23fe8a2001ab83016"].into(),
				// Zero treasury account
				// 5EeG87W6f5X39SUeTLM6oBT8M5BZ4ApQbncKw78QFvgkGnqd
				hex!["72102ee7e9bd0170c9829054bf8aaab65ea2555b1c30b04747160fe4a86d5b47"].into(),
				// Gamedao treasury account
				// 5EnLtQYf87Kab8vR8CyGmFXjo1HjYLUbFAdCsK5rFMAhfT4R
				hex!["783a2eb5d5b4f1f3a7b49bea0c82119cccb366fd7b05d4c44b21ea160b52b160"].into(),
				// Game3 treasury account
				// 5DtGqwDh1rgcGYX2QxqxBs5S7VsNFbowX7cUiGiLEQ2aP9UV
				hex!["5083d014500f360fa17ec74d50f0b8336e205f11d7a9e4cfc4bb42a7a3da4c6d"].into(),
				// Org Prime
				// 5GWchgf4qbaJZTBbmKTGWDCVPJt1XPLQfxaKsDe3Y3LqyHbT
				hex!["c4b3f61d4f896a660d35bdf6a78c54cf2aa2af48ba0945f568dc67674247e136"].into(),
				DEFAULT_PARA_ID.into(),
			)
		},
		vec![
			"/dns/collator-1/tcp/30333/p2p/12D3KooWBQB9MUka9nMcqXi3ANLUdu4FweQpMvw9VHpHveiYhsPx"
				.parse()
				.unwrap(),
		],
		// Telemetry
		None,
		// Protocol ID
		Some("subzero-dev"),
		// Fork ID
		None,
		Some(properties),
		Extensions {
			relay_chain: "rococo-local".into(), // You MUST set this to the correct network!
			para_id: DEFAULT_PARA_ID,
		},
	)
}

pub fn staging_subzero_config() -> ChainSpec {
	let properties = get_properties();

	ChainSpec::from_genesis(
		// Name
		"Subzero Staging",
		// ID
		"subzero-stage",
		ChainType::Local,
		// SECRET="..."
		// docker run --rm parity/subkey inspect "$SECRET//subzero//root" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//1//collator" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//1//aura" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//2//collator" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//2//aura" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//3//collator" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//3//aura" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//4//collator" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//4//aura" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//zero-treasury" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//gamedao-treasury" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//game3-treasury" 
		// docker run --rm parity/subkey inspect "$SECRET//subzero//prime" 
		move || {
			subzero_genesis(
				// Initial collators
				vec![
					(
						// 5EJVzfYgcPEncpwNoQp7ryzfJRjNfxQs4b4gUUdT7ya2Hg16
						hex!["62fdf8994e59cf79ef96338ccf1fa6fffd37c1a843841db0d3963efef0e4d31d"].into(),
						hex!["90bf7542167747bcceb0614d4e461df631a8557cf9a7009899bacb203479f642"].unchecked_into(),
					),
					(
						// 5GH83LCgSC6Kp5WZtF7pvaBiyqY22ZeH2gr1vCAh3TxpM83i
						hex!["ba6949eb864d92d360bf997043b4229b7f54a6df5136af5b4f885a1ce89fd462"].into(),
						hex!["eab75e30bd998c79befb81938daf6189f983273b0aeb292b15667d01245bfa6c"].unchecked_into(),
					),
					(
						// 5EZZFSPHc2myoFFPFoM7k7Cy1p3niGoXCP8iG4qAKWcDMVVM
						hex!["6e799bc5437690e6bd86aa0df8451eaf605ae7b2ec0266f9899905265572b018"].into(),
						hex!["a2d2896c87b1db42b952835a775ff7418375ef4dc171b92640b4c11c22a2250f"].unchecked_into(),
					),
					(
						// 5FKnYx26KziwtQeXmR6gaD1LwgMUUjCFjG5YAQQES1RfHFbV
						hex!["90352f3b6f33a62c3f43cfe08726724d48cbc4919e27a5d1e55efa6881bad229"].into(),
						hex!["96750acce3e01bedb5b2ffd7706f2e7b4a83c5e2f8714257d9d2ab95fb61da0d"].unchecked_into(),
					),
				],
				vec![
					// Faucet Bot
					// 5Ef24TjfGNPnxrALTsPN3wJ9XoTTY4coMvryNed4MkZmqKaL
					hex!["72a416a9270487e9fa301883780fe22edfac394bcf018b5da4444dff38937574"].into(),
					// Org Prime
					// 5GeBJnjVxueXj5PsTFLTBoPQzt5scehWkKL84JLJ8hXWexaP
					hex!["ca7870338715348c17ffb5a299c4ae9e2ceec0726d8d72a2f502060942af045d"].into(),
					// Root
					// 5HdZAx6ieBdLPdeQjj778ST7oq6poUkhr7nZ32mWzEjWGwQa
					hex!["f63b07ad8610dcbdff485d2497fb359077ec8bfef9d9ffdb38022d7cdc986318"].into(),
				],
				// Root
				// 5HdZAx6ieBdLPdeQjj778ST7oq6poUkhr7nZ32mWzEjWGwQa
				hex!["f63b07ad8610dcbdff485d2497fb359077ec8bfef9d9ffdb38022d7cdc986318"].into(),
				// Zero treasury account
				// 5EXAdVUvu3dyDb4Wn55XUdxWBv3fPhvf9Vzf76LsFBe72WA4
				hex!["6ca6f9057fa6374afd5c0b92a40905340f56e96f4905b36a0526917016f48667"].into(),
				// Gamedao treasury account
				// 5CSM7WW5icde1aqCK4xfbhBXCVqE76dZX2tvMtntaVvEBuFv
				hex!["10819164c87b9f4dc70e64879d170d52378358b538b80fdabc0e90028a66f570"].into(),
				// Game3 treasury account
				// 5GNFNuNAJbD4ECDVHjYKHZibPUfGXHdFqKRoqVDbopsSGAim
				hex!["be52373f74b5fa074de8edaa76e288305f200a6831d3550c0ebe4e02da9fa52d"].into(),
				// Org Prime
				// 5GeBJnjVxueXj5PsTFLTBoPQzt5scehWkKL84JLJ8hXWexaP
				hex!["ca7870338715348c17ffb5a299c4ae9e2ceec0726d8d72a2f502060942af045d"].into(),
				DEFAULT_PARA_ID.into(),
			)
		},
		vec![
			"/dns/collator-1/tcp/30333/p2p/12D3KooWBQB9MUka9nMcqXi3ANLUdu4FweQpMvw9VHpHveiYhsPx"
				.parse()
				.unwrap(),
		],
		// Telemetry
		None,
		// Protocol ID
		Some("subzero-stage"),
		// Fork ID
		None,
		Some(properties),
		Extensions {
			relay_chain: "rococo-local".into(), // You MUST set this to the correct network!
			para_id: DEFAULT_PARA_ID,
		},
	)
}

fn subzero_genesis(
	invulnerables: Vec<(AccountId, AuraId)>,
	endowed_accounts: Vec<AccountId>,
	root_key: AccountId,
	zero_treasury: AccountId,
	gamedao_treasury: AccountId,
	game3_treasury: AccountId,
	org_prime: AccountId,
	id: ParaId,
) -> subzero_dev_runtime::GenesisConfig {
	subzero_dev_runtime::GenesisConfig {
		system: subzero_dev_runtime::SystemConfig {
			code: subzero_dev_runtime::WASM_BINARY
				.expect("WASM binary was not build, please build it!")
				.to_vec(),
		},
		sudo: SudoConfig { key: Some(root_key) },
		balances: subzero_dev_runtime::BalancesConfig {
			balances: balances_config(endowed_accounts.clone(), zero_treasury.clone(), game3_treasury.clone(), gamedao_treasury.clone()),
		},
		parachain_info: subzero_dev_runtime::ParachainInfoConfig { parachain_id: id },
		collator_selection: subzero_dev_runtime::CollatorSelectionConfig {
			invulnerables: invulnerables.iter().cloned().map(|(acc, _)| acc).collect(),
			candidacy_bond: cent(ZERO) * 16,
			..Default::default()
		},
		session: subzero_dev_runtime::SessionConfig {
			keys: invulnerables
				.into_iter()
				.map(|(acc, aura)| {
					(
						acc.clone(),                 // account id
						acc,                         // validator id
						subzero_session_keys(aura), // session keys
					)
				})
				.collect(),
		},
		// no need to pass anything to aura, in fact it will panic if we do. Session will take care
		// of this.
		aura: Default::default(),
		aura_ext: Default::default(),
		parachain_system: Default::default(),
		polkadot_xcm: subzero_dev_runtime::PolkadotXcmConfig {
			safe_xcm_version: Some(SAFE_XCM_VERSION),
		},
		transaction_payment: Default::default(),
		council: Default::default(),
		treasury: Default::default(),
		tokens: orml_tokens::GenesisConfig {
			balances: tokens_config(endowed_accounts.clone(), zero_treasury.clone(), game3_treasury.clone(), gamedao_treasury.clone()),
		},
		democracy: Default::default(),
		technical_committee: Default::default(),
		elections: Default::default(),
		technical_membership: Default::default(),
		control: gamedao_control::GenesisConfig {
			orgs: vec![
				// Zero Network
				(org_prime.clone(), org_prime.clone(), zero_treasury, BoundedVec::truncate_from(b"Zero Network".to_vec()),
					BoundedVec::truncate_from(b"QmUWu6zoFM4j1fwT6fHGEjiuCSft2sNcbmJtT23R9su3Bk".to_vec()),
					OrgType::Individual, AccessModel::Open, FeeModel::NoFees, Zero::zero(),
					GAME, PLAY, 1000, 1 * dollar(GAME)
				),
				// GameDao
				(org_prime.clone(), org_prime.clone(), gamedao_treasury, BoundedVec::truncate_from(b"GameDao".to_vec()),
					BoundedVec::truncate_from(b"QmcNp8hsRaxVDhKgbgo6Jtcb8papyPnKbqjzFtqzqxDE7j".to_vec()),
					OrgType::Individual, AccessModel::Open, FeeModel::NoFees, Zero::zero(),
					GAME, PLAY, 1000, 1 * dollar(GAME)
				),
				// Game3 Foundation
				(org_prime.clone(), org_prime.clone(), game3_treasury, BoundedVec::truncate_from(b"Game3 Foundation".to_vec()),
					BoundedVec::truncate_from(b"QmNYVx6bhRUGbMp6UEPHKhaLRwFfpsbsP4sFT9XCvMxgfz".to_vec()),
					OrgType::Individual, AccessModel::Open, FeeModel::NoFees, Zero::zero(),
					GAME, PLAY, 1000, 1 * dollar(GAME)
				),
				]
		},
		asset_registry: Default::default(),
	}
}

fn balances_config(accounts: Vec<AccountId>, zero_treasury: AccountId,
	game3_treasury: AccountId, gamedao_treasury: AccountId) -> Vec<(AccountId, Balance)> {
	// Zero Network Treasury owns ZERO issuance:
	let mut zero_issuance = 1_000_000_000 * dollar(ZERO);
	// Game3 Foundation Treasury:
	let game3_zero = 1_000_000 * dollar(ZERO);
	// GameDAO Treasury:
	let gamedao_zero = 1_000_000 * dollar(ZERO);

	let account_balances = accounts.iter().cloned().map(|x| (x, 1_000_000 * dollar(ZERO)))
		.fold(
			BTreeMap::<AccountId, Balance>::new(),
			|mut acc, (account_id, amount)| {
				if let Some(balance) = acc.get_mut(&account_id) {
					*balance = balance
						.checked_add(amount)
						.expect("balance overflow");
				} else {
					zero_issuance = zero_issuance.saturating_sub(amount);
					acc.insert(account_id.clone(), amount);
				}
				acc
			},
		)
		.into_iter()
		.collect::<Vec<(AccountId, Balance)>>();

	zero_issuance = zero_issuance.saturating_sub(game3_zero).saturating_sub(gamedao_zero);
	let mut balances = vec![
		(zero_treasury, zero_issuance),
		(gamedao_treasury, gamedao_zero),
		(game3_treasury, game3_zero),
	];
	balances.extend(account_balances);
	balances
}

fn tokens_config(accounts: Vec<AccountId>, zero_treasury: AccountId,
		game3_treasury: AccountId, gamedao_treasury: AccountId) -> Vec<(AccountId, CurrencyId, Balance)> {
	// Game3 Foundation Treasury owns GAME & PLAY issuance:
	let mut game_issuance = 100_000_000 * dollar(GAME);
	let mut play_issuance = 10_000_000 * dollar(PLAY);

	// Zero Network Treasury:
	let zeronet_game = 100_000 * dollar(GAME);
	let zeronet_play = 1_000_000 * dollar(PLAY);

	// GameDAO:
	let gamedao_game = 100_000 * dollar(GAME);
	let gamedao_play = 1_000_000 * dollar(PLAY);

	let account_balances = accounts
		.iter().cloned()
			.flat_map(|x| vec![(x.clone(), GAME, 1_000_000 * dollar(GAME)), (x.clone(), PLAY, 1_000_000 * dollar(PLAY))])
		.fold(
			Vec::new(),
			|mut vec, (account_id, currency_id, amount)| {
				match currency_id {
					GAME => { game_issuance = game_issuance.saturating_sub(amount); },
					PLAY => { play_issuance = play_issuance.saturating_sub(amount); },
					_ => ()
				};
				vec.push((account_id.clone(), currency_id, amount));
				vec
			},
		)
		.into_iter()
		.collect::<Vec<(AccountId, CurrencyId, Balance)>>();

	game_issuance = game_issuance.saturating_sub(zeronet_game).saturating_sub(gamedao_game);
	play_issuance = play_issuance.saturating_sub(zeronet_play).saturating_sub(gamedao_play);

	let mut token_balances = vec![
		(zero_treasury.clone(), GAME, zeronet_game),
		(zero_treasury, PLAY, zeronet_play),
		(game3_treasury.clone(), GAME, game_issuance),
		(game3_treasury, PLAY, play_issuance),
		(gamedao_treasury.clone(), GAME, gamedao_game),
		(gamedao_treasury, PLAY, gamedao_play),
	];
	token_balances.extend(account_balances);
	token_balances
}
