#![cfg(test)]

use super::*;
use frame_support::{assert_noop, assert_ok};
use frame_system::RawOrigin;
use mock::{Event, *};
use primitives::TokenSymbol;

#[test]
fn register_native_asset_works() {
	new_test_ext().execute_with(|| {
		System::set_block_number(3);
		// SBP-M2 review: you can create metadate once and then compare to copy instead of
		// initalization of 2 the same structs.
		assert_ok!(AssetRegistry::register_native_asset(
			// SBP-M2 review: you should provide entity like council to get rid of sudo calls
			RawOrigin::Root.into(),
			CurrencyId::Token(TokenSymbol::DOT),
			Box::new(AssetMetadata {
				name: b"Token Name".to_vec(),
				symbol: b"TN".to_vec(),
				decimals: 12,
				minimal_balance: 1,
			})
		));
		System::assert_has_event(Event::AssetRegistry(crate::Event::AssetRegistered {
			asset_id: AssetIds::NativeAssetId(CurrencyId::Token(TokenSymbol::DOT)),
			metadata: AssetMetadata {
				name: b"Token Name".to_vec(),
				symbol: b"TN".to_vec(),
				decimals: 12,
				minimal_balance: 1,
			},
		}));

		assert_eq!(
			AssetMetadatas::<Test>::get(AssetIds::NativeAssetId(CurrencyId::Token(TokenSymbol::DOT))),
			Some(AssetMetadata {
				name: b"Token Name".to_vec(),
				symbol: b"TN".to_vec(),
				decimals: 12,
				minimal_balance: 1,
			})
		);
		// SBP-M2 review: it should be another test case
		// Try to follow assumption - 1 test case per simple check
		// Can't duplicate
		assert_noop!(
			AssetRegistry::register_native_asset(
				RawOrigin::Root.into(),
				CurrencyId::Token(TokenSymbol::DOT),
				Box::new(AssetMetadata {
					name: b"Token Name".to_vec(),
					symbol: b"TN".to_vec(),
					decimals: 12,
					minimal_balance: 1,
				})
			),
			Error::<Test>::AssetIdExisted
		);
	});
}

// SBP-M2 review: above comments should be applied here
#[test]
fn update_native_asset_works() {
	new_test_ext().execute_with(|| {
		System::set_block_number(3);
		assert_noop!(
			AssetRegistry::update_native_asset(
				RawOrigin::Root.into(),
				CurrencyId::Token(TokenSymbol::DOT),
				Box::new(AssetMetadata {
					name: b"New Token Name".to_vec(),
					symbol: b"NTN".to_vec(),
					decimals: 13,
					minimal_balance: 2,
				})
			),
			Error::<Test>::AssetIdNotExists
		);

		assert_ok!(AssetRegistry::register_native_asset(
			RawOrigin::Root.into(),
			CurrencyId::Token(TokenSymbol::DOT),
			Box::new(AssetMetadata {
				name: b"Token Name".to_vec(),
				symbol: b"TN".to_vec(),
				decimals: 12,
				minimal_balance: 1,
			})
		));

		assert_ok!(AssetRegistry::update_native_asset(
			RawOrigin::Root.into(),
			CurrencyId::Token(TokenSymbol::DOT),
			Box::new(AssetMetadata {
				name: b"New Token Name".to_vec(),
				symbol: b"NTN".to_vec(),
				decimals: 13,
				minimal_balance: 2,
			})
		));
		System::assert_has_event(Event::AssetRegistry(crate::Event::AssetUpdated {
			asset_id: AssetIds::NativeAssetId(CurrencyId::Token(TokenSymbol::DOT)),
			metadata: AssetMetadata {
				name: b"New Token Name".to_vec(),
				symbol: b"NTN".to_vec(),
				decimals: 13,
				minimal_balance: 2,
			},
		}));

		assert_eq!(
			AssetMetadatas::<Test>::get(AssetIds::NativeAssetId(CurrencyId::Token(TokenSymbol::DOT))),
			Some(AssetMetadata {
				name: b"New Token Name".to_vec(),
				symbol: b"NTN".to_vec(),
				decimals: 13,
				minimal_balance: 2,
			})
		);
	});
}
