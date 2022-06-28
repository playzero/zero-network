#![cfg(test)]

use super::*;
use mock::{*, Event};
use frame_support::{assert_noop, assert_ok};
use frame_system::RawOrigin;
use primitives::TokenSymbol;


#[test]
fn register_native_asset_works() {
	new_test_ext().execute_with(|| {
		System::set_block_number(3);
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
