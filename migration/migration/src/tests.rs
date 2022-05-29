#![cfg(test)]

use super::*;
use frame_support::assert_ok;
use mock::*;


#[test]
fn migration_tokens_airdrop() {
	new_test_ext().execute_with(|| {
		// Set initial Balances
		Balances::make_free_balance_be(&ALICE, 100);
		Balances::make_free_balance_be(&BOB, 50);
		Balances::make_free_balance_be(&DAVE, 0);
		// Proceed with airdrop
		assert_ok!(Migration::tokens_airdrop(Origin::root()));
		// Check if tokens were sent
		assert_eq!(orml_tokens::Accounts::<Test>::get(ALICE, PROTOCOL_TOKEN_ID).free, 100);
		assert_eq!(orml_tokens::Accounts::<Test>::get(ALICE, PAYMENT_TOKEN_ID).free, 100);
		assert_eq!(orml_tokens::Accounts::<Test>::get(BOB, PROTOCOL_TOKEN_ID).free, 50);
		assert_eq!(orml_tokens::Accounts::<Test>::get(BOB, PAYMENT_TOKEN_ID).free, 50);
		assert_eq!(orml_tokens::Accounts::<Test>::get(DAVE, PROTOCOL_TOKEN_ID).free, 0);
		assert_eq!(orml_tokens::Accounts::<Test>::get(DAVE, PAYMENT_TOKEN_ID).free, 0);
		// TODO: Event test
		// System::assert_has_event(mock::Event::Migration(crate::Event::TokensAirDropped(1)));
	});
}
