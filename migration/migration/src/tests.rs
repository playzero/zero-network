#![cfg(test)]

use super::*;
use codec::Encode;
// use frame_support::traits::Hooks;
use frame_support::{assert_noop, assert_ok};
use frame_system::{EventRecord, Phase, RawOrigin};
use mock::{Event, Moment, *};
// use sp_core::H256;


#[test]
fn migration_tokens_airdrop() {
	new_test_ext().execute_with(|| {

		Balances::make_free_balance_be(&ALICE, 100);
		// Balances::make_free_balance_be(&BOB, 50);
		// Balances::make_free_balance_be(&DAVE, 0);

		assert_ok!(Migration::tokens_airdrop(Origin::root()));

		// for (account_id, account_data) in frame_system::Account::<Test>::iter() {
		// 	assert_eq!(account_id, 2);
		// }

		assert_eq!(
			orml_tokens::Accounts::<Test>::get(ALICE, PROTOCOL_TOKEN_ID).free,
			100
		);
		
		
	});
}
