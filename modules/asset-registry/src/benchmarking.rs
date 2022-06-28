#![cfg(feature = "runtime-benchmarks")]

use crate::*;
use crate::Pallet as AssetRegistry;
use frame_benchmarking::{account, benchmarks, impl_benchmark_test_suite, whitelisted_caller};
use frame_system::RawOrigin;
use sp_runtime::{DispatchError, traits::SaturatedConversion};
use sp_std::{boxed::Box, vec};
use primitives::{currency::AssetMetadata, TokenSymbol};


benchmarks! {

	register_native_asset {
		let asset_metadata = AssetMetadata {
			name: b"Token Name".to_vec(),
			symbol: b"TN".to_vec(),
			decimals: 12,
			minimal_balance: 1,
		};
	}: _(RawOrigin::Root, CurrencyId::Token(TokenSymbol::DOT), Box::new(asset_metadata))

	update_native_asset {
		let currency_id = CurrencyId::Token(TokenSymbol::DOT);
		let asset_metadata = AssetMetadata {
			name: b"Token Name".to_vec(),
			symbol: b"TN".to_vec(),
			decimals: 12,
			minimal_balance: 1,
		};

		AssetRegistry::<T>::register_native_asset(RawOrigin::Root.into(), currency_id, Box::new(asset_metadata.clone()))?;
	}: _(RawOrigin::Root, currency_id, Box::new(asset_metadata))

}

impl_benchmark_test_suite!(AssetRegistry, crate::mock::new_test_ext(), crate::mock::Test);