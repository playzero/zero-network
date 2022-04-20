use super::*;
use frame_support::{BoundedVec};


struct IdentityItem<T: Config> {
	account_id: T::AccountId,
	registration: Registration<BalanceOf<T>, T::MaxRegistrars, T::MaxAdditionalFields>
}


pub fn migrate_to_v2<T: Config>() -> frame_support::weights::Weight {
	if <PalletVersion<T>>::get() == StorageVersion::V2Imported {
		return 0;
	}
	let identities: Vec<IdentityItem<T>> = vec![
		IdentityItem {
			account_id: hex_literal::hex!["2aeddc77fe58c98d50bd37f1b90840f9cd7f37317cd20b61e9bd46fab8704714066248836b2715eb5ee033aa8b119115b8746cd9d995912a9186547604c039c5ca655f5b5d23644a"].into(),
			registration: Registration {
				judgements: BoundedVec::default(),
				deposit: 1000000000000000.into(),
				info: IdentityInfo {
					additional: BoundedVec::default(),
					display: Data::Raw(b"Pok3".to_vec().try_into().unwrap()),
					legal: Data::None,
					web: Data::None,
					riot: Data::None,
					email: Data::None,
					pgp_fingerprint: Option::None,
					image: Data::None,
					twitter: Data::None
				}
			}
		}

	];
	for i in identities {
		<IdentityOf<T>>::insert(&i.account_id, i.registration);
	}
	<PalletVersion<T>>::set(StorageVersion::V2Imported)

	T::DbWeight::get().reads_writes(1 as Weight, identities.len() as Weight + 1)
}
