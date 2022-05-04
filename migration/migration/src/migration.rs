use frame_support::{
	weights::Weight,
	storage::unhashed::kill_prefix,
	traits::Get
};
use sp_io::{
	KillStorageResult,
	hashing::twox_128
};
use super::{Config, MigrationVersion, StorageVersion};

pub fn migrate<T: Config>() -> Weight {
	if <MigrationVersion<T>>::get() == StorageVersion::V2Imported {
		return 0 as Weight;
	}
	let names = [
		"ZeroSense", "Sense48",
		"GameDaoControl", "Control50",
		"GameDaoGovernance", "Signal50",
		"GameDaoCrowdfunding", "Flow50",
		"GameDaoTangram", "Tangram50"
	];
	let mut total_removed: u32 = 0;
	for name in names {
		match kill_prefix(&twox_128(&name.as_bytes()), None) {
			KillStorageResult::AllRemoved(n) => {
				total_removed += n;
			},
			KillStorageResult::SomeRemaining(n) => {
				total_removed += n;
			}
		}
	}
	<MigrationVersion<T>>::set(StorageVersion::V2Imported);
	T::DbWeight::get().reads_writes(total_removed as Weight, total_removed as Weight)
}
