# Local parachain setup

Docker-compose setup to run relay chain and parachain locally.
Using substrate `0.9.25` version.
Consists of:
- 2 relay chain validators;
- 1 parachain collator.


### How to run
1. Start docker-compose services
```bash
docker-compose -f .docker/local-relay-parachain/docker-compose.yaml up
```
2. Register new paraid on the relay chain.
   Open polkadot UI and connect to one of relay chain nodes, availabe on `9944` and `9945` ports.
   Navigate to `Network > Parathreads > Add ParaId`.
   Make sure it's value is `2000`, otherwise you would need to update plain parachain chainspec and generate new raw chainspec.
3. Register parachain calling `paraSudoWrapper.sudoScheduleParaInitialize` extrinsic, using binary files from [wasm](./wasm) directory and 2000 paraid (make sure to check Parachain flag).
4. Wait for the new epoch and check that parachain is active through `Parachains` tab.


### Upgrading to new version
1. Generate polkadot rococo chain spec
2. Generate parachain chainspec
3. Generate new chain spec, wasm and genesis binaries.
```bash
./target/release/subzero build-spec --disable-default-bootnode > .docker/local-relay-parachain/chainspec/subzero-parachain-plain.json
./target/release/subzero build-spec --chain .docker/local-relay-parachain/chainspec/subzero-parachain-plain.json --raw --disable-default-bootnode > .docker/local-relay-parachain/chainspec/subzero-parachain-raw.json
./target/release/subzero export-genesis-wasm --chain .docker/local-relay-parachain/chainspec/subzero-parachain-raw.json > .docker/local-relay-parachain/wasm/subzero-wasm
./target/release/subzero export-genesis-state --chain .docker/local-relay-parachain/chainspec/subzero-parachain-raw.json > .docker/local-relay-parachain/wasm/subzero-genesis
```


### Helpful links
* [Start a local relay chain](https://docs.substrate.io/tutorials/connect-other-chains/local-relay/)
* [Connect a local parachain](https://docs.substrate.io/tutorials/connect-other-chains/local-parachain/)
* [Connect to Rococo testnet](https://docs.substrate.io/tutorials/connect-other-chains/rococo-slot/)
