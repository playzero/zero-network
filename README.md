# Substrate &middot; [![GitHub license](https://img.shields.io/badge/license-GPL3%2FApache2-blue)](#LICENSE) [![GitLab Status](https://gitlab.parity.io/parity/substrate/badges/master/pipeline.svg)](https://gitlab.parity.io/parity/substrate/pipelines) [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](docs/CONTRIBUTING.adoc) [![Matrix](https://img.shields.io/matrix/substrate-technical:matrix.org)](https://matrix.to/#/#substrate-technical:matrix.org)

<p align="center">
  <img src="/docs/media/sub.gif">
</p>

Substrate is a next-generation framework for blockchain innovation 🚀.

## Trying it out

Simply go to [docs.substrate.io](https://docs.substrate.io) and follow the
[installation](https://docs.substrate.io/v3/getting-started/overview) instructions. You can
also try out one of the [tutorials](https://docs.substrate.io/tutorials/).

## Contributions & Code of Conduct

Please follow the contributions guidelines as outlined in [`docs/CONTRIBUTING.adoc`](docs/CONTRIBUTING.adoc). In all communications and contributions, this project follows the [Contributor Covenant Code of Conduct](docs/CODE_OF_CONDUCT.md).

## Security

The security policy and procedures can be found in [`docs/SECURITY.md`](docs/SECURITY.md).

## License

- Substrate Primitives (`sp-*`), Frame (`frame-*`) and the pallets (`pallets-*`), binaries (`/bin`) and all other utilities are licensed under [Apache 2.0](LICENSE-APACHE2).
- Substrate Client (`/client/*` / `sc-*`) is licensed under [GPL v3.0 with a classpath linking exception](LICENSE-GPL3).

The reason for the split-licensing is to ensure that for the vast majority of teams using Substrate to create feature-chains, then all changes can be made entirely in Apache2-licensed code, allowing teams full freedom over what and how they release and giving licensing clarity to commercial teams.

In the interests of the community, we require any deeper improvements made to Substrate's core logic (e.g. Substrate's internal consensus, crypto or database code) to be contributed back so everyone can benefit.

## Running the node
In case you would like to run the node on your machine, please follow next steps.

1. Make sure you have correct version of Rust installed. Required to execute only once:
```bash
rustup toolchain install nightly-2021-11-07
rustup target add wasm32-unknown-unknown --toolchain nightly-2021-11-07
```
2. Build project:
```bash
cargo +nightly-2021-11-07 build --release
```
3. Generate (and optionally edit) chainspec config:
```bash
./target/release/subzero build-spec --chain=dev > customSpec.json
```
4. Start the node:
```bash
./target/release/subzero --chain=customSpec.json [... other run options]
```

## Running with Docker
It is possible to run the node with Docker solution.
You can find docker file in `docker/subzero.Dockerfile`.

1. Build docker image:
```bash
docker build . -f docker/subzero.Dockerfile -t subzero
````
2. Run node container:
```bash
docker run subzero /usr/local/bin/subzero [run options]
```
