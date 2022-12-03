<p align="center">
  <img src="https://zero.io/img/z-control-45-full.png" width="128"><br/>
  <b>ZERO network — for video games in the web3 era</b>

  <div align="center">  

[![GitHub license](https://img.shields.io/badge/license-GPL3%2FApache2-blue)](#LICENSE)
[![GitLab Status](https://gitlab.parity.io/parity/substrate/badges/master/pipeline.svg)](https://gitlab.parity.io/parity/substrate/pipelines)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](docs/CONTRIBUTING.adoc)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/playzero/zero-network/Test?label=Actions&logo=github)](https://github.com/playzero/zero-network/actions?query=workflow%3ATest)
[![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/playzero/zero-network)](https://github.com/playzero/zero-network/tags)  
[![Discord](https://img.shields.io/badge/Discord-gray?logo=discord)](https://discord.gg/rhwtr7p) [![Twitter URL](https://img.shields.io/twitter/url?style=social&url=https%3A%2F%2Ftwitter.com%2Fzerodotio)](https://twitter.com/zerodotio)
[![Medium](https://img.shields.io/badge/Medium-gray?logo=medium)](https://medium.com/playzero)

  </div>

</p>

# Overview

<!-- TOC -->
- [1. Introduction](#1-introduction)
- [2. Economics](#2-economics)
- [3. Project Status](#3-project-status)
- [4. Build](#4-build)
- [5. Run](#5-run)
<!-- /TOC -->

# 1. Introduction

ZERO is a multichain network, Multiverse + DAO for next generation videogames. It provides native asset-, finance-, governance protocols, Smart Contracts, a Hypergraph and Computation at its core. To provide cross economic interoperability between totally different ecosystems. Futhermore it will enable generative creation of games and game economies through algorithms, generators and provision of autonomous agents to drive transition of old economy videogames and creators into a tokenized and decentralized future.

- Network currency: $ZERO
- Join our growing game community on [Discord](http://discord.gg/rhwtr7p)
- Parachain launch information and tokenomics on [Blog](https://blog.zero.io)

# 2. Economics

ZERO Token ($ZERO) features the following utilities, and the value of $ZERO token will accrue with the increased usage of the network and revenue from stability fees and liquidation penalties

	- Network utility and stability fees
		-- Asset, Finance and Governance protocols
		-- Payment
		-- Identity
		-- Computation
		-- Oracles
	- Governance: vote for/against risk parameters and network change proposals
	- Economic Capital: in case of liquidation/defaulting of contracts without sufficient collaterals

To enable cross-chain functionality, ZERO will connect to the Polkadot Ecosystem ( starting with Kusama ) in one of three ways:

	- parathread —— pay-as-you-go connection to Polkadot
	- parachain —— permanent connection for a given period
	- bridge —— independent chain bridged to Polkadot

Becoming a parachain would be an ideal option to bootstrap ZERO Network, to maximize its benefits and to reach to other chains and applications on the Polkadot network.

To secure a parachain slot, ZERO Network will require supportive DOT/KSM holders to lock their DOT/KSM to bid for a slot collectively — a process known as the Initial Parachain Offering (IPO). $ZERO tokens will be offered as a reward for those who participated in the IPO, as compensation for their opportunity cost of staking DOT/KSM.

# 3. Project Status

Zero Network is the common description for all contributing protocols and chains, subzero is the network core preparing for parachain operation.

- subzero code base has been migrated from early 3.0.0 to current to prepare for parachain operation
- subzero testnet alphaville is live since 08/2020
- chain governance, staking, nomination, validator operation through native FRAME pallets
- project governance through [GameDAO Protocol](https://gamedao.co)
- further information in our [Discord](https://img.shields.io/badge/Discord-gray?logo=discord)

# 4. Build

Rust.
```bash
	curl https://sh.rustup.rs -sSf | sh
```
Recursion for submodules in git
```bash
	git config --global submodule.recurse true
```
Build for your current machine architecture
```bash
	make build
```

# 5. Run

Run your local dev chain
```bash
	make run
```
Purge the cache
```bash
	make purge
```
Update submodules
```bash
	make update
```

# 0. Notes
 This is still work in progress, we will update more information as we progress. Refer to the token economy working paper for more details. This project and text was inspired by the excellent work of many growing projects in the Polkadot ecosystem. Thank you!.
