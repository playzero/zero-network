# Note: We don't use Alpine and its packaged Rust/Cargo
# because they're too often out of date,
# preventing them from being used to build subzero/Polkadot.

# FROM phusion/baseimage:jammy-1.0.1 as baseimage
FROM phusion/baseimage:focal-1.2.0 as baseimage
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
	apt-get dist-upgrade -y -o Dpkg::Options::="--force-confold" && \
	apt-get install -y cmake pkg-config libssl-dev git clang

WORKDIR /subzero
COPY . /subzero

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y && \
	export PATH="$PATH:$HOME/.cargo/bin" && \
	rustup toolchain install nightly-2022-06-24 && \
	rustup target add wasm32-unknown-unknown --toolchain nightly-2022-06-24 && \
	rustup default nightly-2022-06-24 &&\
	rustup show

# ===== STAGE 2 ======

FROM baseimage as builder
ARG PROFILE=release

RUN	export PATH="$PATH:$HOME/.cargo/bin" && \
cargo build "--$PROFILE"

# ===== STAGE 3 ======

FROM phusion/baseimage:focal-1.2.0
LABEL maintainer="devops@zero.io"
LABEL description="This is the 2nd stage: a very small image where we copy the subzero binary."
ARG PROFILE=release

RUN mv /usr/share/ca* /tmp && \
	rm -rf /usr/share/*  && \
	mv /tmp/ca-certificates /usr/share/ && \
	useradd -m -u 1000 -U -s /bin/sh -d /subzero subzero && \
	mkdir -p /subzero/.local/share/subzero && \
	chown -R subzero:subzero /subzero/.local && \
	ln -s /subzero/.local/share/subzero /data

COPY --from=builder /subzero/target/$PROFILE/subzero-dev /usr/local/bin
COPY --from=builder /subzero/.docker/chainspec /chainspec

# checks
RUN ldd /usr/local/bin/subzero-dev && \
	/usr/local/bin/subzero-dev --version

# Shrinking
# RUN rm -rf /usr/lib/python* && \
# 	rm -rf /usr/bin /usr/sbin /usr/share/man

USER subzero
EXPOSE 30333 9933 9944 9615
VOLUME ["/data"]

CMD ["/usr/local/bin/subzero-dev"]
