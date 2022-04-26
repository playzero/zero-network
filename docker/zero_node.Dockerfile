# Note: We don't use Alpine and its packaged Rust/Cargo
# because they're too often out of date,
# preventing them from being used to build subzero/Polkadot.

FROM phusion/baseimage:0.11 as baseimage
ENV DEBIAN_FRONTEND=noninteractive


RUN apt-get update && \
	apt-get dist-upgrade -y -o Dpkg::Options::="--force-confold" && \
	apt-get install -y cmake pkg-config libssl-dev git clang

WORKDIR /zero-node
COPY . /zero-node

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y && \
	export PATH="$PATH:$HOME/.cargo/bin" && \
	rustup toolchain install nightly-2021-11-07 && \
	rustup target add wasm32-unknown-unknown --toolchain nightly-2021-11-07 && \
	rustup default nightly-2021-11-07 &&\
	rustup show

# ===== STAGE 2 ======

FROM baseimage as builder
ARG PROFILE=release

RUN	export PATH="$PATH:$HOME/.cargo/bin" && \
cargo build "--$PROFILE"

# ===== STAGE 3 ======

FROM phusion/baseimage:0.11
LABEL maintainer="devops@zero.io"
LABEL description="This is the 2nd stage: a very small image where we copy the subzero binary."
ARG PROFILE=release

RUN mv /usr/share/ca* /tmp && \
	rm -rf /usr/share/*  && \
	mv /tmp/ca-certificates /usr/share/ && \
	useradd -m -u 1000 -U -s /bin/sh -d /zero-node subzero && \
	mkdir -p /zero-node/.local/share/zero-node && \
	chown -R subzero:subzero /zero-node/.local && \
	ln -s /zero-node/.local/share/zero-node /data

COPY --from=builder /zero-node/target/$PROFILE/zero-node /usr/local/bin

# checks
RUN ldd /usr/local/bin/zero-node && \
	/usr/local/bin/zero-node --version

# Shrinking
RUN rm -rf /usr/lib/python* && \
	rm -rf /usr/bin /usr/sbin /usr/share/man

USER subzero
EXPOSE 30333 9933 9944 9615
VOLUME ["/data"]

CMD ["/usr/local/bin/zero-node"]
