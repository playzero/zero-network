# Note: We don't use Alpine and its packaged Rust/Cargo
# because they're too often out of date,
# preventing them from being used to build alphaville/Polkadot.

FROM phusion/baseimage:focal-1.2.0 as baseimage
ENV DEBIAN_FRONTEND=noninteractive


RUN apt-get update && \
	apt-get dist-upgrade -y -o Dpkg::Options::="--force-confold" && \
	apt-get install -y cmake pkg-config libssl-dev git clang

WORKDIR /alphaville
COPY . /alphaville

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y && \
	export PATH="$PATH:$HOME/.cargo/bin" && \
	rustup toolchain install nightly && \
	rustup target add wasm32-unknown-unknown --toolchain nightly && \
	rustup default nightly &&\

# ===== STAGE 2 ======

FROM baseimage as builder
ARG PROFILE=release

RUN	export PATH="$PATH:$HOME/.cargo/bin" && \
cargo build "--$PROFILE"

# ===== STAGE 3 ======

FROM phusion/baseimage:focal-1.2.0
LABEL maintainer="devops@zero.io"
LABEL description="This is the 2nd stage: a very small image where we copy the alphaville binary."
ARG PROFILE=release

RUN mv /usr/share/ca* /tmp && \
	rm -rf /usr/share/*  && \
	mv /tmp/ca-certificates /usr/share/ && \
	useradd -m -u 1000 -U -s /bin/sh -d /alphaville alphaville && \
	mkdir -p /alphaville/.local/share/alphaville && \
	chown -R alphaville:alphaville /alphaville/.local && \
	ln -s /alphaville/.local/share/alphaville /data

COPY --from=builder /alphaville/target/$PROFILE/alphaville /usr/local/bin
COPY --from=builder /alphaville/.docker/chainspec /chainspec

# checks
RUN ldd /usr/local/bin/alphaville && \
	/usr/local/bin/alphaville --version

# Shrinking
# RUN rm -rf /usr/lib/python* && \
# 	rm -rf /usr/bin /usr/sbin /usr/share/man

USER alphaville
EXPOSE 30333 9933 9944 9615
VOLUME ["/data"]

CMD ["/usr/local/bin/alphaville"]
