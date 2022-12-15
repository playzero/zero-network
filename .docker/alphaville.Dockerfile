FROM phusion/baseimage:focal-1.2.0 as builder
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
	apt-get dist-upgrade -y -o Dpkg::Options::="--force-confold" && \
	apt-get install -y cmake pkg-config libssl-dev git clang

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y && \
	export PATH="$PATH:$HOME/.cargo/bin" && \
	rustup toolchain install nightly-2022-06-24 && \
	rustup target add wasm32-unknown-unknown --toolchain nightly-2022-06-24 && \
	rustup default nightly-2022-06-24 &&\
	rustup show

WORKDIR /alphaville
COPY . /alphaville

RUN	export PATH="$PATH:$HOME/.cargo/bin" && \
	cargo build -p alphaville --release

FROM docker.io/library/ubuntu:20.04

LABEL maintainer="devops@zero.io"

COPY --from=builder /alphaville/target/release/alphaville /usr/local/bin

RUN useradd -m -u 1000 -U -s /bin/sh -d /alphaville alphaville && \
	mkdir -p /data /alphaville/.local/share/alphaville && \
	chown -R alphaville:alphaville /data && \
	ln -s /data /alphaville/.local/share/alphaville && \
	rm -rf /usr/bin /usr/sbin && \
	ldd /usr/local/bin/alphaville && \
	/usr/local/bin/alphaville --version

USER alphaville
EXPOSE 30333 9933 9944 9615
VOLUME ["/data"]

CMD ["/usr/local/bin/alphaville"]
