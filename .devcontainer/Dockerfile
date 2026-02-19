# Language runtimes (Node.js, Go, Rust, Python) are installed via devcontainer
# features. This Dockerfile handles Erlang/OTP (no feature available) and
# tools like Caddy, process-compose, rebar3, uv, ffmpeg, and exiftool.

FROM erlang:28-slim AS erlang

FROM mcr.microsoft.com/devcontainers/base:debian-13

ARG DEBIAN_FRONTEND=noninteractive
ARG REBAR3_VERSION=3.24.0
ARG PROCESS_COMPOSE_VERSION=1.90.0

# Both erlang:28-slim and debian-13 are Trixie-based, so OpenSSL versions match.
COPY --from=erlang /usr/local/lib/erlang /usr/local/lib/erlang
RUN ln -sf /usr/local/lib/erlang/bin/* /usr/local/bin/

RUN apt-get update && apt-get install -y --no-install-recommends \
	libncurses6 libsctp1 \
	build-essential pkg-config \
	ffmpeg libimage-exiftool-perl \
	sqlite3 libsqlite3-dev \
	libssl-dev openssl \
	gettext-base lsof iproute2 \
	&& rm -rf /var/lib/apt/lists/*

RUN curl -fsSL "https://github.com/erlang/rebar3/releases/download/${REBAR3_VERSION}/rebar3" \
	-o /usr/local/bin/rebar3 \
	&& chmod +x /usr/local/bin/rebar3

RUN curl -fsSL "https://caddyserver.com/api/download?os=linux&arch=amd64" \
	-o /usr/local/bin/caddy \
	&& chmod +x /usr/local/bin/caddy

RUN curl -fsSL "https://github.com/F1bonacc1/process-compose/releases/download/v${PROCESS_COMPOSE_VERSION}/process-compose_linux_amd64.tar.gz" \
	| tar xz -C /usr/local/bin process-compose \
	&& chmod +x /usr/local/bin/process-compose

RUN curl -fsSL "https://github.com/astral-sh/uv/releases/latest/download/uv-x86_64-unknown-linux-gnu.tar.gz" \
	| tar xz --strip-components=1 -C /usr/local/bin \
	&& chmod +x /usr/local/bin/uv /usr/local/bin/uvx
