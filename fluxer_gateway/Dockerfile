# SPDX-License-Identifier: AGPL-3.0-or-later
FROM erlang:28-slim AS build

ARG LOGGER_LEVEL=info
ARG RUST_TOOLCHAIN=1.93.0

ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /usr/src/app

RUN apt-get update && apt-get install -y --no-install-recommends \
	git \
	curl \
	make \
	gcc \
	g++ \
	libc6-dev \
	gettext-base \
	ca-certificates \
	&& rm -rf /var/lib/apt/lists/*

RUN curl --retry 5 --retry-delay 2 --retry-all-errors -fsSL https://github.com/erlang/rebar3/releases/download/3.24.0/rebar3 -o /usr/local/bin/rebar3 && \
	chmod +x /usr/local/bin/rebar3

RUN curl --retry 5 --retry-delay 2 --retry-all-errors --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs | \
	sh -s -- -y --profile minimal --default-toolchain "${RUST_TOOLCHAIN}"

COPY . .
WORKDIR /usr/src/app/fluxer_gateway
RUN rebar3 compile --deps_only

RUN LOGGER_LEVEL=${LOGGER_LEVEL} envsubst < config/sys.config.template > config/sys.config && \
	rebar3 as prod release

FROM erlang:28-slim

ARG BUILD_VERSION=""
ARG SOURCE_SHA=""
ARG SOURCE_DATE=""

LABEL org.opencontainers.image.title="fluxer-gateway"
LABEL org.opencontainers.image.description="Fluxer realtime gateway"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="Fluxer"
LABEL org.opencontainers.image.url="https://fluxer.app"
LABEL org.opencontainers.image.documentation="https://docs.fluxer.app"
LABEL org.opencontainers.image.source="https://github.com/fluxerapp/fluxer"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL org.opencontainers.image.revision="${SOURCE_SHA}"
LABEL org.opencontainers.image.created="${SOURCE_DATE}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

WORKDIR /opt/fluxer_gateway

RUN apt-get update && apt-get install -y --no-install-recommends \
	ca-certificates \
	curl \
	libstdc++6 \
	&& rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/src/app/fluxer_gateway/_build/prod/rel/fluxer_gateway .
COPY fluxer_gateway/scripts/docker_entrypoint.sh /opt/fluxer_gateway/bin/docker_entrypoint.sh
COPY fluxer_gateway/scripts/fluxer_gateway_release_wrapper.sh /opt/fluxer_gateway/bin/fluxer_gateway.wrapper

RUN mv /opt/fluxer_gateway/bin/fluxer_gateway /opt/fluxer_gateway/bin/fluxer_gateway.real && \
	mv /opt/fluxer_gateway/bin/fluxer_gateway.wrapper /opt/fluxer_gateway/bin/fluxer_gateway && \
	chmod +x /opt/fluxer_gateway/bin/docker_entrypoint.sh /opt/fluxer_gateway/bin/fluxer_gateway && \
	groupadd -r -g 65532 fluxer && \
	useradd -r -u 65532 -g 65532 -s /sbin/nologin -d /opt/fluxer_gateway fluxer && \
	chown -R 65532:65532 /opt/fluxer_gateway

USER 65532:65532

ENV BUILD_VERSION="${BUILD_VERSION}"

EXPOSE 8080 8081 4369

ENTRYPOINT ["/opt/fluxer_gateway/bin/docker_entrypoint.sh"]
