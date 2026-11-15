# SPDX-License-Identifier: AGPL-3.0-or-later
FROM erlang:28.5.0.8-slim@sha256:45e0fcffac25a41ed047898a57c7913cc69b5114c1aea0db12c85ed4b1164d77 AS build

ARG REBAR3_VERSION=3.27.3
ARG REBAR3_SHA256=f1d87d78decfc56b87d971c75ee6355698ec0c49fadd57f2c056043b7ca291b5

ENV RUSTUP_HOME=/usr/local/rustup \
	CARGO_HOME=/usr/local/cargo \
	PATH="/usr/local/cargo/bin:${PATH}"

WORKDIR /usr/src/app

RUN apt-get update && apt-get install -y --no-install-recommends \
	git \
	curl \
	make \
	gcc \
	g++ \
	libc6-dev \
	ca-certificates \
	&& rm -rf /var/lib/apt/lists/*

RUN curl --retry 5 --retry-delay 2 --retry-all-errors -fsSL "https://github.com/erlang/rebar3/releases/download/${REBAR3_VERSION}/rebar3" -o /tmp/rebar3 && \
	echo "${REBAR3_SHA256}  /tmp/rebar3" | sha256sum -c - && \
	install -m 0755 /tmp/rebar3 /usr/local/bin/rebar3 && \
	rm /tmp/rebar3

COPY --from=rust:1.98.1-slim-trixie@sha256:4cd829461bd5c4d511c32e269da9cb8929223b666519d8004e35fc8d1d771ab7 /usr/local/rustup /usr/local/rustup
COPY --from=rust:1.98.1-slim-trixie@sha256:4cd829461bd5c4d511c32e269da9cb8929223b666519d8004e35fc8d1d771ab7 /usr/local/cargo /usr/local/cargo

COPY . .
WORKDIR /usr/src/app/fluxer_gateway
RUN rebar3 compile --deps_only

RUN cp config/sys.config.template config/sys.config && \
	rebar3 as prod release

FROM erlang:28.5.0.8-slim@sha256:45e0fcffac25a41ed047898a57c7913cc69b5114c1aea0db12c85ed4b1164d77

ARG BUILD_VERSION=""
ARG SOURCE_SHA=""
ARG SOURCE_DATE=""

LABEL org.opencontainers.image.title="fluxer-gateway"
LABEL org.opencontainers.image.description="Fluxer realtime gateway"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="Fluxer"
LABEL org.opencontainers.image.url="https://fluxer.com"
LABEL org.opencontainers.image.documentation="https://docs.fluxer.com"
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
	&& rm -rf /var/lib/apt/lists/* && \
	groupadd -r -g 65532 fluxer && \
	useradd -r -u 65532 -g 65532 -s /sbin/nologin -d /opt/fluxer_gateway fluxer && \
	chown 65532:65532 /opt/fluxer_gateway

COPY --from=build --chown=65532:65532 /usr/src/app/fluxer_gateway/_build/prod/rel/fluxer_gateway .
COPY --chown=65532:65532 fluxer_gateway/scripts/docker_entrypoint.sh /opt/fluxer_gateway/bin/docker_entrypoint.sh
COPY --chown=65532:65532 fluxer_gateway/scripts/fluxer_gateway_release_wrapper.sh /opt/fluxer_gateway/bin/fluxer_gateway.wrapper

RUN mv /opt/fluxer_gateway/bin/fluxer_gateway /opt/fluxer_gateway/bin/fluxer_gateway.real && \
	mv /opt/fluxer_gateway/bin/fluxer_gateway.wrapper /opt/fluxer_gateway/bin/fluxer_gateway

USER 65532:65532

ENV BUILD_VERSION="${BUILD_VERSION}"

EXPOSE 8080 8081 4369

ENTRYPOINT ["/opt/fluxer_gateway/bin/docker_entrypoint.sh"]
