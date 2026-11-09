# SPDX-License-Identifier: AGPL-3.0-or-later

ARG APP_ASSETS_REF=app-assets-missing
ARG APP_ASSETS_PLATFORM=$BUILDPLATFORM

FROM alpine:3.24 AS app-assets-missing
RUN echo "set APP_ASSETS_REF to a fluxer-app-proxy-self-hosted <version>-assets image" >&2 && exit 1

FROM --platform=${APP_ASSETS_PLATFORM} ${APP_ASSETS_REF} AS app-static

FROM rust:1-trixie AS builder

ARG TARGETARCH
ARG PROFILE=release

WORKDIR /usr/src/app

RUN apt-get update \
	&& apt-get install -y --no-install-recommends ca-certificates nodejs npm \
	&& rm -rf /var/lib/apt/lists/*

RUN npm install --no-audit --no-fund @tailwindcss/cli@4.2.1 tailwindcss@4.2.1
RUN case "${TARGETARCH}" in \
		amd64) OXIDE_PKG="@tailwindcss/oxide-linux-x64-gnu" ;; \
		arm64) OXIDE_PKG="@tailwindcss/oxide-linux-arm64-gnu" ;; \
		*) echo "unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
	esac \
	&& npm install --no-audit --no-fund "${OXIDE_PKG}@4.2.1" \
	&& test -x node_modules/.bin/tailwindcss

COPY . .

RUN cargo build --profile "${PROFILE}" --locked -p fluxer-server \
	&& cp "target/${PROFILE}/fluxer-server" /usr/local/bin/fluxer-server \
	&& test -s target/${PROFILE}/build/fluxer_admin-*/out/static/app.css \
	&& test "$(ls target/${PROFILE}/build/fluxer_admin-*/out/static/fonts/*.woff2 | wc -l)" -eq 16

FROM debian:trixie-slim

ARG BUILD_VERSION=""
ARG SOURCE_SHA=""
ARG SOURCE_DATE=""

LABEL org.opencontainers.image.title="fluxer-server"
LABEL org.opencontainers.image.description="Fluxer Rust services in one process"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="Fluxer"
LABEL org.opencontainers.image.url="https://fluxer.com"
LABEL org.opencontainers.image.documentation="https://docs.fluxer.com"
LABEL org.opencontainers.image.source="https://github.com/fluxerapp/fluxer"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL org.opencontainers.image.revision="${SOURCE_SHA}"
LABEL org.opencontainers.image.created="${SOURCE_DATE}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

WORKDIR /srv/app

RUN apt-get update && apt-get install -y --no-install-recommends \
	ca-certificates libjemalloc2 \
	&& rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local/bin/fluxer-server /usr/local/bin/fluxer-server
COPY --from=app-static /assets ./static/

ENV BUILD_VERSION="${BUILD_VERSION}"
ENV LD_PRELOAD=libjemalloc.so.2
ENV MALLOC_CONF="background_thread:true,metadata_thp:auto,dirty_decay_ms:5000,muzzy_decay_ms:5000"

USER 65532:65532

HEALTHCHECK --interval=10s --timeout=5s --start-period=60s --retries=30 \
	CMD ["/usr/local/bin/fluxer-server", "healthcheck"]

CMD ["/usr/local/bin/fluxer-server"]
