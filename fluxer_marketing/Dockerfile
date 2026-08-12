# SPDX-License-Identifier: AGPL-3.0-or-later

FROM rust:1-bookworm AS builder

ARG BUILD_VERSION=""
ARG TARGETARCH

WORKDIR /usr/src/app

RUN apt-get update \
	&& apt-get install -y --no-install-recommends ca-certificates nodejs npm pkg-config \
	&& npm install -g pnpm@10.29.3 \
	&& rm -rf /var/lib/apt/lists/*

COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches patches
COPY fluxer_marketing/package.json fluxer_marketing/package.json
RUN pnpm install --frozen-lockfile --filter fluxer_marketing --config.block-exotic-subdeps=false

COPY Cargo.lock Cargo.lock
COPY fluxer_common fluxer_common
COPY fluxer_marketing fluxer_marketing
COPY packages/fonts/manifest.json packages/fonts/manifest.json
COPY packages/fonts/NOTICE.md packages/fonts/NOTICE.md
COPY packages/fonts/LICENSE-IBM-PLEX.txt packages/fonts/LICENSE-IBM-PLEX.txt
COPY packages/fonts/css/locale-fallbacks.css packages/fonts/css/locale-fallbacks.css
COPY packages/fonts/files/FluxerSans packages/fonts/files/FluxerSans
COPY packages/fonts/files/FluxerMono packages/fonts/files/FluxerMono
RUN printf '%s\n' \
	'[workspace]' \
	'members = ["fluxer_common", "fluxer_marketing"]' \
	'resolver = "2"' \
	'' \
	'[workspace.package]' \
	'edition = "2024"' \
	'license = "AGPL-3.0-or-later"' \
	> Cargo.toml
RUN pnpm install --frozen-lockfile --filter fluxer_marketing --config.block-exotic-subdeps=false
RUN OXIDE_VERSION="$(node -p "require('./fluxer_marketing/package.json').optionalDependencies['@tailwindcss/oxide-linux-x64-gnu']")" \
	&& case "${TARGETARCH}" in \
		amd64) OXIDE_PKG="@tailwindcss/oxide-linux-x64-gnu" ;; \
		arm64) OXIDE_PKG="@tailwindcss/oxide-linux-arm64-gnu" ;; \
		*) echo "unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
	esac \
	&& npm install --prefix /tmp/tailwind-oxide --no-audit --no-fund "${OXIDE_PKG}@${OXIDE_VERSION}" \
	&& mkdir -p node_modules/@tailwindcss \
	&& cp -R "/tmp/tailwind-oxide/node_modules/${OXIDE_PKG}" "node_modules/${OXIDE_PKG}" \
	&& rm -rf /tmp/tailwind-oxide

ENV FLUXER_BUILD_VERSION="${BUILD_VERSION}"

RUN cargo test -p fluxer_marketing \
	&& cargo build --release -p fluxer_marketing \
	&& cp target/release/fluxer_marketing /usr/local/bin/fluxer-marketing

FROM debian:bookworm-slim AS runtime

ARG BUILD_VERSION=""

WORKDIR /usr/local/bin

RUN apt-get update \
	&& apt-get install -y --no-install-recommends ca-certificates \
	&& rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local/bin/fluxer-marketing /usr/local/bin/fluxer-marketing

ENV BUILD_VERSION="${BUILD_VERSION}"
ENV FLUXER_MARKETING_HOST="0.0.0.0"
ENV FLUXER_MARKETING_PORT="8080"

USER 65532:65532

EXPOSE 8080

CMD ["/usr/local/bin/fluxer-marketing"]
