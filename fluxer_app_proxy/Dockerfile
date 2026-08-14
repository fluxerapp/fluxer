# SPDX-License-Identifier: AGPL-3.0-or-later

ARG BUILD_VERSION=""
ARG PUBLIC_ASSET_BASE_URL=""
ARG FLUXER_APP_PROXY_TIME_FREEZE_ENABLED="true"
ARG BUNDLE_LOCAL_ASSETS="true"

# ---------- Stage 1: Build the SPA (fluxer_app) ----------
FROM node:24-bookworm-slim AS app-build

ARG BUILD_VERSION
ARG PUBLIC_ASSET_BASE_URL

WORKDIR /usr/src/app

RUN corepack enable && corepack prepare pnpm@10.29.3 --activate

RUN apt-get update && apt-get install -y --no-install-recommends \
	curl ca-certificates build-essential clang \
	&& rm -rf /var/lib/apt/lists/*

ARG RUST_VERSION=1.93.0
ENV RUSTUP_HOME=/usr/local/rustup \
	CARGO_HOME=/usr/local/cargo \
	PATH=/usr/local/cargo/bin:$PATH
RUN set -eux; \
	curl --retry 3 --retry-all-errors --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs \
		| sh -s -- -y --no-modify-path --profile minimal --default-toolchain "${RUST_VERSION}" \
			--target wasm32-unknown-unknown; \
	rustc --version; cargo --version


COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
COPY patches/ ./patches/
COPY fluxer_app/package.json ./fluxer_app/
COPY packages/constants/package.json ./packages/constants/
COPY packages/date_utils/package.json ./packages/date_utils/
COPY packages/geo_utils/package.json ./packages/geo_utils/
COPY packages/instance_bootstrap/package.json ./packages/instance_bootstrap/
COPY packages/limits/package.json ./packages/limits/
COPY packages/schema/package.json ./packages/schema/
COPY packages/snowflake/package.json ./packages/snowflake/
COPY packages/voice_engine_v2/package.json ./packages/voice_engine_v2/
COPY fluxer_app/pkgs/list_utils/package.json ./fluxer_app/pkgs/list_utils/
COPY fluxer_app/pkgs/number_utils/package.json ./fluxer_app/pkgs/number_utils/
COPY fluxer_app/pkgs/livekit-client/package.json ./fluxer_app/pkgs/livekit-client/

RUN pnpm install --frozen-lockfile --filter fluxer_app...

COPY tsconfigs ./tsconfigs
COPY packages/constants/ ./packages/constants/
COPY packages/date_utils/ ./packages/date_utils/
COPY packages/geo_utils/ ./packages/geo_utils/
COPY packages/instance_bootstrap/ ./packages/instance_bootstrap/
COPY packages/limits/ ./packages/limits/
COPY packages/schema/ ./packages/schema/
COPY packages/snowflake/ ./packages/snowflake/
COPY packages/voice_engine_v2/ ./packages/voice_engine_v2/
COPY packages/fonts/css/ ./packages/fonts/css/
COPY packages/fonts/files/ ./packages/fonts/files/
COPY packages/fonts/NOTICE.md packages/fonts/LICENSE-IBM-PLEX.txt ./packages/fonts/
COPY packages/markdown_parser/rust/Cargo.toml ./packages/markdown_parser/rust/
COPY packages/markdown_parser/rust/.cargo/ ./packages/markdown_parser/rust/.cargo/
COPY packages/markdown_parser/rust/benches/parser.rs ./packages/markdown_parser/rust/benches/
COPY packages/markdown_parser/rust/src/ ./packages/markdown_parser/rust/src/
COPY tools/ci/ ./tools/ci/
COPY fluxer_app/index.html fluxer_app/lingui.config.js fluxer_app/package.json fluxer_app/postcss.config.js fluxer_app/rspack.config.mjs fluxer_app/tsconfig.base.json fluxer_app/tsconfig.json ./fluxer_app/
COPY fluxer_app/pkgs/list_utils/ ./fluxer_app/pkgs/list_utils/
COPY fluxer_app/pkgs/number_utils/ ./fluxer_app/pkgs/number_utils/
COPY fluxer_app/pkgs/livekit-client/ ./fluxer_app/pkgs/livekit-client/
COPY fluxer_app/rust/ ./fluxer_app/rust/
COPY fluxer_app/scripts/ ./fluxer_app/scripts/
COPY fluxer_app/src/ ./fluxer_app/src/

ENV BUILD_VERSION=${BUILD_VERSION} \
	PUBLIC_ASSET_BASE_URL=${PUBLIC_ASSET_BASE_URL}

RUN --mount=type=cache,target=/usr/local/cargo/registry \
	--mount=type=cache,target=/usr/local/cargo/git \
	cd fluxer_app && pnpm build

# Extract the full SPA asset tree for local serving by the app proxy.
FROM alpine:3.21 AS app-assets
ARG BUNDLE_LOCAL_ASSETS=true
ARG PUBLIC_ASSET_BASE_URL=""
COPY --from=app-build /usr/src/app/fluxer_app/dist /assets
RUN if [ "${BUNDLE_LOCAL_ASSETS}" != "true" ]; then \
		case "${PUBLIC_ASSET_BASE_URL}" in \
		http://* | https://*) ;; \
		*) \
			echo "BUNDLE_LOCAL_ASSETS=false requires an absolute PUBLIC_ASSET_BASE_URL." >&2; \
			echo "Without one, index.html links same-origin /assets/* that this image would no longer contain." >&2; \
			exit 1 \
			;; \
		esac; \
		find /assets/assets -type f ! -name 'fonts-*.txt' -delete; \
	fi

# Stage for CI to extract the full dist (docker bake app-dist target)
FROM alpine:3.21 AS app-dist
COPY --from=app-build /usr/src/app/fluxer_app/dist /dist

# ---------- Stage 2: Build the Rust proxy binary ----------
FROM rust:1-bookworm AS rust-builder

ARG FLUXER_APP_PROXY_TIME_FREEZE_ENABLED="true"

WORKDIR /usr/src/app

COPY Cargo.lock ./
COPY fluxer_common/ fluxer_common/
COPY fluxer_app_proxy/ fluxer_app_proxy/
COPY fluxer_svc/ fluxer_svc/

RUN printf '%s\n' \
	'[workspace]' \
	'members = [' \
	'    "fluxer_app_proxy",' \
	'    "fluxer_common",' \
	'    "fluxer_svc",' \
	']' \
	'resolver = "2"' \
	'' \
	'[workspace.package]' \
	'edition = "2024"' \
	'license = "AGPL-3.0-or-later"' \
	> Cargo.toml

RUN if [ "${FLUXER_APP_PROXY_TIME_FREEZE_ENABLED}" = "false" ]; then \
		cargo build --release -p fluxer_app_proxy --no-default-features; \
	else \
		cargo build --release -p fluxer_app_proxy; \
	fi \
	&& cp target/release/fluxer_app_proxy /usr/local/bin/fluxer-app-proxy

# ---------- Stage 3: Runtime ----------
FROM debian:bookworm-slim

ARG BUILD_VERSION=""

WORKDIR /srv/app

RUN apt-get update && apt-get install -y --no-install-recommends \
	ca-certificates curl \
	&& rm -rf /var/lib/apt/lists/*

COPY --from=rust-builder /usr/local/bin/fluxer-app-proxy /usr/local/bin/fluxer-app-proxy
COPY --from=app-assets /assets ./static/

ENV BUILD_VERSION="${BUILD_VERSION}"
ENV FLUXER_APP_PROXY_HOST="0.0.0.0"
ENV FLUXER_APP_PROXY_PORT="8080"

USER 65532:65532

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
	CMD curl -f http://localhost:8080/_health || exit 1

CMD ["/usr/local/bin/fluxer-app-proxy"]
