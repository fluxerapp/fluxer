# SPDX-License-Identifier: AGPL-3.0-or-later

FROM rust:1-bookworm AS builder

ARG BUILD_VERSION=""
ARG TARGETARCH

WORKDIR /usr/src/app

RUN apt-get update \
	&& apt-get install -y --no-install-recommends ca-certificates nodejs npm pkg-config \
	&& npm install -g pnpm@10.29.3 \
	&& rm -rf /var/lib/apt/lists/*

RUN npm install --no-audit --no-fund @tailwindcss/cli@4.2.1 tailwindcss@4.2.1
RUN TAILWIND_OXIDE_VERSION="4.2.1" \
	&& case "${TARGETARCH}" in \
		amd64) OXIDE_PKG="@tailwindcss/oxide-linux-x64-gnu" ;; \
		arm64) OXIDE_PKG="@tailwindcss/oxide-linux-arm64-gnu" ;; \
		*) echo "unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
	esac \
	&& npm install --no-audit --no-fund "${OXIDE_PKG}@${TAILWIND_OXIDE_VERSION}" \
	&& ls -la node_modules/.bin/tailwindcss

COPY Cargo.lock Cargo.lock
COPY fluxer_admin fluxer_admin
COPY fluxer_common fluxer_common
COPY packages/fonts/manifest.json packages/fonts/manifest.json
COPY packages/fonts/NOTICE.md packages/fonts/NOTICE.md
COPY packages/fonts/LICENSE-IBM-PLEX.txt packages/fonts/LICENSE-IBM-PLEX.txt
COPY packages/fonts/files/FluxerSans packages/fonts/files/FluxerSans
COPY packages/fonts/files/FluxerMono packages/fonts/files/FluxerMono
RUN printf '%s\n' \
	'[workspace]' \
	'members = ["fluxer_admin", "fluxer_common"]' \
	'resolver = "2"' \
	'' \
	'[workspace.package]' \
	'edition = "2024"' \
	'license = "AGPL-3.0-or-later"' \
	'' \
	'[profile.release]' \
	'lto = "fat"' \
	'codegen-units = 1' \
	'strip = "symbols"' \
	> Cargo.toml

ENV FLUXER_BUILD_VERSION="${BUILD_VERSION}"

RUN cargo build --release -p fluxer_admin \
	&& cp target/release/fluxer_admin /usr/local/bin/fluxer-admin

RUN test -s target/release/build/fluxer_admin-*/out/static/app.css \
	&& echo "Tailwind CSS compiled successfully"

RUN test "$(ls target/release/build/fluxer_admin-*/out/static/fonts/*.woff2 | wc -l)" -eq 16 \
	&& ls target/release/build/fluxer_admin-*/out/static/fonts/fonts.*.css \
	&& echo "Latin-core fonts bundled successfully"

FROM debian:bookworm-slim AS runtime

ARG BUILD_VERSION=""
ARG SOURCE_SHA=""
ARG SOURCE_DATE=""

LABEL org.opencontainers.image.title="fluxer-admin"
LABEL org.opencontainers.image.description="Fluxer admin console"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="Fluxer"
LABEL org.opencontainers.image.url="https://fluxer.app"
LABEL org.opencontainers.image.documentation="https://docs.fluxer.app"
LABEL org.opencontainers.image.source="https://github.com/fluxerapp/fluxer"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL org.opencontainers.image.revision="${SOURCE_SHA}"
LABEL org.opencontainers.image.created="${SOURCE_DATE}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

WORKDIR /usr/local/bin

RUN apt-get update \
	&& apt-get install -y --no-install-recommends ca-certificates \
	&& rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local/bin/fluxer-admin /usr/local/bin/fluxer-admin

ENV BUILD_VERSION="${BUILD_VERSION}"
ENV FLUXER_ADMIN_HOST="0.0.0.0"
ENV FLUXER_ADMIN_PORT="8080"

USER 65532:65532

EXPOSE 8080

CMD ["/usr/local/bin/fluxer-admin"]
