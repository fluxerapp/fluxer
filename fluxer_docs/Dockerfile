# SPDX-License-Identifier: AGPL-3.0-or-later
ARG BUILD_VERSION

FROM node:26-trixie-slim AS base

WORKDIR /usr/src/app

RUN npm install -g pnpm@11.27.0

FROM base AS deploy

RUN apt-get update && apt-get install -y --no-install-recommends \
		build-essential \
		gcc \
		libvips-dev \
		pkg-config \
		python3 && \
	rm -rf /var/lib/apt/lists/*

COPY . .

RUN pnpm install --frozen-lockfile
RUN pnpm --filter fluxer_docs run build
RUN pnpm deploy --legacy --filter=fluxer_docs --prod --config.allow-unused-patches=true /out

FROM node:26-trixie-slim

ARG BUILD_VERSION=""
ARG SOURCE_SHA=""
ARG SOURCE_DATE=""

WORKDIR /usr/src/app/fluxer_docs

RUN apt-get update && apt-get install -y --no-install-recommends \
		ca-certificates \
		libvips42t64 && \
	rm -rf /var/lib/apt/lists/*

RUN npm install -g pnpm@11.27.0

LABEL maintainer="Fluxer"
LABEL org.opencontainers.image.title="fluxer-docs"
LABEL org.opencontainers.image.description="Fluxer API and protocol reference"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="Fluxer"
LABEL org.opencontainers.image.url="https://fluxer.app"
LABEL org.opencontainers.image.documentation="https://docs.fluxer.app"
LABEL org.opencontainers.image.source="https://github.com/fluxerapp/fluxer"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL org.opencontainers.image.revision="${SOURCE_SHA}"
LABEL org.opencontainers.image.created="${SOURCE_DATE}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

COPY --from=deploy /out ./
COPY --from=deploy /usr/src/app/fluxer_docs/dist ./dist

RUN chown -R 65532:65532 /usr/src/app

ENV HOME=/usr/src/app
ENV NODE_ENV=production
ENV NODE_OPTIONS="--enable-source-maps"
ENV NODE_EXTRA_CA_CERTS=/etc/ssl/certs/ca-certificates.crt
ENV BUILD_VERSION=${BUILD_VERSION}

USER 65532:65532

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
	CMD ["./node_modules/.bin/tsx", "scripts/Healthcheck.ts"]

CMD ["./node_modules/.bin/tsx", "scripts/StartServer.ts"]
