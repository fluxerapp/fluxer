# SPDX-License-Identifier: AGPL-3.0-or-later
ARG BUILD_VERSION

FROM node:24-bookworm-slim AS base

WORKDIR /usr/src/app

RUN corepack enable && corepack prepare pnpm@10.29.3 --activate

FROM base AS deploy

RUN apt-get update && apt-get install -y --no-install-recommends \
		build-essential \
		gcc \
		libssl-dev \
		pkg-config \
		openssl \
		libvips-dev \
		python3 && \
	rm -rf /var/lib/apt/lists/*

COPY . .

RUN pnpm install --frozen-lockfile
RUN pnpm --filter @fluxer/config run --if-present generate
RUN pnpm deploy --legacy --filter=fluxer_api --prod /out

FROM node:24-bookworm-slim

ARG BUILD_VERSION

WORKDIR /usr/src/app/fluxer_api

RUN echo 'deb http://deb.debian.org/debian bookworm-backports main' > /etc/apt/sources.list.d/backports.list && \
	apt-get update && apt-get install -y --no-install-recommends \
		ca-certificates \
		ffmpeg \
		libimage-exiftool-perl \
		libwebp7 \
		libwebpmux3 \
		libheif1 \
		libvips42 && \
	apt-get install -y --no-install-recommends -t bookworm-backports \
		libheif-plugin-libde265 \
		libheif-plugin-dav1d && \
	rm -rf /var/lib/apt/lists/*

RUN corepack enable && corepack prepare pnpm@10.29.3 --activate

COPY --from=deploy /out ./
COPY --from=deploy /usr/src/app/tsconfigs /usr/src/app/tsconfigs

COPY scripts/file-secrets.sh /file-secrets.sh
RUN chmod +x /file-secrets.sh

RUN rm -rf pkgs && \
	mkdir -p /usr/src/app/.cache/corepack && \
	chown -R 65532:65532 /usr/src/app

ENV HOME=/usr/src/app
ENV COREPACK_HOME=/usr/src/app/.cache/corepack
ENV NODE_ENV=production
ENV NODE_OPTIONS="--max-old-space-size=2048"
ENV NODE_EXTRA_CA_CERTS=/etc/ssl/certs/ca-certificates.crt
ENV BUILD_VERSION=${BUILD_VERSION}



USER 65532:65532

EXPOSE 8080
CMD ["/file-secrets.sh", "./node_modules/.bin/tsx", "src/AppEntrypoint.ts"]
