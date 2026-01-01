FROM node:24-bookworm-slim AS base

WORKDIR /usr/src/app

RUN corepack enable && corepack prepare pnpm@10.26.0 --activate

FROM base AS deps

RUN apt-get update && apt-get install -y --no-install-recommends \
		build-essential \
		gcc \
		libssl-dev \
		pkg-config \
		openssl \
		libvips-dev && \
	rm -rf /var/lib/apt/lists/*

COPY package.json pnpm-lock.yaml ./
RUN pnpm fetch --prod && pnpm install --frozen-lockfile --prod --offline

FROM node:24-bookworm-slim

WORKDIR /usr/src/app

RUN apt-get update && apt-get install -y --no-install-recommends \
	curl \
	openssl \
	libimage-exiftool-perl \
	libvips \
	libvips-dev && \
	rm -rf /var/lib/apt/lists/*

RUN corepack enable && corepack prepare pnpm@10.26.0 --activate

COPY --from=deps /usr/src/app/node_modules ./node_modules
COPY package.json pnpm-lock.yaml ./
COPY tsconfig.json tsconfig.worker.json ./
COPY src ./src

RUN mkdir -p /usr/src/app/.cache/corepack && \
	chown -R nobody:nogroup /usr/src/app

ENV HOME=/usr/src/app
ENV COREPACK_HOME=/usr/src/app/.cache/corepack

USER nobody

EXPOSE 8080
