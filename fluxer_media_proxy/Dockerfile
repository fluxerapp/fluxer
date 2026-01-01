FROM node:24-bookworm-slim AS base

WORKDIR /usr/src/app

RUN corepack enable && corepack prepare pnpm@10.26.0 --activate

FROM base AS deps

RUN apt-get update && apt-get install -y --no-install-recommends \
		build-essential \
		gcc \
		libssl-dev \
		pkg-config \
		ffmpeg \
		libvips-dev \
		libgomp1 \
		libatomic1 \
		python3 \
		make && \
	rm -rf /var/lib/apt/lists/*

COPY package.json pnpm-lock.yaml ./
RUN pnpm fetch --prod && pnpm install --frozen-lockfile --prod --offline

COPY tsconfig.json ./
COPY src ./src
COPY data ./data

FROM node:24-bookworm-slim AS build

WORKDIR /usr/src/app

RUN apt-get update && apt-get install -y --no-install-recommends \
	curl \
	ffmpeg \
	libvips \
	libgomp1 \
	libatomic1 && \
	rm -rf /var/lib/apt/lists/*

RUN corepack enable && corepack prepare pnpm@10.26.0 --activate

COPY --from=deps /usr/src/app/node_modules ./node_modules
COPY --from=deps /usr/src/app/package.json .
COPY --from=deps /usr/src/app/pnpm-lock.yaml .
COPY --from=deps /usr/src/app/tsconfig.json .
COPY --from=deps /usr/src/app/src ./src
COPY --from=deps /usr/src/app/data ./data

RUN mkdir -p /opt/data /usr/src/app/.cache/corepack && \
	cp data/model.onnx /opt/data/model.onnx && \
	chown -R nobody:nogroup /usr/src/app

ENV HOME=/usr/src/app
ENV COREPACK_HOME=/usr/src/app/.cache/corepack

USER nobody

EXPOSE 8080

ENV NODE_ENV=production
ENV PORT=8080

CMD ["pnpm", "start"]
