# SPDX-License-Identifier: AGPL-3.0-or-later

FROM node:26-trixie-slim AS build

WORKDIR /usr/src/app

RUN npm install -g pnpm@11.27.0

COPY . .

RUN pnpm --filter @fluxer/music... install --frozen-lockfile
RUN pnpm --filter @fluxer/music run build
RUN pnpm --filter @fluxer/music deploy --legacy --prod --config.allow-unused-patches=true /out

FROM node:26-trixie-slim

LABEL org.opencontainers.image.title="fluxer-music"
LABEL org.opencontainers.image.description="Fluxer music bot"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"

RUN apt-get update && apt-get install -y --no-install-recommends \
		ffmpeg \
		python3-venv \
		ca-certificates && \
	rm -rf /var/lib/apt/lists/*

# Fallback yt-dlp; the entrypoint tries to update it into /var/lib/ytdlp at start.
RUN python3 -m venv /opt/ytdlp && \
	/opt/ytdlp/bin/pip install --no-cache-dir 'yt-dlp[default]'

RUN useradd --uid 10001 --no-create-home --shell /usr/sbin/nologin music && \
	mkdir -p /var/lib/ytdlp /app && \
	chown 10001:10001 /var/lib/ytdlp

WORKDIR /app

COPY --from=build /out/node_modules ./node_modules
COPY --from=build /out/package.json ./package.json
COPY --from=build /usr/src/app/fluxer_music/dist ./dist
COPY fluxer_music/docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh

ENV NODE_ENV=production
ENV HOME=/tmp
ENV PIP_CACHE_DIR=/tmp/pip-cache

USER 10001:10001

ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
