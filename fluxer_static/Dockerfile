# syntax=docker/dockerfile:1.7
# SPDX-License-Identifier: AGPL-3.0-or-later

FROM caddy:2.10-alpine

ARG BUILD_VERSION=""

LABEL org.opencontainers.image.title="fluxer-static"
LABEL org.opencontainers.image.description="Self-hostable static asset bundle for Fluxer"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.source="https://github.com/fluxerapp/fluxer/tree/main/fluxer_static"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

COPY fluxer_static/Caddyfile /etc/caddy/Caddyfile
COPY --chown=65532:65532 fluxer_static/avatars /srv/fluxer-static/avatars
COPY --chown=65532:65532 fluxer_static/badges /srv/fluxer-static/badges
COPY --chown=65532:65532 fluxer_static/desktop /srv/fluxer-static/desktop
COPY --chown=65532:65532 fluxer_static/embeds /srv/fluxer-static/embeds
COPY --chown=65532:65532 fluxer_static/emoji /srv/fluxer-static/emoji
COPY --chown=65532:65532 fluxer_static/libs /srv/fluxer-static/libs
COPY --chown=65532:65532 fluxer_static/web /srv/fluxer-static/web

RUN mkdir -p /config/caddy /data/caddy \
	&& chown -R 65532:65532 /config /data /srv/fluxer-static

USER 65532:65532

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
	CMD wget -q -O - http://127.0.0.1:8080/_health | grep -q OK
