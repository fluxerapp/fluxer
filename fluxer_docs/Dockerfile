# SPDX-License-Identifier: AGPL-3.0-or-later

FROM python:3.13-alpine AS builder

WORKDIR /src/fluxer_docs

COPY requirements.txt requirements.txt
RUN python -m pip install --no-cache-dir --upgrade pip \
	&& python -m pip install --no-cache-dir -r requirements.txt

COPY zensical.toml zensical.toml
COPY docs docs
COPY scripts scripts
RUN zensical build --clean \
	&& python scripts/write_csp_header.py site /tmp/security-headers.conf

FROM nginxinc/nginx-unprivileged:1.29-alpine AS runtime

ARG BUILD_VERSION=""
ARG SOURCE_SHA=""
ARG SOURCE_DATE=""

LABEL maintainer="Fluxer"
LABEL org.opencontainers.image.title="fluxer-docs"
LABEL org.opencontainers.image.description="Fluxer documentation site"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="Fluxer"
LABEL org.opencontainers.image.url="https://fluxer.app"
LABEL org.opencontainers.image.documentation="https://docs.fluxer.app"
LABEL org.opencontainers.image.source="https://github.com/fluxerapp/fluxer"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL org.opencontainers.image.revision="${SOURCE_SHA}"
LABEL org.opencontainers.image.created="${SOURCE_DATE}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=builder /tmp/security-headers.conf /etc/nginx/conf.d/security-headers.conf
COPY --from=builder /src/fluxer_docs/site /usr/share/nginx/html

EXPOSE 8080
