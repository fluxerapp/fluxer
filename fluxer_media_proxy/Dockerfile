# syntax=docker/dockerfile:1.7

ARG DEBIAN_BASE=debian:trixie-slim
ARG RUST_IMAGE=rust:1-trixie

FROM ${DEBIAN_BASE} AS native

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates curl xz-utils build-essential pkg-config cmake nasm yasm git \
        zlib1g-dev libde265-dev libdav1d-dev libaom-dev \
        libvips-dev libwebp-dev liblcms2-dev libyuv-dev \
    && rm -rf /var/lib/apt/lists/*

COPY fluxer_media_proxy/tools/install-native-deps.sh /tmp/install-native-deps.sh
RUN /tmp/install-native-deps.sh /usr/local

RUN mkdir -p /native/lib \
    && cp -a /usr/local/lib/*.so.* /native/lib/

FROM ${RUST_IMAGE} AS builder
WORKDIR /build

COPY --from=native /usr/local /usr/local
RUN ldconfig

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates pkg-config build-essential \
        libcurl4-openssl-dev libvips-dev libwebp-dev \
        liblcms2-dev libyuv-dev libde265-dev libdav1d-dev libaom-dev \
    && rm -rf /var/lib/apt/lists/* \
    && rm -f /usr/lib/*/libheif.so.1* \
    && ldconfig \
    && [ "$(ldconfig -p | grep -c 'libheif\.so\.1 ')" -eq 1 ] \
    && ldconfig -p | grep -q 'libheif\.so\.1 .* => /usr/local/lib/libheif\.so\.1$'

COPY . .

RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/usr/local/cargo/git \
    --mount=type=cache,target=/build/target \
    cargo test -p fluxer-media-proxy \
    && cargo build --release -p fluxer-media-proxy \
    && cp target/release/fluxer-media-proxy /usr/local/bin/fluxer-media-proxy

FROM ${DEBIAN_BASE} AS runtime
ARG BUILD_VERSION=""
ARG SOURCE_SHA=""
ARG SOURCE_DATE=""

LABEL org.opencontainers.image.title="fluxer-media-proxy"
LABEL org.opencontainers.image.description="High-throughput media transcoding proxy: libvips + libavformat + libcurl"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="Fluxer"
LABEL org.opencontainers.image.url="https://fluxer.app"
LABEL org.opencontainers.image.documentation="https://docs.fluxer.app"
LABEL org.opencontainers.image.source="https://github.com/fluxerapp/fluxer"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL org.opencontainers.image.revision="${SOURCE_SHA}"
LABEL org.opencontainers.image.created="${SOURCE_DATE}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

COPY --from=native /native/lib /usr/local/lib

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates tini \
        libcurl4t64 libvips42t64 \
        libwebp7 libwebpmux3 libwebpdemux2 \
        liblcms2-2 libyuv0 libde265-0 libdav1d7 libaom3 \
        libjemalloc2 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && rm -f /usr/lib/*/libheif.so.1* \
    && ldconfig

RUN groupadd --system --gid 10001 fluxer \
    && useradd --system --uid 10001 --gid 10001 --home /var/lib/fluxer --create-home fluxer

COPY --from=builder /usr/local/bin/fluxer-media-proxy /usr/local/bin/fluxer-media-proxy

RUN install -d -o fluxer -g fluxer /var/lib/fluxer/media_proxy_storage

RUN ldd /usr/local/bin/fluxer-media-proxy \
    && ! ldd /usr/local/bin/fluxer-media-proxy | grep -q 'not found' \
    && [ "$(ldconfig -p | grep -c 'libheif\.so\.1 ')" -eq 1 ] \
    && ldconfig -p | grep -q 'libheif\.so\.1 .* => /usr/local/lib/libheif\.so\.1$' \
    && ldd /usr/local/bin/fluxer-media-proxy | grep -q 'libheif\.so\.1 => /usr/local/lib/libheif\.so\.1'

ENV LD_PRELOAD=libjemalloc.so.2
ENV MALLOC_CONF="background_thread:true,metadata_thp:auto,dirty_decay_ms:5000,muzzy_decay_ms:5000"

ENV VIPS_CONCURRENCY=1
ENV VIPS_DISC_THRESHOLD=1g

ENV G_MESSAGES_DEBUG=

ENV FLUXER_MEDIA_PROXY_HOST=0.0.0.0
ENV FLUXER_MEDIA_PROXY_PORT=8080
ENV BUILD_VERSION="${BUILD_VERSION}"

USER 10001:10001
WORKDIR /var/lib/fluxer

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD ["/usr/local/bin/fluxer-media-proxy", "healthcheck"]

ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["/usr/local/bin/fluxer-media-proxy"]
