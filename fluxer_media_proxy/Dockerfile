# syntax=docker/dockerfile:1.7

ARG DEBIAN_BASE=debian:trixie-slim
ARG RUST_IMAGE=rust:1-trixie

FROM ${RUST_IMAGE} AS builder
WORKDIR /build

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates pkg-config build-essential \
        libcurl4-openssl-dev libvips-dev \
        libavformat-dev libavcodec-dev libavfilter-dev libavutil-dev libswscale-dev libswresample-dev \
        libheif-dev libwebp-dev \
    && rm -rf /var/lib/apt/lists/*

COPY . .

RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/usr/local/cargo/git \
    --mount=type=cache,target=/build/target \
    cargo test -p fluxer-media-proxy \
    && cargo build --release -p fluxer-media-proxy \
    && cp target/release/fluxer-media-proxy /usr/local/bin/fluxer-media-proxy

FROM ${DEBIAN_BASE} AS runtime
ARG BUILD_VERSION=""

LABEL org.opencontainers.image.title="fluxer-media-proxy"
LABEL org.opencontainers.image.description="High-throughput media transcoding proxy: libvips + libavformat + libcurl"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.source="https://github.com/fluxer/fluxer/tree/main/fluxer_media_proxy"
LABEL org.opencontainers.image.documentation="https://github.com/fluxer/fluxer/blob/main/fluxer_media_proxy/README.md"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates tini \
        libcurl4t64 libvips42t64 \
        libavformat61 libavcodec61 libavfilter10 libavutil59 libswscale8 libswresample5 \
        libheif1 libheif-plugin-libde265 libheif-plugin-dav1d \
        libwebp7 libwebpmux3 \
        libjemalloc2 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN groupadd --system --gid 10001 fluxer \
    && useradd --system --uid 10001 --gid 10001 --home /var/lib/fluxer --create-home fluxer

COPY --from=builder /usr/local/bin/fluxer-media-proxy /usr/local/bin/fluxer-media-proxy
COPY scripts/file-secrets.sh /file-secrets.sh
RUN chmod +x /file-secrets.sh

RUN install -d -o fluxer -g fluxer /var/lib/fluxer/media_proxy_storage

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
    CMD ["sh", "-c", "exec 3<>/dev/tcp/127.0.0.1/${FLUXER_MEDIA_PROXY_PORT:-8080} && printf 'GET /_health HTTP/1.0\\r\\n\\r\\n' >&3 && grep -q '200' <&3"]

ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["/file-secrets.sh", "/usr/local/bin/fluxer-media-proxy"]
