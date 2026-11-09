# syntax=docker/dockerfile:1.7
# SPDX-License-Identifier: AGPL-3.0-or-later

ARG SOURCE=export
ARG DIST=fluxer-*-linux-*.tar.zst

FROM erlang:28.5.0.8-slim@sha256:45e0fcffac25a41ed047898a57c7913cc69b5114c1aea0db12c85ed4b1164d77 AS toolchain

COPY tools/dist/toolchain.sh /usr/local/src/fluxer-dist/toolchain.sh
RUN /usr/local/src/fluxer-dist/toolchain.sh /opt/toolchain \
    && rm -rf /var/lib/apt/lists/*

FROM toolchain AS native

RUN useradd --create-home --uid 1000 build \
    && install -d -o build -g build /opt/native

COPY fluxer_media_proxy/tools/install-native-deps.sh /usr/local/src/fluxer-dist/install-native-deps.sh

USER build
RUN /usr/local/src/fluxer-dist/install-native-deps.sh /opt/native
USER root

FROM native AS build

ARG BUILD_VERSION
ARG PROFILE
ARG CARGO_BUILD_JOBS

ENV FLUXER_TOOLCHAIN=/opt/toolchain \
    FLUXER_NATIVE_PREFIX=/opt/native \
    PNPM_STORE_DIR=/root/.cache/pnpm-store

WORKDIR /src
COPY . .

RUN --mount=type=cache,target=/opt/toolchain/cargo/registry \
    --mount=type=cache,target=/opt/toolchain/cargo/git \
    --mount=type=cache,target=/src/target \
    --mount=type=cache,target=/root/.cache \
    --mount=type=cache,target=/root/go/pkg/mod \
    tools/dist/build.sh /out

FROM scratch AS export
COPY --from=build /out/*.tar.zst /out/SHA256SUMS /

FROM scratch AS fromdist
ARG DIST
COPY --from=dist ${DIST} /

FROM ${SOURCE} AS release

FROM debian:trixie-slim@sha256:a72b4433de1cf7d6bae0c4cee74d0547f58601a643b3375f070c51600d63b6b2 AS unpack
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean \
    && apt-get update \
    && apt-get install -y --no-install-recommends zstd
RUN --mount=type=bind,from=release,target=/dist \
    set -eu; \
    set -- /dist/fluxer-*-linux-*.tar.zst; \
    [ "$#" -eq 1 ] && [ -f "$1" ] || { echo "expected exactly one fluxer-*-linux-*.tar.zst, found: $*" >&2; exit 1; }; \
    mkdir -p /stage; \
    tar --zstd -xf "$1" -C /stage --no-same-owner; \
    if [ -e /stage/bin ]; then mv /stage /release; else mv /stage/* /release; fi; \
    test -x /release/bin/fluxerd

FROM debian:trixie-slim@sha256:a72b4433de1cf7d6bae0c4cee74d0547f58601a643b3375f070c51600d63b6b2 AS runtime

ARG BUILD_VERSION=""
ARG SOURCE_SHA=""
ARG SOURCE_DATE=""

LABEL org.opencontainers.image.title="fluxer"
LABEL org.opencontainers.image.description="Fluxer in one container, supervised by fluxerd"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="Fluxer"
LABEL org.opencontainers.image.url="https://fluxer.com"
LABEL org.opencontainers.image.documentation="https://docs.fluxer.com"
LABEL org.opencontainers.image.source="https://github.com/fluxerapp/fluxer"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL org.opencontainers.image.revision="${SOURCE_SHA}"
LABEL org.opencontainers.image.created="${SOURCE_DATE}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    --mount=type=bind,from=unpack,source=/release/share/runtime-packages,target=/run/runtime-packages \
    rm -f /etc/apt/apt.conf.d/docker-clean \
    && apt-get update \
    && apt-get install -y --no-install-recommends postgresql-common \
    && sed -ri 's/^#?[[:space:]]*create_main_cluster[[:space:]]*=.*/create_main_cluster = false/' \
        /etc/postgresql-common/createcluster.conf \
    && grep -q '^create_main_cluster = false$' /etc/postgresql-common/createcluster.conf \
    && apt-get install -y --no-install-recommends tini libcap2-bin $(cat /run/runtime-packages) \
    && rm -f /usr/lib/*/libheif.so.1* \
    && ldconfig \
    && test ! -e /etc/postgresql/17/main

COPY --from=unpack /release /opt/fluxer/current

RUN groupadd --gid 65532 fluxer \
    && useradd --uid 65532 --gid 65532 --home-dir /var/lib/fluxer --no-create-home --shell /usr/sbin/nologin fluxer \
    && install -d -o 65532 -g 65532 -m 0700 /var/lib/fluxer /run/fluxer \
    && setcap cap_net_bind_service=+p /opt/fluxer/current/bin/fluxerd \
    && for f in /opt/fluxer/current/bin/* /opt/fluxer/current/gateway/erts-*/bin/beam.smp; do \
        out="$(LD_LIBRARY_PATH=/opt/fluxer/current/lib ldd "$f" 2>&1)" || continue; \
        if printf '%s\n' "$out" | grep 'not found'; then echo "$f has unresolved libraries" >&2; exit 1; fi; \
    done

ENV FLUXER_DATA_DIR=/var/lib/fluxer
ENV BUILD_VERSION="${BUILD_VERSION}"

USER 65532:65532
WORKDIR /var/lib/fluxer
VOLUME /var/lib/fluxer

EXPOSE 80 443 443/udp 7881 7882/udp

HEALTHCHECK --interval=30s --timeout=10s --start-period=300s --retries=3 \
    CMD ["/opt/fluxer/current/bin/fluxerd", "health"]

STOPSIGNAL SIGTERM

ENTRYPOINT ["/usr/bin/tini", "--", "/opt/fluxer/current/bin/fluxerd", "run"]
