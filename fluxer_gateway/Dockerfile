# SPDX-License-Identifier: AGPL-3.0-or-later
FROM erlang:28.5.0.8-slim@sha256:45e0fcffac25a41ed047898a57c7913cc69b5114c1aea0db12c85ed4b1164d77 AS build

ARG GLEAM_VERSION=1.19.2
ARG TARGETARCH

ENV RUSTUP_HOME=/usr/local/rustup \
	CARGO_HOME=/usr/local/cargo \
	RUSTUP_TOOLCHAIN=1.98.1 \
	CARGO_PROFILE_RELEASE_STRIP=symbols \
	RUSTFLAGS="--remap-path-prefix=/usr/local/rustup=rustup --remap-path-prefix=/usr/local/cargo=cargo --remap-path-prefix=/usr/local/cargo/registry/src=deps --remap-path-prefix=/usr/src/app/packages=packages --remap-path-prefix=/usr/src/app/fluxer_gateway/rust=." \
	PATH="/usr/local/cargo/bin:${PATH}"

RUN apt-get update && apt-get install -y --no-install-recommends \
	ca-certificates \
	curl \
	gcc \
	libc6-dev \
	&& rm -rf /var/lib/apt/lists/*

RUN case "${TARGETARCH:-amd64}" in \
	amd64) gleam_arch=x86_64 ;; \
	arm64) gleam_arch=aarch64 ;; \
	*) echo "unsupported architecture ${TARGETARCH}" >&2 && exit 1 ;; \
	esac && \
	gleam_archive="gleam-v${GLEAM_VERSION}-${gleam_arch}-unknown-linux-musl.tar.gz" && \
	gleam_url="https://github.com/gleam-lang/gleam/releases/download/v${GLEAM_VERSION}/${gleam_archive}" && \
	curl --retry 5 --retry-delay 2 --retry-all-errors -fsSL "${gleam_url}" -o "/tmp/${gleam_archive}" && \
	curl --retry 5 --retry-delay 2 --retry-all-errors -fsSL "${gleam_url}.sha256" -o "/tmp/${gleam_archive}.sha256" && \
	cd /tmp && echo "$(cut -d ' ' -f 1 "${gleam_archive}.sha256")  ${gleam_archive}" | sha256sum -c - && \
	tar -xzf "/tmp/${gleam_archive}" -C /usr/local/bin gleam && \
	rm -f "/tmp/${gleam_archive}" "/tmp/${gleam_archive}.sha256"

COPY --from=rust:1.98.1-slim-trixie@sha256:4cd829461bd5c4d511c32e269da9cb8929223b666519d8004e35fc8d1d771ab7 /usr/local/rustup /usr/local/rustup
COPY --from=rust:1.98.1-slim-trixie@sha256:4cd829461bd5c4d511c32e269da9cb8929223b666519d8004e35fc8d1d771ab7 /usr/local/cargo /usr/local/cargo

WORKDIR /usr/src/app

COPY packages/markdown_parser/rust ./packages/markdown_parser/rust
COPY fluxer_gateway/rust/Cargo.toml fluxer_gateway/rust/Cargo.lock ./fluxer_gateway/rust/
COPY fluxer_gateway/rust/crates ./fluxer_gateway/rust/crates

WORKDIR /usr/src/app/fluxer_gateway/rust
RUN cargo build --locked --release -p gateway_nif && \
	mkdir -p /usr/src/app/fluxer_gateway/priv && \
	strip --strip-unneeded -o /usr/src/app/fluxer_gateway/priv/gateway_nif.so target/release/libgateway_nif.so

WORKDIR /usr/src/app/fluxer_gateway

COPY fluxer_gateway/gleam.toml fluxer_gateway/manifest.toml ./
COPY fluxer_gateway/bouncer_proto ./bouncer_proto
RUN gleam deps download

COPY fluxer_gateway/bouncer/gleam.toml fluxer_gateway/bouncer/manifest.toml ./bouncer/
RUN cd bouncer && gleam deps download

COPY fluxer_gateway/src ./src
RUN gleam export erlang-shipment && \
	test -f build/erlang-shipment/fluxer_gateway/priv/gateway_nif.so && \
	test -x build/erlang-shipment/entrypoint.sh

COPY fluxer_gateway/bouncer/src ./bouncer/src
RUN cd bouncer && gleam export erlang-shipment && \
	test -x build/erlang-shipment/entrypoint.sh && \
	! find build/erlang-shipment -name 'fluxer_gateway@*' | grep -q .

RUN mkdir -p /out/fluxer_gateway /out/fluxer_bouncer && \
	cp -R build/erlang-shipment/. /out/fluxer_gateway/ && \
	cp -R bouncer/build/erlang-shipment/. /out/fluxer_bouncer/ && \
	rm -f /out/*/entrypoint.sh /out/*/entrypoint.ps1 && \
	find /out -depth -type d -name include -exec rm -rf {} + && \
	find /out -type f ! -name '*.beam' ! -name '*.app' ! -path '/out/fluxer_gateway/fluxer_gateway/priv/gateway_nif.so' -delete && \
	find /out -depth -type d -empty -delete && \
	erl -noshell -eval 'Files = filelib:wildcard("/out/*/*/ebin/*.beam"), {ok, _} = beam_lib:strip_files(Files), halt().' && \
	test -f /out/fluxer_gateway/fluxer_gateway/priv/gateway_nif.so && \
	test -z "$(find /out -type f ! -name '*.beam' ! -name '*.app' ! -name 'gateway_nif.so')"

FROM erlang:28.5.0.8-slim@sha256:45e0fcffac25a41ed047898a57c7913cc69b5114c1aea0db12c85ed4b1164d77

ARG BUILD_VERSION=""
ARG SOURCE_SHA=""
ARG SOURCE_DATE=""

LABEL org.opencontainers.image.title="fluxer-gateway"
LABEL org.opencontainers.image.description="Fluxer realtime gateway"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.vendor="Fluxer"
LABEL org.opencontainers.image.url="https://fluxer.com"
LABEL org.opencontainers.image.documentation="https://docs.fluxer.com"
LABEL org.opencontainers.image.source="https://github.com/fluxerapp/fluxer"
LABEL org.opencontainers.image.version="${BUILD_VERSION}"
LABEL org.opencontainers.image.revision="${SOURCE_SHA}"
LABEL org.opencontainers.image.created="${SOURCE_DATE}"
LABEL app.fluxer.build-version="${BUILD_VERSION}"

WORKDIR /opt/fluxer_gateway

RUN apt-get update && apt-get install -y --no-install-recommends \
	ca-certificates \
	curl \
	libstdc++6 \
	&& rm -rf /var/lib/apt/lists/* /usr/share/doc /usr/share/man /usr/share/info && \
	groupadd -r -g 65532 fluxer && \
	useradd -r -u 65532 -g 65532 -s /sbin/nologin -d /opt/fluxer_gateway fluxer && \
	chown 65532:65532 /opt/fluxer_gateway

COPY --from=build --chown=65532:65532 /out/ /opt/
COPY --chown=65532:65532 fluxer_gateway/config/vm.args.src fluxer_gateway/config/bouncer_vm.args.src fluxer_gateway/config/sys.config /opt/fluxer_gateway/config/
COPY --chown=65532:65532 --chmod=0755 fluxer_gateway/scripts/docker_entrypoint.sh /opt/fluxer_gateway/bin/docker_entrypoint.sh

USER 65532:65532

ENV BUILD_VERSION="${BUILD_VERSION}"
ENV BUILD_REVISION="${SOURCE_SHA}"
ENV FLUXER_GATEWAY_HOME=/opt/fluxer_gateway
ENV FLUXER_BOUNCER_HOME=/opt/fluxer_bouncer
ENV FLUXER_GATEWAY_RUN_DIR=/tmp

EXPOSE 8080 8081 8082

ENTRYPOINT ["/opt/fluxer_gateway/bin/docker_entrypoint.sh"]
