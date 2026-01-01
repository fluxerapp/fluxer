ARG BUILD_TIMESTAMP=0
FROM erlang:27.1.1.0-alpine AS builder

COPY --from=ghcr.io/gleam-lang/gleam:nightly-erlang /bin/gleam /bin/gleam

RUN apk add --no-cache git curl

WORKDIR /app

COPY gleam.toml manifest.toml ./
COPY src ./src
COPY priv ./priv
COPY tailwind.css ./

RUN gleam deps download
RUN gleam export erlang-shipment

ARG TAILWIND_VERSION=v4.1.17
RUN ARCH=$(uname -m) && \
	if [ "$ARCH" = "x86_64" ]; then \
	TAILWIND_ARCH="x64"; \
	elif [ "$ARCH" = "aarch64" ]; then \
	TAILWIND_ARCH="arm64"; \
	else \
	TAILWIND_ARCH="x64"; \
	fi && \
	echo "Downloading Tailwind CSS $TAILWIND_VERSION for Alpine Linux: linux-$TAILWIND_ARCH-musl" && \
	curl -sSLf -o /tmp/tailwindcss "https://github.com/tailwindlabs/tailwindcss/releases/download/${TAILWIND_VERSION}/tailwindcss-linux-${TAILWIND_ARCH}-musl" && \
	chmod +x /tmp/tailwindcss && \
	/tmp/tailwindcss -i ./tailwind.css -o ./priv/static/app.css --minify

FROM erlang:27.1.1.0-alpine

ARG BUILD_TIMESTAMP

RUN apk add --no-cache openssl ncurses-libs curl

WORKDIR /app

COPY --from=builder /app/build/erlang-shipment /app
COPY --from=builder /app/priv ./priv

EXPOSE 8080

ENV PORT=8080
ENV BUILD_TIMESTAMP=${BUILD_TIMESTAMP}

CMD ["/app/entrypoint.sh", "run"]
