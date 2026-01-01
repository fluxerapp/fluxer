FROM rustlang/rust:nightly-slim AS builder

WORKDIR /app

RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

COPY Cargo.toml ./

RUN mkdir -p src && echo "fn main() {}" > src/main.rs

RUN cargo build --release 2>/dev/null || true

RUN rm -rf src

COPY src ./src

RUN touch src/main.rs && cargo build --release

FROM debian:bookworm-slim

WORKDIR /app

RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/target/release/fluxer_metrics /app/fluxer_metrics

RUN useradd -r -s /bin/false fluxer
USER fluxer

HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/_health || exit 1

EXPOSE 8080

CMD ["/app/fluxer_metrics"]
