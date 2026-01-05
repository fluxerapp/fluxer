FROM rustlang/rust:nightly AS builder

WORKDIR /workspace

COPY scripts/cassandra-migrate/Cargo.toml scripts/cassandra-migrate/Cargo.toml
COPY scripts/cassandra-migrate/Cargo.lock scripts/cassandra-migrate/Cargo.lock
COPY scripts/cassandra-migrate/src scripts/cassandra-migrate/src

WORKDIR /workspace/scripts/cassandra-migrate
RUN cargo build --release

FROM debian:bookworm-slim

WORKDIR /workspace

RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates && rm -rf /var/lib/apt/lists/*

COPY --from=builder /workspace/scripts/cassandra-migrate/target/release/cassandra-migrate /usr/local/bin/cassandra-migrate
COPY fluxer_devops/cassandra/migrations fluxer_devops/cassandra/migrations

ENTRYPOINT ["cassandra-migrate"]
