# Fluxer Development Justfile
# Uses the dev CLI (Go) as a compose wrapper

# Dev CLI command
devctl := "go run ./dev"

# ---------------------------------------------------------------------------
# Docker Compose: Service Management
# ---------------------------------------------------------------------------

# Start all or selected services in the background
# Usage:
#   just up              # all services
#   just up api gateway  # specific services
up *SERVICES:
  {{devctl}} ensure-network
  {{devctl}} up {{SERVICES}}

# Start all or selected services and watch for changes
# Usage:
#   just watch
#   just watch api gateway
watch *SERVICES:
  {{devctl}} ensure-network
  docker compose --env-file dev/.env -f dev/compose.yaml watch {{SERVICES}}

# Stop and remove containers (preserves volumes)
# Usage:
#   just down
down:
  {{devctl}} down

# Stop and remove containers including volumes
# Usage:
#   just nuke
nuke:
  {{devctl}} down --volumes

# Restart all or selected services
# Usage:
#   just restart
#   just restart api gateway
restart *SERVICES:
  {{devctl}} restart {{SERVICES}}

# Show logs for all or selected services
# Usage:
#   just logs
#   just logs api
#   just logs api gateway
logs *SERVICES:
  {{devctl}} logs {{SERVICES}}

# List running containers
# Usage:
#   just ps
ps:
  {{devctl}} ps

# Open a shell in a service container
# Usage:
#   just sh api
sh SERVICE:
  {{devctl}} sh {{SERVICE}}

# Execute a command in a service container
# Usage:
#   just exec api "env | sort"
exec SERVICE CMD:
  {{devctl}} exec {{SERVICE}} sh -c "{{CMD}}"

# ---------------------------------------------------------------------------
# Configuration & Setup
# ---------------------------------------------------------------------------

# Sync LiveKit configuration from environment variables
# Usage:
#   just livekit-sync
livekit-sync:
  {{devctl}} livekit-sync

# Download GeoIP database
# Usage:
#   just geoip-download
#   just geoip-download TOKEN=xxx
geoip-download TOKEN='':
  if [ "{{TOKEN}}" = "" ]; then {{devctl}} geoip-download; else {{devctl}} geoip-download --token {{TOKEN}}; fi

# Ensure Docker network exists
# Usage:
#   just ensure-network
ensure-network:
  {{devctl}} ensure-network

# Bootstrap development environment
# Usage:
#   just bootstrap
bootstrap:
  just ensure-network
  just livekit-sync
  just geoip-download

# ---------------------------------------------------------------------------
# Cassandra Migrations
# ---------------------------------------------------------------------------

mig name:
  @cargo run --release --quiet --manifest-path scripts/cassandra-migrate/Cargo.toml -- create "{{name}}"

mig-check:
  @cargo run --release --quiet --manifest-path scripts/cassandra-migrate/Cargo.toml -- check

mig-up host="localhost" user="cassandra" pass="cassandra":
  @cargo run --release --quiet --manifest-path scripts/cassandra-migrate/Cargo.toml -- --host "{{host}}" --username "{{user}}" --password "{{pass}}" up

mig-status host="localhost" user="cassandra" pass="cassandra":
  @cargo run --release --quiet --manifest-path scripts/cassandra-migrate/Cargo.toml -- --host "{{host}}" --username "{{user}}" --password "{{pass}}" status

# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------

# Run license enforcer
lic:
  @cargo run --release --quiet --manifest-path scripts/license-enforcer/Cargo.toml

# Generate snowflake IDs
snow count="1":
  @cargo run --release --quiet --manifest-path scripts/snowflake-generator/Cargo.toml -- --count {{count}}

# ---------------------------------------------------------------------------
# Integration Tests
# ---------------------------------------------------------------------------

# Spin up the full integration stack, run the Go tests, then tear everything down
integration-tests:
  set -euo pipefail
  trap 'docker compose -f tests/integration/compose.yaml down' EXIT
  docker compose -f tests/integration/compose.yaml up --build --abort-on-container-exit integration-tests

# ---------------------------------------------------------------------------
# Go Tooling & QA
# ---------------------------------------------------------------------------

# Install pinned Go tooling (staticcheck, golangci-lint) with Go 1.25.5
go-tools-install:
  GOTOOLCHAIN=go1.25.5 go install honnef.co/go/tools/cmd/staticcheck@2025.1.1
  GOTOOLCHAIN=go1.25.5 go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.61.0

# Run formatting, tests, and linters for integration tests
go-integration-check:
  gofmt -w tests/integration
  go test ./tests/integration/...
  $(go env GOPATH)/bin/staticcheck ./tests/integration/...
  $(go env GOPATH)/bin/golangci-lint run ./tests/integration/...
