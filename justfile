env_file := "dev/.env"
compose_file := "dev/compose.yaml"
data_compose := "dev/compose.data.yaml"
network_name := "fluxer-shared"
compose_base := "docker compose --env-file " + env_file + " -f " + compose_file
livekit_template := "dev/templates/livekit.yaml"

up *SERVICES:
  just ensure-network
  {{compose_base}} up -d {{SERVICES}}

watch *SERVICES:
  just ensure-network
  {{compose_base}} watch {{SERVICES}}

down:
  {{compose_base}} down

nuke:
  {{compose_base}} down -v

restart *SERVICES:
  {{compose_base}} restart {{SERVICES}}

logs *SERVICES:
  {{compose_base}} logs -f --tail 100 {{SERVICES}}

ps:
  {{compose_base}} ps

sh SERVICE shell="sh":
  {{compose_base}} exec {{SERVICE}} {{shell}}

exec SERVICE CMD:
  {{compose_base}} exec {{SERVICE}} sh -c "{{CMD}}"

livekit-sync:
  set -euo pipefail
  if [ ! -f {{env_file}} ]; then
  echo "{{env_file}} missing"
  exit 1
  fi
  node --env-file {{env_file}} scripts/just/livekit-sync.js --output dev/livekit.yaml

ensure-network:
  set -euo pipefail
  docker network inspect {{network_name}} >/dev/null 2>&1 || docker network create {{network_name}}

bootstrap:
  just ensure-network
  just livekit-sync

setup:
  set -euo pipefail
  just ensure-network
  if [ ! -f dev/.env ]; then
  cp dev/.env.example dev/.env
  fi
  if [ ! -f dev/livekit.yaml ]; then
  cp {{livekit_template}} dev/livekit.yaml
  fi

mig name:
  @cargo run --release --quiet --manifest-path scripts/cassandra-migrate/Cargo.toml -- create "{{name}}"

mig-check:
  @cargo run --release --quiet --manifest-path scripts/cassandra-migrate/Cargo.toml -- check

mig-up host="localhost" user="cassandra" pass="cassandra":
  @cargo run --release --quiet --manifest-path scripts/cassandra-migrate/Cargo.toml -- --host "{{host}}" --username "{{user}}" --password "{{pass}}" up

mig-status host="localhost" user="cassandra" pass="cassandra":
  @cargo run --release --quiet --manifest-path scripts/cassandra-migrate/Cargo.toml -- --host "{{host}}" --username "{{user}}" --password "{{pass}}" status

lic:
  @cargo run --release --quiet --manifest-path scripts/license-enforcer/Cargo.toml

snow count="1":
  @cargo run --release --quiet --manifest-path scripts/snowflake-generator/Cargo.toml -- --count {{count}}

integration-tests:
  set -euo pipefail
  trap 'docker compose -f tests/integration/compose.yaml down' EXIT
  docker compose -f tests/integration/compose.yaml up --build --abort-on-container-exit integration-tests

go-tools-install:
  GOTOOLCHAIN=go1.25.5 go install honnef.co/go/tools/cmd/staticcheck@2025.1.1
  GOTOOLCHAIN=go1.25.5 go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.61.0

go-integration-check:
  gofmt -w tests/integration
  go test ./tests/integration/...
  $(go env GOPATH)/bin/staticcheck ./tests/integration/...
  $(go env GOPATH)/bin/golangci-lint run ./tests/integration/...
