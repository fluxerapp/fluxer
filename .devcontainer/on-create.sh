#!/usr/bin/env bash

# Runs once when the container is first created.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
export FLUXER_CONFIG="${FLUXER_CONFIG:-$REPO_ROOT/config/config.json}"

GREEN='\033[0;32m'
NC='\033[0m'
info() { printf "%b\n" "${GREEN}[devcontainer]${NC} $1"; }

info "Installing pnpm dependencies..."
pnpm install

# Codegen outputs (e.g. MasterZodSchema.generated.tsx) are gitignored.
info "Generating config schema..."
pnpm --filter @fluxer/config generate

if [ ! -f "$FLUXER_CONFIG" ]; then
	info "Creating config from development template..."
	cp "$REPO_ROOT/config/config.dev.template.json" "$FLUXER_CONFIG"
fi

# Point services at Docker Compose hostnames and adjust settings that differ
# from the default dev template.
info "Patching config for Docker Compose networking..."
jq '
	# rspack defaults public_scheme to "https" when unset
	.domain.public_scheme = "http" |
	# Relative path so the app works on any hostname (localhost, 127.0.0.1, etc.)
	.app_public.bootstrap_api_endpoint = "/api" |

	.internal.kv = "redis://valkey:6379/0" |

	.integrations.search.url = "http://meilisearch:7700" |
	.integrations.search.api_key = "fluxer-devcontainer-meili-master-key" |

	# Credentials must match .devcontainer/livekit.yaml
	.integrations.voice.url = "ws://livekit:7880" |
	.integrations.voice.webhook_url = "http://app:49319/api/webhooks/livekit" |
	.integrations.voice.api_key = "fluxer-devcontainer-key" |
	.integrations.voice.api_secret = "fluxer-devcontainer-secret-key-00000000" |

	.integrations.email.smtp.host = "mailpit" |
	.integrations.email.smtp.port = 1025 |

	.services.nats.core_url = "nats://nats-core:4222" |
	.services.nats.jetstream_url = "nats://nats-jetstream:4223" |

	# Bluesky OAuth requires HTTPS + loopback IPs (RFC 8252), incompatible with
	# the HTTP-only devcontainer setup.
	.auth.bluesky.enabled = false
' "$FLUXER_CONFIG" > "$FLUXER_CONFIG.tmp" && mv "$FLUXER_CONFIG.tmp" "$FLUXER_CONFIG"

info "Running bootstrap..."
"$REPO_ROOT/scripts/dev_bootstrap.sh"

info "Pre-compiling Erlang gateway dependencies..."
(cd "$REPO_ROOT/fluxer_gateway" && rebar3 compile) || {
	info "Gateway pre-compilation failed (non-fatal, will compile on first start)"
}

info "Devcontainer setup complete."
info ""
info "  Start all dev processes:  process-compose -f .devcontainer/process-compose.yml up"
info "  Open the app:             http://127.0.0.1:48763"
info "  Dev email inbox:          http://127.0.0.1:48763/mailpit/"
info ""
