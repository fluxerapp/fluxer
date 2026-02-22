#!/usr/bin/env sh

# Copyright (C) 2026 Fluxer Contributors
#
# This file is part of Fluxer.
#
# Fluxer is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Fluxer is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

info() { printf "%b\n" "${GREEN}[INFO]${NC} $1"; }
warn() { printf "%b\n" "${YELLOW}[WARN]${NC} $1"; }
error() { printf "%b\n" "${RED}[ERROR]${NC} $1"; }

prepare_log_dir() {
    info "Ensuring dev log directory exists..."
    mkdir -p "$REPO_ROOT/dev/logs"
}

check_config() {
    config_path="${FLUXER_CONFIG:-$REPO_ROOT/config/config.json}"
    template_path="$REPO_ROOT/config/config.dev.template.json"

    if [ ! -f "$config_path" ]; then
        if [ -f "$template_path" ]; then
            info "No config found, creating from development template..."
            cp "$template_path" "$config_path"
        else
            error "Configuration file not found: $config_path"
            error "Template file also missing: $template_path"
            exit 1
        fi
    fi
}

random_hex() {
    byte_count="$1"

    node - "$byte_count" <<'NODE'
const {randomBytes} = require('node:crypto');

const byteCount = Number(process.argv[2]);
if (!Number.isInteger(byteCount) || byteCount <= 0) {
	process.exit(1);
}

process.stdout.write(randomBytes(byteCount).toString('hex'));
NODE
}

is_empty_or_placeholder() {
    value="$1"
    shift

    if [ -z "$value" ]; then
        return 0
    fi

    for placeholder in "$@"; do
        if [ "$value" = "$placeholder" ]; then
            return 0
        fi
    done

    return 1
}

seed_hex_secret() {
    current_value="$1"
    byte_count="$2"
    shift 2

    if is_empty_or_placeholder "$current_value" "$@"; then
        random_hex "$byte_count"
    else
        printf '%s' "$current_value"
    fi
}

sync_meilisearch_key_file() {
    has_search="$1"
    api_key="$2"

    if [ "$has_search" != "true" ]; then
        return 0
    fi

    meilisearch_key_path="$REPO_ROOT/dev/meilisearch_master_key"
    meilisearch_key_file_value="$(cat "$meilisearch_key_path" 2>/dev/null || true)"

    if [ "$meilisearch_key_file_value" != "$api_key" ]; then
        printf '%s' "$api_key" > "$meilisearch_key_path"
        chmod 600 "$meilisearch_key_path" 2>/dev/null || true
    fi

    mkdir -p "$REPO_ROOT/dev/data/meilisearch"
}

ensure_core_secrets() {
    config_path="${FLUXER_CONFIG:-$REPO_ROOT/config/config.json}"

    info "Checking development secret configuration..."

    if [ ! -f "$config_path" ]; then
        warn "Config file not found, skipping secret generation"
        return 0
    fi

    current_s3_access_key_id=$(jq -r '.s3.access_key_id // empty' "$config_path" 2>/dev/null || true)
    current_s3_secret_access_key=$(jq -r '.s3.secret_access_key // empty' "$config_path" 2>/dev/null || true)
    current_media_proxy_secret_key=$(jq -r '.services.media_proxy.secret_key // empty' "$config_path" 2>/dev/null || true)
    current_admin_secret_key_base=$(jq -r '.services.admin.secret_key_base // empty' "$config_path" 2>/dev/null || true)
    current_admin_oauth_client_secret=$(jq -r '.services.admin.oauth_client_secret // empty' "$config_path" 2>/dev/null || true)
    current_marketing_secret_key_base=$(jq -r '.services.marketing.secret_key_base // empty' "$config_path" 2>/dev/null || true)
    current_gateway_admin_reload_secret=$(jq -r '.services.gateway.admin_reload_secret // empty' "$config_path" 2>/dev/null || true)
    current_queue_secret=$(jq -r '.services.queue.secret // empty' "$config_path" 2>/dev/null || true)
    current_meilisearch_api_key=$(jq -r '.integrations.search.api_key // empty' "$config_path" 2>/dev/null || true)
    has_deprecated_gateway_config=$(jq -r '.gateway != null' "$config_path" 2>/dev/null || echo "false")
    current_sudo_mode_secret=$(jq -r '.auth.sudo_mode_secret // empty' "$config_path" 2>/dev/null || true)
    current_connection_initiation_secret=$(jq -r '.auth.connection_initiation_secret // empty' "$config_path" 2>/dev/null || true)
    current_smtp_password=$(jq -r '.integrations.email.smtp.password // empty' "$config_path" 2>/dev/null || true)
    current_voice_api_key=$(jq -r '.integrations.voice.api_key // empty' "$config_path" 2>/dev/null || true)
    current_voice_api_secret=$(jq -r '.integrations.voice.api_secret // empty' "$config_path" 2>/dev/null || true)
    has_smtp=$(jq -r '.integrations.email.smtp != null' "$config_path" 2>/dev/null || echo "false")
    has_marketing=$(jq -r '.services.marketing != null' "$config_path" 2>/dev/null || echo "false")
    has_queue=$(jq -r '.services.queue != null' "$config_path" 2>/dev/null || echo "false")
    has_search=$(jq -r '.integrations.search != null' "$config_path" 2>/dev/null || echo "false")
    has_voice=$(jq -r '.integrations.voice != null' "$config_path" 2>/dev/null || echo "false")

    seeded_s3_access_key_id=$(seed_hex_secret "$current_s3_access_key_id" 16 "dev-access-key" "fluxer-dev-access-key")
    seeded_s3_secret_access_key=$(seed_hex_secret "$current_s3_secret_access_key" 32 "dev-secret-key" "fluxer-dev-secret-key")
    seeded_media_proxy_secret_key=$(seed_hex_secret "$current_media_proxy_secret_key" 32 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
    seeded_admin_secret_key_base=$(seed_hex_secret "$current_admin_secret_key_base" 32 "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789")
    seeded_admin_oauth_client_secret=$(seed_hex_secret "$current_admin_oauth_client_secret" 32 "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210")
    seeded_marketing_secret_key_base="$current_marketing_secret_key_base"
    if [ "$has_marketing" = "true" ]; then
        seeded_marketing_secret_key_base=$(seed_hex_secret "$current_marketing_secret_key_base" 32 "marketing0123456789abcdef0123456789abcdef0123456789abcdef01234567")
    fi
    seeded_gateway_admin_reload_secret=$(seed_hex_secret "$current_gateway_admin_reload_secret" 32 "deadbeef0123456789abcdef0123456789abcdef0123456789abcdef01234567")
    seeded_queue_secret="$current_queue_secret"
    if [ "$has_queue" = "true" ]; then
        seeded_queue_secret=$(seed_hex_secret "$current_queue_secret" 32 "queue00123456789abcdef0123456789abcdef0123456789abcdef0123456789")
    fi
    seeded_meilisearch_api_key="$current_meilisearch_api_key"
    if [ "$has_search" = "true" ]; then
        seeded_meilisearch_api_key=$(seed_hex_secret "$current_meilisearch_api_key" 32 "meilisearch0123456789abcdef0123456789abcdef0123456789abcdef012345")
    fi
    seeded_sudo_mode_secret=$(seed_hex_secret "$current_sudo_mode_secret" 32 "c0ffee000123456789abcdef0123456789abcdef0123456789abcdef01234567")
    seeded_connection_initiation_secret=$(seed_hex_secret "$current_connection_initiation_secret" 32 "d0d0ca000123456789abcdef0123456789abcdef0123456789abcdef01234567")
    seeded_smtp_password="$current_smtp_password"
    if [ "$has_smtp" = "true" ]; then
        seeded_smtp_password=$(seed_hex_secret "$current_smtp_password" 16 "dev")
    fi
    seeded_voice_api_key="$current_voice_api_key"
    seeded_voice_api_secret="$current_voice_api_secret"
    if [ "$has_voice" = "true" ]; then
        seeded_voice_api_key=$(seed_hex_secret "$current_voice_api_key" 32 "5VCKLGhj3Yz0q2GIBnuumpOP1GlSTSw5mLPZDvZNIvQpiocQXDQIwTS5CRrnOhe7" "devkey")
        seeded_voice_api_secret=$(seed_hex_secret "$current_voice_api_secret" 32 "devsecret")
    fi

    has_changes=false
    if [ "$seeded_s3_access_key_id" != "$current_s3_access_key_id" ]; then has_changes=true; fi
    if [ "$seeded_s3_secret_access_key" != "$current_s3_secret_access_key" ]; then has_changes=true; fi
    if [ "$seeded_media_proxy_secret_key" != "$current_media_proxy_secret_key" ]; then has_changes=true; fi
    if [ "$seeded_admin_secret_key_base" != "$current_admin_secret_key_base" ]; then has_changes=true; fi
    if [ "$seeded_admin_oauth_client_secret" != "$current_admin_oauth_client_secret" ]; then has_changes=true; fi
    if [ "$has_marketing" = "true" ] && [ "$seeded_marketing_secret_key_base" != "$current_marketing_secret_key_base" ]; then has_changes=true; fi
    if [ "$seeded_gateway_admin_reload_secret" != "$current_gateway_admin_reload_secret" ]; then has_changes=true; fi
    if [ "$has_queue" = "true" ] && [ "$seeded_queue_secret" != "$current_queue_secret" ]; then has_changes=true; fi
    if [ "$has_search" = "true" ] && [ "$seeded_meilisearch_api_key" != "$current_meilisearch_api_key" ]; then has_changes=true; fi
    if [ "$seeded_sudo_mode_secret" != "$current_sudo_mode_secret" ]; then has_changes=true; fi
    if [ "$seeded_connection_initiation_secret" != "$current_connection_initiation_secret" ]; then has_changes=true; fi
    if [ "$has_smtp" = "true" ] && [ "$seeded_smtp_password" != "$current_smtp_password" ]; then has_changes=true; fi
    if [ "$has_voice" = "true" ] && [ "$seeded_voice_api_key" != "$current_voice_api_key" ]; then has_changes=true; fi
    if [ "$has_voice" = "true" ] && [ "$seeded_voice_api_secret" != "$current_voice_api_secret" ]; then has_changes=true; fi
    if [ "$has_deprecated_gateway_config" = "true" ]; then has_changes=true; fi

    if [ "$has_changes" = false ]; then
        info "Development secrets already configured"
        sync_meilisearch_key_file "$has_search" "$seeded_meilisearch_api_key"
        return 0
    fi

    # Development secrets are generated locally during bootstrap to avoid
    # committing placeholder values that look like real credentials.
    info "Generating local development secrets..."

    temp_config="$config_path.tmp"
    jq \
        --arg s3_access_key_id "$seeded_s3_access_key_id" \
        --arg s3_secret_access_key "$seeded_s3_secret_access_key" \
        --arg media_proxy_secret_key "$seeded_media_proxy_secret_key" \
        --arg admin_secret_key_base "$seeded_admin_secret_key_base" \
        --arg admin_oauth_client_secret "$seeded_admin_oauth_client_secret" \
        --arg marketing_secret_key_base "$seeded_marketing_secret_key_base" \
        --arg gateway_admin_reload_secret "$seeded_gateway_admin_reload_secret" \
        --arg queue_secret "$seeded_queue_secret" \
        --arg meilisearch_api_key "$seeded_meilisearch_api_key" \
        --arg sudo_mode_secret "$seeded_sudo_mode_secret" \
        --arg connection_initiation_secret "$seeded_connection_initiation_secret" \
        --arg smtp_password "$seeded_smtp_password" \
        --arg voice_api_key "$seeded_voice_api_key" \
        --arg voice_api_secret "$seeded_voice_api_secret" \
        '.s3.access_key_id = $s3_access_key_id |
         .s3.secret_access_key = $s3_secret_access_key |
         .services.media_proxy.secret_key = $media_proxy_secret_key |
         .services.admin.secret_key_base = $admin_secret_key_base |
         .services.admin.oauth_client_secret = $admin_oauth_client_secret |
         (if .services.marketing != null then .services.marketing.secret_key_base = $marketing_secret_key_base else . end) |
         .services.gateway.admin_reload_secret = $gateway_admin_reload_secret |
         (if .services.queue != null then .services.queue.secret = $queue_secret else . end) |
         (if .integrations.search != null then .integrations.search.api_key = $meilisearch_api_key else . end) |
         del(.gateway) |
         .auth.sudo_mode_secret = $sudo_mode_secret |
         .auth.connection_initiation_secret = $connection_initiation_secret |
         (if .integrations.email.smtp != null then .integrations.email.smtp.password = $smtp_password else . end) |
         (if .integrations.voice != null then .integrations.voice.api_key = $voice_api_key | .integrations.voice.api_secret = $voice_api_secret else . end)' \
        "$config_path" > "$temp_config"

    if [ $? -eq 0 ]; then
        mv "$temp_config" "$config_path"
        info "Development secrets configured"

        sync_meilisearch_key_file "$has_search" "$seeded_meilisearch_api_key"
    else
        error "Failed to update config.json with development secrets"
        rm -f "$temp_config"
        return 1
    fi
}

validate_vapid_keys() {
    public_key="$1"
    private_key="$2"

    node - "$public_key" "$private_key" >/dev/null 2>&1 <<'NODE'
const [publicKey, privateKey] = process.argv.slice(2);

try {
	if (!publicKey || !privateKey) {
		process.exit(1);
	}

	const publicRaw = Buffer.from(publicKey, 'base64url');
	const privateRaw = Buffer.from(privateKey, 'base64url');
	if (publicRaw.length !== 65 || publicRaw[0] !== 0x04 || privateRaw.length !== 32) {
		process.exit(1);
	}

	process.exit(0);
} catch (_error) {
	process.exit(1);
}
NODE
}

generate_vapid_keypair() {
    node - <<'NODE'
const {generateKeyPairSync} = require('node:crypto');

const {privateKey, publicKey} = generateKeyPairSync('ec', {namedCurve: 'prime256v1'});
const publicJwk = publicKey.export({format: 'jwk'});
const privateJwk = privateKey.export({format: 'jwk'});
const publicRaw = Buffer.concat([
	Buffer.from([0x04]),
	Buffer.from(publicJwk.x, 'base64url'),
	Buffer.from(publicJwk.y, 'base64url'),
]);

process.stdout.write(
	JSON.stringify({
		public_key: publicRaw.toString('base64url'),
		private_key: privateJwk.d,
	})
);
NODE
}

ensure_vapid_keys() {
    config_path="${FLUXER_CONFIG:-$REPO_ROOT/config/config.json}"

    info "Checking VAPID configuration..."

    if [ ! -f "$config_path" ]; then
        warn "Config file not found, skipping VAPID key generation"
        return 0
    fi

    vapid_public_key=$(jq -r '.auth.vapid.public_key // empty' "$config_path" 2>/dev/null || true)
    vapid_private_key=$(jq -r '.auth.vapid.private_key // empty' "$config_path" 2>/dev/null || true)

    if validate_vapid_keys "$vapid_public_key" "$vapid_private_key"; then
        info "VAPID keys already configured"
        return 0
    fi

    # Development VAPID keys are generated locally by bootstrap, not issued by
    # an external provider. There is no external renewal process – if keys are
    # missing or invalid we generate a fresh pair here.
    info "Generating development-only VAPID keypair..."
    vapid_keys_json=$(generate_vapid_keypair)
    generated_public_key=$(printf '%s' "$vapid_keys_json" | jq -r '.public_key // empty')
    generated_private_key=$(printf '%s' "$vapid_keys_json" | jq -r '.private_key // empty')

    if ! validate_vapid_keys "$generated_public_key" "$generated_private_key"; then
        error "Failed to generate valid VAPID keys"
        return 1
    fi

    temp_config="$config_path.tmp"
    jq --arg vapid_public_key "$generated_public_key" \
       --arg vapid_private_key "$generated_private_key" \
       '.auth.vapid.public_key = $vapid_public_key |
        .auth.vapid.private_key = $vapid_private_key' "$config_path" > "$temp_config"

    if [ $? -eq 0 ]; then
        mv "$temp_config" "$config_path"
        info "VAPID keys configured for development"
    else
        error "Failed to update config.json with VAPID keys"
        rm -f "$temp_config"
        return 1
    fi
}

generate_bluesky_oauth_keys() {
    config_path="${FLUXER_CONFIG:-$REPO_ROOT/config/config.json}"
    key_path="$REPO_ROOT/dev/bluesky_oauth_key.pem"

    info "Checking Bluesky OAuth configuration..."

    if [ ! -f "$config_path" ]; then
        warn "Config file not found, skipping Bluesky OAuth key generation"
        return 0
    fi

    keys_length=$(jq -r '.auth.bluesky.keys | length' "$config_path" 2>/dev/null || echo "0")

    if [ "$keys_length" != "0" ]; then
        all_keys_exist=true
        for key_file in $(jq -r '.auth.bluesky.keys[].private_key_path // empty' "$config_path" 2>/dev/null); do
            if [ ! -f "$key_file" ]; then
                warn "Configured key file missing: $key_file"
                all_keys_exist=false
                continue
            fi
            if ! openssl pkey -in "$key_file" -text -noout 2>/dev/null | grep -Eq "prime256v1|secp256r1"; then
                warn "Configured key file is not an ES256 (P-256) key: $key_file"
                all_keys_exist=false
            elif ! openssl pkcs8 -topk8 -nocrypt -in "$key_file" -out /dev/null >/dev/null 2>&1; then
                warn "Configured key file is not PKCS#8 encoded: $key_file"
                all_keys_exist=false
            fi
        done
        if [ "$all_keys_exist" = true ]; then
            info "Bluesky OAuth keys already configured"
            return 0
        fi
        info "Regenerating Bluesky OAuth key files..."
    fi

    info "Generating Bluesky OAuth ES256 (P-256) keypair..."

    mkdir -p "$REPO_ROOT/dev"

    if ! openssl genpkey -algorithm EC -pkeyopt ec_paramgen_curve:prime256v1 -out "$key_path" >/dev/null 2>&1; then
        error "Failed to generate ES256 key for Bluesky OAuth"
        return 1
    fi

    info "Generated ES256 key at: $key_path"

    info "Updating config.json with Bluesky OAuth key..."

    temp_config="$config_path.tmp"

    jq --arg kid "dev-key-1" \
       --arg key_path "$key_path" \
       '.auth.bluesky.enabled = true |
        .auth.bluesky.logo_uri = "https://fluxerstatic.com/web/apple-touch-icon.png" |
        .auth.bluesky.tos_uri = "https://fluxer.app/terms" |
        .auth.bluesky.policy_uri = "https://fluxer.app/privacy" |
        .auth.bluesky.token_endpoint_auth_signing_alg = "ES256" |
        .auth.bluesky.keys = [{
            "kid": $kid,
            "private_key_path": $key_path
        }]' "$config_path" > "$temp_config"

    if [ $? -eq 0 ]; then
        mv "$temp_config" "$config_path"
        info "Bluesky OAuth configured with dev key (enabled: true)"
    else
        error "Failed to update config.json with Bluesky OAuth key"
        rm -f "$temp_config"
        return 1
    fi
}

generate_livekit_config() {
    config_path="${FLUXER_CONFIG:-$REPO_ROOT/config/config.json}"
    livekit_config="$REPO_ROOT/dev/livekit.yaml"
    template="$REPO_ROOT/dev/livekit.template.yaml"

    info "Generating LiveKit configuration..."

    api_key=
    api_secret=
    webhook_url=
    base_domain=

    api_key=$(jq -r '.integrations.voice.api_key // empty' "$config_path" 2>/dev/null || true)
    api_secret=$(jq -r '.integrations.voice.api_secret // empty' "$config_path" 2>/dev/null || true)
    webhook_url=$(jq -r '.integrations.voice.webhook_url // empty' "$config_path" 2>/dev/null || true)
    base_domain=$(jq -r '.domain.base_domain // empty' "$config_path" 2>/dev/null || true)

    api_key="${api_key:-devkey}"
    api_secret="${api_secret:-devsecret}"
    webhook_url="${webhook_url:-http://localhost:49319/api/webhooks/livekit}"
    base_domain="${base_domain:-localhost}"

    if [ "$base_domain" = "localhost" ] || [ "$base_domain" = "127.0.0.1" ]; then
        node_ip="127.0.0.1"
        turn_domain="localhost"
    else
        turn_domain="$base_domain"
        node_ip=$(curl -4 -sf --max-time 5 https://ifconfig.me 2>/dev/null || true)
        if [ -z "$node_ip" ]; then
            node_ip=$(curl -4 -sf --max-time 5 https://api.ipify.org 2>/dev/null || true)
        fi
        if [ -z "$node_ip" ]; then
            warn "Could not resolve public IP for LiveKit. Voice may not work for remote clients."
            warn "Set rtc.node_ip manually in dev/livekit.yaml to your server's public IP."
            node_ip="127.0.0.1"
        else
            info "Resolved public IP for LiveKit: $node_ip"
        fi
    fi

    sed -e "s|{{API_KEY}}|$api_key|g" \
        -e "s|{{API_SECRET}}|$api_secret|g" \
        -e "s|{{WEBHOOK_URL}}|$webhook_url|g" \
        -e "s|{{NODE_IP}}|$node_ip|g" \
        -e "s|{{TURN_DOMAIN}}|$turn_domain|g" \
        "$template" > "$livekit_config"

    info "LiveKit config generated at: $livekit_config (domain: $base_domain, node_ip: $node_ip)"
}

setup_model_symlink() {
    source="$REPO_ROOT/fluxer_media_proxy/data/model.onnx"
    target_dir="$REPO_ROOT/fluxer_server/data"
    target="$target_dir/model.onnx"

    info "Setting up ONNX model symlink..."

    if [ ! -f "$source" ]; then
        warn "Source model not found: $source"
        warn "NSFW detection will not work until model.onnx is provided"
        return 0
    fi

    mkdir -p "$target_dir"

    if ls -ld "$target" 2>/dev/null | grep -q '^l'; then
        info "Model symlink already exists"
    elif [ -f "$target" ]; then
        source_size=$(stat -f%z "$source" 2>/dev/null || stat -c%s "$source" 2>/dev/null)
        target_size=$(stat -f%z "$target" 2>/dev/null || stat -c%s "$target" 2>/dev/null)
        if [ "$target_size" -lt 1000 ]; then
            info "Replacing empty/corrupt model file with symlink"
            rm -f "$target"
            ln -s "$source" "$target"
        else
            info "Model file already exists (not a symlink)"
        fi
    else
        ln -s "$source" "$target"
        info "Created model symlink: $target -> $source"
    fi
}

main() {
    echo ""
    info "Fluxer Development Bootstrap"
    echo ""

    prepare_log_dir
    check_config
    ensure_core_secrets
    ensure_vapid_keys
    generate_bluesky_oauth_keys
    generate_livekit_config
    setup_model_symlink

    echo ""
    info "Bootstrap complete"
    echo ""
}

main "$@"
