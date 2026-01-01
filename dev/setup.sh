#!/usr/bin/env bash

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

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

check_command() {
    if command -v "$1" &> /dev/null; then
        echo -e "${GREEN}[OK]${NC} $1 is installed"
        return 0
    else
        echo -e "${RED}[MISSING]${NC} $1 is not installed"
        return 1
    fi
}

check_node_version() {
    if command -v node &> /dev/null; then
        NODE_VERSION=$(node -v | sed 's/v//' | cut -d. -f1)
        if [ "$NODE_VERSION" -ge 20 ]; then
            echo -e "${GREEN}[OK]${NC} Node.js $(node -v) is installed"
            return 0
        else
            echo -e "${YELLOW}[WARN]${NC} Node.js $(node -v) is installed, but v20+ is recommended"
            return 0
        fi
    else
        echo -e "${RED}[MISSING]${NC} Node.js is not installed"
        return 1
    fi
}

echo "=== Fluxer Development Setup ==="
echo ""

echo "Checking prerequisites..."
echo ""

MISSING=0

check_node_version || MISSING=1
check_command pnpm || MISSING=1
check_command docker || MISSING=1
check_command rustc || echo -e "${YELLOW}[OPTIONAL]${NC} Rust is not installed (needed for fluxer_app WASM modules)"
check_command wasm-pack || echo -e "${YELLOW}[OPTIONAL]${NC} wasm-pack is not installed (needed for fluxer_app WASM modules)"
check_command go || echo -e "${YELLOW}[OPTIONAL]${NC} Go is not installed (needed for fluxer_geoip)"

echo ""

if [ "$MISSING" -eq 1 ]; then
    echo -e "${RED}Some required dependencies are missing. Please install them before continuing.${NC}"
    exit 1
fi

echo "Creating Docker network if needed..."
if docker network inspect fluxer-shared &> /dev/null; then
    echo -e "${GREEN}[OK]${NC} Docker network 'fluxer-shared' already exists"
else
    docker network create fluxer-shared
    echo -e "${GREEN}[OK]${NC} Created Docker network 'fluxer-shared'"
fi

echo ""

if [ ! -f "$SCRIPT_DIR/.env" ]; then
    echo "Creating .env from .env.example..."
    cp "$SCRIPT_DIR/.env.example" "$SCRIPT_DIR/.env"
    echo -e "${GREEN}[OK]${NC} Created .env file"
else
    echo -e "${GREEN}[OK]${NC} .env file already exists"
fi

mkdir -p "$SCRIPT_DIR/geoip_data"
if [ ! -f "$SCRIPT_DIR/geoip_data/ipinfo_lite.mmdb" ]; then
    echo -e "${YELLOW}[INFO]${NC} GeoIP database not found."
    echo "       Set IPINFO_TOKEN in .env and run the geoip service to download it,"
    echo "       or manually download ipinfo_lite.mmdb to dev/geoip_data/"
else
    echo -e "${GREEN}[OK]${NC} GeoIP database exists"
fi

if [ ! -f "$SCRIPT_DIR/livekit.yaml" ]; then
    echo "Creating default livekit.yaml..."
    cat > "$SCRIPT_DIR/livekit.yaml" << 'EOF'
port: 7880

redis:
  address: 'redis:6379'
  db: 0

keys:
  'e1dG953yAoJPIsK1dzfTWAKMNE9gmnPL': 'rCtIICXHtAwSAJ4glb11jARcXCCgMTGvvTKLIlpD0pEoANLgjCNPD1Ysm8uWhQTB'

rtc:
  tcp_port: 7881

webhook:
  api_key: 'e1dG953yAoJPIsK1dzfTWAKMNE9gmnPL'
  urls:
    - 'http://api:8080/webhooks/livekit'

room:
  auto_create: true
  max_participants: 100
  empty_timeout: 300

development: true
EOF
    echo -e "${GREEN}[OK]${NC} Created livekit.yaml"
else
    echo -e "${GREEN}[OK]${NC} livekit.yaml already exists"
fi

echo ""
echo "=== Setup Complete ==="
echo ""
echo "Next steps:"
echo ""
echo "1. Start data stores:"
echo "   docker compose -f compose.data.yaml up -d"
echo ""
echo "2. Start app services:"
echo "   docker compose up -d api worker media gateway admin marketing docs geoip metrics caddy"
echo ""
echo "3. Run the frontend on your host machine:"
echo "   cd ../fluxer_app && pnpm install && pnpm dev"
echo ""
echo "4. Access the app at: http://localhost:8088"
echo ""
echo "Optional: Start Cloudflare tunnel:"
echo "   docker compose up -d cloudflared"
echo ""