#!/bin/sh

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

set -e

sed -e "s|\${LIVEKIT_DOMAIN}|${LIVEKIT_DOMAIN}|g" \
    -e "s|\${LIVEKIT_DOMAIN_TURN}|${LIVEKIT_DOMAIN_TURN}|g" \
    /etc/caddy.yaml.template > /etc/caddy.yaml

exec caddy run --config /etc/caddy.yaml --adapter yaml "$@"