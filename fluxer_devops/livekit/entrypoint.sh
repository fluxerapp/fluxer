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

sed -e "s|\${NODE_IP}|${NODE_IP}|g" \
    -e "s|\${REDIS_PASSWORD}|${REDIS_PASSWORD}|g" \
    -e "s|\${LIVEKIT_API_KEY}|${LIVEKIT_API_KEY}|g" \
    -e "s|\${LIVEKIT_API_SECRET}|${LIVEKIT_API_SECRET}|g" \
    -e "s|\${LIVEKIT_WEBHOOK_URL}|${LIVEKIT_WEBHOOK_URL}|g" \
    -e "s|\${LIVEKIT_DOMAIN_TURN}|${LIVEKIT_DOMAIN_TURN}|g" \
    /etc/livekit.yaml.template > /tmp/livekit.yaml

exec /livekit-server --config /tmp/livekit.yaml "$@"