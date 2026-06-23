#!/usr/bin/env sh

# SPDX-License-Identifier: AGPL-3.0-or-later

set -eu

: "${FLUXER_ERLANG_NODE_NAME:=fluxer_gateway@127.0.0.1}"
: "${FLUXER_ERLANG_COOKIE:=fluxer_gateway_dev_cookie}"
: "${FLUXER_ERLANG_DIST_PORT:=8081}"

export FLUXER_ERLANG_NODE_NAME
export FLUXER_ERLANG_COOKIE
export FLUXER_ERLANG_DIST_PORT
export RELX_REPLACE_OS_VARS="${RELX_REPLACE_OS_VARS:-true}"

exec /opt/fluxer_gateway/bin/fluxer_gateway foreground
