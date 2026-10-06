#!/usr/bin/env sh

# SPDX-License-Identifier: AGPL-3.0-or-later

set -eu

command="${1:-}"
case "$command" in
eval | console | remote_console | remote | remsh | console_clean | console_boot | daemon | daemon_boot | daemon_attach | escript)
	echo "fluxer_gateway: '$command' is disabled in production; use bounded rpc/runtime tools instead." >&2
	exit 64
	;;
esac

script_dir="$(CDPATH='' cd "$(dirname "$0")" && pwd -P)"
node_name_file="$script_dir/../node_name"
if [ -r "$node_name_file" ]; then
	FLUXER_ERLANG_NODE_NAME="$(cat "$node_name_file")"
	export FLUXER_ERLANG_NODE_NAME
fi
exec "$script_dir/fluxer_gateway.real" "$@"
