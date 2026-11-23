#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

if [ "$#" -ne 1 ] || [ "$1" != foreground ]; then
    printf 'usage: fluxer_gateway foreground\n' >&2
    exit 64
fi

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
erts=("$root"/erts-*/bin/erlexec)
if [ "${#erts[@]}" -ne 1 ] || [ ! -x "${erts[0]}" ]; then
    printf 'fluxer_gateway: expected exactly one bundled ERTS in %s\n' "$root" >&2
    exit 1
fi

schedulers="${FLUXER_ERLANG_SCHEDULERS:-2}"
dirty_cpu="${FLUXER_ERLANG_DIRTY_CPU_SCHEDULERS:-1}"
dirty_io="${FLUXER_ERLANG_DIRTY_IO_SCHEDULERS:-2}"
export ROOTDIR="$root" BINDIR="${erts[0]%/*}" EMU=beam PROGNAME=fluxer_gateway
export ERL_CRASH_DUMP="${ERL_CRASH_DUMP:-${TMPDIR:-/tmp}/erl_crash.dump}"

exec "$BINDIR/erlexec" \
    -args_file "$root/config/vm.args" \
    -config "$root/config/sys.config" \
    +S "$schedulers:$schedulers" \
    +SDcpu "$dirty_cpu:$dirty_cpu" \
    +SDio "$dirty_io" \
    -pa "$root"/packages/*/ebin \
    -eval 'fluxer_gateway:main()' \
    -noshell \
    -extra
