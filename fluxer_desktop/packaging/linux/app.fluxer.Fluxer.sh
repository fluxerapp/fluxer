#!/bin/sh

export TMPDIR="${XDG_RUNTIME_DIR}/app/${FLATPAK_ID}"
export FLUXER_FLATPAK_COMMAND="${FLATPAK_ID}"

exec zypak-wrapper "/app/fluxer/fluxer" "$@"
