#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
set -eu
cd "$(dirname "$0")/.."
set -- ../fluxer_docs/src/content/docs/operator/rexctl.mdx \
	../fluxer_docs/src/content/docs/operator/settings.mdx
go run . docs --out "$1" --settings-page "$2"
changed="$(git status --porcelain -- "$@" | grep -v '^. ' || true)"
if [ -n "${changed}" ]; then
	echo "${changed}" >&2
	git diff -- "$@" >&2
	exit 1
fi
