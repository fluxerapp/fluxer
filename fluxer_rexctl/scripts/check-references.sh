#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
set -eu
cd "$(dirname "$0")/.."
set -- ../fluxer_docs/src/content/docs/operator/rexctl.mdx \
	../fluxer_docs/src/content/docs/operator/environment.mdx \
	internal/stack/files/.env.example
go run . docs --out "$1" --env-page "$2" --env-example "$3"
changed="$(git status --porcelain -- "$@")"
if [ -n "${changed}" ]; then
	echo "${changed}" >&2
	git diff -- "$@" >&2
	exit 1
fi
