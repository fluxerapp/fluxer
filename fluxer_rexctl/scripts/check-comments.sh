#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
exec go run ./internal/lint/checkcomments .
