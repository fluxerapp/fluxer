#!/usr/bin/env sh

# SPDX-License-Identifier: AGPL-3.0-or-later

set -eu

root="$(CDPATH='' cd "$(dirname "$0")/.." && pwd -P)"

"$root/scripts/nif.sh"
cd "$root"
gleam export erlang-shipment
