#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

[ "$#" -eq 1 ] || { echo "usage: image-check.sh IMAGE" >&2; exit 2; }
ref="$1"

dir="$(mktemp -d)"
trap 'rm -rf "$dir"' EXIT
for part in Manifest SBOM Provenance; do
    docker buildx imagetools inspect "$ref" --format "{{json .$part}}" >"$dir/$part.json"
done

python3 - "$ref" "$dir" <<'EOF'
import json
import os
import sys

ref, dir = sys.argv[1], sys.argv[2]
manifest, sbom, provenance = (json.load(open(os.path.join(dir, f"{part}.json"))) for part in ("Manifest", "SBOM", "Provenance"))
want = ["linux/amd64", "linux/arm64"]
platforms = sorted(
    f'{m["platform"]["os"]}/{m["platform"]["architecture"]}'
    for m in manifest.get("manifests", [])
    if m.get("platform", {}).get("os") not in (None, "unknown")
)
problems = []
if platforms != want:
    problems.append(f"has the platforms {platforms}, expected {want}")
for platform in want:
    if not (sbom or {}).get(platform, {}).get("SPDX"):
        problems.append(f"has no SBOM for {platform}")
    if not (provenance or {}).get(platform, {}).get("SLSA"):
        problems.append(f"has no provenance for {platform}")
if problems:
    sys.exit("\n".join(f"{ref} {p}" for p in problems))
print(f"{ref} has {' and '.join(want)}, each with an SBOM and provenance")
EOF
