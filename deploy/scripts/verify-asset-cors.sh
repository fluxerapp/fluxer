#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

ENDPOINT="${ASSET_CORS_ENDPOINT:-https://fluxerstatic.com}"
MANIFEST="${ASSET_MANIFEST:-app-dist-output/dist/assets-manifest.txt}"
ORIGIN="${ASSET_CORS_ORIGIN:-https://fluxer.app}"
SAMPLES="${ASSET_CORS_SAMPLES:-5}"
EXPLICIT_KEYS=()

usage() {
	sed -n '3,22p' "$0"
	exit "${1:-2}"
}

while [ $# -gt 0 ]; do
	case "$1" in
	--endpoint)
		ENDPOINT="${2:?--endpoint needs a URL}"
		shift 2
		;;
	--manifest)
		MANIFEST="${2:?--manifest needs a path}"
		shift 2
		;;
	--origin)
		ORIGIN="${2:?--origin needs an origin}"
		shift 2
		;;
	--samples)
		SAMPLES="${2:?--samples needs a count}"
		shift 2
		;;
	--key)
		EXPLICIT_KEYS+=("${2:?--key needs an S3 key}")
		shift 2
		;;
	-h | --help) usage 0 ;;
	*)
		echo "verify-asset-cors: unknown argument '$1'" >&2
		usage 2
		;;
	esac
done

ENDPOINT="${ENDPOINT%/}"

keys=()
if [ "${#EXPLICIT_KEYS[@]}" -gt 0 ]; then
	keys=("${EXPLICIT_KEYS[@]}")
elif [ -f "$MANIFEST" ]; then
	while IFS= read -r key; do
		keys+=("$key")
	done < <(grep -E '\.woff2$' "$MANIFEST" | awk -v n="$SAMPLES" '
		{ lines[NR] = $0 }
		END {
			if (NR == 0) exit
			if (n > NR) n = NR
			for (i = 0; i < n; i++) print lines[int(i * NR / n) + 1]
		}')
else
	cat >&2 <<-EOF
		verify-asset-cors: no asset manifest at '$MANIFEST' and no --key given.

		The manifest is produced by \`build-app-proxy --step generate_asset_manifest\` and uploaded
		by the release workflow as the 'app-proxy-assets-manifest' artifact. Pass --manifest to point
		at a downloaded copy, or --key to check a single face by hand.
	EOF
	exit 2
fi

if [ "${#keys[@]}" -eq 0 ]; then
	echo "verify-asset-cors: no .woff2 keys to check (manifest '$MANIFEST' has none)" >&2
	exit 2
fi

echo "verify-asset-cors: endpoint=$ENDPOINT origin=$ORIGIN keys=${#keys[@]}"

failures=0
for key in "${keys[@]}"; do
	url="$ENDPOINT/$key"
	headers="$(curl --silent --show-error --location --max-time 20 \
		--header "Origin: $ORIGIN" --output /dev/null --dump-header - "$url" 2>&1 || true)"

	status="$(printf '%s\n' "$headers" | awk '/^HTTP\//{code=$2} END{print code+0}')"
	header_value() {
		printf '%s\n' "$headers" |
			tr -d '\r' |
			awk -v want="$1" 'index(tolower($0), want ":") == 1 {sub(/^[^:]*:[ \t]*/, ""); v=$0} END{print v}'
	}
	acao="$(header_value 'access-control-allow-origin')"
	ctype="$(header_value 'content-type')"
	cache="$(header_value 'cache-control')"
	cdn_cache="$(header_value 'cdn-cache-control')"

	problems=()
	[ "$status" = "200" ] || problems+=("status=$status (want 200)")
	if [ -z "$acao" ]; then
		problems+=("no Access-Control-Allow-Origin -- the browser will drop this face")
	elif [ "$acao" != "*" ] && [ "$acao" != "$ORIGIN" ]; then
		problems+=("Access-Control-Allow-Origin='$acao' (want '*' or '$ORIGIN')")
	fi
	case "$ctype" in
	font/woff2*) ;;
	*) problems+=("Content-Type='$ctype' (want font/woff2)") ;;
	esac
	caching_note=""
	case "$cache$cdn_cache" in
	*immutable*) ;;
	*) caching_note=" (note: no 'immutable' token; cache-control='$cache')" ;;
	esac

	if [ "${#problems[@]}" -eq 0 ]; then
		echo "  OK   $key  [$status $ctype acao=$acao]$caching_note"
	else
		failures=$((failures + 1))
		echo "  FAIL $key"
		for problem in "${problems[@]}"; do
			echo "         - $problem"
		done
	fi
done

if [ "$failures" -gt 0 ]; then
	cat >&2 <<-EOF

		verify-asset-cors: $failures of ${#keys[@]} sampled faces are NOT usable from $ORIGIN.

		Most likely cause: the bucket's CORS rule is scoped to the retired 'fonts/' prefix and does
		not cover 'assets/'. Every bundled face fails in production while JS and CSS keep working,
		and the CDN still answers 200, so no server-side alarm fires.

		Fix the bucket rule so it allows GET on 'assets/*' from the app origin (or bucket-wide),
		then re-run this check.
	EOF
	exit 1
fi

echo "verify-asset-cors: all ${#keys[@]} sampled faces are usable from $ORIGIN"
