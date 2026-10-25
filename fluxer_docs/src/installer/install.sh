#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later

set -eu

resolver=${REXCTL_RELEASE_URL:-https://fluxer.dev/rexctl/release.json}

if [ -t 2 ] && [ -z "${NO_COLOR:-}" ] && [ "${TERM:-}" != dumb ]; then
	glyph_ok='✓'
	glyph_warn='!'
	glyph_fail='✗'
else
	glyph_ok='ok'
	glyph_warn='!'
	glyph_fail='x'
fi

fail() {
	printf '%s %s\n' "$glyph_fail" "$1" >&2
	if [ -n "${2:-}" ]; then
		printf '  %s\n' "$2" >&2
	fi
	exit "${3:-1}"
}

need() {
	command -v "$1" >/dev/null 2>&1 || fail "This script needs $1, and this machine doesn't have it." "Install $1, then run this again."
}

fetch() {
	if curl -fsSL --proto '=https' --tlsv1.2 --retry 3 --retry-delay 2 -o "$2" "$1" 2>/dev/null; then
		return 0
	fi
	curl -4 -fsSL --proto '=https' --tlsv1.2 -o "$2" "$1"
}

sha256_of() {
	if command -v sha256sum >/dev/null 2>&1; then
		sha256sum "$1" | cut -d ' ' -f 1
	else
		shasum -a 256 "$1" | cut -d ' ' -f 1
	fi
}

quote() {
	printf "'%s'" "$(printf '%s' "$1" | sed "s/'/'\\\\''/g")"
}

args=''
add() {
	for arg in "$@"; do
		args="$args $(quote "$arg")"
	done
}

has_instance() {
	[ -f "$1/.rexctl/state.json" ] || [ -f "$1/.env" ]
}

map_legacy_flags() {
	mode=install
	dir=''
	domain=''
	engine=''
	tls=''
	bind=''
	dry_run=0
	yes=0
	no_backup=0
	no_start=0
	help=0
	while [ $# -gt 0 ]; do
		case $1 in
			--update) mode=upgrade ;;
			--check) mode=check ;;
			--rollback) mode=rollback ;;
			--dry-run) dry_run=1 ;;
			--non-interactive) yes=1 ;;
			--skip-backup-accept-data-loss) no_backup=1 ;;
			--no-start) no_start=1 ;;
			--help | -h) help=1 ;;
			--dir | --domain | --engine | --tls | --edge-bind | --email | --ref | --image-tag | --backup-dir | --keep-backups)
				if [ $# -lt 2 ]; then
					fail "$1 needs a value." "" 2
				fi
				case $1 in
					--dir) dir=$2 ;;
					--domain) domain=$2 ;;
					--engine) engine=$2 ;;
					--tls) tls=$2 ;;
					--edge-bind) bind=$2 ;;
				esac
				shift
				;;
		esac
		shift
	done

	if [ -z "$dir" ] && [ "$mode" != install ]; then
		if [ -n "$self_dir" ] && has_instance "$self_dir"; then
			dir=$self_dir
		elif has_instance "$PWD"; then
			dir=$PWD
		fi
	fi

	if [ "$dry_run" -eq 1 ] || [ "$mode" = check ]; then
		trial=1
	fi
	case $mode in
		install)
			add install
			[ -z "$domain" ] || add --domain "$domain"
			[ -z "$engine" ] || add --engine "$engine"
			if [ "$tls" = proxy ]; then
				add --behind-proxy
				[ -z "$bind" ] || add --bind "$bind"
			fi
			[ "$no_start" -eq 0 ] || add --no-start
			[ "$dry_run" -eq 0 ] || add --dry-run
			;;
		upgrade)
			add upgrade
			[ "$no_backup" -eq 0 ] || add --no-backup
			[ "$dry_run" -eq 0 ] || add --dry-run
			;;
		check)
			add upgrade --check
			;;
		rollback)
			add rollback
			[ "$dry_run" -eq 0 ] || add --dry-run
			;;
	esac
	[ -z "$dir" ] || add --dir "$dir"
	[ "$yes" -eq 0 ] || add --yes
	[ "$help" -eq 0 ] || add --help
}

self_dir=''
case $0 in
	*/install.sh) self_dir=$(cd "$(dirname "$0")" 2>/dev/null && pwd) || self_dir='' ;;
esac

trial=0
case ${1:-} in
	'' | -*) map_legacy_flags "$@" ;;
	*)
		add "$@"
		for arg in "$@"; do
			case $arg in
				--dry-run | --check) trial=1 ;;
			esac
		done
		;;
esac

case $(uname -s) in
	Linux) os=linux ;;
	Darwin) os=darwin ;;
	*) fail "This script runs on Linux and macOS." "On Windows, run install.ps1 in PowerShell instead." ;;
esac

case $(uname -m) in
	x86_64 | amd64) arch=amd64 ;;
	aarch64 | arm64) arch=arm64 ;;
	*) fail "Fluxer runs on 64-bit Intel, AMD and Arm machines, and this one is $(uname -m)." ;;
esac

need curl
need tar
if ! command -v sha256sum >/dev/null 2>&1; then
	need shasum
fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
trap 'exit 130' INT HUP TERM

if ! fetch "$resolver?format=sh&os=$os&arch=$arch" "$tmp/release"; then
	fail "Couldn't find the latest rexctl release." "Check that this machine can reach fluxer.dev, then run this again."
fi

version=''
url=''
sum=''
while read -r key value; do
	case $key in
		version) version=$value ;;
		url) url=$value ;;
		sha256) sum=$value ;;
	esac
done <"$tmp/release"

case $sum in
	'' | *[!0-9a-f]*) fail "The release information from fluxer.dev doesn't look right." "Run this again in a few minutes." ;;
esac
case $url in
	https://*) ;;
	*) fail "The release information from fluxer.dev doesn't look right." "Run this again in a few minutes." ;;
esac

if ! fetch "$url" "$tmp/rexctl.tar.gz"; then
	fail "Couldn't download rexctl $version." "Check that this machine can reach github.com, then run this again."
fi
if [ "$(sha256_of "$tmp/rexctl.tar.gz")" != "$sum" ]; then
	fail "The rexctl download doesn't match its checksum." "Run this again. If it keeps happening, something between you and GitHub is changing the file."
fi
mkdir "$tmp/out"
tar -xzf "$tmp/rexctl.tar.gz" -C "$tmp/out"
if [ ! -f "$tmp/out/rexctl" ]; then
	fail "The rexctl download doesn't contain rexctl." "Run this again in a few minutes."
fi

chmod 755 "$tmp/out/rexctl"
eval "set -- $args"
if [ "$trial" -eq 1 ]; then
	trap - EXIT
	status=0
	if [ ! -t 0 ] && (: </dev/tty) 2>/dev/null; then
		"$tmp/out/rexctl" "$@" </dev/tty || status=$?
	else
		"$tmp/out/rexctl" "$@" || status=$?
	fi
	rm -rf "$tmp"
	exit "$status"
fi

if [ "$(id -u)" -eq 0 ]; then
	bin_dir=/usr/local/bin
else
	bin_dir=$HOME/.local/bin
fi
mkdir -p "$bin_dir"
mv -f "$tmp/out/rexctl" "$bin_dir/rexctl.new"
mv -f "$bin_dir/rexctl.new" "$bin_dir/rexctl"
printf '%s Installed rexctl %s\n' "$glyph_ok" "$version" >&2

case ":$PATH:" in
	*":$bin_dir:"*) ;;
	*)
		case $bin_dir in
			"$HOME"/*)
				shown="~${bin_dir#"$HOME"}"
				path_dir="\$HOME${bin_dir#"$HOME"}"
				;;
			*)
				shown=$bin_dir
				path_dir=$bin_dir
				;;
		esac
		printf '%s rexctl is in %s, which isn'"'"'t on your PATH yet.\n' "$glyph_warn" "$shown" >&2
		printf '  Run it as %s/rexctl for now. To run rexctl by name in this terminal, run:\n' "$shown" >&2
		printf '  export PATH="%s:%s"\n' "$path_dir" "\$PATH" >&2
		printf '  On most Linux distributions, rexctl works by name in a new terminal.\n' >&2
		;;
esac

rm -rf "$tmp"
trap - EXIT INT HUP TERM

if [ ! -t 0 ] && (: </dev/tty) 2>/dev/null; then
	exec "$bin_dir/rexctl" "$@" </dev/tty
fi
exec "$bin_dir/rexctl" "$@"
