#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Fluxer self-hosting installer and upgrader for Linux and macOS.
#
# Three modes, one file:
#
#   default     Check the host, download the stack files, write .env with every
#               value the stack requires, start the containers.
#   --update    Upgrade an instance that already exists. Record the running
#               images, back up, refresh the stack files, pull, recreate, verify.
#   --rollback  Put the images and the stack files of the last recorded upgrade
#               back.
#
# Why one script and not a separate upgrader: an upgrade needs the host checks,
# the stack download, the readiness poll and the health probe that the install
# already carries. A second script either copies them or drifts from them, and
# the operator has two downloads and two checksums to verify instead of one.
# The modes share one contract, one digest and one set of exit codes.
#
# This file is also the procedure. Every step of the upgrade carries the command
# an operator types to do that step by hand, and the reason the step exists.
#
# Read this file before you run it. The default mode writes .env, which holds
# every secret the instance has. The upgrade generates no secret and rewrites no
# secret. The only line either upgrade mode ever changes in .env is
# FLUXER_IMAGE_TAG, and only during a rollback that moves off a pinned tag.
#
# Rewriting a secret in .env against volumes that already exist is the one thing
# this script could do that an operator cannot undo. A fresh POSTGRES_PASSWORD
# does not open the existing postgres-data volume, and the instance never starts
# again. Every path below is built so that no run can reach that state.
#
# Exit codes:
#   0    success
#   1    usage error
#   2    unmet prerequisite
#   3    refused to overwrite existing state
#   4    a download failed
#   5    secret generation failed
#   6    the stack did not come up
#   7    a backup failed
#   130  interrupted

set -eu

# The C locale keeps the character ranges in the validation patterns byte based.
# Under a UTF-8 locale a-z also matches uppercase and accented letters.
LC_ALL=C
export LC_ALL

FLUXER_RAW_BASE='https://raw.githubusercontent.com/fluxerapp/fluxer'
FLUXER_STACK_PATH='deploy/self-hosting'
FLUXER_MIN_ENGINE='24.0.0'
FLUXER_MIN_COMPOSE='2.20.2'
FLUXER_READY_TIMEOUT=600
FLUXER_READY_INTERVAL=5
FLUXER_VAPID_ATTEMPTS=8
FLUXER_SEC1_HEADER='30770201010420'
FLUXER_INSPECT_FORMAT='{{index .Config.Labels "com.docker.compose.service"}} {{.State.Status}} {{if .State.Health}}{{.State.Health.Status}}{{else}}none{{end}} {{.State.ExitCode}}'

# The image that copies volumes and measures them. Pinned, because an upgrade
# that reaches for a moving tag to take its backup has one more thing that can
# change under it.
FLUXER_HELPER_IMAGE='alpine:3.22'

# Names inside a record directory. A record is one upgrade: what was running,
# what the stack files said, and the backup taken before the images moved.
FLUXER_RECORD_PREFIX='record-'
FLUXER_IMAGES_FILE='images'
FLUXER_TAG_FILE='image-tag'
FLUXER_DUMP_FILE='fluxer.dump'

# Free space demanded before a volume copy, as a percentage of the measured
# volume size. The tarball compresses, so this is generous on purpose. A backup
# that fills the disk it writes to takes the instance down with it.
FLUXER_VOLUME_HEADROOM=110

# The keys .env carries, in the order they are written. The installer iterates
# these two lists, so a key that leaves a list is a key the installer stops
# writing. The docs CI parses the same text and compares it against
# deploy/self-hosting/.env.example.

fluxer_non_secret_keys() {
	cat <<'KEYS'
FLUXER_DOMAIN domain
FLUXER_PUBLIC_SCHEME literal https
FLUXER_PUBLIC_PORT literal 443
FLUXER_VAPID_EMAIL email
FLUXER_IMAGE_TAG image_tag
FLUXER_S3_ACCESS_KEY literal fluxer
LIVEKIT_API_KEY literal fluxer
KEYS
}

fluxer_secret_keys() {
	cat <<'KEYS'
POSTGRES_PASSWORD hex
MEILI_MASTER_KEY hex
FLUXER_S3_SECRET_KEY hex
FLUXER_SUDO_MODE_SECRET hex
FLUXER_CONNECTION_INITIATION_SECRET hex
FLUXER_GATEWAY_RPC_AUTH_TOKEN hex
FLUXER_ERLANG_COOKIE hex
FLUXER_MEDIA_PROXY_SECRET_KEY hex
FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 base64
FLUXER_ADMIN_SECRET_KEY_BASE hex
FLUXER_ADMIN_OAUTH_CLIENT_SECRET hex
LIVEKIT_API_SECRET hex
FLUXER_VAPID_PUBLIC_KEY vapid_public
FLUXER_VAPID_PRIVATE_KEY vapid_private
KEYS
}

fluxer_stack_files() {
	cat <<'FILES'
docker-compose.yml
docker-compose.proxy.yml
tunnel.compose.yml
Caddyfile
.env.example
FILES
}

# The file Compose bind-mounts from the working directory, with the service that
# mounts it.
#
# Compose decides whether to recreate a container by comparing its configuration.
# The contents of a bind-mounted file are not part of that comparison, so a
# changed Caddyfile survives docker compose up -d with the old bytes still loaded
# in the running container. The service has to be restarted by name.
#
# By hand, after a refresh that changed the file:
#   docker compose restart edge
fluxer_mounted_files() {
	cat <<'MOUNTS'
Caddyfile edge
MOUNTS
}

# The volumes an upgrade copies, and the reason the rest are absent.
#
# postgres-data is not here because the dump supersedes it. A custom-format dump
# restores into the major version that wrote it or a newer one, a tarball of the
# data directory restores only into the major that wrote it, and taking both
# doubles the downtime and the disk for a strictly weaker artifact.
#
# valkey-data and nats-data hold queued work, not records. Losing them drops
# scheduled bulk deletions and background jobs, which is degradation rather than
# loss, and the upgrade never removes a volume, so nothing here can lose them.
#
# meilisearch-data rebuilds on the next API start, edge-data is one certificate
# request, edge-config is rewritten by Caddy on every start.
#
# seaweedfs-data is the one that matters. It holds every upload, avatar, report
# and harvest, and nothing else in the stack can recreate any of it.
fluxer_backup_volumes() {
	cat <<'VOLUMES'
seaweedfs-data
VOLUMES
}

fluxer_usage() {
	cat <<'USAGE'
Usage: sh install.sh --domain <host> --email <address> [options]
       sh install.sh --update [options]
       sh install.sh --rollback [options]

Options:
  --domain <host>          Hostname the instance answers on. Prompted when absent.
  --email <address>        Address the operator reads. Prompted when absent.
  --dir <path>             Working directory. Default ~/fluxer.
  --ref <git ref>          Ref the stack files come from. Default: the image tag,
                           and main when that tag is v1 or latest.
  --image-tag <tag>        Image tag the stack runs. Default v1.
  --tls bundled|proxy      Certificate mode. Default bundled.
  --edge-bind <addr:port>  Plain HTTP bind under --tls proxy. Default 127.0.0.1:8080.
  --non-interactive        Never prompt. A missing required value is an error.
  --dry-run                Print the plan. Change nothing.
  --no-start               Write everything and skip docker compose up -d.
  --update                 Upgrade: record, back up, refresh, pull, recreate, verify.
  --rollback               Restore the images and stack files of the last record.
  --backup-dir <path>      Where records go. Default <dir>/backups.
  --no-volume-backup       Take the database dump and skip the uploads copy.
  --skip-backup-accept-data-loss
                           Upgrade with no backup at all. Losable data is lost.
  --allow-root             Permit running as root.
  --help                   Print this text.
USAGE
}

fluxer_say() {
	printf '%s\n' "$1"
}

fluxer_fail() {
	printf '%s\n' "$2" >&2
	exit "$1"
}

fluxer_bad_usage() {
	printf '%s\n\n' "$1" >&2
	fluxer_usage >&2
	exit 1
}

fluxer_take_value() {
	if [ "$2" -lt 2 ]; then
		fluxer_bad_usage "$1 needs a value."
	fi
	case $3 in
		''|--*) fluxer_bad_usage "$1 needs a value." ;;
	esac
}

fluxer_scratch=''
fluxer_record=''

fluxer_cleanup() {
	if [ -n "$fluxer_scratch" ] && [ -d "$fluxer_scratch" ]; then
		rm -rf "$fluxer_scratch"
	fi
}

fluxer_on_signal() {
	fluxer_cleanup
	if [ -n "$fluxer_record" ] && [ -d "$fluxer_record" ]; then
		printf 'Interrupted. The backup and the version record are in %s.\n' "$fluxer_record" >&2
	else
		printf 'Interrupted. The stack files and .env are left as they were.\n' >&2
	fi
	exit 130
}

trap fluxer_cleanup EXIT
trap fluxer_on_signal INT
trap fluxer_on_signal HUP
trap fluxer_on_signal TERM

opt_domain=''
opt_email=''
opt_dir=''
opt_ref=''
opt_image_tag='v1'
opt_tls='bundled'
opt_edge_bind='127.0.0.1:8080'
opt_non_interactive=0
opt_dry_run=0
opt_no_start=0
opt_update=0
opt_rollback=0
opt_backup_dir=''
opt_no_volume_backup=0
opt_skip_backup=0
opt_allow_root=0

while [ $# -gt 0 ]; do
	case $1 in
		--domain)
			fluxer_take_value '--domain' $# "${2:-}"
			opt_domain=$2
			shift 2
			;;
		--email)
			fluxer_take_value '--email' $# "${2:-}"
			opt_email=$2
			shift 2
			;;
		--dir)
			fluxer_take_value '--dir' $# "${2:-}"
			opt_dir=$2
			shift 2
			;;
		--ref)
			fluxer_take_value '--ref' $# "${2:-}"
			opt_ref=$2
			shift 2
			;;
		--image-tag)
			fluxer_take_value '--image-tag' $# "${2:-}"
			opt_image_tag=$2
			shift 2
			;;
		--tls)
			fluxer_take_value '--tls' $# "${2:-}"
			opt_tls=$2
			shift 2
			;;
		--edge-bind)
			fluxer_take_value '--edge-bind' $# "${2:-}"
			opt_edge_bind=$2
			shift 2
			;;
		--backup-dir)
			fluxer_take_value '--backup-dir' $# "${2:-}"
			opt_backup_dir=$2
			shift 2
			;;
		--non-interactive)
			opt_non_interactive=1
			shift
			;;
		--dry-run)
			opt_dry_run=1
			shift
			;;
		--no-start)
			opt_no_start=1
			shift
			;;
		--update)
			opt_update=1
			shift
			;;
		--rollback)
			opt_rollback=1
			shift
			;;
		--no-volume-backup)
			opt_no_volume_backup=1
			shift
			;;
		--skip-backup-accept-data-loss)
			opt_skip_backup=1
			shift
			;;
		--allow-root)
			opt_allow_root=1
			shift
			;;
		--help)
			fluxer_usage
			exit 0
			;;
		*)
			fluxer_bad_usage "Unknown option $1."
			;;
	esac
done

if [ -z "$opt_dir" ]; then
	if [ -n "${HOME:-}" ]; then
		opt_dir="$HOME/fluxer"
	else
		opt_dir="$(pwd)/fluxer"
	fi
fi
case $opt_dir in
	/*) ;;
	*) opt_dir="$(pwd)/$opt_dir" ;;
esac

if [ -z "$opt_backup_dir" ]; then
	opt_backup_dir="$opt_dir/backups"
fi
case $opt_backup_dir in
	/*) ;;
	*) opt_backup_dir="$(pwd)/$opt_backup_dir" ;;
esac

fluxer_docker_hint() {
	if [ "$(uname -s)" = 'Darwin' ]; then
		printf '%s' 'Install Docker Desktop from https://docs.docker.com/desktop/setup/install/mac-install/.'
		return 0
	fi
	if command -v apt-get >/dev/null 2>&1; then
		printf '%s' 'On Debian and Ubuntu, follow https://docs.docker.com/engine/install/ and install docker-ce with docker-compose-plugin. The distribution docker.io package ships no Compose plugin.'
		return 0
	fi
	if command -v dnf >/dev/null 2>&1; then
		printf '%s' 'On Fedora, RHEL and derivatives, follow https://docs.docker.com/engine/install/ and install docker-ce with docker-compose-plugin.'
		return 0
	fi
	if command -v zypper >/dev/null 2>&1; then
		printf '%s' 'On openSUSE, install the docker and docker-compose packages with zypper, then enable the docker service.'
		return 0
	fi
	if command -v pacman >/dev/null 2>&1; then
		printf '%s' 'On Arch Linux, install the docker and docker-compose packages with pacman, then enable the docker service.'
		return 0
	fi
	if command -v apk >/dev/null 2>&1; then
		printf '%s' 'On Alpine Linux, install the docker and docker-cli-compose packages with apk, then enable the docker service.'
		return 0
	fi
	printf '%s' 'Follow https://docs.docker.com/engine/install/ for this distribution.'
}

fluxer_host_tool_hint() {
	if [ "$(uname -s)" = 'Darwin' ]; then
		printf '%s' "Install $1 with Homebrew."
		return 0
	fi
	if command -v apt-get >/dev/null 2>&1; then
		printf '%s' "Install it with apt-get install -y $1."
		return 0
	fi
	if command -v dnf >/dev/null 2>&1; then
		printf '%s' "Install it with dnf install -y $1."
		return 0
	fi
	if command -v zypper >/dev/null 2>&1; then
		printf '%s' "Install it with zypper install -y $1."
		return 0
	fi
	if command -v pacman >/dev/null 2>&1; then
		printf '%s' "Install it with pacman -S $1."
		return 0
	fi
	if command -v apk >/dev/null 2>&1; then
		printf '%s' "Install it with apk add $1."
		return 0
	fi
	printf '%s' "Install $1 with the package manager of this distribution."
}

fluxer_version_field() {
	printf '%s' "$1" | cut -d. -f"$2"
}

fluxer_version_ge() {
	fluxer_vg_index=1
	while [ "$fluxer_vg_index" -le 3 ]; do
		fluxer_vg_left=$(fluxer_version_field "$1" "$fluxer_vg_index")
		fluxer_vg_right=$(fluxer_version_field "$2" "$fluxer_vg_index")
		case $fluxer_vg_left in
			''|*[!0-9]*) fluxer_vg_left=0 ;;
		esac
		case $fluxer_vg_right in
			''|*[!0-9]*) fluxer_vg_right=0 ;;
		esac
		if [ "$fluxer_vg_left" -gt "$fluxer_vg_right" ]; then
			return 0
		fi
		if [ "$fluxer_vg_left" -lt "$fluxer_vg_right" ]; then
			return 1
		fi
		fluxer_vg_index=$((fluxer_vg_index + 1))
	done
	return 0
}

fluxer_engine_version=''
fluxer_compose_version=''

fluxer_preflight() {
	if [ "$opt_allow_root" -eq 0 ] && [ "$(id -u)" -eq 0 ]; then
		fluxer_fail 2 'Running as root. Use an account in the docker group, or pass --allow-root.'
	fi
	if ! command -v docker >/dev/null 2>&1; then
		fluxer_fail 2 "Docker is not installed. $(fluxer_docker_hint)"
	fi
	if ! docker compose version >/dev/null 2>&1; then
		fluxer_fail 2 "The Docker Compose v2 plugin is missing. $(fluxer_docker_hint)"
	fi
	fluxer_engine_version=$(docker --version 2>/dev/null | sed -n 's/^Docker version \([0-9][0-9.]*\).*/\1/p')
	fluxer_compose_version=$(docker compose version --short 2>/dev/null | sed -n 's/^v\{0,1\}\([0-9][0-9.]*\).*/\1/p')
	if [ -z "$fluxer_engine_version" ] || [ -z "$fluxer_compose_version" ]; then
		fluxer_fail 2 'Cannot read the Docker Engine and Compose versions.'
	fi
	if ! fluxer_version_ge "$fluxer_engine_version" "$FLUXER_MIN_ENGINE"; then
		fluxer_fail 2 "Docker Engine $fluxer_engine_version with Compose $fluxer_compose_version. Fluxer needs Engine $FLUXER_MIN_ENGINE or newer."
	fi
	if ! fluxer_version_ge "$fluxer_compose_version" "$FLUXER_MIN_COMPOSE"; then
		fluxer_fail 2 "Docker Engine $fluxer_engine_version with Compose $fluxer_compose_version. Fluxer needs Compose $FLUXER_MIN_COMPOSE or newer."
	fi
	if ! docker info >/dev/null 2>&1; then
		fluxer_fail 2 'The Docker daemon does not answer. Start Docker and run this again.'
	fi
	if ! command -v curl >/dev/null 2>&1; then
		fluxer_fail 2 "curl is not installed. $(fluxer_host_tool_hint curl)"
	fi
	if ! command -v openssl >/dev/null 2>&1; then
		fluxer_fail 2 "openssl is not installed. $(fluxer_host_tool_hint openssl)"
	fi
	fluxer_say "Docker Engine $fluxer_engine_version with Compose $fluxer_compose_version."
}

fluxer_valid_domain() {
	case $1 in
		''|*[!a-z0-9.-]*) return 1 ;;
		.*|-*|*.|*-) return 1 ;;
		*..*) return 1 ;;
	esac
	case $1 in
		*.*) return 0 ;;
	esac
	return 1
}

fluxer_valid_email() {
	case $1 in
		*@*@*) return 1 ;;
		@*|*@) return 1 ;;
		*' '*) return 1 ;;
		*@*) return 0 ;;
	esac
	return 1
}

fluxer_valid_edge_bind() {
	case $1 in
		*:*) ;;
		*) return 1 ;;
	esac
	fluxer_bind_port=${1##*:}
	fluxer_bind_host=${1%:*}
	case $fluxer_bind_port in
		''|*[!0-9]*) return 1 ;;
	esac
	case $fluxer_bind_host in
		''|*' '*) return 1 ;;
	esac
	return 0
}

fluxer_prompt() {
	fluxer_prompt_label=$1
	fluxer_prompt_tries=0
	fluxer_prompt_value=''
	while [ "$fluxer_prompt_tries" -lt 3 ]; do
		printf '%s: ' "$fluxer_prompt_label" >&2
		if ! read -r fluxer_prompt_value; then
			return 1
		fi
		if [ -n "$fluxer_prompt_value" ]; then
			return 0
		fi
		fluxer_prompt_tries=$((fluxer_prompt_tries + 1))
	done
	return 1
}

fluxer_resolve_values() {
	if [ "$opt_update" -eq 1 ] || [ "$opt_rollback" -eq 1 ]; then
		return 0
	fi
	if [ -z "$opt_domain" ]; then
		if [ "$opt_non_interactive" -eq 1 ] || [ "$opt_dry_run" -eq 1 ] || [ ! -t 0 ]; then
			fluxer_bad_usage '--domain is required.'
		fi
		fluxer_prompt 'Hostname the instance answers on' || fluxer_fail 1 'No hostname given.'
		opt_domain=$fluxer_prompt_value
	fi
	if [ -z "$opt_email" ]; then
		if [ "$opt_non_interactive" -eq 1 ] || [ "$opt_dry_run" -eq 1 ] || [ ! -t 0 ]; then
			fluxer_bad_usage '--email is required.'
		fi
		fluxer_prompt 'Address you read' || fluxer_fail 1 'No address given.'
		opt_email=$fluxer_prompt_value
	fi
	fluxer_valid_domain "$opt_domain" || fluxer_bad_usage "--domain $opt_domain is not a lowercase hostname. Give a bare hostname such as chat.example.com."
	fluxer_valid_email "$opt_email" || fluxer_bad_usage "--email $opt_email is not an address."
}

fluxer_validate_options() {
	case $opt_tls in
		bundled|proxy) ;;
		*) fluxer_bad_usage "--tls takes bundled or proxy, not $opt_tls." ;;
	esac
	case $opt_ref in
		*' '*|*/../*|../*|*/..) fluxer_bad_usage "--ref $opt_ref is not a git ref." ;;
	esac
	case $opt_image_tag in
		''|*' '*|*/*) fluxer_bad_usage "--image-tag $opt_image_tag is not an image tag." ;;
	esac
	if [ "$opt_tls" = 'proxy' ]; then
		fluxer_valid_edge_bind "$opt_edge_bind" || fluxer_bad_usage "--edge-bind $opt_edge_bind is not an address and port."
	fi
	if [ "$opt_update" -eq 1 ] && [ "$opt_rollback" -eq 1 ]; then
		fluxer_bad_usage '--update and --rollback do not combine.'
	fi
	if [ "$opt_no_start" -eq 1 ] && { [ "$opt_update" -eq 1 ] || [ "$opt_rollback" -eq 1 ]; }; then
		fluxer_bad_usage '--no-start belongs to an install. An upgrade that does not recreate is not an upgrade.'
	fi
	if [ "$opt_skip_backup" -eq 1 ] && [ "$opt_update" -eq 0 ]; then
		fluxer_bad_usage '--skip-backup-accept-data-loss belongs to --update.'
	fi
	if [ "$opt_no_volume_backup" -eq 1 ] && [ "$opt_update" -eq 0 ]; then
		fluxer_bad_usage '--no-volume-backup belongs to --update.'
	fi
	if [ "$opt_skip_backup" -eq 1 ] && [ "$opt_no_volume_backup" -eq 1 ]; then
		fluxer_bad_usage '--skip-backup-accept-data-loss already skips the volume copy.'
	fi
}

fluxer_ref_for_tag() {
	case $1 in
		v1|latest) printf 'main' ;;
		*) printf '%s' "$1" ;;
	esac
}

# The images come from FLUXER_IMAGE_TAG and the stack files come from a git ref.
# A release tags its images and its commit with the same CalVer string, so a
# pinned tag names the commit that carries its compose files. The moving tags v1
# and latest track main.
fluxer_resolve_ref() {
	[ -z "$opt_ref" ] || return 0
	if [ "$opt_update" -eq 1 ] || [ "$opt_rollback" -eq 1 ]; then
		opt_ref=$(fluxer_ref_for_tag "$(fluxer_env_value FLUXER_IMAGE_TAG)")
	else
		opt_ref=$(fluxer_ref_for_tag "$opt_image_tag")
	fi
	if [ -z "$opt_ref" ]; then
		fluxer_fail 3 "$opt_dir/.env declares no FLUXER_IMAGE_TAG, so no ref can be derived. Pass --ref."
	fi
	case $opt_ref in
		*' '*|*/../*|../*|*/..) fluxer_fail 3 "FLUXER_IMAGE_TAG is $opt_ref, which is not a git ref. Pass --ref." ;;
	esac
}

fluxer_print_plan() {
	fluxer_say 'Plan:'
	fluxer_say "  directory     $opt_dir"
	fluxer_say "  ref           $opt_ref"
	fluxer_say "  image tag     $opt_image_tag"
	fluxer_say "  tls           $opt_tls"
	if [ "$opt_tls" = 'proxy' ]; then
		fluxer_say "  edge bind     $opt_edge_bind"
	fi
	fluxer_say "  domain        $opt_domain"
	fluxer_say "  email         $opt_email"
	fluxer_say '  action        download the stack files, write .env, start the stack'
	fluxer_say "  .env keys     $(fluxer_non_secret_keys | wc -l | tr -d ' ') non-secret values and $(fluxer_secret_keys | wc -l | tr -d ' ') secrets"
	fluxer_say '  files         docker-compose.yml docker-compose.proxy.yml tunnel.compose.yml Caddyfile .env.example'
	if [ -e "$opt_dir/.env" ]; then
		fluxer_say "  note          $opt_dir/.env exists. A run without --update refuses it."
	fi
	fluxer_say 'Nothing is written. Drop --dry-run to run this.'
}

# The scratch directory sits beside the files it will become, so the rename that
# puts a downloaded file in place is a rename within one filesystem and cannot
# leave a half-written file behind. The dry run passes a temporary parent
# instead, because it renames nothing.
fluxer_open_scratch() {
	mkdir -p "$1"
	fluxer_scratch="$1/.fluxer-install.$$"
	rm -rf "$fluxer_scratch"
	mkdir -m 700 "$fluxer_scratch"
}

# By hand, for one file:
#   curl -fsSL https://raw.githubusercontent.com/fluxerapp/fluxer/main/deploy/self-hosting/docker-compose.yml -o docker-compose.yml
#
# The files come from a git ref and the images come from FLUXER_IMAGE_TAG. The
# ref is derived from the tag unless --ref names one, which is the pairing rule
# that stops a compose file from asking for a variable the running images do not
# read, or from pinning a service image the release never built.
fluxer_fetch_stack() {
	fluxer_say "Downloading the stack files from ref $opt_ref."
	fluxer_stack_files > "$fluxer_scratch/files"
	while read -r fluxer_file; do
		[ -n "$fluxer_file" ] || continue
		fluxer_part="$fluxer_scratch/$fluxer_file.part"
		if ! curl -fsSL --proto '=https' --tlsv1.2 -o "$fluxer_part" "$FLUXER_RAW_BASE/$opt_ref/$FLUXER_STACK_PATH/$fluxer_file"; then
			fluxer_fail 4 "Download failed for $fluxer_file at ref $opt_ref. Pass --ref to name the ref the stack files come from."
		fi
		if [ ! -s "$fluxer_part" ]; then
			fluxer_fail 4 "Downloaded $fluxer_file is empty."
		fi
		if [ "$fluxer_file" = 'docker-compose.yml' ] && ! grep -q '^services:' "$fluxer_part"; then
			fluxer_fail 4 'The downloaded docker-compose.yml carries no services block.'
		fi
	done < "$fluxer_scratch/files"
}

# Every file lands only after all of them arrive, so a failed download leaves the
# directory on the set it already had.
#
# None of these files is part of an image, and all four are read from the working
# directory, so docker compose pull never updates any of them. That is why an
# upgrade refreshes them itself.
#
# A refreshed docker-compose.yml can declare a variable the running .env does not
# carry. Compose writes ${NAME:?message} for a variable the stack requires and
# stops with that message until .env sets it, and ${NAME:-default} for one that
# needs nothing from the operator. Every optional override ships commented out in
# .env.example, so a new required key is the only kind that asks for an edit.
fluxer_place_stack() {
	while read -r fluxer_file; do
		[ -n "$fluxer_file" ] || continue
		mv "$fluxer_scratch/$fluxer_file.part" "$opt_dir/$fluxer_file"
	done < "$fluxer_scratch/files"
	fluxer_say "Stack files in $opt_dir are at ref $opt_ref."
}

fluxer_compose_project() {
	if [ -n "${COMPOSE_PROJECT_NAME:-}" ]; then
		printf '%s' "$COMPOSE_PROJECT_NAME"
		return 0
	fi
	sed -n 's/^name: *//p' "$opt_dir/docker-compose.yml" | head -n 1
}

fluxer_project=''

fluxer_set_project() {
	fluxer_project=$(fluxer_compose_project)
	if [ -z "$fluxer_project" ]; then
		fluxer_fail 2 "docker-compose.yml in $opt_dir declares no project name, so the volume names cannot be derived."
	fi
}

# A second install reuses the volumes of the first, because the compose project
# name is fixed. Those volumes hold the old secrets, so the new .env never opens
# them and the stack starts into an authentication loop.
fluxer_check_volumes() {
	fluxer_project=$(fluxer_compose_project)
	if [ -z "$fluxer_project" ]; then
		return 0
	fi
	docker volume ls -q --filter "label=com.docker.compose.project=$fluxer_project" > "$fluxer_scratch/volumes" 2>/dev/null || return 0
	if [ -s "$fluxer_scratch/volumes" ]; then
		fluxer_fail 3 "Docker already holds volumes for the $fluxer_project project. They carry the secrets of an earlier install, and this .env does not open them. Run install.sh --update in the directory that holds that instance, or remove the volumes with docker volume rm before you install again."
	fi
}

fluxer_base64url() {
	openssl base64 -A | tr '+/' '-_' | tr -d '='
}

fluxer_vapid_public=''
fluxer_vapid_private=''

# The VAPID pair is the one secret in .env that no random draw produces.
# FLUXER_VAPID_PUBLIC_KEY is base64url of the 65-byte uncompressed P-256 point
# and FLUXER_VAPID_PRIVATE_KEY is base64url of its 32-byte scalar. The stack
# requires both even when nobody enables browser notifications.
#
# By hand:
#   openssl ecparam -name prime256v1 -genkey -noout -out vapid.pem
#   openssl ec -in vapid.pem -outform DER | od -An -tx1 -N7 | tr -d ' \n'
#   The line above must print 30770201010420. Any other value means a short
#   scalar, so delete vapid.pem and draw again before going on.
#   openssl ec -in vapid.pem -outform DER | tail -c +8 | head -c 32 | openssl base64 -A | tr '+/' '-_' | tr -d '='
#   openssl ec -in vapid.pem -pubout -outform DER | tail -c 65 | openssl base64 -A | tr '+/' '-_' | tr -d '='
#
# The private key is 43 characters and the public key is 87.
#
# LibreSSL trims a leading zero byte off the SEC1 private key, which shifts the
# scalar and yields a well formed value that no push service accepts. The header
# check rejects that key and the loop draws another one.
fluxer_generate_vapid() {
	fluxer_vapid_try=1
	while [ "$fluxer_vapid_try" -le "$FLUXER_VAPID_ATTEMPTS" ]; do
		openssl ecparam -name prime256v1 -genkey -noout -out "$fluxer_scratch/vapid.pem" 2>/dev/null
		openssl ec -in "$fluxer_scratch/vapid.pem" -outform DER -out "$fluxer_scratch/vapid.der" 2>/dev/null
		openssl ec -in "$fluxer_scratch/vapid.pem" -pubout -outform DER -out "$fluxer_scratch/vapid.pub.der" 2>/dev/null
		fluxer_vapid_header=$(od -An -tx1 -N7 < "$fluxer_scratch/vapid.der" | tr -d ' \n')
		if [ "$fluxer_vapid_header" = "$FLUXER_SEC1_HEADER" ]; then
			fluxer_vapid_private=$(tail -c +8 "$fluxer_scratch/vapid.der" | head -c 32 | fluxer_base64url)
			fluxer_vapid_public=$(tail -c 65 "$fluxer_scratch/vapid.pub.der" | fluxer_base64url)
			rm -f "$fluxer_scratch/vapid.pem" "$fluxer_scratch/vapid.der" "$fluxer_scratch/vapid.pub.der"
			if [ ${#fluxer_vapid_private} -eq 43 ] && [ ${#fluxer_vapid_public} -eq 87 ]; then
				return 0
			fi
			return 1
		fi
		fluxer_vapid_try=$((fluxer_vapid_try + 1))
	done
	return 1
}

# The same file by hand, which is what this function does in one pass:
#
#   cp .env.example .env
#   chmod 600 .env
#
# Then set FLUXER_DOMAIN and FLUXER_VAPID_EMAIL, the two values only the
# operator knows. The five other non-secret keys in the list above ship correct
# in .env.example and need no edit.
#
# Every secret in .env.example carries the literal CHANGE_ME. A key whose name
# ends in _BASE64 takes openssl rand -base64 32, every other key takes
# openssl rand -hex 32, and the VAPID pair comes from the generator above.
#
# Under --tls proxy, COMPOSE_FILE and FLUXER_EDGE_BIND already sit in
# .env.example as commented lines, so by hand they are uncommented rather than
# added.
fluxer_write_env() {
	fluxer_env_tmp="$fluxer_scratch/env"
	fluxer_old_umask=$(umask)
	umask 077
	: > "$fluxer_env_tmp"
	fluxer_non_secret_keys > "$fluxer_scratch/non-secret-keys"
	while read -r fluxer_key fluxer_kind fluxer_literal; do
		[ -n "$fluxer_key" ] || continue
		case $fluxer_kind in
			domain) fluxer_value=$opt_domain ;;
			email) fluxer_value=$opt_email ;;
			image_tag) fluxer_value=$opt_image_tag ;;
			literal) fluxer_value=$fluxer_literal ;;
			*) fluxer_fail 5 "Unknown non-secret kind $fluxer_kind for $fluxer_key." ;;
		esac
		printf '%s=%s\n' "$fluxer_key" "$fluxer_value" >> "$fluxer_env_tmp"
	done < "$fluxer_scratch/non-secret-keys"
	if [ "$opt_tls" = 'proxy' ]; then
		printf 'COMPOSE_FILE=%s\n' 'docker-compose.yml:docker-compose.proxy.yml' >> "$fluxer_env_tmp"
		printf 'FLUXER_EDGE_BIND=%s\n' "$opt_edge_bind" >> "$fluxer_env_tmp"
	fi
	fluxer_secret_keys > "$fluxer_scratch/secret-keys"
	while read -r fluxer_key fluxer_kind; do
		[ -n "$fluxer_key" ] || continue
		case $fluxer_kind in
			hex) fluxer_value=$(openssl rand -hex 32) ;;
			base64) fluxer_value=$(openssl rand -base64 32) ;;
			vapid_public) fluxer_value=$fluxer_vapid_public ;;
			vapid_private) fluxer_value=$fluxer_vapid_private ;;
			*) fluxer_fail 5 "Unknown secret kind $fluxer_kind for $fluxer_key." ;;
		esac
		if [ -z "$fluxer_value" ]; then
			fluxer_fail 5 "Generated an empty value for $fluxer_key."
		fi
		printf '%s=%s\n' "$fluxer_key" "$fluxer_value" >> "$fluxer_env_tmp"
	done < "$fluxer_scratch/secret-keys"
	mv "$fluxer_env_tmp" "$opt_dir/.env"
	chmod 600 "$opt_dir/.env"
	umask "$fluxer_old_umask"
}

fluxer_stack_ready() {
	docker compose ps -aq > "$fluxer_scratch/ids" 2>/dev/null || return 1
	[ -s "$fluxer_scratch/ids" ] || return 1
	xargs docker inspect --format "$FLUXER_INSPECT_FORMAT" < "$fluxer_scratch/ids" > "$fluxer_scratch/state" 2>/dev/null || return 1
	fluxer_ready=1
	fluxer_init_done=0
	while read -r fluxer_service fluxer_status fluxer_health fluxer_code; do
		case $fluxer_status in
			running)
				case $fluxer_health in
					healthy|none) ;;
					*) fluxer_ready=0 ;;
				esac
				;;
			exited)
				if [ "$fluxer_service" = 'seaweedfs-init' ] && [ "$fluxer_code" = '0' ]; then
					fluxer_init_done=1
				else
					fluxer_ready=0
				fi
				;;
			*)
				fluxer_ready=0
				;;
		esac
	done < "$fluxer_scratch/state"
	[ "$fluxer_ready" -eq 1 ] && [ "$fluxer_init_done" -eq 1 ]
}

# Readiness comes from Compose state, which is local and authoritative.
#
# By hand:
#   docker compose ps
#
# Every service reads running or healthy except seaweedfs-init, which reads
# exited (0) because it is a one-shot bucket initialiser.
fluxer_wait_ready() {
	fluxer_waited=0
	while [ "$fluxer_waited" -lt "$FLUXER_READY_TIMEOUT" ]; do
		if fluxer_stack_ready; then
			return 0
		fi
		sleep "$FLUXER_READY_INTERVAL"
		fluxer_waited=$((fluxer_waited + FLUXER_READY_INTERVAL))
	done
	return 1
}

# The public probe is informational. A host behind hairpin NAT cannot always
# reach its own hostname, and a false failure there would be worse than no probe.
fluxer_probe() {
	fluxer_probe_code=$(curl -sS -o /dev/null -w '%{http_code}' --max-time 15 "https://$1/_health" 2>/dev/null || true)
	if [ -z "$fluxer_probe_code" ]; then
		fluxer_probe_code='000'
	fi
	if [ "$fluxer_probe_code" = '200' ]; then
		fluxer_say "https://$1/_health answers 200."
		return 0
	fi
	fluxer_say "https://$1/_health answers $fluxer_probe_code from this host. Check the DNS record for $1 and inbound ports 80 and 443."
}

fluxer_env_value() {
	sed -n "s/^$1=\\(.*\\)\$/\\1/p" "$opt_dir/.env" | head -n 1
}

fluxer_require_instance() {
	if [ ! -e "$opt_dir/.env" ]; then
		fluxer_fail 2 "No .env in $opt_dir. That directory holds no instance. Run install.sh with neither --update nor --rollback to set one up."
	fi
	if [ ! -e "$opt_dir/docker-compose.yml" ]; then
		fluxer_fail 2 "No docker-compose.yml in $opt_dir. That directory does not hold an instance."
	fi
}

# The image references the stack resolves to, one per line, deduplicated. Compose
# expands them from docker-compose.yml and .env, so this is the same resolution
# docker compose pull performs. It names the images, not the versions of them.
#
# By hand:
#   docker compose config --images
fluxer_compose_images() {
	docker compose config --images 2>/dev/null | sort -u
}

# The image ID each container was actually created from, against the reference
# Compose created it under.
#
# docker image inspect <reference> answers a different question: what that tag
# points at on this host now. The two disagree whenever a newer image already
# sits under a moving tag, which is the state an upgrade that pulled and then
# failed leaves behind, and that is the state the operator re-runs --update from.
# Reading the tag there records the image the stack is about to move to as the
# image it is moving from, and the rollback that follows puts back what is
# already running while reporting success.
#
# By hand:
#   docker compose ps -aq | xargs docker inspect --format '{{.Config.Image}} {{.Image}}'
fluxer_running_image_ids() {
	docker compose ps -aq > "$fluxer_scratch/containers" 2>/dev/null || return 0
	[ -s "$fluxer_scratch/containers" ] || return 0
	xargs docker inspect --format '{{.Config.Image}} {{.Image}}' < "$fluxer_scratch/containers" 2>/dev/null | sort -u
}

# The recorded ID for one reference, or nothing when no container carries it.
fluxer_recorded_id_for() {
	awk -v fluxer_want="$1" '$1 == fluxer_want {print $2; exit}' "$fluxer_scratch/running"
}

# Step 1 of an upgrade: record what is running before anything moves.
#
# FLUXER_IMAGE_TAG defaults to v1, which tracks the latest compatible release.
# The tag therefore reads v1 before and after the upgrade, and the image ID is
# the only thing that tells two releases apart. That is what makes this record
# the version history the stack does not otherwise keep, and what makes a
# rollback possible on a moving tag.
#
# The reference list comes from Compose and the ID under each reference comes
# from the container running it, for the reason in fluxer_running_image_ids. A
# reference no container carries is recorded as `-`, which a rollback skips,
# because a version that was not running is not a version to go back to.
#
# By hand:
#   docker compose images
fluxer_record_state() {
	printf '%s\n' "$(fluxer_env_value FLUXER_IMAGE_TAG)" > "$fluxer_record/$FLUXER_TAG_FILE"
	fluxer_running_image_ids > "$fluxer_scratch/running"
	fluxer_compose_images > "$fluxer_scratch/refs"
	if [ ! -s "$fluxer_scratch/refs" ]; then
		fluxer_fail 2 "docker compose config --images returned nothing in $opt_dir, so the running version cannot be recorded."
	fi
	: > "$fluxer_record/$FLUXER_IMAGES_FILE"
	while read -r fluxer_ref; do
		[ -n "$fluxer_ref" ] || continue
		fluxer_id=$(fluxer_recorded_id_for "$fluxer_ref")
		if [ -z "$fluxer_id" ]; then
			fluxer_id='-'
		fi
		printf '%s %s\n' "$fluxer_ref" "$fluxer_id" >> "$fluxer_record/$FLUXER_IMAGES_FILE"
	done < "$fluxer_scratch/refs"
	fluxer_say "Recorded $(wc -l < "$fluxer_record/$FLUXER_IMAGES_FILE" | tr -d ' ') image references in $fluxer_record."
}

# .env and the stack files go into the record because nothing else regenerates
# them. .env holds every secret the instance was built with, so the record
# directory is created 700 and the copy is 600, and the record belongs wherever
# the operator already keeps secrets.
fluxer_save_current_files() {
	cp -p "$opt_dir/.env" "$fluxer_record/.env"
	chmod 600 "$fluxer_record/.env"
	while read -r fluxer_file; do
		[ -n "$fluxer_file" ] || continue
		if [ -e "$opt_dir/$fluxer_file" ]; then
			cp -p "$opt_dir/$fluxer_file" "$fluxer_record/$fluxer_file"
		fi
	done < "$fluxer_scratch/files"
}

fluxer_postgres_running() {
	fluxer_pg_id=$(docker compose ps -q postgres 2>/dev/null | head -n 1)
	[ -n "$fluxer_pg_id" ] || return 1
	[ "$(docker inspect --format '{{.State.Status}}' "$fluxer_pg_id" 2>/dev/null)" = 'running' ]
}

# Step 2 of an upgrade, and the part that gates the rest.
#
# api, worker, users-shard and messages-shard apply schema changes while they
# start, and starting an older image does not undo them. The dump taken before
# the pull is the only way back across a schema change.
#
# By hand:
#   docker compose exec -T postgres pg_dump -U fluxer -d fluxer --format=custom > backups/fluxer.dump
#
# The database and the role are both named fluxer and are fixed in
# docker-compose.yml. Keep -T. Without it Docker attaches a terminal to the
# command and the dump arrives corrupted, which is why the first five bytes are
# checked against the custom-format magic below rather than only the size.
#
# The dump runs against the live stack. pg_dump reads inside one transaction, so
# it sees a consistent database without stopping anything. The volume copy below
# has no equivalent and is why the stack stops there and not here.
#
# The volume copy measures its volume and refuses when the disk is short. This
# step does not, because the size of a custom-format dump is not knowable before
# pg_dump writes it. Point --backup-dir at a filesystem with room for the
# database.
fluxer_dump_postgres() {
	if ! fluxer_postgres_running; then
		fluxer_say 'Postgres is not running. Starting it for the dump.'
		if ! docker compose up -d --wait postgres; then
			fluxer_fail 7 "Postgres does not start in $opt_dir, so no dump can be taken."
		fi
	fi
	fluxer_dump_path="$fluxer_record/$FLUXER_DUMP_FILE"
	fluxer_say 'Dumping the database.'
	if ! docker compose exec -T postgres pg_dump -U fluxer -d fluxer --format=custom > "$fluxer_dump_path"; then
		rm -f "$fluxer_dump_path"
		fluxer_fail 7 'pg_dump failed. The instance is untouched.'
	fi
	if [ "$(head -c 5 "$fluxer_dump_path")" != 'PGDMP' ]; then
		rm -f "$fluxer_dump_path"
		fluxer_fail 7 'The dump does not carry the custom-format header. The instance is untouched.'
	fi
	fluxer_say "Dumped the database to $fluxer_dump_path."
}

fluxer_volume_size_kb() {
	docker run --rm -v "$1:/data:ro" "$FLUXER_HELPER_IMAGE" du -sk /data 2>/dev/null | awk 'NR==1 {print $1}'
}

fluxer_free_kb() {
	df -Pk "$1" 2>/dev/null | awk 'NR==2 {print $4}'
}

# Step 2 continued: the volume copy.
#
# A live copy can catch a file mid-write, so the stack stops for it. The stack
# comes back up on the images it was already running before anything else
# happens, so a failure here leaves a working instance rather than a stopped one.
#
# By hand:
#   docker compose stop
#   docker run --rm -v fluxer_seaweedfs-data:/data -v "$PWD/backups:/backup" alpine tar czf /backup/seaweedfs-data.tgz -C /data .
#   docker compose up -d
fluxer_copy_volumes() {
	fluxer_backup_volumes > "$fluxer_scratch/backup-volumes"
	fluxer_copy_any=0
	while read -r fluxer_volume; do
		[ -n "$fluxer_volume" ] || continue
		fluxer_full="${fluxer_project}_${fluxer_volume}"
		if ! docker volume inspect "$fluxer_full" >/dev/null 2>&1; then
			fluxer_fail 7 "The volume $fluxer_full does not exist, so the uploads cannot be copied."
		fi
		fluxer_size=$(fluxer_volume_size_kb "$fluxer_full")
		case ${fluxer_size:-} in
			''|*[!0-9]*) fluxer_fail 7 "Cannot measure the volume $fluxer_full." ;;
		esac
		fluxer_free=$(fluxer_free_kb "$fluxer_record")
		case ${fluxer_free:-} in
			''|*[!0-9]*) fluxer_fail 7 "Cannot read the free space on $fluxer_record." ;;
		esac
		fluxer_need=$((fluxer_size * FLUXER_VOLUME_HEADROOM / 100))
		if [ "$fluxer_free" -lt "$fluxer_need" ]; then
			fluxer_fail 7 "$fluxer_full holds $((fluxer_size / 1024)) MB and $fluxer_record has $((fluxer_free / 1024)) MB free. Point --backup-dir at a filesystem with room, or pass --no-volume-backup to take the database dump alone."
		fi
		fluxer_copy_any=1
	done < "$fluxer_scratch/backup-volumes"
	if [ "$fluxer_copy_any" -eq 0 ]; then
		return 0
	fi
	fluxer_say 'Stopping the stack for a consistent copy of the uploads.'
	if ! docker compose stop; then
		fluxer_fail 7 "docker compose stop failed in $opt_dir."
	fi
	while read -r fluxer_volume; do
		[ -n "$fluxer_volume" ] || continue
		fluxer_full="${fluxer_project}_${fluxer_volume}"
		fluxer_say "Copying $fluxer_full."
		if ! docker run --rm -v "$fluxer_full:/data:ro" -v "$fluxer_record:/backup" "$FLUXER_HELPER_IMAGE" tar czf "/backup/$fluxer_volume.tgz" -C /data .; then
			docker compose up -d --remove-orphans || true
			fluxer_fail 7 "Copying $fluxer_full failed. The stack is started again on the images it was running."
		fi
	done < "$fluxer_scratch/backup-volumes"
	fluxer_say 'Starting the stack again before the upgrade continues.'
	if ! docker compose up -d --remove-orphans; then
		fluxer_fail 7 "docker compose up -d failed in $opt_dir after the copy. Read docker compose logs there."
	fi
}

fluxer_backup() {
	if [ "$opt_skip_backup" -eq 1 ]; then
		fluxer_say 'Skipping the backup. --skip-backup-accept-data-loss was given, so a schema change has no way back.'
		return 0
	fi
	fluxer_dump_postgres
	if [ "$opt_no_volume_backup" -eq 1 ]; then
		fluxer_say 'Skipping the uploads copy. --no-volume-backup was given.'
		return 0
	fi
	fluxer_copy_volumes
}

fluxer_postgres_major() {
	sed -n 's/^[[:space:]]*image:[[:space:]]*postgres:\([0-9][0-9]*\).*/\1/p' "$1" | head -n 1
}

# A newer Postgres major does not read the data directory an older major wrote,
# so moving between majors means dumping, removing the volume that holds the
# database, and restoring into an empty directory. Removing that volume is the
# one destructive act in the whole procedure, and it is not something a script
# should do on an operator's behalf while they read scrolling output.
#
# The refreshed file is still in the scratch directory when this runs, so a
# refusal here leaves the instance exactly as it was.
fluxer_guard_postgres_major() {
	fluxer_old_major=$(fluxer_postgres_major "$opt_dir/docker-compose.yml")
	fluxer_new_major=$(fluxer_postgres_major "$fluxer_scratch/docker-compose.yml.part")
	if [ -z "$fluxer_old_major" ] || [ -z "$fluxer_new_major" ]; then
		return 0
	fi
	if [ "$fluxer_old_major" = "$fluxer_new_major" ]; then
		return 0
	fi
	fluxer_fail 3 "The refreshed docker-compose.yml pins postgres:$fluxer_new_major and this instance runs postgres:$fluxer_old_major. A major version change goes through a dump, an empty data directory and a restore, which this script does not do because it destroys the volume holding the database. Nothing was changed. The procedure is at https://fluxer.dev/operator/upgrading/"
}

# Which mounted files the refresh changes, and therefore which services need a
# restart rather than a recreate. The comparison happens while the new copies are
# still in the scratch directory, because once they are in place the old bytes
# are gone.
fluxer_note_mounted_changes() {
	: > "$fluxer_scratch/restart"
	fluxer_mounted_files > "$fluxer_scratch/mounts"
	while read -r fluxer_file fluxer_service; do
		[ -n "$fluxer_file" ] || continue
		if [ ! -e "$opt_dir/$fluxer_file" ]; then
			continue
		fi
		if ! cmp -s "$opt_dir/$fluxer_file" "$fluxer_scratch/$fluxer_file.part"; then
			printf '%s %s\n' "$fluxer_file" "$fluxer_service" >> "$fluxer_scratch/restart"
		fi
	done < "$fluxer_scratch/mounts"
}

# The service names the docker-compose.yml in place defines, one per line.
#
# A rollback puts back the stack files the record holds, and a record taken
# before a service was renamed names the old service. Restarting a service the
# file in place does not define fails, so the name is checked first.
#
# By hand:
#   docker compose config --services
fluxer_compose_services() {
	docker compose config --services 2>/dev/null | sort -u
}

fluxer_restart_mounted() {
	[ -s "$fluxer_scratch/restart" ] || return 0
	fluxer_compose_services > "$fluxer_scratch/services"
	while read -r fluxer_file fluxer_service; do
		[ -n "$fluxer_service" ] || continue
		if ! grep -qxF "$fluxer_service" "$fluxer_scratch/services"; then
			fluxer_say "Skipping the restart of $fluxer_service, because the docker-compose.yml in $opt_dir defines no service by that name."
			continue
		fi
		fluxer_say "Restarting $fluxer_service, because $fluxer_file is mounted into it and up -d does not reload a mounted file."
		if ! docker compose restart "$fluxer_service"; then
			fluxer_fail 6 "docker compose restart $fluxer_service failed in $opt_dir."
		fi
	done < "$fluxer_scratch/restart"
}

# A record is created before the upgrade touches anything, so a run that refuses
# or fails after this point still leaves one behind. Such a record describes the
# state the instance is already in, which makes a rollback to it a no-op rather
# than a mistake. Only the newest record is ever used.
fluxer_prepare_record() {
	mkdir -p "$opt_backup_dir"
	chmod 700 "$opt_backup_dir"
	fluxer_record="$opt_backup_dir/$FLUXER_RECORD_PREFIX$(date -u +%Y%m%dT%H%M%SZ)"
	if [ -e "$fluxer_record" ]; then
		fluxer_fail 3 "$fluxer_record exists already."
	fi
	mkdir -m 700 "$fluxer_record"
}

# Record names carry a UTC stamp, so the shell expands the glob in byte order
# and the last match is the most recent upgrade.
fluxer_newest_record() {
	fluxer_newest=''
	for fluxer_candidate in "$opt_backup_dir/$FLUXER_RECORD_PREFIX"*; do
		[ -d "$fluxer_candidate" ] || continue
		fluxer_newest=${fluxer_candidate##*/}
	done
	printf '%s' "$fluxer_newest"
}

fluxer_verify_stack() {
	fluxer_say 'Waiting for every service to report ready.'
	if ! fluxer_wait_ready; then
		fluxer_fail 6 "The stack is not ready after $FLUXER_READY_TIMEOUT seconds. Read docker compose logs in $opt_dir."
	fi
	fluxer_domain_value=$(fluxer_env_value FLUXER_DOMAIN)
	if [ -n "$fluxer_domain_value" ]; then
		fluxer_probe "$fluxer_domain_value"
	fi
}

fluxer_plan_update() {
	fluxer_say 'Plan: upgrade'
	fluxer_say "  directory     $opt_dir"
	fluxer_say "  ref           $opt_ref"
	fluxer_say "  image tag     $(fluxer_env_value FLUXER_IMAGE_TAG) from .env"
	fluxer_say "  backup dir    $opt_backup_dir"
	fluxer_say '  running now'
	fluxer_running_image_ids > "$fluxer_scratch/running"
	fluxer_compose_images > "$fluxer_scratch/refs" || true
	while read -r fluxer_ref; do
		[ -n "$fluxer_ref" ] || continue
		fluxer_id=$(fluxer_recorded_id_for "$fluxer_ref")
		if [ -z "$fluxer_id" ]; then
			fluxer_id='no container carries this image'
		fi
		fluxer_say "    $fluxer_ref $fluxer_id"
	done < "$fluxer_scratch/refs"
	if [ "$opt_skip_backup" -eq 1 ]; then
		fluxer_say '  backup        none, and a schema change would have no way back'
	elif [ "$opt_no_volume_backup" -eq 1 ]; then
		fluxer_say '  backup        the database dump, .env, and the stack files'
	else
		fluxer_say '  backup        the database dump, the uploads volume, .env, and the stack files'
		fluxer_say '  downtime      the stack stops for the uploads copy, then again for the recreate'
	fi
	fluxer_fetch_stack
	fluxer_say '  file changes'
	fluxer_changed=0
	while read -r fluxer_file; do
		[ -n "$fluxer_file" ] || continue
		if [ ! -e "$opt_dir/$fluxer_file" ]; then
			fluxer_say "    $fluxer_file is new"
			fluxer_changed=1
		elif cmp -s "$opt_dir/$fluxer_file" "$fluxer_scratch/$fluxer_file.part"; then
			fluxer_say "    $fluxer_file is unchanged"
		else
			fluxer_say "    $fluxer_file changes"
			fluxer_changed=1
		fi
	done < "$fluxer_scratch/files"
	if [ "$fluxer_changed" -eq 0 ]; then
		fluxer_say "  note          ref $opt_ref moves no stack file"
	fi
	fluxer_old_major=$(fluxer_postgres_major "$opt_dir/docker-compose.yml")
	fluxer_new_major=$(fluxer_postgres_major "$fluxer_scratch/docker-compose.yml.part")
	if [ -n "$fluxer_old_major" ] && [ -n "$fluxer_new_major" ] && [ "$fluxer_old_major" != "$fluxer_new_major" ]; then
		fluxer_say "  refusal       postgres moves from $fluxer_old_major to $fluxer_new_major, which this script does not do"
		fluxer_say '  outcome       the run stops at that refusal and changes nothing'
		fluxer_say 'Nothing outside a temporary directory was written.'
		return 0
	fi
	fluxer_note_mounted_changes
	if [ -s "$fluxer_scratch/restart" ]; then
		while read -r fluxer_file fluxer_service; do
			[ -n "$fluxer_service" ] || continue
			fluxer_say "  restart       $fluxer_service, because $fluxer_file changes and a mounted file survives up -d"
		done < "$fluxer_scratch/restart"
	fi
	fluxer_say '  commands      docker compose pull, docker compose up -d'
	fluxer_say 'Nothing outside a temporary directory was written. Drop --dry-run to run this.'
}

fluxer_plan_rollback() {
	fluxer_rollback_name=$(fluxer_newest_record)
	if [ -z "$fluxer_rollback_name" ]; then
		fluxer_fail 2 "No record in $opt_backup_dir. A rollback needs an upgrade that recorded what was running."
	fi
	fluxer_rollback_dir="$opt_backup_dir/$fluxer_rollback_name"
	fluxer_recorded_tag=$(head -n 1 "$fluxer_rollback_dir/$FLUXER_TAG_FILE" 2>/dev/null || true)
	fluxer_current_tag=$(fluxer_env_value FLUXER_IMAGE_TAG)
	fluxer_say 'Plan: rollback'
	fluxer_say "  directory     $opt_dir"
	fluxer_say "  record        $fluxer_rollback_dir"
	if [ "$fluxer_recorded_tag" = "$fluxer_current_tag" ]; then
		fluxer_say "  image tag     stays $fluxer_current_tag, so the recorded image IDs move back onto it"
	else
		fluxer_say "  image tag     $fluxer_current_tag becomes $fluxer_recorded_tag in .env"
	fi
	fluxer_say '  images'
	while read -r fluxer_ref fluxer_id; do
		[ -n "$fluxer_ref" ] || continue
		if [ "$fluxer_id" = '-' ]; then
			fluxer_say "    $fluxer_ref was not recorded with an ID"
		elif docker image inspect --format '{{.Id}}' "$fluxer_id" >/dev/null 2>&1; then
			fluxer_say "    $fluxer_ref back to $fluxer_id"
		else
			fluxer_say "    $fluxer_ref is gone from this host, so $fluxer_id cannot come back"
		fi
	done < "$fluxer_rollback_dir/$FLUXER_IMAGES_FILE"
	fluxer_say "  files         restored from $fluxer_rollback_dir"
	fluxer_say '  database      stays where the new release left it'
	fluxer_say 'Nothing is written. Drop --dry-run to run this.'
}

# The full upgrade, in the order that leaves a working instance behind at every
# point it can fail.
fluxer_run_update() {
	fluxer_open_scratch "$opt_dir"
	fluxer_stack_files > "$fluxer_scratch/files"
	fluxer_prepare_record
	fluxer_record_state
	fluxer_save_current_files
	fluxer_backup
	fluxer_fetch_stack
	fluxer_guard_postgres_major
	fluxer_note_mounted_changes
	fluxer_place_stack
	# The pull runs while the old containers still serve, so the long part of an
	# upgrade costs no downtime.
	#
	# By hand:
	#   docker compose pull
	fluxer_say 'Pulling images.'
	if ! docker compose pull; then
		fluxer_fail 4 "docker compose pull failed in $opt_dir. The stack files are refreshed and the instance still runs the old images."
	fi
	# Recreates the containers whose image or configuration changed and leaves
	# the rest running.
	#
	# By hand:
	#   docker compose up -d --remove-orphans
	#
	# Behind your own reverse proxy every command names the overlay, which
	# COMPOSE_FILE in .env does once. An invocation without it recreates the edge
	# from the base file, which binds 80 and 443 and requests its own
	# certificate.
	#
	# api, worker, users-shard and messages-shard each apply the database schema
	# while they start, so the upgrade is not finished until all four are back
	# up. All four take the same Postgres advisory lock around that work, so
	# several of them starting at once is safe.
	#
	# app-proxy waits for api to report healthy and the edge waits for the
	# Gateway, so the hostname returns errors for a minute or two after this
	# call. The api healthcheck allows 90 seconds before it counts a failure.
	fluxer_say 'Recreating the stack.'
	if ! docker compose up -d --remove-orphans; then
		fluxer_fail 6 "docker compose up -d failed in $opt_dir. Read docker compose logs there."
	fi
	fluxer_restart_mounted
	fluxer_verify_stack
	fluxer_say "Instance upgraded in $opt_dir."
	fluxer_say "The record of what it ran before is in $fluxer_record."
	fluxer_say "Go back with sh install.sh --rollback --dir $opt_dir."
	exit 0
}

# Puts one line of .env back, and proves it touched nothing else.
#
# FLUXER_IMAGE_TAG is the only key either upgrade mode writes. Every other line,
# which is every secret, is compared byte for byte before the new file replaces
# the old one, so a rewrite that lost or changed a secret cannot land.
fluxer_set_image_tag() {
	if ! grep -q '^FLUXER_IMAGE_TAG=' "$opt_dir/.env"; then
		fluxer_fail 3 "$opt_dir/.env declares no FLUXER_IMAGE_TAG, so the tag cannot be put back. Set it by hand."
	fi
	fluxer_old_umask=$(umask)
	umask 077
	sed "s|^FLUXER_IMAGE_TAG=.*|FLUXER_IMAGE_TAG=$1|" "$opt_dir/.env" > "$fluxer_scratch/env.new"
	grep -v '^FLUXER_IMAGE_TAG=' "$opt_dir/.env" > "$fluxer_scratch/env.before" || true
	grep -v '^FLUXER_IMAGE_TAG=' "$fluxer_scratch/env.new" > "$fluxer_scratch/env.after" || true
	if [ ! -s "$fluxer_scratch/env.before" ] || ! cmp -s "$fluxer_scratch/env.before" "$fluxer_scratch/env.after"; then
		umask "$fluxer_old_umask"
		fluxer_fail 3 'Rewriting FLUXER_IMAGE_TAG would have changed another line in .env. Nothing was written.'
	fi
	mv "$fluxer_scratch/env.new" "$opt_dir/.env"
	chmod 600 "$opt_dir/.env"
	umask "$fluxer_old_umask"
	fluxer_say "FLUXER_IMAGE_TAG in .env is $1."
}

# A rollback moves the images and the stack files back. The database stays where
# the new release left it, because api, worker, users-shard and messages-shard
# apply schema work in place while they start and an older image does not undo
# it. Across a release that changed the schema, the dump in the record is the
# only way back, and putting it back is a separate decision an operator makes.
#
# Two shapes, depending on what the upgrade moved:
#
#   A pinned tag moved, so the old images still carry their own tag. The tag goes
#   back into .env and Compose finds them.
#
#     By hand: set FLUXER_IMAGE_TAG back, then docker compose up -d
#
#   A moving tag such as v1 stayed put and the images under it changed. The
#   recorded image IDs are still on the host until a prune removes them, so the
#   old ID goes back onto the tag it had.
#
#     By hand: docker image tag <recorded id> ghcr.io/fluxerapp/fluxer-api:v1
#
# Neither shape pulls. A pull is what moved the instance forward in the first
# place, and running one here would undo the rollback in the same breath.
fluxer_run_rollback() {
	fluxer_rollback_name=$(fluxer_newest_record)
	if [ -z "$fluxer_rollback_name" ]; then
		fluxer_fail 2 "No record in $opt_backup_dir. A rollback needs an upgrade that recorded what was running."
	fi
	fluxer_rollback_dir="$opt_backup_dir/$fluxer_rollback_name"
	if [ ! -s "$fluxer_rollback_dir/$FLUXER_IMAGES_FILE" ]; then
		fluxer_fail 2 "$fluxer_rollback_dir records no images."
	fi
	fluxer_open_scratch "$opt_dir"
	fluxer_stack_files > "$fluxer_scratch/files"
	fluxer_say "Rolling back to $fluxer_rollback_dir."

	fluxer_recorded_tag=$(head -n 1 "$fluxer_rollback_dir/$FLUXER_TAG_FILE" 2>/dev/null || true)
	fluxer_current_tag=$(fluxer_env_value FLUXER_IMAGE_TAG)
	fluxer_moved=0
	if [ -n "$fluxer_recorded_tag" ] && [ "$fluxer_recorded_tag" != "$fluxer_current_tag" ]; then
		case $fluxer_recorded_tag in
			''|*' '*|*/*) fluxer_fail 3 "The recorded image tag $fluxer_recorded_tag is not an image tag." ;;
		esac
		fluxer_set_image_tag "$fluxer_recorded_tag"
		fluxer_moved=1
	else
		while read -r fluxer_ref fluxer_id; do
			[ -n "$fluxer_ref" ] || continue
			[ "$fluxer_id" != '-' ] || continue
			if ! docker image inspect --format '{{.Id}}' "$fluxer_id" >/dev/null 2>&1; then
				fluxer_say "$fluxer_ref is gone from this host, so it keeps the image it has now."
				continue
			fi
			if ! docker image tag "$fluxer_id" "$fluxer_ref"; then
				fluxer_fail 3 "Cannot put $fluxer_id back on $fluxer_ref."
			fi
			fluxer_moved=1
		done < "$fluxer_rollback_dir/$FLUXER_IMAGES_FILE"
	fi
	if [ "$fluxer_moved" -eq 0 ]; then
		fluxer_fail 2 "Nothing in $fluxer_rollback_dir can be put back. The recorded tag is the one in .env and every recorded image has been removed from this host, which a docker image prune does."
	fi

	while read -r fluxer_file; do
		[ -n "$fluxer_file" ] || continue
		if [ -e "$fluxer_rollback_dir/$fluxer_file" ]; then
			cp -p "$fluxer_rollback_dir/$fluxer_file" "$opt_dir/$fluxer_file"
		fi
	done < "$fluxer_scratch/files"
	fluxer_say "Stack files in $opt_dir are the ones the record holds."

	fluxer_say 'Recreating the stack.'
	if ! docker compose up -d --remove-orphans; then
		fluxer_fail 6 "docker compose up -d failed in $opt_dir. Read docker compose logs there."
	fi
	# The mounted file came back from the record, so its service restarts.
	# Comparing it first would save one restart and cost the reader a reason.
	fluxer_mounted_files > "$fluxer_scratch/restart"
	fluxer_restart_mounted
	fluxer_verify_stack
	fluxer_say "Instance rolled back in $opt_dir."
	if [ -s "$fluxer_rollback_dir/$FLUXER_DUMP_FILE" ]; then
		fluxer_say "The database did not move. Restore it from $fluxer_rollback_dir/$FLUXER_DUMP_FILE only when the release you left changed the schema."
	else
		fluxer_say "The database did not move. That record holds no dump, because the upgrade ran with --skip-backup-accept-data-loss, so a release that changed the schema has no way back."
	fi
	exit 0
}

fluxer_preflight
fluxer_validate_options
fluxer_resolve_values

if [ "$opt_update" -eq 1 ] || [ "$opt_rollback" -eq 1 ]; then
	fluxer_require_instance
	fluxer_resolve_ref
	cd "$opt_dir"
	fluxer_set_project
	if [ "$opt_dry_run" -eq 1 ]; then
		# The dry run downloads into a temporary directory so it can name the
		# files that actually change, the services that actually restart, and a
		# Postgres major that would stop the run. It removes that directory on
		# exit and touches nothing in the working directory.
		fluxer_open_scratch "${TMPDIR:-/tmp}"
		fluxer_stack_files > "$fluxer_scratch/files"
		if [ "$opt_rollback" -eq 1 ]; then
			fluxer_plan_rollback
		else
			fluxer_plan_update
		fi
		exit 0
	fi
	if [ "$opt_rollback" -eq 1 ]; then
		fluxer_run_rollback
	fi
	fluxer_run_update
fi

fluxer_resolve_ref

if [ "$opt_dry_run" -eq 1 ]; then
	fluxer_print_plan
	exit 0
fi

mkdir -p "$opt_dir"
cd "$opt_dir"

if [ -e "$opt_dir/.env" ]; then
	fluxer_fail 3 "$opt_dir/.env exists. Run with --update to upgrade the instance and keep the secrets."
fi

fluxer_open_scratch "$opt_dir"
fluxer_fetch_stack
fluxer_place_stack
fluxer_check_volumes

fluxer_say 'Generating secrets.'
if ! fluxer_generate_vapid; then
	fluxer_fail 5 "Could not generate a VAPID key of the required shape in $FLUXER_VAPID_ATTEMPTS attempts."
fi
fluxer_write_env
fluxer_say "Wrote $opt_dir/.env, readable by you alone."

if [ "$opt_no_start" -eq 1 ]; then
	fluxer_say "Start the instance with docker compose up -d in $opt_dir."
	fluxer_say 'Open it and create the first admin account. Finish the setup wizard in the same sitting.'
	fluxer_say "Secrets live in $opt_dir/.env. Back that file up."
	exit 0
fi

fluxer_say 'Starting the stack.'
if ! docker compose up -d; then
	fluxer_fail 6 "docker compose up -d failed in $opt_dir. Read docker compose logs there."
fi
fluxer_say 'Waiting for every service to report ready. This takes several minutes on the first start, which pulls eighteen images.'
if ! fluxer_wait_ready; then
	fluxer_fail 6 "The stack is not ready after $FLUXER_READY_TIMEOUT seconds. Read docker compose logs in $opt_dir."
fi
fluxer_probe "$opt_domain"

fluxer_say "Instance ready at https://$opt_domain"
fluxer_say 'Open it and create the first admin account. Finish the setup wizard in the same sitting.'
fluxer_say "Secrets live in $opt_dir/.env. Back that file up."
