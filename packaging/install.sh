#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

PREFIX=/opt/fluxer
RELEASES="$PREFIX/releases"
CURRENT="$PREFIX/current"
CONFIG_DIR=/etc/fluxer
ENV_FILE="$CONFIG_DIR/fluxer.env"
DATA_DIR=/var/lib/fluxer
UPGRADE_BACKUPS="$DATA_DIR/backups/pre-upgrade"
UNIT=fluxerd.service
UNIT_PATH="/etc/systemd/system/$UNIT"
DROPIN_DIR="/etc/systemd/system/$UNIT.d"
KEEP_RELEASES=3
KEEP_BACKUPS=3
CONTROL_VARS=(FLUXER_TARBALL)
PACKAGE_LIST=share/runtime-packages

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ARCH=""

log() { printf '==> %s\n' "$*" >&2; }
warn() { printf 'install.sh: warning: %s\n' "$*" >&2; }
die() {
    printf 'install.sh: %s\n' "$*" >&2
    exit 1
}

usage() {
    cat <<EOF
Usage: install.sh [TARBALL]
       install.sh --upgrade TARBALL

Installs Fluxer from fluxer-<version>-linux-<arch>.tar.zst into $RELEASES/<version>
and runs it as $UNIT. Without TARBALL, FLUXER_TARBALL or the single matching
tarball next to this script is used.

A new install writes every FLUXER_* variable from the environment into $ENV_FILE.
FLUXER_DOMAIN is required then. An existing $ENV_FILE is never changed.

Fluxer's internal services listen on 127.0.0.1, and some of them don't ask
for a password. Install it on a machine or container where every local account
is trusted.
EOF
}

as_fluxer() {
    (cd / && runuser -u fluxer -- env -i PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
        HOME="$DATA_DIR" LANG=C.UTF-8 FLUXER_DATA_DIR="$DATA_DIR" "$@")
}

check_host() {
    [ "$(id -u)" = 0 ] || die "run this as root"
    [ -r /etc/os-release ] || die "/etc/os-release is missing, Debian 13 (trixie) is required"
    local codename
    codename="$(sed -n 's/^VERSION_CODENAME=//p' /etc/os-release | tr -d '"')"
    [ "$codename" = trixie ] || die "Debian 13 (trixie) is required, this host is '${codename:-unknown}'"
    [ -d /run/systemd/system ] || die "systemd must be running as init"
    ARCH="$(dpkg --print-architecture)"
    case "$ARCH" in
        amd64 | arm64) ;;
        *) die "unsupported architecture $ARCH, only amd64 and arm64 are built" ;;
    esac
    if [ -n "${FLUXER_DATA_DIR:-}" ] && [ "$FLUXER_DATA_DIR" != "$DATA_DIR" ]; then
        die "FLUXER_DATA_DIR must be $DATA_DIR, $UNIT only allows writes there"
    fi
    [ ! -e "$PREFIX/.rexctl" ] || die "$PREFIX holds a Fluxer instance managed by rexctl, use rexctl for it"
    check_other_instance
}

check_other_instance() {
    local sock port
    for sock in /run/rexctl/*/fluxerd.sock; do
        [ -S "$sock" ] || continue
        die "a Fluxer instance managed by rexctl runs on this machine ($sock), stop it with rexctl stop in its folder first"
    done
    systemctl is-active --quiet "$UNIT" && return 0
    for port in 14222 15432; do
        if (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null; then
            die "127.0.0.1:$port is in use, so another Fluxer instance or another program runs on this machine, stop it first"
        fi
    done
}

resolve_tarball() {
    local given="$1"
    if [ -z "$given" ]; then
        local found=()
        shopt -s nullglob
        found=("$SCRIPT_DIR"/fluxer-*-linux-"$ARCH".tar.zst)
        shopt -u nullglob
        [ "${#found[@]}" -eq 1 ] || die "pass the release tarball, found ${#found[@]} candidates next to $0"
        given="${found[0]}"
    fi
    [ -f "$given" ] || die "$given does not exist"
    printf '%s/%s\n' "$(cd "$(dirname "$given")" && pwd)" "$(basename "$given")"
}

tarball_version() {
    local name
    name="$(basename "$1")"
    [[ "$name" =~ ^fluxer-([0-9A-Za-z][0-9A-Za-z._+~-]*)-linux-(amd64|arm64)\.tar\.zst$ ]] \
        || die "$name does not match fluxer-<version>-linux-<arch>.tar.zst"
    [ "${BASH_REMATCH[2]}" = "$ARCH" ] || die "$name is built for ${BASH_REMATCH[2]}, this host is $ARCH"
    printf '%s\n' "${BASH_REMATCH[1]}"
}

verify_tarball() {
    local tarball="$1" dir name sums line
    dir="$(dirname "$tarball")"
    name="$(basename "$tarball")"
    sums="$dir/SHA256SUMS"
    if [ ! -f "$sums" ]; then
        warn "no SHA256SUMS next to $name, skipping the checksum check"
        return
    fi
    line="$(awk -v n="$name" '$1 ~ /^[0-9a-f]{64}$/ && ($2 == n || $2 == "*" n)' "$sums")"
    [ -n "$line" ] || die "$sums has no entry for $name"
    (cd "$dir" && printf '%s\n' "$line" | sha256sum --check --quiet -) || die "checksum mismatch for $name"
}

current_version() {
    [ -L "$CURRENT" ] || return 0
    basename "$(readlink "$CURRENT")"
}

install_bootstrap() {
    export DEBIAN_FRONTEND=noninteractive
    log "installing the packages that unpack the release"
    apt-get update
    apt-get install -y --no-install-recommends zstd postgresql-common
    sed -ri 's/^#?[[:space:]]*create_main_cluster[[:space:]]*=.*/create_main_cluster = false/' \
        /etc/postgresql-common/createcluster.conf
    grep -q '^create_main_cluster = false$' /etc/postgresql-common/createcluster.conf \
        || die "could not set create_main_cluster = false in /etc/postgresql-common/createcluster.conf"
}

install_packages() {
    local list="$1/$PACKAGE_LIST" packages=()
    [ -f "$list" ] || die "$1 has no $PACKAGE_LIST"
    mapfile -t packages < <(grep -v '^[[:space:]]*$' "$list")
    log "installing runtime packages: ${packages[*]}"
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends "${packages[@]}"
    if [ -z "$(pg_lsclusters -h 2>/dev/null)" ]; then
        systemctl disable --now postgresql.service >/dev/null 2>&1 || true
    else
        warn "existing PostgreSQL clusters were found and left running, Fluxer runs its own on 127.0.0.1:15432"
    fi
}

ensure_user() {
    getent group fluxer >/dev/null || groupadd --system fluxer
    getent passwd fluxer >/dev/null \
        || useradd --system --gid fluxer --home-dir "$DATA_DIR" --no-create-home --shell /usr/sbin/nologin fluxer
}

ensure_dirs() {
    install -d -m 0755 "$PREFIX" "$RELEASES"
    install -d -o root -g fluxer -m 0750 "$CONFIG_DIR" "$CONFIG_DIR/caddy.d" "$CONFIG_DIR/caddy.d/sites" "$CONFIG_DIR/certs"
    install -d -o fluxer -g fluxer -m 0750 "$DATA_DIR"
}

unpack_release() {
    local tarball="$1" version="$2" dest="$RELEASES/$2" tmp root entries
    if [ -d "$dest" ]; then
        [ -x "$dest/bin/fluxerd" ] || die "$dest exists without bin/fluxerd, remove it and run again"
        log "release $version is already unpacked"
        return
    fi
    rm -rf "$RELEASES"/.unpack-*
    log "unpacking $version into $dest"
    tmp="$(mktemp -d "$RELEASES/.unpack-XXXXXX")"
    tar --zstd -xf "$tarball" -C "$tmp" --no-same-owner
    root="$tmp"
    if [ ! -e "$tmp/bin" ]; then
        entries=("$tmp"/*)
        if [ "${#entries[@]}" -eq 1 ] && [ -d "${entries[0]}" ]; then
            root="${entries[0]}"
        fi
    fi
    [ -x "$root/bin/fluxerd" ] || die "$(basename "$tarball") has no bin/fluxerd"
    chown -R root:root "$root"
    chmod -R go-w "$root"
    chmod 0755 "$root"
    mv "$root" "$dest"
    rm -rf "$tmp"
}

flip_current() {
    ln -sfn "releases/$1" "$PREFIX/.current.new"
    mv -Tf "$PREFIX/.current.new" "$CURRENT"
    touch "$RELEASES/$1"
    log "$CURRENT now points to releases/$1"
}

write_env_file() {
    local names name value given=()
    mapfile -t names < <(compgen -e | grep '^FLUXER_' | sort)
    for name in "${names[@]}"; do
        [[ " ${CONTROL_VARS[*]} " == *" $name "* ]] || given+=("$name")
    done
    if [ -e "$ENV_FILE" ]; then
        log "keeping the existing $ENV_FILE"
        [ "${#given[@]}" -eq 0 ] || warn "not applied from the environment: ${given[*]}, edit $ENV_FILE instead"
        return
    fi
    [ -n "${FLUXER_DOMAIN:-}" ] || die "FLUXER_DOMAIN is required for a new install"
    local tmp
    tmp="$(mktemp "$CONFIG_DIR/.fluxer.env.XXXXXX")"
    for name in "${given[@]}"; do
        value="${!name}"
        [[ "$value" != *$'\n'* ]] || die "$name contains a newline"
        printf '%s=%s\n' "$name" "$value" >>"$tmp"
    done
    chown root:fluxer "$tmp"
    chmod 0640 "$tmp"
    mv "$tmp" "$ENV_FILE"
    log "wrote $ENV_FILE"
}

install_unit() {
    local src="$1/share/systemd/$UNIT"
    [ -f "$src" ] || src="$SCRIPT_DIR/systemd/$UNIT"
    [ -f "$src" ] || die "no $UNIT in $1/share/systemd or $SCRIPT_DIR/systemd"
    if cmp -s "$src" "$UNIT_PATH"; then
        return 1
    fi
    install -m 0644 "$src" "$UNIT_PATH"
    systemctl daemon-reload
    log "installed $UNIT_PATH"
}

start_unit() {
    local verb="$1"
    log "running systemctl $verb $UNIT, this waits until every service is ready"
    systemctl reset-failed "$UNIT" 2>/dev/null || true
    if systemctl "$verb" "$UNIT"; then
        return 0
    fi
    if [ "$(systemctl show -p ExecMainStatus --value "$UNIT")" = 226 ] && [ ! -e "$DROPIN_DIR/no-namespaces.conf" ]; then
        warn "$UNIT could not set up its mount namespace (status 226), which happens in LXC containers without nesting"
        warn "turning off PrivateTmp and ProtectSystem in $DROPIN_DIR/no-namespaces.conf and trying again"
        install -d -m 0755 "$DROPIN_DIR"
        printf '[Service]\nPrivateTmp=no\nProtectSystem=no\nReadWritePaths=\n' >"$DROPIN_DIR/no-namespaces.conf"
        systemctl daemon-reload
        systemctl reset-failed "$UNIT"
        systemctl "$verb" "$UNIT" && return 0
        warn "that did not help, removing $DROPIN_DIR/no-namespaces.conf again"
        rm -f "$DROPIN_DIR/no-namespaces.conf"
        systemctl daemon-reload
    fi
    return 1
}

newest_backup() {
    find "$UPGRADE_BACKUPS" -mindepth 1 -maxdepth 1 -newer "$1" -printf '%T@ %p\n' 2>/dev/null \
        | sort -rn | head -n 1 | cut -d' ' -f2-
}

prune_releases() {
    local current n=0 dir
    current="$(current_version)"
    while IFS= read -r dir; do
        n=$((n + 1))
        if [ "$n" -le "$KEEP_RELEASES" ] || [ "$(basename "$dir")" = "$current" ]; then
            continue
        fi
        log "removing old release $(basename "$dir")"
        rm -rf "$dir"
    done < <(find "$RELEASES" -mindepth 1 -maxdepth 1 -type d ! -name '.*' -printf '%T@ %p\n' | sort -rn | cut -d' ' -f2-)
}

prune_backups() {
    local n=0 file
    while IFS= read -r file; do
        n=$((n + 1))
        [ "$n" -gt "$KEEP_BACKUPS" ] || continue
        log "removing old pre-upgrade backup $(basename "$file")"
        rm -f "$file"
    done < <(find "$UPGRADE_BACKUPS" -mindepth 1 -maxdepth 1 -type f -name 'fluxer-*.tar.zst' -printf '%T@ %p\n' 2>/dev/null | sort -rn | cut -d' ' -f2-)
}

print_done() {
    log "Fluxer $(current_version) is running"
    printf '%s\n' \
        "Logs:          journalctl -u $UNIT" \
        "Services:      runuser -u fluxer -- $CURRENT/bin/fluxerd ps" \
        "Setup token:   runuser -u fluxer -- $CURRENT/bin/fluxerd exec api -- printenv FLUXER_SETUP_TOKEN" \
        "Settings:      $ENV_FILE, then systemctl restart $UNIT"
}

install_fresh() {
    local tarball="$1" version current unit_changed=0
    version="$(tarball_version "$tarball")"
    current="$(current_version)"
    if [ -n "$current" ] && [ "$current" != "$version" ]; then
        die "release $current is installed, use --upgrade $tarball to move to $version"
    fi
    verify_tarball "$tarball"
    install_bootstrap
    ensure_user
    ensure_dirs
    unpack_release "$tarball" "$version"
    install_packages "$RELEASES/$version"
    [ "$current" = "$version" ] || flip_current "$version"
    write_env_file
    if [ -e "$DATA_DIR/secrets.env" ]; then
        log "secrets already exist in $DATA_DIR/secrets.env"
    else
        log "creating secrets, note the setup token printed below"
        as_fluxer "$CURRENT/bin/fluxerd" init
    fi
    as_fluxer "$CURRENT/bin/fluxerd" check-config || die "check-config failed, fix $ENV_FILE and run again"
    install_unit "$CURRENT" && unit_changed=1
    systemctl enable "$UNIT"
    if [ "$unit_changed" = 1 ] && systemctl is-active --quiet "$UNIT"; then
        start_unit restart || die "$UNIT did not become ready, see journalctl -u $UNIT"
    else
        start_unit start || die "$UNIT did not become ready, see journalctl -u $UNIT"
    fi
    print_done
}

upgrade() {
    local tarball="$1" old new marker backup
    old="$(current_version)"
    [ -n "$old" ] || die "Fluxer is not installed, run install.sh without --upgrade first"
    new="$(tarball_version "$tarball")"
    [ "$new" != "$old" ] || die "$new is already the current release"
    systemctl is-active --quiet "$UNIT" || die "$UNIT is not running, start it so the pre-upgrade backup can be taken"
    verify_tarball "$tarball"
    install_bootstrap
    unpack_release "$tarball" "$new"
    install_packages "$RELEASES/$new"
    if ! as_fluxer "$RELEASES/$new/bin/fluxerd" check-config; then
        rm -rf "${RELEASES:?}/$new"
        die "check-config failed for $new, nothing was changed"
    fi

    log "taking the pre-upgrade backup"
    marker="$(mktemp)"
    as_fluxer "$RELEASES/$old/bin/fluxerd" backup --pre-upgrade --dir "$UPGRADE_BACKUPS" || {
        rm -f "$marker"
        die "the pre-upgrade backup failed, nothing was changed"
    }
    backup="$(newest_backup "$marker")"
    rm -f "$marker"
    [ -n "$backup" ] || die "the pre-upgrade backup did not appear in $UPGRADE_BACKUPS, nothing was changed"
    log "backup written to $backup"

    flip_current "$new"
    install_unit "$RELEASES/$new" || true
    if start_unit restart; then
        prune_releases
        prune_backups
        print_done
        return
    fi

    warn "$new did not become ready, rolling back to $old"
    systemctl stop "$UNIT" || true
    flip_current "$old"
    install_unit "$RELEASES/$old" || true
    as_fluxer "$RELEASES/$old/bin/fluxerd" restore "$backup" \
        || die "restoring $backup failed, $UNIT is stopped on $old, see journalctl -u $UNIT"
    start_unit start || die "$old did not become ready after the rollback, the backup is $backup"
    rm -rf "${RELEASES:?}/$new"
    die "upgrade to $new failed, $old is running again with the data from $backup"
}

main() {
    local mode=install tarball="${FLUXER_TARBALL:-}"
    while [ "$#" -gt 0 ]; do
        case "$1" in
            --upgrade)
                mode=upgrade
                [ "$#" -ge 2 ] || die "--upgrade needs a tarball"
                tarball="$2"
                shift
                ;;
            -h | --help)
                usage
                exit 0
                ;;
            -*) die "unknown option $1, see --help" ;;
            *) tarball="$1" ;;
        esac
        shift
    done

    check_host
    exec 9>/run/lock/fluxer-install.lock
    flock -n 9 || die "another install.sh is running"
    tarball="$(resolve_tarball "$tarball")"
    if [ "$mode" = upgrade ]; then
        upgrade "$tarball"
    else
        install_fresh "$tarball"
    fi
}

main "$@"
