#!/usr/bin/env sh

# SPDX-License-Identifier: AGPL-3.0-or-later

set -eu

gateway_home="${FLUXER_GATEWAY_HOME:-/opt/fluxer_gateway}"
bouncer_home="${FLUXER_BOUNCER_HOME:-/opt/fluxer_bouncer}"
run_dir="${FLUXER_GATEWAY_RUN_DIR:-/tmp}"
vm_args_file="$run_dir/vm.args"
node_name_file="$run_dir/node_name"

case "${FLUXER_GATEWAY_ROLE:-}" in
bouncer)
	program_home="$bouncer_home"
	program_eval="fluxer_bouncer@@main:run(fluxer_bouncer)"
	vm_args_template="$gateway_home/config/bouncer_vm.args.src"
	default_schedulers_max=4
	;;
*)
	program_home="$gateway_home"
	program_eval="fluxer_gateway:main()"
	vm_args_template="$gateway_home/config/vm.args.src"
	default_schedulers_max=16
	;;
esac

is_positive_int() {
	case "${1:-}" in
	'' | 0* | *[!0-9]*) return 1 ;;
	*) [ "$1" -gt 0 ] ;;
	esac
}

cpu_quota_count() {
	cpu_quota=""
	cpu_period=""
	if [ -r /sys/fs/cgroup/cpu.max ]; then
		read -r cpu_quota cpu_period </sys/fs/cgroup/cpu.max || true
	else
		for cgroup_dir in /sys/fs/cgroup/cpu /sys/fs/cgroup/cpu,cpuacct; do
			if [ -r "$cgroup_dir/cpu.cfs_quota_us" ] && [ -r "$cgroup_dir/cpu.cfs_period_us" ]; then
				read -r cpu_quota <"$cgroup_dir/cpu.cfs_quota_us" || true
				read -r cpu_period <"$cgroup_dir/cpu.cfs_period_us" || true
				break
			fi
		done
	fi
	if is_positive_int "$cpu_quota" && is_positive_int "$cpu_period"; then
		echo "$(((cpu_quota + cpu_period - 1) / cpu_period))"
	fi
}

available_cpu_count() {
	cpu_count="$(nproc 2>/dev/null || echo 1)"
	is_positive_int "$cpu_count" || cpu_count=1
	quota_count="$(cpu_quota_count)"
	if is_positive_int "$quota_count" && [ "$quota_count" -lt "$cpu_count" ]; then
		cpu_count="$quota_count"
	fi
	echo "$cpu_count"
}

clamp_int() {
	value="$1"
	min="$2"
	max="$3"
	if [ "$value" -lt "$min" ]; then
		value="$min"
	fi
	if [ "$value" -gt "$max" ]; then
		value="$max"
	fi
	echo "$value"
}

render_vm_args() {
	mkdir -p "$run_dir"
	rm -f "$vm_args_file"
	(
		umask 077
		awk '
	{
		line = $0
		out = ""
		while (match(line, /\$\{[A-Za-z_][A-Za-z0-9_]*(:-[^}]*)?\}/)) {
			token = substr(line, RSTART + 2, RLENGTH - 3)
			name = token
			fallback = ""
			split_at = index(token, ":-")
			if (split_at > 0) {
				name = substr(token, 1, split_at - 1)
				fallback = substr(token, split_at + 2)
			}
			value = fallback
			if ((name in ENVIRON) && ENVIRON[name] != "") {
				value = ENVIRON[name]
			}
			out = out substr(line, 1, RSTART - 1) value
			line = substr(line, RSTART + RLENGTH)
		}
		print out line
	}
	' "$vm_args_template" >"$vm_args_file"
	)
}

is_positive_int "${FLUXER_ERLANG_SCHEDULERS_MIN:-}" || FLUXER_ERLANG_SCHEDULERS_MIN=2
is_positive_int "${FLUXER_ERLANG_SCHEDULERS_MAX:-}" || FLUXER_ERLANG_SCHEDULERS_MAX="$default_schedulers_max"
: "${FLUXER_ERLANG_DIST_PORT:=8081}"
: "${FLUXER_GATEWAY_SYS_CONFIG:=$gateway_home/config/sys.config}"
: "${ERL_CRASH_DUMP:=$run_dir/erl_crash.dump}"
export ERL_CRASH_DUMP

if ! is_positive_int "$FLUXER_ERLANG_DIST_PORT"; then
	echo 'FLUXER_ERLANG_DIST_PORT must be a positive integer.' >&2
	exit 1
fi

if [ "$program_home" = "$bouncer_home" ]; then
	: "${FLUXER_BOUNCER_COUNT:=${FLUXER_GATEWAY_BOUNCER_COUNT:-}}"
	: "${FLUXER_BOUNCER_BASENAME:=${FLUXER_GATEWAY_BOUNCER_BASENAME:-gateway-bouncer}}"
	: "${FLUXER_BOUNCER_DNS_NAME:=${FLUXER_GATEWAY_BOUNCER_DNS_NAME:-}}"
	: "${FLUXER_BOUNCER_LOGGER_LEVEL:=warning}"
	if [ -z "${HOSTNAME:-}" ]; then
		echo 'HOSTNAME is required, it carries the StatefulSet ordinal.' >&2
		exit 2
	fi
	if [ -z "$FLUXER_BOUNCER_DNS_NAME" ]; then
		echo 'FLUXER_BOUNCER_DNS_NAME or FLUXER_GATEWAY_BOUNCER_DNS_NAME is required.' >&2
		exit 2
	fi
	case "$FLUXER_BOUNCER_LOGGER_LEVEL" in
	debug | info | notice | warning | error | critical | alert | emergency) ;;
	*)
		echo 'FLUXER_BOUNCER_LOGGER_LEVEL must be one of debug, info, notice, warning, error, critical, alert or emergency.' >&2
		exit 2
		;;
	esac
	FLUXER_ERLANG_NODE_NAME="${FLUXER_BOUNCER_BASENAME}@${HOSTNAME}.${FLUXER_BOUNCER_DNS_NAME}"
	export FLUXER_BOUNCER_COUNT FLUXER_BOUNCER_BASENAME FLUXER_BOUNCER_DNS_NAME FLUXER_BOUNCER_LOGGER_LEVEL
fi

: "${FLUXER_ERLANG_NODE_NAME:=fluxer_gateway@127.0.0.1}"

if [ -n "${FLUXER_ERLANG_NODE_NAME_PREFIX:-}" ] && [ "$program_home" = "$gateway_home" ]; then
	case "$FLUXER_ERLANG_NODE_NAME_PREFIX" in
	*[!A-Za-z0-9_]*)
		echo 'FLUXER_ERLANG_NODE_NAME_PREFIX may only contain letters, digits and underscores.' >&2
		exit 1
		;;
	esac
	node_nonce="$(od -An -N4 -tx1 /dev/urandom | tr -d ' \n')"
	FLUXER_ERLANG_NODE_NAME="${FLUXER_ERLANG_NODE_NAME_PREFIX}_${node_nonce}@${FLUXER_ERLANG_NODE_NAME#*@}"
	printf '%s\n' "$FLUXER_ERLANG_NODE_NAME" >"$node_name_file"
else
	rm -f "$node_name_file"
fi

case "${FLUXER_ERLANG_COOKIE_FILE:-}" in
*[![:space:]]*)
	case "${FLUXER_ERLANG_COOKIE:-}" in
	*[![:space:]]*)
		echo 'FLUXER_ERLANG_COOKIE and FLUXER_ERLANG_COOKIE_FILE are both set, set only one.' >&2
		exit 1
		;;
	esac
	if ! FLUXER_ERLANG_COOKIE="$(cat -- "$FLUXER_ERLANG_COOKIE_FILE")"; then
		echo "FLUXER_ERLANG_COOKIE_FILE could not read $FLUXER_ERLANG_COOKIE_FILE." >&2
		exit 1
	fi
	;;
esac

if [ -z "${FLUXER_ERLANG_COOKIE:-}" ]; then
	echo 'FLUXER_ERLANG_COOKIE is required.' >&2
	exit 1
fi

if ! is_positive_int "${FLUXER_ERLANG_SCHEDULERS:-}"; then
	FLUXER_ERLANG_SCHEDULERS="$(clamp_int "$(available_cpu_count)" "$FLUXER_ERLANG_SCHEDULERS_MIN" "$FLUXER_ERLANG_SCHEDULERS_MAX")"
fi

if ! is_positive_int "${FLUXER_ERLANG_DIRTY_CPU_SCHEDULERS:-}"; then
	FLUXER_ERLANG_DIRTY_CPU_SCHEDULERS="$FLUXER_ERLANG_SCHEDULERS"
fi

if ! is_positive_int "${FLUXER_ERLANG_DIRTY_IO_SCHEDULERS:-}"; then
	FLUXER_ERLANG_DIRTY_IO_SCHEDULERS="$(clamp_int "$FLUXER_ERLANG_SCHEDULERS" 2 4)"
fi

if [ -z "${FLUXER_ERLANG_CONNECT_ALL:-}" ]; then
	if is_positive_int "${FLUXER_GATEWAY_BOUNCER_COUNT:-}"; then
		FLUXER_ERLANG_CONNECT_ALL=false
	else
		FLUXER_ERLANG_CONNECT_ALL=true
	fi
fi

export FLUXER_ERLANG_CONNECT_ALL
export FLUXER_ERLANG_NODE_NAME
export FLUXER_ERLANG_COOKIE
export FLUXER_ERLANG_DIST_PORT
export FLUXER_ERLANG_SCHEDULERS
export FLUXER_ERLANG_DIRTY_CPU_SCHEDULERS
export FLUXER_ERLANG_DIRTY_IO_SCHEDULERS
export FLUXER_GATEWAY_SYS_CONFIG

render_vm_args
chmod 600 "$vm_args_file"
unset FLUXER_ERLANG_COOKIE

ERL_FLAGS="-args_file $vm_args_file${ERL_FLAGS:+ $ERL_FLAGS}"
export ERL_FLAGS

exec erl -pa "$program_home"/*/ebin -eval "$program_eval" -noshell -extra
