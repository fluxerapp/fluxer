#!/usr/bin/env bash
set -euo pipefail

readonly ELECTRON_VERSION=44.4.1
readonly ELECTRON_REVISION=09d1a6590e3148a1a2132b07a5b107b7c2414191
readonly DEPOT_TOOLS_REVISION=889099791deff38466d471fa6590951c91a1d8ab
readonly BUILD_IMAGE='ghcr.io/electron/build:daad061f4b99a0ae1c841be4aa09188280a9c8a4@sha256:7a161ad561103ebbfb5ff18eaf47cb579ca0c528bb7ec9ed8cad30a41d5fa63e'
readonly PATCH_NAME=fix_synchronize_vaapi_surfaces_before_output.patch
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
readonly SCRIPT_DIR

die() { printf 'Error: %s\n' "$*" >&2; exit 1; }

usage() {
	cat <<'USAGE'
Build patched Electron 44.4.1 on a Linux x64 host with Podman or Docker.

Usage: build-linux.sh [--arch x64|arm64|all] [--build-root PATH] [--output-dir PATH] [--check]

--arch        Target architecture; all builds x64 then arm64 (default: all).
--build-root  Dedicated persistent cache, source and output tree.
              Default: ${XDG_CACHE_HOME:-$HOME/.cache}/fluxer/electron-44.4.1
--output-dir  Distribution ZIPs and .fluxer.json manifests (default: BUILD_ROOT/artifacts).
--check       Validate host resources and configuration without pulling or compiling.

Requires at least 300 GiB free in the build filesystem. Recommended/default
container memory is 48 GiB; use CONTAINER_MEMORY=32g for the lower-memory minimum.
Available host RAM must cover the selected container limit.
The build uses host networking and builds architectures sequentially.
Optional environment: CONTAINER_ENGINE=podman|docker, CONTAINER_CPUS=8,
CONTAINER_MEMORY=48g, CONTAINER_CPUSET=0-7, BUILD_JOBS=8.
Without CONTAINER_CPUSET, CPU affinity is limited to the smaller CPU/job budget
using CPUs permitted by the host. An explicit CPU set overrides that selection.
The container runs as root; completed archives are returned to the invoking UID/GID.
USAGE
}

inside_container() {
	[[ ${FLUXER_ELECTRON_CONTAINER:-} == 1 && "$PWD" == /work ]] || die 'Internal container invocation is invalid.'
	export DEPOT_TOOLS_UPDATE=0
	export DEPOT_TOOLS_METRICS=0
	export GIT_TERMINAL_PROMPT=0
	export GIT_CACHE_PATH=/work/git-cache
	export PATH="/work/depot_tools:$PATH"
	local arch

	checkout_pinned depot_tools https://chromium.googlesource.com/chromium/tools/depot_tools.git "$DEPOT_TOOLS_REVISION"
	[[ -z $(git -C depot_tools status --porcelain --untracked-files=no) ]] || die 'Cached depot_tools contains tracked edits.'
	DEPOT_TOOLS_DIR=/work/depot_tools /work/depot_tools/ensure_bootstrap
	[[ -s depot_tools/python3_bin_reldir.txt ]] || die 'Pinned depot_tools Python bootstrap did not complete.'
	depot_tools/python-bin/python3 --version

	checkout_pinned src/electron https://github.com/electron/electron.git "$ELECTRON_REVISION"
	python3 - "$SCRIPT_DIR/patches/$PATCH_NAME" "src/electron/patches/chromium/$PATCH_NAME" <<'PY'
import pathlib
import shutil
import subprocess
import sys

source, destination = map(pathlib.Path, sys.argv[1:])
allowed = {"patches/chromium/.patches", "patches/chromium/" + destination.name}
changes = subprocess.check_output(
    ["git", "-C", "src/electron", "status", "--porcelain=v1", "-z"], text=True
).split("\0")
for change in filter(None, changes):
    if change[:2].strip() not in {"M", "??"} or change[3:] not in allowed:
        raise SystemExit("Cached Electron has unrelated edits: " + repr(change))
manifest = destination.parent / ".patches"
upstream = subprocess.check_output(
    ["git", "-C", "src/electron", "show", "HEAD:patches/chromium/.patches"]
)
patched = upstream.rstrip(b"\n") + b"\n" + destination.name.encode() + b"\n"
if manifest.read_bytes() not in (upstream, patched):
    raise SystemExit("Cached Chromium patch manifest differs from the pinned upstream series")
shutil.copyfile(source, destination)
manifest.write_bytes(patched)
PY
	gclient config --name src/electron --unmanaged --custom-var=checkout_arm64=True https://github.com/electron/electron
	gclient sync --no-history --with_branch_heads --with_tags --jobs "$BUILD_JOBS"
	git -C src apply --reverse --check "$SCRIPT_DIR/patches/$PATCH_NAME"
	for arch in "$@"; do
		python3 src/electron/script/pgo/download-profiles.py --targets "linux-$arch,v8-builtins"
		(
			cd src
			gn gen "out/Release-$arch" --fail-on-unused-args --args="import(\"//electron/build/args/release.gn\") target_cpu=\"$arch\" override_electron_version=\"$ELECTRON_VERSION\" use_remoteexec=false"
			python3 - "$arch" <<'PY'
import json
import subprocess
import sys

arch = sys.argv[1]
arguments = json.loads(subprocess.check_output(
    ["gn", "args", "out/Release-" + arch, "--list", "--short", "--json"], text=True
))
resolved = {
    argument["name"]: argument.get("current", argument["default"])["value"]
    for argument in arguments
}
expected = {
    "target_cpu": json.dumps(arch),
    "use_vaapi": "true",
    "enable_platform_hevc": "true",
    "enable_hevc_parser_and_hw_decoder": "true",
}
for name, value in expected.items():
    if resolved.get(name) != value:
        raise SystemExit(f"Electron build requires {name} = {value}; resolved {resolved.get(name)!r}")
    print(f"Verified GN argument: {name} = {value}", flush=True)
PY
			autoninja -C "out/Release-$arch" -j "$BUILD_JOBS" electron:electron_dist_zip
			python3 electron/script/zip_manifests/check-zip-manifest.py "out/Release-$arch/dist.zip" "electron/script/zip_manifests/dist_zip.linux.$arch.manifest"
		)
		python3 - "$arch" "$ELECTRON_VERSION" "$ELECTRON_REVISION" "$SCRIPT_DIR/patches/$PATCH_NAME" <<'PY'
import hashlib
import json
import os
import pathlib
import shutil
import struct
import sys
import tempfile
import zipfile

arch, version, revision, patch_name = sys.argv[1:]
source = pathlib.Path("src/out/Release-" + arch + "/dist.zip")
with zipfile.ZipFile(source) as archive:
    if archive.testzip() is not None:
        raise SystemExit("Built Electron ZIP has a corrupt member")
    with archive.open("electron") as executable:
        header = executable.read(64)
        if len(header) != 64 or header[:6] != b"\x7fELF\x02\x01" or struct.unpack_from("<H", header, 18)[0] != {"x64": 62, "arm64": 183}[arch]:
            raise SystemExit("Built Electron executable has the wrong ELF architecture")
        marker = b"failed to synchronize VA surface before output"
        tail = header
        has_patch_marker = marker in header
        while chunk := executable.read(1024 * 1024):
            window = tail + chunk
            has_patch_marker = has_patch_marker or marker in window
            tail = window[-(len(marker) - 1):]
        if not has_patch_marker:
            raise SystemExit("Built Electron executable is missing the VA-API synchronization patch marker")
    if archive.read("version").decode().strip() != version:
        raise SystemExit("Built Electron version differs from the pinned version")

def sha256(path):
    digest = hashlib.sha256()
    with open(path, "rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()

output = pathlib.Path("/artifacts")
name = "electron-v" + version + "-linux-" + arch
archive_path = output / (name + ".zip")
manifest_path = output / (name + ".fluxer.json")
manifest = {
    "schemaVersion": 1,
    "electronVersion": version,
    "electronRevision": revision,
    "chromiumPatchSha256": sha256(patch_name),
    "platform": "linux",
    "arch": arch,
    "archiveName": archive_path.name,
    "archiveSha256": sha256(source),
}
for destination, contents in [(archive_path, None), (manifest_path, json.dumps(manifest, indent=2) + "\n")]:
    descriptor, temporary = tempfile.mkstemp(prefix="." + destination.name + ".", dir=output)
    try:
        with os.fdopen(descriptor, "wb") as stream:
            if contents is None:
                with source.open("rb") as runtime:
                    shutil.copyfileobj(runtime, stream)
            else:
                stream.write(contents.encode())
        os.chmod(temporary, 0o644)
        os.chown(temporary, int(os.environ["FLUXER_HOST_UID"]), int(os.environ["FLUXER_HOST_GID"]))
        os.replace(temporary, destination)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)
print("Prepared " + str(archive_path) + " and " + str(manifest_path), flush=True)
PY
	done
}

checkout_pinned() {
	local directory=$1 remote=$2 revision=$3 current
	if [[ ! -d $directory/.git ]]; then
		[[ ! -e $directory || -z $(ls -A "$directory") ]] || die "Source directory is not an empty Git checkout: $directory"
		git init -q "$directory"
	fi
	if ! git -C "$directory" remote get-url origin >/dev/null 2>&1; then
		git -C "$directory" remote add origin "$remote"
	fi
	[[ $(git -C "$directory" remote get-url origin) == "$remote" ]] || die "Cached Git remote differs: $directory"
	if ! current=$(git -C "$directory" rev-parse --verify HEAD 2>/dev/null); then
		git -C "$directory" fetch --depth=1 origin "$revision"
		git -C "$directory" checkout --detach FETCH_HEAD
		current=$(git -C "$directory" rev-parse HEAD)
	fi
	[[ $current == "$revision" ]] || die "Cached revision differs in $directory; choose a new build root."
}

if [[ ${1:-} == --inside-container ]]; then
	shift
	inside_container "$@"
	exit
fi

ARCH=all
BUILD_ROOT="${XDG_CACHE_HOME:-$HOME/.cache}/fluxer/electron-$ELECTRON_VERSION"
OUTPUT_DIR=''
CHECK_ONLY=0
while (($#)); do
	case $1 in
		--arch|--build-root|--output-dir)
			(($# >= 2)) || die "Missing value for $1"
			case $1 in
				--arch) ARCH=$2 ;;
				--build-root) BUILD_ROOT=$2 ;;
				--output-dir) OUTPUT_DIR=$2 ;;
			esac
			shift 2 ;;
		--check) CHECK_ONLY=1; shift ;;
		--help|-h) usage; exit ;;
		*) die "Unknown argument: $1" ;;
	esac
done
case $ARCH in
	all) ARCHES=(x64 arm64) ;;
	x64|arm64) ARCHES=("$ARCH") ;;
	*) die '--arch must be x64, arm64 or all.' ;;
esac
[[ $(uname -s) == Linux && $(uname -m) == x86_64 ]] || die 'Build on a Linux x64 host; macOS and emulated Linux builds are unsupported.'
for command in python3 flock; do command -v "$command" >/dev/null || die "Required command is unavailable: $command"; done
if [[ -z ${CONTAINER_ENGINE:-} ]]; then
	if command -v podman >/dev/null; then CONTAINER_ENGINE=podman; else CONTAINER_ENGINE=docker; fi
fi
case $CONTAINER_ENGINE in podman|docker) ;; *) die 'CONTAINER_ENGINE must be podman or docker.' ;; esac
command -v "$CONTAINER_ENGINE" >/dev/null || die "Required container engine is unavailable: $CONTAINER_ENGINE"
BUILD_JOBS=${BUILD_JOBS:-8}
CONTAINER_CPUS=${CONTAINER_CPUS:-8}
CONTAINER_MEMORY=${CONTAINER_MEMORY:-48g}
[[ $BUILD_JOBS =~ ^[1-9][0-9]*$ ]] || die 'BUILD_JOBS must be a positive integer.'
[[ $CONTAINER_CPUS =~ ^[1-9][0-9]*$ ]] || die 'CONTAINER_CPUS must be a positive integer.'
CONTAINER_CPUSET=$(python3 - "$CONTAINER_CPUS" "$BUILD_JOBS" "${CONTAINER_CPUSET:-}" <<'PY'
import os
import re
import sys

allowed = sorted(os.sched_getaffinity(0))
if not allowed:
    raise SystemExit("No CPUs are permitted by the host affinity")
explicit = sys.argv[3]
if explicit:
    if not re.fullmatch(r"[0-9]+(?:-[0-9]+)?(?:,[0-9]+(?:-[0-9]+)?)*", explicit):
        raise SystemExit("CONTAINER_CPUSET must be a comma-separated CPU list or ranges, e.g. 0-7,9")
    requested = set()
    for part in explicit.split(","):
        bounds = [int(value) for value in part.split("-")]
        first, last = bounds[0], bounds[-1]
        if first > last or last > allowed[-1]:
            raise SystemExit("CONTAINER_CPUSET contains an invalid or unavailable CPU range: " + part)
        requested.update(range(first, last + 1))
    unavailable = requested.difference(allowed)
    if unavailable:
        raise SystemExit("CONTAINER_CPUSET includes CPUs outside the host affinity: " + ",".join(map(str, sorted(unavailable))))
    print(explicit)
else:
    budget = min(int(sys.argv[1]), int(sys.argv[2]), len(allowed))
    print(",".join(map(str, allowed[:budget])))
PY
)

BUILD_ROOT=$(python3 -c 'import pathlib,sys; print(pathlib.Path(sys.argv[1]).expanduser().resolve())' "$BUILD_ROOT")
OUTPUT_DIR=${OUTPUT_DIR:-$BUILD_ROOT/artifacts}
OUTPUT_DIR=$(python3 -c 'import pathlib,sys; print(pathlib.Path(sys.argv[1]).expanduser().resolve())' "$OUTPUT_DIR")
python3 - "$BUILD_ROOT" "$CONTAINER_MEMORY" "$CONTAINER_CPUS" <<'PY'
import os
import pathlib
import re
import shutil
import sys

root = pathlib.Path(sys.argv[1])
if root == pathlib.Path("/") or root == pathlib.Path.home():
    raise SystemExit("Choose a dedicated build root, not / or your home directory")
probe = root
while not probe.exists():
    probe = probe.parent
free = shutil.disk_usage(probe).free / 2**30
if free < 300:
    raise SystemExit(f"Build filesystem has {free:.1f} GiB free; at least 300 GiB is required")
memory = dict(line.split(":", 1) for line in pathlib.Path("/proc/meminfo").read_text().splitlines())
total = int(memory["MemTotal"].split()[0]) * 1024
available = int(memory["MemAvailable"].split()[0]) * 1024
match = re.fullmatch(r"([1-9][0-9]*)([mgMG])", sys.argv[2])
if not match:
    raise SystemExit("CONTAINER_MEMORY must use whole megabytes or gigabytes, e.g. 48g")
limit = int(match[1]) * {"m": 2**20, "g": 2**30}[match[2].lower()]
if total < 31 * 2**30 or limit < 32 * 2**30 or available < limit:
    raise SystemExit(f"Insufficient RAM: host {total / 2**30:.1f} GiB total, {available / 2**30:.1f} GiB available, container limit {limit / 2**30:.1f} GiB; need at least a 32 GiB container limit and available RAM covering that limit")
if int(sys.argv[3]) > (os.cpu_count() or 1):
    raise SystemExit("CONTAINER_CPUS exceeds the host's logical CPU count")
print(f"Host ready: {free:.1f} GiB free, {available / 2**30:.1f} GiB available RAM; container limit {limit / 2**30:.1f} GiB", flush=True)
PY
"$CONTAINER_ENGINE" info >/dev/null
printf 'Engine: %s\nImage: %s\nBuild root: %s\nOutput: %s\nTargets: %s\nCPU set: %s\n' "$CONTAINER_ENGINE" "$BUILD_IMAGE" "$BUILD_ROOT" "$OUTPUT_DIR" "${ARCHES[*]}" "$CONTAINER_CPUSET"
((CHECK_ONLY == 0)) || exit

if [[ -d $BUILD_ROOT && ! -f $BUILD_ROOT/.fluxer-electron-build-id && -n $(ls -A "$BUILD_ROOT") ]]; then
	die 'Build root is not empty and has no ownership marker; choose a dedicated empty directory.'
fi
mkdir -p "$BUILD_ROOT" "$OUTPUT_DIR"
exec 9>"$BUILD_ROOT/.fluxer-electron-build.lock"
flock -n 9 || die 'Another build is using this build root.'
if [[ -f $BUILD_ROOT/.fluxer-electron-build-id ]]; then
	[[ $(cat "$BUILD_ROOT/.fluxer-electron-build-id") == "$ELECTRON_REVISION" ]] || die 'Build root belongs to a different Electron revision.'
else
	printf '%s\n' "$ELECTRON_REVISION" > "$BUILD_ROOT/.fluxer-electron-build-id"
fi
mkdir -p "$BUILD_ROOT/container-home" "$BUILD_ROOT/git-cache"
ENGINE_ARGS=(run --rm --platform linux/amd64 --user 0 --network host --workdir /work --cpus "$CONTAINER_CPUS" --cpuset-cpus "$CONTAINER_CPUSET" --memory "$CONTAINER_MEMORY")
ENGINE_ARGS+=(
	--volume "$BUILD_ROOT:/work"
	--volume "$OUTPUT_DIR:/artifacts"
	--volume "$BUILD_ROOT/container-home:/root"
	--volume "$SCRIPT_DIR:/fluxer-electron:ro"
	--env FLUXER_ELECTRON_CONTAINER=1
	--env "FLUXER_HOST_UID=$(id -u)"
	--env "FLUXER_HOST_GID=$(id -g)"
	--env "BUILD_JOBS=$BUILD_JOBS"
)
"$CONTAINER_ENGINE" pull "$BUILD_IMAGE"
"$CONTAINER_ENGINE" "${ENGINE_ARGS[@]}" "$BUILD_IMAGE" bash /fluxer-electron/build-linux.sh --inside-container "${ARCHES[@]}"
