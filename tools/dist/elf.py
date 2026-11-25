#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later
import os
import re
import shutil
import subprocess
import sys

BASE = {
    "libc.so.6", "libm.so.6", "libdl.so.2", "libpthread.so.0", "librt.so.1", "libresolv.so.2", "libutil.so.1",
    "ld-linux-x86-64.so.2", "ld-linux-aarch64.so.1",
    "libgcc_s.so.1", "libstdc++.so.6", "libatomic.so.1", "libz.so.1", "liblzma.so.5", "libbz2.so.1.0", "libtinfo.so.6",
}
CEILINGS = {"GLIBC": (2, 35), "GLIBCXX": (3, 4, 30), "CXXABI": (1, 3, 13), "GCC": (12, 0, 0)}
ROOTS = ("bin", "lib", "gateway")
BUILT = (
    ("libavcodec", "ffmpeg"), ("libavfilter", "ffmpeg"), ("libavformat", "ffmpeg"), ("libavutil", "ffmpeg"),
    ("libswresample", "ffmpeg"), ("libswscale", "ffmpeg"), ("libheif", "libheif"), ("libdav1d", "dav1d"),
    ("libhwy", "highway"), ("libjxl", "libjxl"), ("libcgif", "cgif"), ("libvips", "libvips"),
)
INDEX = "share/LICENSES/libraries"
INDEX_HEAD = (
    "# Each library in lib/, the folder in share/LICENSES with its licence, and its source.\n"
    "# An Ubuntu source is a package and version from Ubuntu 22.04, at https://launchpad.net/ubuntu/+source/<package>/<version>.\n"
)


def die(msg):
    print(f"elf.py: {msg}", file=sys.stderr)
    sys.exit(1)


def is_elf(path):
    if os.path.islink(path) or not os.path.isfile(path):
        return False
    with open(path, "rb") as f:
        return f.read(4) == b"\x7fELF"


def elves(dist):
    for root in ROOTS:
        for dirpath, _, files in os.walk(os.path.join(dist, root)):
            for name in sorted(files):
                path = os.path.join(dirpath, name)
                if is_elf(path):
                    yield path


def readelf(path, *flags):
    return subprocess.run(["readelf", "-W", *flags, path], capture_output=True, text=True, check=True).stdout


def dynamic(path):
    out = readelf(path, "-d")
    needed = re.findall(r"\(NEEDED\)\s+Shared library: \[(.+?)\]", out)
    paths = re.findall(r"\((?:RUNPATH|RPATH)\)\s+Library (?:runpath|rpath): \[(.*?)\]", out)
    return needed, paths


def ldd(path):
    r = subprocess.run(["ldd", path], capture_output=True, text=True)
    if r.returncode != 0:
        return {}
    found = {}
    for line in r.stdout.splitlines():
        m = re.match(r"\s*(\S+) => (.+?) \(0x", line)
        if m:
            found[m.group(1)] = m.group(2)
        elif "=> not found" in line:
            die(f"{path} needs {line.split()[0]}, which isn't found on this machine")
    return found


def origin_path(path, lib):
    rel = os.path.relpath(lib, os.path.dirname(path))
    return "$ORIGIN" if rel == "." else "$ORIGIN/" + rel


def owner(path):
    candidates = [path]
    for a, b in (("/usr/lib/", "/lib/"), ("/lib/", "/usr/lib/")):
        if path.startswith(a):
            candidates.append(b + path[len(a):])
    for candidate in candidates:
        r = subprocess.run(["dpkg-query", "-S", candidate], capture_output=True, text=True)
        if r.returncode == 0:
            return r.stdout.split(":", 1)[0].split(",")[0].strip()
    return None


def licence(dist, soname, resolved):
    real = os.path.realpath(resolved)
    pkg = owner(real) or owner(resolved)
    if pkg:
        copyright_file = f"/usr/share/doc/{pkg}/copyright"
        if not os.path.isfile(copyright_file):
            die(f"{soname} comes from {pkg}, which has no {copyright_file}")
        dest = os.path.join(dist, "share", "LICENSES", pkg)
        os.makedirs(dest, exist_ok=True)
        shutil.copyfile(copyright_file, os.path.join(dest, "copyright"))
        source = subprocess.run(["dpkg-query", "-W", "-f=${source:Package} ${source:Version}", pkg],
                                capture_output=True, text=True, check=True).stdout.strip()
        return f"{pkg} ubuntu {source}"
    for prefix, project in BUILT:
        if soname.startswith(prefix):
            return f"{project} built from source"
    die(f"{soname} from {real} belongs to no Ubuntu package and to nothing tools/dist builds, so its licence is unknown")


def read_index(dist):
    try:
        with open(os.path.join(dist, INDEX)) as f:
            lines = f.read().splitlines()
    except FileNotFoundError:
        return {}
    return dict(line.split(" ", 1) for line in lines if line and not line.startswith("#"))


def write_index(dist, index):
    path = os.path.join(dist, INDEX)
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(INDEX_HEAD)
        for soname in sorted(index):
            f.write(f"{soname} {index[soname]}\n")


def bundle(dist):
    lib = os.path.join(dist, "lib")
    os.makedirs(lib, exist_ok=True)
    index = read_index(dist)
    pending = list(elves(dist))
    while pending:
        path = pending.pop()
        for soname, resolved in ldd(path).items():
            target = os.path.join(lib, soname)
            if soname in BASE or os.path.exists(target):
                continue
            index[soname] = licence(dist, soname, resolved)
            shutil.copyfile(os.path.realpath(resolved), target)
            os.chmod(target, 0o755)
            pending.append(target)
    vendored = set(os.listdir(lib))
    write_index(dist, {k: v for k, v in index.items() if k in vendored})
    for path in elves(dist):
        needed, paths = dynamic(path)
        if vendored & set(needed):
            want = origin_path(path, lib)
            if paths != [want]:
                subprocess.run(["patchelf", "--set-rpath", want, path], check=True)
        elif paths:
            subprocess.run(["patchelf", "--remove-rpath", path], check=True)


def versions(path):
    found = set()
    for m in re.finditer(r"Name: ([A-Za-z_]+)_([0-9][0-9.]*)\s", readelf(path, "-V")):
        found.add((m.group(1), m.group(2)))
    return found


def check(dist):
    lib = os.path.join(dist, "lib")
    vendored = set(os.listdir(lib)) if os.path.isdir(lib) else set()
    problems = []
    for path in elves(dist):
        rel = os.path.relpath(path, dist)
        needed, paths = dynamic(path)
        for soname in needed:
            if soname not in BASE and soname not in vendored:
                problems.append(f"{rel} needs {soname}, which is neither in lib/ nor a base library")
        if vendored & set(needed):
            dirs = [os.path.normpath(p.replace("$ORIGIN", os.path.dirname(path))) for p in ":".join(paths).split(":") if p]
            if os.path.normpath(lib) not in dirs:
                problems.append(f"{rel} needs libraries from lib/ but its RUNPATH is {':'.join(paths) or 'empty'}")
        for prefix, version in sorted(versions(path)):
            if prefix == "OPENSSL":
                problems.append(f"{rel} needs the system OpenSSL ({prefix}_{version})")
            ceiling = CEILINGS.get(prefix)
            if ceiling and tuple(int(x) for x in version.split(".")) > ceiling:
                limit = ".".join(map(str, ceiling))
                problems.append(f"{rel} needs {prefix}_{version}, above the {prefix}_{limit} floor of Ubuntu 22.04")
    if problems:
        die("the dist won't run on every supported release:\n  " + "\n  ".join(problems))
    index = read_index(dist)
    for soname in sorted(vendored):
        folder = index.get(soname, "").split(" ", 1)[0]
        licences = os.path.join(dist, "share", "LICENSES", folder)
        if not folder or not os.path.isdir(licences) or not os.listdir(licences):
            problems.append(f"lib/{soname} has no licence in share/LICENSES")
    if problems:
        die("the dist ships libraries without their licences:\n  " + "\n  ".join(problems))


def main():
    if len(sys.argv) != 3 or sys.argv[1] not in ("bundle", "check"):
        print("usage: elf.py bundle|check <dist-dir>", file=sys.stderr)
        sys.exit(64)
    dist = os.path.abspath(sys.argv[2])
    if sys.argv[1] == "bundle":
        bundle(dist)
    else:
        check(dist)


if __name__ == "__main__":
    main()
