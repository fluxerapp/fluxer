// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"bufio"
	"bytes"
	"context"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

type Info struct {
	OS            string
	Distro        string
	DistroVersion string
	Arch          string
	MemoryBytes   uint64
	FreeDiskBytes uint64
	Root          bool
	WSL           bool
}

func Detect(ctx context.Context, dir string) (*Info, error) {
	info := &Info{OS: runtime.GOOS, Arch: runtime.GOARCH, Root: isRoot()}
	if info.OS == "linux" {
		if data, err := os.ReadFile("/etc/os-release"); err == nil {
			info.Distro, info.DistroVersion = parseOSRelease(data)
		}
		if data, err := os.ReadFile("/proc/sys/kernel/osrelease"); err == nil {
			info.WSL = strings.Contains(strings.ToLower(string(data)), "microsoft")
		}
	}
	if mem, err := Memory(); err == nil {
		info.MemoryBytes = mem
	}
	if free, err := FreeDisk(existingParent(dir)); err == nil {
		info.FreeDiskBytes = free
	}
	return info, ctx.Err()
}

func isRoot() bool {
	return runtime.GOOS != "windows" && os.Geteuid() == 0
}

func parseOSRelease(data []byte) (id, version string) {
	s := bufio.NewScanner(bytes.NewReader(data))
	for s.Scan() {
		k, v, ok := strings.Cut(strings.TrimSpace(s.Text()), "=")
		if !ok {
			continue
		}
		v = strings.Trim(v, `"'`)
		switch k {
		case "ID":
			id = strings.ToLower(v)
		case "VERSION_ID":
			version = v
		}
	}
	return id, version
}

func existingParent(dir string) string {
	if dir == "" {
		dir = "."
	}
	p, err := filepath.Abs(dir)
	if err != nil {
		return dir
	}
	for {
		if _, err := os.Stat(p); err == nil {
			return p
		}
		parent := filepath.Dir(p)
		if parent == p {
			return p
		}
		p = parent
	}
}
