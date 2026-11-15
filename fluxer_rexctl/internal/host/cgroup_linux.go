// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

const cgroupRoot = "/sys/fs/cgroup"

func cgroupLimits() (memoryBytes uint64, cpus int) {
	data, err := os.ReadFile("/proc/self/cgroup")
	if err != nil {
		return 0, 0
	}
	var own string
	for line := range strings.Lines(string(data)) {
		if rest, ok := strings.CutPrefix(strings.TrimSpace(line), "0::"); ok {
			own = rest
			break
		}
	}
	if own == "" {
		return 0, 0
	}
	dir := filepath.Join(cgroupRoot, own)
	for strings.HasPrefix(dir, cgroupRoot) {
		if v, ok := cgroupMemoryMax(filepath.Join(dir, "memory.max")); ok && (memoryBytes == 0 || v < memoryBytes) {
			memoryBytes = v
		}
		if n := cgroupCPUMax(filepath.Join(dir, "cpu.max")); n > 0 && (cpus == 0 || n < cpus) {
			cpus = n
		}
		if dir == cgroupRoot {
			break
		}
		dir = filepath.Dir(dir)
	}
	return memoryBytes, cpus
}

func cgroupMemoryMax(path string) (uint64, bool) {
	data, err := os.ReadFile(path)
	if err != nil {
		return 0, false
	}
	v, err := strconv.ParseUint(strings.TrimSpace(string(data)), 10, 64)
	return v, err == nil && v > 0
}

func cgroupCPUMax(path string) int {
	data, err := os.ReadFile(path)
	if err != nil {
		return 0
	}
	f := strings.Fields(string(data))
	if len(f) != 2 {
		return 0
	}
	quota, err1 := strconv.ParseUint(f[0], 10, 64)
	period, err2 := strconv.ParseUint(f[1], 10, 64)
	if err1 != nil || err2 != nil || period == 0 {
		return 0
	}
	return int((quota + period - 1) / period)
}
