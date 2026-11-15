// SPDX-License-Identifier: AGPL-3.0-or-later

package sizing

import (
	"bufio"
	"maps"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
)

type Host struct {
	CPUs        int
	MemoryBytes uint64
}

var small = map[string]string{
	"FLUXER_GIFS_SHARD_CACHE_MAX_BYTES":                          "67108864",
	"FLUXER_LIVEKIT_GOMEMLIMIT":                                  "272MiB",
	"FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES":                   "67108864",
	"FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_TOTAL_BYTES": "67108864",
	"FLUXER_MEILISEARCH_MAX_INDEXING_MEMORY":                     "128mb",
	"FLUXER_MEILISEARCH_MAX_INDEXING_THREADS":                    "1",
	"FLUXER_POSTGRES_EFFECTIVE_CACHE_SIZE":                       "768MB",
	"FLUXER_POSTGRES_SHARED_BUFFERS":                             "192MB",
	"FLUXER_POSTGRES_WORK_MEM":                                   "4MB",
	"FLUXER_SEAWEEDFS_GOMEMLIMIT":                                "380MiB",
	"FLUXER_VALKEY_MAXMEMORY":                                    "128mb",
}

const (
	apiPool       = 15
	usersPool     = 4
	messagesPool  = 4
	apiHeapMiB    = 640
	kvBytesFactor = 4
)

func Defaults(h Host) map[string]string {
	out := maps.Clone(small)
	c := max(h.CPUs, 1)
	threads := min(c, clamp((c+3)/4, 2, 4))
	out["FLUXER_TOKIO_WORKER_THREADS"] = strconv.Itoa(threads)
	out["FLUXER_MEDIA_PROXY_TOKIO_WORKER_THREADS"] = strconv.Itoa(min(c, clamp((c+1)/2, 2, 8)))
	out["FLUXER_RUST_SERVER_TOKIO_WORKER_THREADS"] = strconv.Itoa(min(c, clamp((c+1)/2, 2, 8)))
	out["FLUXER_MALLOC_ARENA_MAX"] = strconv.Itoa(max(2, threads))
	out["FLUXER_API_POSTGRES_MAX_CONNECTIONS"] = strconv.Itoa(apiPool)
	out["FLUXER_USERS_POSTGRES_MAX_CONNECTIONS"] = strconv.Itoa(usersPool)
	out["FLUXER_MESSAGES_POSTGRES_MAX_CONNECTIONS"] = strconv.Itoa(messagesPool)
	out["FLUXER_API_NODE_HEAP_MB"] = strconv.Itoa(apiHeapMiB)
	out["FLUXER_KV_POSTGRES_MAX_CONNECTIONS"] = strconv.Itoa(apiPool)
	if b, ok := megabytes(small["FLUXER_VALKEY_MAXMEMORY"]); ok {
		out["FLUXER_KV_POSTGRES_MAX_BYTES"] = strconv.FormatUint(b*kvBytesFactor, 10)
	}
	return out
}

func Schedulers(h Host, env map[string]string) (schedulers, dirtyCPU, dirtyIO int) {
	lo := positive(env["FLUXER_ERLANG_SCHEDULERS_MIN"], 2)
	hi := positive(env["FLUXER_ERLANG_SCHEDULERS_MAX"], 16)
	schedulers = positive(env["FLUXER_ERLANG_SCHEDULERS"], clamp(max(h.CPUs, 1), lo, hi))
	dirtyCPU = positive(env["FLUXER_ERLANG_DIRTY_CPU_SCHEDULERS"], clamp(schedulers/2, 1, schedulers))
	dirtyIO = positive(env["FLUXER_ERLANG_DIRTY_IO_SCHEDULERS"], clamp(schedulers, 2, 4))
	return schedulers, dirtyCPU, dirtyIO
}

func Detect() Host {
	return detect("/proc/self/cgroup", "/sys/fs/cgroup", "/proc/meminfo", runtime.NumCPU())
}

func detect(selfCgroup, cgroupRoot, meminfo string, cpus int) Host {
	h := Host{CPUs: cpus, MemoryBytes: memTotal(meminfo)}
	for _, dir := range cgroupDirs(selfCgroup, cgroupRoot) {
		if n := quotaCPUs(dir); n > 0 && n < h.CPUs {
			h.CPUs = n
		}
		if b := memoryLimit(dir); b > 0 && (h.MemoryBytes == 0 || b < h.MemoryBytes) {
			h.MemoryBytes = b
		}
	}
	return h
}

func cgroupDirs(selfCgroup, root string) []string {
	dirs := []string{root, filepath.Join(root, "cpu"), filepath.Join(root, "cpu,cpuacct"), filepath.Join(root, "memory")}
	data, err := os.ReadFile(selfCgroup)
	if err != nil {
		return dirs
	}
	for line := range strings.Lines(string(data)) {
		rel, ok := strings.CutPrefix(strings.TrimSpace(line), "0::")
		if !ok {
			continue
		}
		for d := filepath.Join(root, rel); d != root && strings.HasPrefix(d, root); d = filepath.Dir(d) {
			dirs = append(dirs, d)
		}
	}
	return dirs
}

func quotaCPUs(dir string) int {
	var quota, period string
	if f, err := os.ReadFile(filepath.Join(dir, "cpu.max")); err == nil {
		fields := strings.Fields(string(f))
		if len(fields) == 2 {
			quota, period = fields[0], fields[1]
		}
	} else {
		quota = readTrim(filepath.Join(dir, "cpu.cfs_quota_us"))
		period = readTrim(filepath.Join(dir, "cpu.cfs_period_us"))
	}
	q, p := positive(quota, 0), positive(period, 0)
	if q == 0 || p == 0 {
		return 0
	}
	return (q + p - 1) / p
}

func memoryLimit(dir string) uint64 {
	for _, name := range []string{"memory.max", "memory.limit_in_bytes"} {
		if n, err := strconv.ParseUint(readTrim(filepath.Join(dir, name)), 10, 64); err == nil && n > 0 && n < 1<<62 {
			return n
		}
	}
	return 0
}

func memTotal(meminfo string) uint64 {
	f, err := os.Open(meminfo)
	if err != nil {
		return 0
	}
	defer f.Close()
	s := bufio.NewScanner(f)
	for s.Scan() {
		fields := strings.Fields(s.Text())
		if len(fields) >= 2 && fields[0] == "MemTotal:" {
			if kib, err := strconv.ParseUint(fields[1], 10, 64); err == nil {
				return kib << 10
			}
		}
	}
	return 0
}

func readTrim(path string) string {
	b, err := os.ReadFile(path)
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(b))
}

func megabytes(s string) (uint64, bool) {
	n, err := strconv.ParseUint(strings.TrimSuffix(strings.ToLower(s), "mb"), 10, 64)
	return n << 20, err == nil
}

func positive(s string, fallback int) int {
	if n, err := strconv.Atoi(strings.TrimSpace(s)); err == nil && n > 0 {
		return n
	}
	return fallback
}

func clamp(v, lo, hi int) int {
	if v < lo {
		v = lo
	}
	if v > hi {
		v = hi
	}
	return v
}
