// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"maps"
	"slices"
	"strconv"
	"strings"
)

type Hardware struct {
	MemoryBytes uint64
	CPUs        int
}

const (
	mib              = 1 << 20
	smallHostBytes   = 7 << 30
	defaultWriterMiB = 7168
	minSpoolBytes    = 2 << 30
)

func Sizing(hw Hardware, memory bool, fixed map[string]string) map[string]string {
	out := map[string]string{}
	spool := uint64(minSpoolBytes)
	if body, err := strconv.ParseUint(strings.TrimSpace(fixed["FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES"]), 10, 64); err == nil {
		spool = max(spool, body)
	}
	out["FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES"] = strconv.FormatUint(spool, 10)
	if hw.CPUs <= 0 {
		return out
	}
	get := func(k string) string {
		if v := strings.TrimSpace(fixed[k]); v != "" {
			return v
		}
		return out[k]
	}
	num := func(k string, fallback int) int {
		if n, err := strconv.Atoi(get(k)); err == nil && n > 0 {
			return n
		}
		return fallback
	}
	size := func(k string, fallback int) int {
		if n := parseMiB(get(k)); n > 0 {
			return n
		}
		return fallback
	}

	c := hw.CPUs
	small := min(c, clamp((c+3)/4, 2, 4))
	setInt(out, "FLUXER_TOKIO_WORKER_THREADS", small)
	setInt(out, "FLUXER_MEDIA_PROXY_TOKIO_WORKER_THREADS", min(c, clamp((c+1)/2, 2, 8)))
	setInt(out, "FLUXER_MALLOC_ARENA_MAX", max(2, small))
	embedded := embeddedWorker(hw, fixed)
	if embedded {
		setInt(out, "FLUXER_API_POSTGRES_MAX_CONNECTIONS", clamp(3*c, 4, 25))
	} else {
		setInt(out, "FLUXER_API_POSTGRES_MAX_CONNECTIONS", clamp(2*c, 4, 25))
	}
	setInt(out, "FLUXER_WORKER_POSTGRES_MAX_CONNECTIONS", clamp(c, 2, 25))
	setInt(out, "FLUXER_USERS_POSTGRES_MAX_CONNECTIONS", clamp(c, 2, 20))
	setInt(out, "FLUXER_MESSAGES_POSTGRES_MAX_CONNECTIONS", clamp(c, 2, 20))
	clients := num("FLUXER_API_POSTGRES_MAX_CONNECTIONS", 25) +
		num("FLUXER_USERS_POSTGRES_MAX_CONNECTIONS", 20) + num("FLUXER_MESSAGES_POSTGRES_MAX_CONNECTIONS", 20)
	if !embedded {
		clients += num("FLUXER_WORKER_POSTGRES_MAX_CONNECTIONS", 25)
	}
	setInt(out, "FLUXER_POSTGRES_SERVER_MAX_CONNECTIONS", clients+10)
	setInt(out, "FLUXER_POSTGRES_AUTOVACUUM_MAX_WORKERS", clamp(c, 1, 3))
	gather := 0
	if c > 2 {
		gather = min(2, c/4)
	}
	setInt(out, "FLUXER_POSTGRES_MAX_PARALLEL_WORKERS_PER_GATHER", gather)
	setInt(out, "FLUXER_POSTGRES_MAX_PARALLEL_WORKERS", min(8, c))
	setInt(out, "FLUXER_MEILISEARCH_MAX_INDEXING_THREADS", clamp(c/4, 1, 4))

	if !memory || hw.MemoryBytes == 0 {
		return out
	}
	total := int(hw.MemoryBytes / mib)
	m := max(1024, (total-max(256, total/10))&^255)
	share := func(k string, percent, lo, hi int) int {
		v := clamp(m*percent/100, lo, hi)
		out[k] = strconv.Itoa(v) + "mb"
		return size(k, v)
	}
	reserve := func(k string, limit int) {
		out[k] = strconv.Itoa(limit*2/5) + "mb"
	}

	reserve("FLUXER_API_MEMORY_RESERVATION", share("FLUXER_API_MEMORY_LIMIT", 14, 512, 2560))
	reserve("FLUXER_WORKER_MEMORY_RESERVATION", share("FLUXER_WORKER_MEMORY_LIMIT", 12, 448, 2560))
	reserve("FLUXER_GATEWAY_MEMORY_RESERVATION", share("FLUXER_GATEWAY_MEMORY_LIMIT", 22, 512, 4096))
	share("FLUXER_CADDY_MEMORY_LIMIT", 8, 384, 2048)
	if hw.MemoryBytes < smallHostBytes {
		out["FLUXER_EDGE_PROTOCOLS"] = "h1 h2"
	}

	media := share("FLUXER_MEDIA_PROXY_MEMORY_LIMIT", 8, 320, 1024)
	setBytes(out, "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES", media/8)
	setBytes(out, "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_TOTAL_BYTES", media/4)
	setBytes(out, "FLUXER_GIFS_SHARD_CACHE_MAX_BYTES", min(clamp(m/64, 64, 128), size("FLUXER_GIFS_MEMORY_LIMIT", 256)/2))

	out["FLUXER_LIVEKIT_GOMEMLIMIT"] = strconv.Itoa(share("FLUXER_LIVEKIT_MEMORY_LIMIT", 6, 320, 2048)*85/100) + "MiB"
	out["FLUXER_NATS_GOMEMLIMIT"] = strconv.Itoa(share("FLUXER_NATS_MEMORY_LIMIT", 3, 192, 1024)*4/5) + "MiB"
	out["FLUXER_SEAWEEDFS_GOMEMLIMIT"] = strconv.Itoa(share("FLUXER_SEAWEEDFS_MEMORY_LIMIT", 8, 448, 2048)*85/100) + "MiB"
	out["FLUXER_MEILISEARCH_MAX_INDEXING_MEMORY"] = strconv.Itoa(share("FLUXER_MEILISEARCH_MEMORY_LIMIT", 15, 512, 4096)/3) + "mb"

	out["FLUXER_VALKEY_MEMORY_LIMIT"] = strconv.Itoa(clamp(m*45/1000+32, 224, 1568)) + "mb"
	out["FLUXER_VALKEY_MAXMEMORY"] = strconv.Itoa((size("FLUXER_VALKEY_MEMORY_LIMIT", 256)-32)*2/3) + "mb"

	pg := share("FLUXER_POSTGRES_MEMORY_LIMIT", 22, 448, 16384)
	reserve("FLUXER_POSTGRES_MEMORY_RESERVATION", pg)
	out["FLUXER_POSTGRES_SHARED_BUFFERS"] = strconv.Itoa(clamp(pg/4, 64, 4096)&^15) + "MB"
	buffers := size("FLUXER_POSTGRES_SHARED_BUFFERS", 512)
	out["FLUXER_POSTGRES_EFFECTIVE_CACHE_SIZE"] = strconv.Itoa(max(128, pg-buffers-64*c)) + "MB"
	work := clamp((pg-buffers)/(3*num("FLUXER_POSTGRES_SERVER_MAX_CONNECTIONS", 150)), 2, 32)
	out["FLUXER_POSTGRES_WORK_MEM"] = strconv.Itoa(work) + "MB"
	out["FLUXER_POSTGRES_MAINTENANCE_WORK_MEM"] = strconv.Itoa(clamp(pg/16, 32, 1024)) + "MB"
	out["FLUXER_POSTGRES_AUTOVACUUM_WORK_MEM"] = strconv.Itoa(clamp(pg/32, 16, 512)) + "MB"
	maintenance := size("FLUXER_POSTGRES_MAINTENANCE_WORK_MEM", 256)
	out["FLUXER_POSTGRES_SHM_SIZE"] = strconv.Itoa(64+maintenance+2*num("FLUXER_POSTGRES_MAX_PARALLEL_WORKERS_PER_GATHER", 0)*work) + "mb"
	if m < defaultWriterMiB {
		out["FLUXER_POSTGRES_BGWRITER_DELAY"] = "200ms"
		out["FLUXER_POSTGRES_BGWRITER_LRU_MAXPAGES"] = "100"
	}
	return out
}

const EmbeddedWorkerVar = "FLUXER_API_EMBEDDED_WORKER"

const SettingsLoadedVar = "FLUXER_SETTINGS_LOADED"

func embeddedWorker(hw Hardware, fixed map[string]string) bool {
	return strings.ToLower(strings.TrimSpace(fixed[EmbeddedWorkerVar])) == "true"
}

func FoldWorker(hw Hardware, env, fixed map[string]string) map[string]string {
	if !embeddedWorker(hw, fixed) {
		return nil
	}
	out := map[string]string{EmbeddedWorkerVar: "true", "FLUXER_WORKER_REPLICAS": "0"}
	size := func(k string) int {
		if n := parseMiB(fixed[k]); n > 0 {
			return n
		}
		return parseMiB(env[k])
	}
	for api, worker := range map[string]string{
		"FLUXER_API_MEMORY_LIMIT":       "FLUXER_WORKER_MEMORY_LIMIT",
		"FLUXER_API_MEMORY_RESERVATION": "FLUXER_WORKER_MEMORY_RESERVATION",
	} {
		if a, w := size(api), size(worker); a > 0 && w > 0 {
			out[api] = strconv.Itoa(a+w/2) + "mb"
		}
	}
	return out
}

func SizingKeys() []string {
	return slices.Sorted(maps.Keys(Sizing(Hardware{MemoryBytes: 4 << 30, CPUs: 2}, true, nil)))
}

func clamp(v, lo, hi int) int { return max(lo, min(hi, v)) }

func setInt(out map[string]string, k string, v int) { out[k] = strconv.Itoa(v) }

func setBytes(out map[string]string, k string, mebibytes int) {
	out[k] = strconv.FormatInt(int64(mebibytes)*mib, 10)
}

func parseMiB(s string) int {
	s = strings.ToLower(strings.TrimSpace(s))
	i := strings.IndexFunc(s, func(r rune) bool { return (r < '0' || r > '9') && r != '.' })
	if i == 0 {
		return 0
	}
	digits, unit := s, ""
	if i > 0 {
		digits, unit = s[:i], strings.TrimSpace(s[i:])
	}
	n, err := strconv.ParseFloat(digits, 64)
	if err != nil {
		return 0
	}
	switch strings.TrimSuffix(strings.TrimSuffix(unit, "b"), "i") {
	case "":
		return int(n / mib)
	case "k":
		return int(n / 1024)
	case "m":
		return int(n)
	case "g":
		return int(n * 1024)
	}
	return 0
}
