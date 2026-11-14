// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"strconv"
	"strings"
)

func ConvertLegacySettings(settings map[string]string) {
	migrateLiveKitPath(settings)
	migrateEdgeSiteAddress(settings)
	migrateSvcConcurrency(settings)
	migrateSvcCombined(settings)
}

func migrateLiveKitPath(settings map[string]string) {
	const key = "FLUXER_LIVEKIT_URL"
	v, ok := settings[key]
	if !ok {
		return
	}
	trimmed := strings.TrimRight(v, "/")
	if base, found := strings.CutSuffix(trimmed, "/gateway/livekit"); found {
		settings[key] = base + "/livekit"
	}
}

func migrateEdgeSiteAddress(settings map[string]string) {
	const oldKey, newKey = "FLUXER_CADDY_SITE_ADDRESS", "FLUXER_EDGE_SITE_ADDRESS"
	old, ok := settings[oldKey]
	if !ok {
		return
	}
	if current := settings[newKey]; current == "" && old != "" {
		settings[newKey] = old
	}
	delete(settings, oldKey)
}

func migrateSvcConcurrency(settings map[string]string) {
	const key = "FLUXER_SVC_MAX_CONCURRENT_REQUESTS"
	const floor = 320
	v, ok := settings[key]
	if !ok {
		return
	}
	n, err := strconv.Atoi(strings.TrimSpace(v))
	if err != nil || n < floor {
		delete(settings, key)
	}
}

func migrateSvcCombined(settings map[string]string) {
	const floorMiB = 256
	for _, name := range []string{"SNOWFLAKES", "USERS", "GIFS", "MESSAGES", "UNFURL"} {
		limitKey, shardLimitKey := "FLUXER_"+name+"_MEMORY_LIMIT", "FLUXER_"+name+"_SHARD_MEMORY_LIMIT"
		limit, hasLimit := settings[limitKey]
		shardLimit, hasShardLimit := settings[shardLimitKey]
		switch {
		case hasShardLimit && (!hasLimit || parseMiB(shardLimit) > parseMiB(limit)):
			settings[limitKey] = shardLimit
		case !hasShardLimit && hasLimit && parseMiB(limit) < floorMiB:
			delete(settings, limitKey)
		}
		delete(settings, shardLimitKey)

		poolKey, shardPoolKey := "FLUXER_"+name+"_POSTGRES_MAX_CONNECTIONS", "FLUXER_"+name+"_SHARD_POSTGRES_MAX_CONNECTIONS"
		if pool, ok := settings[shardPoolKey]; ok {
			if _, set := settings[poolKey]; !set {
				settings[poolKey] = pool
			}
			delete(settings, shardPoolKey)
		}
	}
}
