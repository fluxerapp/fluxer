// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"maps"
	"regexp"
	"slices"
	"strconv"
	"strings"

	"go.yaml.in/yaml/v4"
)

const (
	DiskProfileSmall  = "small"
	SmallDiskBytes    = 16_000_000_000
	defaultSpoolBytes = 2 << 30
	smallSpoolBytes   = 512 << 20

	LogMaxSizeVar  = "FLUXER_LOG_MAX_SIZE"
	LogMaxFileVar  = "FLUXER_LOG_MAX_FILE"
	MaxWALSizeVar  = "FLUXER_POSTGRES_MAX_WAL_SIZE"
	SpoolTotalVar  = "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES"
	natsStoreVar   = "FLUXER_NATS_MAX_FILE_STORE"
	uploadBodyVar  = "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES"
	smallLogSize   = "5m"
	smallLogFiles  = "2"
	smallMaxWALStr = "512MB"
)

func DiskProfileFor(capacity uint64) string {
	if capacity > 0 && capacity < SmallDiskBytes {
		return DiskProfileSmall
	}
	return ""
}

func DiskSizing(profile string, fixed map[string]string) map[string]string {
	out := map[string]string{}
	spool := uint64(defaultSpoolBytes)
	if profile == DiskProfileSmall {
		spool = smallSpoolBytes
		out[LogMaxSizeVar] = smallLogSize
		out[LogMaxFileVar] = smallLogFiles
		out[MaxWALSizeVar] = smallMaxWALStr
	}
	if body, err := strconv.ParseUint(strings.TrimSpace(fixed[uploadBodyVar]), 10, 64); err == nil {
		spool = max(spool, body)
	}
	out[SpoolTotalVar] = strconv.FormatUint(spool, 10)
	return out
}

func DiskSizingKeys() []string {
	return slices.Sorted(maps.Keys(DiskSizing(DiskProfileSmall, nil)))
}

func GrowthCap(set *Set, env map[string]string) uint64 {
	compose, err := ReadFile(set, ComposeFile)
	if err != nil {
		return 0
	}
	value := sizedValue(compose, env)
	files, _ := strconv.ParseUint(value(LogMaxFileVar), 10, 64)
	logs := uint64(loggedServices(compose)) * files * uint64(parseMiB(value(LogMaxSizeVar))) * mib
	wal := uint64(parseMiB(value(MaxWALSizeVar))) * mib
	spool, _ := strconv.ParseUint(value(SpoolTotalVar), 10, 64)
	nats := uint64(parseMiB(value(natsStoreVar))) * mib
	return logs + wal + spool + nats
}

func WALCap(set *Set, env map[string]string) uint64 {
	compose, err := ReadFile(set, ComposeFile)
	if err != nil {
		return 0
	}
	return uint64(parseMiB(sizedValue(compose, env)(MaxWALSizeVar))) * mib
}

func sizedValue(compose []byte, env map[string]string) func(string) string {
	return func(k string) string {
		if v := strings.TrimSpace(env[k]); v != "" {
			return v
		}
		return composeDefault(compose, k)
	}
}

func composeDefault(compose []byte, name string) string {
	m := regexp.MustCompile(`\$\{` + regexp.QuoteMeta(name) + `:-([^}]*)\}`).FindSubmatch(compose)
	if m == nil {
		return ""
	}
	return string(m[1])
}

func loggedServices(compose []byte) int {
	var doc struct {
		Services map[string]struct {
			Logging any `yaml:"logging"`
		} `yaml:"services"`
	}
	if err := yaml.Unmarshal(compose, &doc); err != nil {
		return 0
	}
	n := 0
	for _, s := range doc.Services {
		if s.Logging != nil {
			n++
		}
	}
	return n
}
