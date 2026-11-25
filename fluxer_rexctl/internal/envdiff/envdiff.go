// SPDX-License-Identifier: AGPL-3.0-or-later

package envdiff

import (
	"cmp"
	"fmt"
	"maps"
	"slices"
	"strings"
)

const (
	prefix     = "FLUXER_SERVER_"
	separator  = "__"
	fileSuffix = "_FILE"
)

func key(svc, key string) string {
	return prefix + strings.ToUpper(strings.ReplaceAll(svc, "-", "_")) + separator + key
}

func Encode(envs map[string]map[string]string, process []string) (map[string]string, error) {
	services := slices.Sorted(maps.Keys(envs))
	keys := map[string]bool{}
	for _, svc := range services {
		if svc == "" || strings.Contains(svc, separator) {
			return nil, fmt.Errorf("envdiff: bad service name %q", svc)
		}
		for k := range envs[svc] {
			if strings.HasPrefix(k, prefix) {
				return nil, fmt.Errorf("envdiff: %s sets %s, and %s is reserved for the merged process", svc, k, prefix)
			}
			if !slices.Contains(process, k) {
				keys[k] = true
			}
		}
	}
	out := map[string]string{}
	overrides := map[string]map[string]string{}
	for k := range keys {
		v, all := majority(envs, services, k)
		if v != "" {
			out[k] = v
		}
		if all {
			continue
		}
		for _, svc := range services {
			if envs[svc][k] != v {
				if overrides[svc] == nil {
					overrides[svc] = map[string]string{}
				}
				overrides[svc][k] = envs[svc][k]
			}
		}
	}
	for svc, own := range overrides {
		for k := range maps.Clone(own) {
			s := sibling(k)
			if _, set := own[s]; !set && out[s] != "" {
				own[s] = envs[svc][s]
			}
		}
		for k, v := range own {
			out[key(svc, k)] = v
		}
	}
	return out, nil
}

func sibling(k string) string {
	if base, ok := strings.CutSuffix(k, fileSuffix); ok {
		return base
	}
	return k + fileSuffix
}

func majority(envs map[string]map[string]string, services []string, k string) (string, bool) {
	count := map[string]int{}
	for _, svc := range services {
		count[envs[svc][k]]++
	}
	values := slices.SortedFunc(maps.Keys(count), func(a, b string) int {
		return cmp.Or(cmp.Compare(count[b], count[a]), cmp.Compare(a, b))
	})
	return values[0], len(values) == 1
}
