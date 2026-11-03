// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"maps"
	"slices"
)

type MemoryProfile string

const (
	ProfileSmall  MemoryProfile = "small"
	ProfileMedium MemoryProfile = "medium"
	ProfileLarge  MemoryProfile = "large"
)

func ProfileKeys(set *Set) []string {
	seen := map[string]bool{}
	for _, env := range set.Profiles {
		for k := range env {
			seen[k] = true
		}
	}
	return slices.Sorted(maps.Keys(seen))
}
