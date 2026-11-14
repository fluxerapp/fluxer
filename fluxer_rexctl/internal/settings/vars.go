// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"io/fs"
	"maps"
	"slices"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const CustomFile = "custom.compose.yml"

func Vars(set *stack.Set, customFile []byte) ([]string, error) {
	seen := map[string]bool{}
	for _, v := range set.Vars {
		seen[v] = true
	}
	if len(customFile) > 0 {
		fv, err := stack.ParseVars(CustomFile, customFile)
		if err != nil {
			return nil, err
		}
		for _, v := range fv.All {
			seen[v] = true
		}
	}
	return slices.Sorted(maps.Keys(seen)), nil
}

func Services(set *stack.Set, customFile []byte, vars []string) []string {
	want := map[string]bool{}
	for _, v := range vars {
		want[v] = true
	}
	found := map[string]bool{}
	add := func(fv *stack.FileVars) {
		for svc, used := range fv.Services {
			for _, v := range used {
				if want[v] {
					found[svc] = true
					break
				}
			}
		}
	}
	if names, err := stack.ComposeFileNames(set.Files); err == nil {
		for _, name := range names {
			data, err := fs.ReadFile(set.Files, name)
			if err != nil {
				continue
			}
			if fv, err := stack.ParseVars(name, data); err == nil {
				add(fv)
			}
		}
	}
	if len(customFile) > 0 {
		if fv, err := stack.ParseVars(CustomFile, customFile); err == nil {
			add(fv)
		}
	}
	return slices.Sorted(maps.Keys(found))
}
