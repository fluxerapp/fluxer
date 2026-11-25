// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"io/fs"
	"maps"
	"slices"
	"sync"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	CustomFile    = "custom.compose.yml"
	FluxerdReader = "fluxerd"
	BackupReader  = "backups"
	UpdateReader  = "updates"
)

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

func WithHeld(vars []string, chosen map[string]string) []string {
	out := slices.Clone(vars)
	for name := range chosen {
		if isRawName(name) && !slices.Contains(out, name) {
			out = append(out, name)
		}
	}
	slices.Sort(out)
	return out
}

var parsedFiles sync.Map

type parsedKey struct {
	set  *stack.Set
	name string
	data string
}

func parsedVars(key parsedKey, read func() ([]byte, error)) *stack.FileVars {
	if v, ok := parsedFiles.Load(key); ok {
		return v.(*stack.FileVars)
	}
	data, err := read()
	if err != nil {
		return nil
	}
	fv, err := stack.ParseVars(key.name, data)
	if err != nil {
		return nil
	}
	parsedFiles.Store(key, fv)
	return fv
}

func Services(set *stack.Set, customFile []byte, vars []string) []string {
	want := map[string]bool{}
	for _, v := range vars {
		want[v] = true
	}
	found := map[string]bool{}
	if set.Catalog != nil {
		for _, s := range set.Catalog {
			if want[s.Name] {
				for _, r := range s.Readers {
					switch r {
					case FluxerdReader:
						return slices.Clone(stack.Services)
					case BackupReader, UpdateReader:
						continue
					}
					found[r] = true
				}
			}
		}
		return slices.Sorted(maps.Keys(found))
	}
	add := func(fv *stack.FileVars) {
		if fv == nil {
			return
		}
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
			add(parsedVars(parsedKey{set: set, name: name}, func() ([]byte, error) { return fs.ReadFile(set.Files, name) }))
		}
	}
	if len(customFile) > 0 {
		add(parsedVars(parsedKey{name: CustomFile, data: string(customFile)}, func() ([]byte, error) { return customFile, nil }))
	}
	return slices.Sorted(maps.Keys(found))
}
