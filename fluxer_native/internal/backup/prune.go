// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
)

type Archive struct {
	Path     string
	Manifest *Manifest
}

func List(dir, reason string) []Archive {
	ents, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var out []Archive
	for _, e := range ents {
		if !e.Type().IsRegular() || !strings.HasSuffix(e.Name(), Extension) || strings.HasPrefix(e.Name(), ".") {
			continue
		}
		p := filepath.Join(dir, e.Name())
		m, err := ReadManifest(p)
		if err != nil || m.Reason != reason {
			continue
		}
		out = append(out, Archive{Path: p, Manifest: m})
	}
	slices.SortFunc(out, func(a, b Archive) int { return b.Manifest.CreatedAt.Compare(a.Manifest.CreatedAt) })
	return out
}

func ReadManifest(path string) (*Manifest, error) {
	data, err := readTrailer(path)
	if err != nil {
		return nil, err
	}
	return parseManifest(data)
}

func Prune(dir, reason string, keep int) error {
	archives := List(dir, reason)
	if len(archives) <= keep {
		return nil
	}
	for _, a := range archives[keep:] {
		if err := os.Remove(a.Path); err != nil {
			return err
		}
	}
	return nil
}
