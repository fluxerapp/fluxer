// SPDX-License-Identifier: AGPL-3.0-or-later

package release

import (
	"os"
	"path/filepath"
	"slices"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestPruneReleases(t *testing.T) {
	images := map[string][]string{
		stack.LegacyRelease: {"fluxer:legacy", "postgres:16"},
		"2026.1.0":          {"fluxer:1", "postgres:17"},
		"2026.2.0":          {"fluxer:2", "postgres:17"},
		"2026.3.0":          {"fluxer:3", "postgres:17"},
		"2026.4.0":          {"fluxer:4", "postgres:17"},
	}
	cases := []struct {
		name     string
		state    instance.State
		remove   []string
		kept     []string
		legacyOn bool
	}{
		{
			name:   "current and previous",
			state:  instance.State{Release: "2026.4.0", PreviousRelease: "2026.3.0"},
			remove: []string{"fluxer:1", "fluxer:2", "fluxer:legacy", "postgres:16"},
			kept:   []string{"2026.3.0", "2026.4.0"},
		},
		{
			name:     "legacy is the rollback target",
			state:    instance.State{Release: "2026.1.0", PreviousRelease: stack.LegacyRelease},
			remove:   []string{"fluxer:2", "fluxer:3", "fluxer:4"},
			kept:     []string{"2026.1.0"},
			legacyOn: true,
		},
		{
			name: "unfinished upgrade",
			state: instance.State{Release: "2026.2.0", PreviousRelease: "2026.1.0",
				Upgrade: &instance.Upgrade{From: "2026.2.0", To: "2026.4.0"}},
			remove: []string{"fluxer:3", "fluxer:legacy", "postgres:16"},
			kept:   []string{"2026.1.0", "2026.2.0", "2026.4.0"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			inst := &instance.Instance{Dir: t.TempDir(), State: &tc.state}
			for v, refs := range images {
				if err := writeImages(Dir(inst, v), v, refs); err != nil {
					t.Fatal(err)
				}
			}
			remove, err := pruneReleases(inst)
			if err != nil {
				t.Fatal(err)
			}
			slices.Sort(remove)
			if !slices.Equal(remove, tc.remove) {
				t.Errorf("removed images %v, want %v", remove, tc.remove)
			}
			entries, err := os.ReadDir(filepath.Dir(Dir(inst, stack.LegacyRelease)))
			if err != nil {
				t.Fatal(err)
			}
			var dirs []string
			for _, d := range entries {
				if d.Name() != stack.LegacyRelease {
					dirs = append(dirs, d.Name())
				}
			}
			if !slices.Equal(dirs, tc.kept) {
				t.Errorf("release folders %v, want %v", dirs, tc.kept)
			}
			refs, err := Images(inst, stack.LegacyRelease)
			if tc.legacyOn != (err == nil && len(refs) > 0) {
				t.Errorf("legacy images %v (%v), want kept %v", refs, err, tc.legacyOn)
			}
			if !exists(Dir(inst, stack.LegacyRelease)) {
				t.Error("the legacy folder was removed")
			}
		})
	}
}
