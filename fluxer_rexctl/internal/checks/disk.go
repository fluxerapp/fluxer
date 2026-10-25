// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"
	"maps"
	"os"
	"slices"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

const (
	diskNeededInstall = 20 << 30
	diskLow           = 10 << 30
	diskCritical      = 5 << 30
	imageGuess        = 1 << 30
	pullHeadroomPct   = 125
)

func init() {
	Register(Check{ID: "disk", Phases: Preinstall | Installed, Run: runDisk})
}

func runDisk(ctx context.Context, e *Env) Result {
	free, ok := e.freeDisk()
	if !ok {
		return skip("Couldn't read how much free disk space this machine has.")
	}
	if !e.installed() {
		if free < diskNeededInstall {
			return fail(fmt.Sprintf("This machine has %s of free disk space and Fluxer needs at least 20 GB.", freeGB(free)),
				"Free up space on this machine, then run rexctl doctor.")
		}
		return pass(fmt.Sprintf("This machine has %s of free disk space.", freeGB(free)))
	}
	switch {
	case free < diskCritical:
		return fail(fmt.Sprintf("This machine has only %s of free disk space left.", freeGB(free)),
			"Free up space on this machine, then run rexctl doctor.")
	case free < diskLow:
		return warn(fmt.Sprintf("This machine has %s of free disk space left and an upgrade needs about 10 GB.", freeGB(free)),
			"Free up space on this machine before you upgrade.")
	}
	return pass(fmt.Sprintf("This machine has %s of free disk space.", freeGB(free)))
}

func (e *Env) freeDisk() (uint64, bool) {
	paths := []string{e.Dir}
	if store := e.imageStore(); store != e.Dir {
		paths = append(paths, store)
	}
	var lowest uint64
	found := false
	for _, p := range paths {
		free, err := host.FreeDisk(p)
		if err != nil {
			continue
		}
		if !found || free < lowest {
			lowest, found = free, true
		}
	}
	return lowest, found
}

func (e *Env) imageStore() string {
	if e.Engine != nil && e.Engine.DataRoot != "" {
		if _, err := os.Stat(e.Engine.DataRoot); err == nil {
			return e.Engine.DataRoot
		}
	}
	return e.Dir
}

type Space struct {
	Path   string
	Images bool
	Need   uint64
	Free   uint64
}

func (s Space) Short() bool { return s.Free < s.Need }

func (e *Env) Space(pull, backup uint64) []Space {
	wants := []Space{{Path: e.imageStore(), Images: true, Need: pull}, {Path: e.Dir}}
	if e.Instance != nil {
		wants = append(wants, Space{Path: e.Instance.BackupDir(), Need: backup})
	}
	var out []Space
	disks := map[string]int{}
	for _, w := range wants {
		id, err := host.DiskID(w.Path)
		if err != nil {
			id = w.Path
		}
		if i, ok := disks[id]; ok {
			out[i].Need += w.Need
			continue
		}
		free, err := host.FreeDisk(w.Path)
		if err != nil {
			continue
		}
		disks[id] = len(out)
		w.Free = free
		w.Need += diskCritical
		out = append(out, w)
	}
	return out
}

func PullNeed(ctx context.Context, e *engine.Engine, cur, next map[string]string) (uint64, error) {
	services := slices.Collect(maps.Keys(next))
	if next == nil {
		services = slices.Collect(maps.Keys(cur))
	}
	var need uint64
	seen := map[string]bool{}
	for _, svc := range services {
		if ref := next[svc]; ref != "" {
			if seen[ref] {
				continue
			}
			seen[ref] = true
			_, found, err := e.LocalImage(ctx, ref)
			if err != nil {
				return 0, err
			}
			if found {
				continue
			}
		}
		size, err := imageSize(ctx, e, cur[svc])
		if err != nil {
			return 0, err
		}
		need += size
	}
	return need * pullHeadroomPct / 100, nil
}

func imageSize(ctx context.Context, e *engine.Engine, ref string) (uint64, error) {
	if ref == "" {
		return imageGuess, nil
	}
	size, found, err := e.ImageSize(ctx, ref)
	if err != nil || !found {
		return imageGuess, err
	}
	return size, nil
}

func freeGB(bytes uint64) string {
	return fmt.Sprintf("%d GB", bytes>>30)
}
