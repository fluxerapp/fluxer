// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"fmt"
	"path/filepath"
	goruntime "runtime"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const movedDataName = "fluxer-data"

var dataFolderOrder = []string{stack.SeaweedFSVolume, stack.ObjectsVolume, "postgres-data"}

func (t *moveBase) pickDataFolder() error {
	if t.runtime != instance.RuntimeContainer {
		return nil
	}
	delete(t.chosen, settings.DataFolder)
	dir, proposed := "", false
	switch {
	case t.data != nil:
		dir = *t.data
	case goruntime.GOOS == "linux":
		dir, proposed = proposedDataFolder(t.mounts), true
	}
	if dir == "" {
		return nil
	}
	refuse := func(msg, fix string) error {
		if proposed {
			fix = fmt.Sprintf("Pass --data-folder with an empty folder on that disk, or --data-folder=\"\" to keep the data in a volume of %s instead.", engineName(t.inst))
		}
		return ui.Refused(msg, fix)
	}
	if msg, fix := checks.NewDataFolderProblem(dir); msg != "" {
		return refuse(msg, fix)
	}
	var used uint64
	for _, m := range t.mounts {
		if m.Bind == "" {
			continue
		}
		if n, err := backup.DirSize(m.Bind); err == nil {
			used += n
		}
	}
	if free, err := backup.FreeSpace(dir); err == nil && free < used*11/10 {
		return refuse(fmt.Sprintf("%s has %s free, and the data the old containers kept in folders needs %s.", dir, ui.Bytes(int64(free)), ui.Bytes(int64(used*11/10))), "Free up space on that disk, or pass --data-folder with a folder on a bigger disk.")
	}
	t.chosen[settings.DataFolder] = dir
	return nil
}

func proposedDataFolder(mounts []migrate.DataMount) string {
	var binds []string
	for _, short := range dataFolderOrder {
		dm := stack.DataMounts[short]
		for _, m := range mounts {
			if m.Bind != "" && m.Service == dm.Service && m.Target == dm.Target {
				binds = append(binds, m.Bind)
			}
		}
	}
	for _, m := range mounts {
		if m.Bind != "" && !slices.Contains(binds, m.Bind) {
			binds = append(binds, m.Bind)
		}
	}
	if len(binds) == 0 {
		return ""
	}
	parent := filepath.Dir(binds[0])
	for n := 1; n < 10; n++ {
		dir := filepath.Join(parent, movedDataName)
		if n > 1 {
			dir += "-" + strconv.Itoa(n)
		}
		if msg, _ := checks.NewDataFolderProblem(dir); msg == "" || !folderHoldsData(dir) {
			return dir
		}
	}
	return filepath.Join(parent, movedDataName)
}

func dataLines(items []ui.PlanItem, target migrate.MoveTarget) {
	t, ok := target.(*containerTarget)
	if !ok {
		return
	}
	dir := t.chosen[settings.DataFolder]
	if t.compose && len(target.Mounts()) > 0 {
		items[len(items)-2].Detail = "the new files keep everything in Fluxer's volume, also what the old containers kept in folders on this machine"
		if dir != "" {
			items[len(items)-2].Detail = "the new files keep everything in " + dir
		}
	}
	if dir == "" {
		return
	}
	for i, item := range items {
		if strings.HasPrefix(item.Text, "Create "+target.Dir()+" ") {
			items[i].Detail = "with its data in " + dir
		}
	}
}
