// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	goruntime "runtime"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

const containerUID = 65532

const (
	localDiskFix = "Pick a folder on a local disk with a Linux file system like ext4, XFS, Btrfs or ZFS."
	forceFix     = " To use this folder anyway and take that risk, pass --force."
	moveDataFix  = "Move the data to a local disk with rexctl config set data.folder."
	ramDisk      = "a RAM disk"
)

var (
	folderAllowed = []string{"lost+found"}
	noOwnersFS    = []string{"FAT", "exFAT"}
)

func init() {
	Register(Check{ID: "data-folder", Phases: Preinstall | Installed, Run: runDataFolder})
}

func (e *Env) dataFolder() string {
	if e.Native && !e.Container {
		return ""
	}
	return strings.TrimSpace(e.chosen(settings.DataFolder))
}

func (e *Env) forcedDataFolder(dir string) bool {
	return e.ForceDataFolder || e.Instance != nil && e.Instance.State.ForcedFolder == dir
}

func runDataFolder(_ context.Context, e *Env) Result {
	dir := e.dataFolder()
	if dir == "" {
		return skip("")
	}
	if !e.installed() {
		if msg, fix := NewDataFolderProblem(dir, e.forcedDataFolder(dir)); msg != "" {
			return fail(msg, fix)
		}
		if fsName, _ := dataFS(dir); fsName != "" {
			return warn(fsProblem(dir, fsName)+" You chose this folder with --force.", localDiskFix)
		}
		return pass(fmt.Sprintf("Fluxer will keep its data in %s.", dir))
	}
	info, err := os.Stat(dir)
	switch {
	case errors.Is(err, fs.ErrNotExist):
		return fail(fmt.Sprintf("Fluxer's data folder %s is missing.", dir), "Mount the disk that holds it again, then run rexctl start.")
	case err != nil:
		return fail(fmt.Sprintf("rexctl can't open Fluxer's data folder %s.", dir), "Check that the disk that holds it is mounted and that you can read it.")
	case !info.IsDir():
		return fail(fmt.Sprintf("Fluxer's data folder %s isn't a folder.", dir), "Set data.folder to a folder with rexctl config set.")
	}
	if entries, err := os.ReadDir(dir); err == nil && !slices.ContainsFunc(entries, func(d os.DirEntry) bool { return !slices.Contains(folderAllowed, d.Name()) }) {
		return fail(fmt.Sprintf("Fluxer's data folder %s is empty.", dir), "If it's on another disk, mount that disk there again, then run rexctl start.")
	}
	var risky string
	if fsName, forcible := dataFS(dir); fsName != "" {
		if !forcible || !e.forcedDataFolder(dir) {
			return fail("Fluxer's data folder "+fsProblem(dir, fsName), moveDataFix)
		}
		risky = "Fluxer's data folder " + fsProblem(dir, fsName) + " You chose this folder with --force."
	}
	if uid, _, ok := host.OwnerOf(info); ok && uid != containerUID && e.Instance != nil && !e.Instance.State.Engine.Rootless() {
		return fail(fmt.Sprintf("Fluxer's data folder %s belongs to user %d, and Fluxer's container runs as user %d.", dir, uid, containerUID),
			fmt.Sprintf("Run chown -R %d:%d %s, then run rexctl restart.", containerUID, containerUID, dir))
	}
	if risky != "" {
		return warn(risky, moveDataFix)
	}
	return pass(fmt.Sprintf("Fluxer keeps its data in %s, on a local disk.", dir))
}

func NewDataFolderProblem(dir string, force bool) (msg, fix string) {
	if goruntime.GOOS != "linux" {
		return "data.folder works only on Linux, where the container engine can use a folder on this machine's own disk.", "Leave data.folder empty on this machine."
	}
	entries, err := os.ReadDir(dir)
	switch {
	case errors.Is(err, fs.ErrNotExist):
	case err != nil:
		return fmt.Sprintf("rexctl can't open %s.", dir), "Check that you can read it, or pick another folder."
	case slices.ContainsFunc(entries, func(d os.DirEntry) bool { return !slices.Contains(folderAllowed, d.Name()) }):
		return fmt.Sprintf("%s already has files in it, and Fluxer's data needs an empty folder.", dir), "Pick an empty or new folder."
	}
	switch fsName, forcible := dataFS(dir); {
	case fsName == "", forcible && force:
	case forcible:
		return fsProblem(dir, fsName), localDiskFix + forceFix
	default:
		return fsProblem(dir, fsName), localDiskFix
	}
	return "", ""
}

func dataFS(dir string) (fsName string, forcible bool) {
	fsName = ForeignFS(dir)
	return fsName, fsName != "" && fsName != ramDisk && !slices.Contains(noOwnersFS, fsName)
}

func fsProblem(dir, fsName string) string {
	switch {
	case fsName == ramDisk:
		return fmt.Sprintf("%s is on a RAM disk, and everything on it is gone when the machine restarts.", dir)
	case slices.Contains(noOwnersFS, fsName):
		return fmt.Sprintf("%s is on %s, which can't keep the file owners and permissions the database needs.", dir, fsName)
	}
	return fmt.Sprintf("%s is on %s, and PostgreSQL can lose data or fail to lock its files there.", dir, fsName)
}

func ForeignFS(path string) string {
	for {
		if _, err := os.Stat(path); err == nil {
			break
		}
		parent := filepath.Dir(path)
		if parent == path {
			return ""
		}
		path = parent
	}
	info, ok := statFS(path)
	if !ok {
		return ""
	}
	return cmp.Or(info.network, info.foreign)
}
