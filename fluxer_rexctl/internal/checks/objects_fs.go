// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

var statDisk = statFS

const (
	minNameMax      = 255
	lowFilesPercent = 10
	fewFilesPercent = 1
	fewFilesCap     = 10_000
)

type fsInfo struct {
	nameMax   int64
	network   string
	files     uint64
	freeFiles uint64
}

type fileRoom int

const (
	roomPlenty fileRoom = iota
	roomLow
	roomFew
)

func filesRoom(total, free uint64) fileRoom {
	if total == 0 {
		return roomPlenty
	}
	switch {
	case free < min(total*fewFilesPercent/100, fewFilesCap):
		return roomFew
	case free < total*lowFilesPercent/100:
		return roomLow
	}
	return roomPlenty
}

func init() {
	Register(Check{ID: "objects-filesystem", Phases: Preinstall | Installed, Run: runObjectsFS})
}

func runObjectsFS(_ context.Context, e *Env) Result {
	var path string
	switch {
	case e.Native:
		path = filepath.Join(e.Dir, "data", "objects")
	case e.chosen(settings.StorageBackend) == settings.StorageFilesystem:
		path = e.stores().Data
	default:
		return skip("")
	}
	return ObjectsFS(path)
}

func ObjectsFS(path string) Result {
	for {
		if _, err := os.Stat(path); err == nil {
			break
		} else if !errors.Is(err, fs.ErrNotExist) {
			return skip("")
		}
		parent := filepath.Dir(path)
		if parent == path {
			return skip("")
		}
		path = parent
	}
	info, ok := statDisk(path)
	switch {
	case !ok:
		return skip("")
	case info.nameMax > 0 && info.nameMax < minNameMax:
		return fail("The disk that holds "+path+" allows file names of only "+strconv.FormatInt(info.nameMax, 10)+" bytes, and uploaded files need "+strconv.Itoa(minNameMax)+".",
			"Keep Fluxer's files on a disk with ext4, XFS or Btrfs.")
	case filesRoom(info.files, info.freeFiles) == roomFew:
		return fail(filesLeft(path, info)+" Fluxer stops saving uploads when it gets this low, so the database and logs on the same disk can keep working.",
			"Delete files you don't need on that disk, or move "+path+" to a disk with more inodes, then run rexctl doctor.")
	case info.network != "":
		return warn("The files Fluxer keeps in "+path+" are on "+info.network+", and file locking and renames there may not work the way Fluxer needs.",
			"Keep Fluxer's files on a local disk.")
	case filesRoom(info.files, info.freeFiles) == roomLow:
		return warn(filesLeft(path, info)+" Each uploaded file uses one, and Fluxer stops saving uploads when almost none are left.",
			"Delete files you don't need on that disk, or move "+path+" to a disk with more inodes.")
	}
	return pass("The disk for " + stack.ObjectsVolume + " can hold Fluxer's files.")
}

func filesLeft(path string, info fsInfo) string {
	return fmt.Sprintf("The disk that holds %s has %d of its %d inodes free (%.1f%%).", path, info.freeFiles, info.files, float64(info.freeFiles)*100/float64(info.files))
}
