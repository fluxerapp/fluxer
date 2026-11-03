// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
)

type Foreign struct {
	Path  string
	Owner int
	Dir   string
}

func (i *Instance) ownDirs() []string {
	return []string{i.Path(StateDirName), i.Path(BackupsDir)}
}

func (i *Instance) GiveFilesToOwner() error {
	if euid() != 0 {
		return nil
	}
	uid, gid, ok := dirIDs(i.Dir)
	if !ok || uid == 0 {
		return nil
	}
	var errs []error
	for _, dir := range i.ownDirs() {
		err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				if errors.Is(err, fs.ErrNotExist) {
					return nil
				}
				return err
			}
			info, err := d.Info()
			if err != nil {
				return nil
			}
			if owner, ok := ownerOf(info); ok && owner == 0 {
				return chownIDs(path, uid, gid)
			}
			return nil
		})
		if err != nil && !errors.Is(err, fs.ErrNotExist) {
			errs = append(errs, err)
		}
	}
	return errors.Join(errs...)
}

func (i *Instance) ForeignFile() (Foreign, bool) {
	me := euid()
	if me == 0 {
		return Foreign{}, false
	}
	dir := i.StateDir()
	var found Foreign
	_ = filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return nil
		}
		info, err := d.Info()
		if err != nil {
			return nil
		}
		if owner, ok := ownerOf(info); ok && owner != me {
			found = Foreign{Path: path, Owner: owner, Dir: dir}
			return fs.SkipAll
		}
		return nil
	})
	return found, found.Path != ""
}

func chownIDs(path string, uid, gid int) error {
	if err := chown(path, uid, gid); err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	return nil
}
