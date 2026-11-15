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

func (i *Instance) GiveFilesToOwner() error {
	if euid() != 0 {
		return nil
	}
	r, err := os.OpenRoot(i.Dir)
	if err != nil {
		return nil
	}
	defer r.Close()
	info, err := r.Stat(".")
	if err != nil {
		return nil
	}
	uid, gid, ok := fileIDs(info)
	if !ok || uid == 0 {
		return nil
	}
	var errs []error
	for _, name := range []string{StateDirName, BackupsDir} {
		top, err := r.Lstat(name)
		if err != nil {
			if !errors.Is(err, fs.ErrNotExist) {
				errs = append(errs, err)
			}
			continue
		}
		if !top.IsDir() {
			continue
		}
		err = fs.WalkDir(r.FS(), name, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				if errors.Is(err, fs.ErrNotExist) {
					return nil
				}
				return err
			}
			if !d.IsDir() && !d.Type().IsRegular() {
				return nil
			}
			return claimForOwner(r, path, uid, gid)
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
