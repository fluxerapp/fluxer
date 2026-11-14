// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package instance

import (
	"os"
	"path/filepath"
	"syscall"
)

func (i *Instance) OwnerUID() (int, bool) {
	info, err := os.Stat(i.Dir)
	if err != nil {
		return 0, false
	}
	return ownerOf(info)
}

var ownerOf = func(info os.FileInfo) (int, bool) {
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		return 0, false
	}
	return int(st.Uid), true
}

func (i *Instance) giveToOwner(path string) error {
	if euid() != 0 {
		return nil
	}
	r, err := os.OpenRoot(i.Dir)
	if err != nil {
		return err
	}
	defer r.Close()
	info, err := r.Stat(".")
	if err != nil {
		return err
	}
	uid, gid, ok := fileIDs(info)
	if !ok || uid == 0 {
		return nil
	}
	rel, err := filepath.Rel(i.Dir, path)
	if err != nil {
		return err
	}
	return claimForOwner(r, rel, uid, gid)
}

func fileIDs(info os.FileInfo) (int, int, bool) {
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		return 0, 0, false
	}
	return int(st.Uid), int(st.Gid), true
}

func claimForOwner(r *os.Root, name string, uid, gid int) error {
	f, err := r.OpenFile(name, os.O_RDONLY|syscall.O_NOFOLLOW|syscall.O_NONBLOCK, 0)
	if err != nil {
		return nil
	}
	defer f.Close()
	info, err := f.Stat()
	if err != nil {
		return err
	}
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok || st.Uid != 0 {
		return nil
	}
	if !info.IsDir() && (!info.Mode().IsRegular() || st.Nlink != 1) {
		return nil
	}
	return f.Chown(uid, gid)
}
