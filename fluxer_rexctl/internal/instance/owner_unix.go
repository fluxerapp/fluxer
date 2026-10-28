// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package instance

import (
	"os"
	"syscall"
)

func (i *Instance) OwnerUID() (int, bool) {
	info, err := os.Stat(i.Dir)
	if err != nil {
		return 0, false
	}
	return ownerOf(info)
}

func ownerOf(info os.FileInfo) (int, bool) {
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
	info, err := os.Stat(i.Dir)
	if err != nil {
		return err
	}
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok || st.Uid == 0 {
		return nil
	}
	return chown(path, int(st.Uid), int(st.Gid))
}
