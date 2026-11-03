// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package backup

import (
	"os"
	"syscall"
)

func fileInode(path string) (uint64, bool) {
	info, err := os.Stat(path)
	if err != nil {
		return 0, false
	}
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		return 0, true
	}
	return uint64(st.Ino), true
}
