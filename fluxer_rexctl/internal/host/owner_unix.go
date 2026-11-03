// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package host

import (
	"os"
	"os/user"
	"strconv"
	"syscall"
)

func dirOwner(dir string) string {
	info, err := os.Stat(dir)
	if err != nil {
		return ""
	}
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok || st.Uid == 0 {
		return ""
	}
	u, err := user.LookupId(strconv.FormatUint(uint64(st.Uid), 10))
	if err != nil {
		return strconv.FormatUint(uint64(st.Uid), 10)
	}
	return u.Username
}

func fileOwner(path string) (int, int, bool) {
	info, err := os.Stat(path)
	if err != nil {
		return 0, 0, false
	}
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		return 0, 0, false
	}
	return int(st.Uid), int(st.Gid), true
}
