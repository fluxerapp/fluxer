// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build unix

package pg

import "golang.org/x/sys/unix"

func fsSize(path string) (uint64, bool) {
	var st unix.Statfs_t
	if err := unix.Statfs(path, &st); err != nil {
		return 0, false
	}
	return uint64(st.Bsize) * uint64(st.Blocks), true
}
