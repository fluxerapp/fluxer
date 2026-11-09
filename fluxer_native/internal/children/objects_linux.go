// SPDX-License-Identifier: AGPL-3.0-or-later

package children

import "golang.org/x/sys/unix"

func nameMax(dir string) (int, bool) {
	var st unix.Statfs_t
	if err := unix.Statfs(dir, &st); err != nil {
		return 0, false
	}
	return int(st.Namelen), true
}

func networkFS(dir string) string {
	var st unix.Statfs_t
	if err := unix.Statfs(dir, &st); err != nil {
		return ""
	}
	switch uint32(st.Type) {
	case unix.NFS_SUPER_MAGIC:
		return "NFS"
	case unix.SMB_SUPER_MAGIC, unix.CIFS_SUPER_MAGIC, unix.SMB2_SUPER_MAGIC:
		return "SMB"
	}
	return ""
}
