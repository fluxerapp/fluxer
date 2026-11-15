// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import "golang.org/x/sys/unix"

var networkFS = map[int64]string{
	0x6969:     "NFS",
	0x517b:     "SMB",
	0xff534d42: "SMB",
	0xfe534d42: "SMB",
}

func statFS(path string) (fsInfo, bool) {
	var st unix.Statfs_t
	if err := unix.Statfs(path, &st); err != nil {
		return fsInfo{}, false
	}
	return fsInfo{nameMax: int64(st.Namelen), network: networkFS[int64(st.Type)], files: st.Files, freeFiles: st.Ffree}, true
}
