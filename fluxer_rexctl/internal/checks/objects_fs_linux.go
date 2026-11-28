// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import "golang.org/x/sys/unix"

var networkFS = map[int64]string{
	0x6969:     "NFS",
	0x517b:     "SMB",
	0xff534d42: "SMB",
	0xfe534d42: "SMB",
}

var foreignFS = map[int64]string{
	0x5346544e: "NTFS",
	0x7366746e: "NTFS",
	0x4d44:     "FAT",
	0x2011bab0: "exFAT",
	0x65735546: "a FUSE file system",
	0x01021997: "a 9p share",
	0x786f4256: "a VirtualBox share",
	0x01021994: "a RAM disk",
	0x858458f6: "a RAM disk",
}

func statFS(path string) (fsInfo, bool) {
	var st unix.Statfs_t
	if err := unix.Statfs(path, &st); err != nil {
		return fsInfo{}, false
	}
	kind := int64(st.Type)
	return fsInfo{nameMax: int64(st.Namelen), network: networkFS[kind], foreign: foreignFS[kind], files: st.Files, freeFiles: st.Ffree}, true
}
