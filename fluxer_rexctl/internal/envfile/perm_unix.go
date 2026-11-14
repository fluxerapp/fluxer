// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package envfile

import (
	"os"
	"syscall"
)

func restrictToOwner(string) error { return nil }

func keepOwner(f *os.File, old os.FileInfo) {
	st, ok := old.Sys().(*syscall.Stat_t)
	if !ok || os.Geteuid() != 0 {
		return
	}
	_ = f.Chown(int(st.Uid), int(st.Gid))
}

func syncDir(dir string) {
	d, err := os.Open(dir)
	if err != nil {
		return
	}
	_ = d.Sync()
	_ = d.Close()
}

func RestrictToOwner(path string) error {
	info, err := os.Stat(path)
	if err != nil {
		return err
	}
	if info.IsDir() {
		return os.Chmod(path, 0o700)
	}
	return os.Chmod(path, 0o600)
}
