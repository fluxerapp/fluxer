// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package cmd

import (
	"os"
	"syscall"
	"testing"
)

func uploadsInode(t *testing.T, path string) uint64 {
	t.Helper()
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	return uint64(info.Sys().(*syscall.Stat_t).Ino)
}
