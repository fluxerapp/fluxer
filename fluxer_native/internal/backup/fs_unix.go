// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build unix

package backup

import (
	"os/exec"
	"syscall"

	"golang.org/x/sys/unix"
)

func freeBytes(dir string) (uint64, error) {
	var st unix.Statfs_t
	if err := unix.Statfs(dir, &st); err != nil {
		return 0, err
	}
	return st.Bavail * uint64(st.Bsize), nil
}

func ownGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
}

func killGroup(pid int) {
	syscall.Kill(-pid, syscall.SIGKILL)
}
