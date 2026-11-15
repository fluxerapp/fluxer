// SPDX-License-Identifier: AGPL-3.0-or-later

package supervise

import (
	"syscall"

	"golang.org/x/sys/unix"
)

func sysProcAttr() *syscall.SysProcAttr {
	return &syscall.SysProcAttr{Setpgid: true, Pdeathsig: syscall.SIGKILL}
}

func ClearAmbientCaps() error {
	_, _, errno := syscall.AllThreadsSyscall(syscall.SYS_PRCTL, unix.PR_CAP_AMBIENT, unix.PR_CAP_AMBIENT_CLEAR_ALL, 0)
	if errno != 0 {
		return errno
	}
	return nil
}
