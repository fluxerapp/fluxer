// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build unix

package supervise

import "syscall"

func signal(pid int, sig syscall.Signal) {
	syscall.Kill(pid, sig)
}

func killGroup(pid int) {
	syscall.Kill(-pid, syscall.SIGKILL)
}
