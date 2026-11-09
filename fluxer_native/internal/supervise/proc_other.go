// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !linux

package supervise

import "syscall"

func sysProcAttr() *syscall.SysProcAttr {
	return &syscall.SysProcAttr{Setpgid: true}
}

func ClearAmbientCaps() error { return nil }
