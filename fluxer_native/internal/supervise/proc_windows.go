// SPDX-License-Identifier: AGPL-3.0-or-later

package supervise

import (
	"os"
	"syscall"
)

func sysProcAttr() *syscall.SysProcAttr {
	return &syscall.SysProcAttr{}
}

func ClearAmbientCaps() error { return nil }

func signal(pid int, _ syscall.Signal) {
	killGroup(pid)
}

func killGroup(pid int) {
	if p, err := os.FindProcess(pid); err == nil {
		p.Kill()
	}
}
