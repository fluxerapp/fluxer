// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package ui

import (
	"os"
	"syscall"
)

func interruptSelf() {
	_ = syscall.Kill(os.Getpid(), syscall.SIGINT)
}
