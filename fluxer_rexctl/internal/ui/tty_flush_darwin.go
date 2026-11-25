// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import "golang.org/x/sys/unix"

func flushInput(fd int) {
	_ = unix.IoctlSetPointerInt(fd, unix.TIOCFLUSH, unix.TCIFLUSH)
}
