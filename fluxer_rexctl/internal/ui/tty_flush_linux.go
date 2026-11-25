// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import "golang.org/x/sys/unix"

func flushInput(fd int) {
	_ = unix.IoctlSetInt(fd, unix.TCFLSH, unix.TCIFLUSH)
}
