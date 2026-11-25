// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build unix

package logs

import (
	"os"

	"golang.org/x/sys/unix"
)

func stdoutID() (uint64, uint64, bool) {
	var st unix.Stat_t
	if err := unix.Fstat(int(os.Stdout.Fd()), &st); err != nil {
		return 0, 0, false
	}
	return uint64(st.Dev), uint64(st.Ino), true
}
