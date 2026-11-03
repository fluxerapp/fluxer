// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package release

import (
	"os"
	"syscall"
)

func Reexec(args []string) error {
	if selfPathErr != nil {
		return selfPathErr
	}
	return syscall.Exec(selfPath, append([]string{selfPath}, args...), os.Environ())
}
