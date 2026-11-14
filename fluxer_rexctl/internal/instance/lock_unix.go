// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package instance

import (
	"errors"
	"os"
	"syscall"
)

func tryLock(f *os.File) error {
	for {
		err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
		if errors.Is(err, syscall.EINTR) {
			continue
		}
		if errors.Is(err, syscall.EWOULDBLOCK) {
			return ErrLocked
		}
		return err
	}
}

func unlock(f *os.File) {
	_ = syscall.Flock(int(f.Fd()), syscall.LOCK_UN)
}
