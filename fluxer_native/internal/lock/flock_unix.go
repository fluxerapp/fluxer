// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build unix

package lock

import (
	"errors"
	"os"

	"golang.org/x/sys/unix"
)

func flock(f *os.File) (bool, error) {
	err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB)
	return errors.Is(err, unix.EWOULDBLOCK), err
}

func probe(f *os.File) (bool, error) {
	err := unix.Flock(int(f.Fd()), unix.LOCK_SH|unix.LOCK_NB)
	if errors.Is(err, unix.EWOULDBLOCK) {
		return true, nil
	}
	return false, err
}
