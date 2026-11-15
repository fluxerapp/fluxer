// SPDX-License-Identifier: AGPL-3.0-or-later

package lock

import (
	"errors"
	"fmt"
	"os"

	"golang.org/x/sys/unix"
)

var ErrHeld = errors.New("lock: another fluxerd already uses this data directory")

type Lock struct {
	f *os.File
}

func Acquire(path string) (*Lock, error) {
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return nil, err
	}
	if err := unix.Flock(int(f.Fd()), unix.LOCK_EX|unix.LOCK_NB); err != nil {
		f.Close()
		if errors.Is(err, unix.EWOULDBLOCK) {
			return nil, fmt.Errorf("%w (%s)", ErrHeld, path)
		}
		return nil, fmt.Errorf("lock %s: %w", path, err)
	}
	if err := f.Truncate(0); err == nil {
		fmt.Fprintf(f, "%d\n", os.Getpid())
	}
	return &Lock{f: f}, nil
}

func (l *Lock) Release() error {
	return l.f.Close()
}
