// SPDX-License-Identifier: AGPL-3.0-or-later

package lock

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"time"
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
	held, err := flock(f)
	for try := 0; held && try < 10; try++ {
		time.Sleep(10 * time.Millisecond)
		held, err = flock(f)
	}
	if err != nil {
		f.Close()
		if held {
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

func Held(path string) (bool, error) {
	f, err := os.Open(path)
	if errors.Is(err, fs.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	defer f.Close()
	return probe(f)
}
