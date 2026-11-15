// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"context"
	"errors"
	"os"
)

var ErrLocked = errors.New("instance: another rexctl command is changing this instance")

type Lock struct {
	path string
	file *os.File
}

func (i *Instance) Lock(ctx context.Context) (*Lock, error) {
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	if err := os.MkdirAll(i.StateDir(), 0o700); err != nil {
		return nil, err
	}
	path := i.Path(StateDirName, LockFileName)
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return nil, err
	}
	if err := tryLock(f); err != nil {
		f.Close()
		return nil, err
	}
	return &Lock{path: path, file: f}, nil
}

func (i *Instance) Busy() bool {
	l, err := i.Lock(context.Background())
	if err != nil {
		return errors.Is(err, ErrLocked)
	}
	l.Release()
	return false
}

func (l *Lock) Release() error {
	if l == nil || l.file == nil {
		return nil
	}
	f := l.file
	l.file = nil
	unlock(f)
	return f.Close()
}
