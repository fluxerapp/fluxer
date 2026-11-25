// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"fmt"
	"io/fs"
	"path/filepath"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

type SpaceUnknownError struct {
	Dir string
	Err error
}

func (e *SpaceUnknownError) Error() string {
	return fmt.Sprintf("backup: can't read the free space in %s: %v", e.Dir, e.Err)
}

func (e *SpaceUnknownError) Unwrap() error { return e.Err }

func FreeSpace(path string) (uint64, error) {
	free, err := host.FreeDisk(path)
	if err != nil {
		return 0, &SpaceUnknownError{Dir: path, Err: err}
	}
	return free, nil
}

func DirSize(root string) (uint64, error) {
	var n uint64
	err := filepath.WalkDir(root, func(_ string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.Type().IsRegular() {
			if info, err := d.Info(); err == nil {
				n += uint64(max(info.Size(), 0))
			}
		}
		return nil
	})
	return n, err
}

func NeedSpace(dir string, need uint64) error {
	free, err := FreeSpace(dir)
	if err != nil {
		return err
	}
	if free < need {
		return &DiskError{Dir: dir, Need: need, Available: free}
	}
	return nil
}
