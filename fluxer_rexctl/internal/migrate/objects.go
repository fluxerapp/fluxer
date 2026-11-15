// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var (
	ErrObjectsNotEmpty = errors.New("migrate: the folder for the files isn't empty")
	ErrObjectsSpace    = errors.New("migrate: not enough free space for the files")
	ErrStagingDevice   = errors.New("migrate: the staging folder is on another disk")
)

const SpaceFactor = 1.1

var objectsAllowed = []string{".layout", ".staging", ".meta", "lost+found"}

func CheckObjectsDir(root string) error {
	entries, err := os.ReadDir(root)
	switch {
	case errors.Is(err, fs.ErrNotExist):
	case err != nil:
		return err
	default:
		for _, e := range entries {
			if !slices.Contains(objectsAllowed, e.Name()) {
				return fmt.Errorf("%w: %s holds %s", ErrObjectsNotEmpty, root, e.Name())
			}
		}
		if a, ok := device(root); ok {
			if b, ok := device(filepath.Join(root, ".staging")); ok && a != b {
				return fmt.Errorf("%w: %s", ErrStagingDevice, filepath.Join(root, ".staging"))
			}
		}
	}
	return nil
}

func CheckSpace(source, target string) error {
	used, err := DirBytes(source)
	if err != nil {
		return err
	}
	free, ok := FreeBytes(existing(target))
	if need := int64(float64(used) * SpaceFactor); ok && free < need {
		return fmt.Errorf("%w: %s free, %s needed", ErrObjectsSpace, ui.Bytes(free), ui.Bytes(need))
	}
	return nil
}

func existing(path string) string {
	for {
		if _, err := os.Stat(path); err == nil {
			return path
		}
		parent := filepath.Dir(path)
		if parent == path {
			return path
		}
		path = parent
	}
}

func DirBytes(root string) (int64, error) {
	var n int64
	err := filepath.WalkDir(root, func(_ string, d fs.DirEntry, err error) error {
		if errors.Is(err, fs.ErrNotExist) {
			return nil
		}
		if err != nil {
			return err
		}
		if d.Type().IsRegular() {
			info, err := d.Info()
			if err != nil {
				return err
			}
			n += info.Size()
		}
		return nil
	})
	return n, err
}
