// SPDX-License-Identifier: AGPL-3.0-or-later

package children

import (
	"bytes"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const (
	objectsDirMode = 0o770 | fs.ModeSetgid
	minNameMax     = 255
)

func PrepareObjects(l spec.Layout) error {
	root := l.ObjectsDir()
	staging := filepath.Join(root, spec.ObjectsStagingDir)
	dirs := []string{root, staging}
	for _, d := range spec.ObjectsStagingDirs {
		dirs = append(dirs, filepath.Join(staging, d))
	}
	for _, d := range dirs {
		err := os.Mkdir(d, objectsDirMode)
		if errors.Is(err, fs.ErrExist) {
			continue
		}
		if err != nil {
			return err
		}
		if err := os.Chmod(d, objectsDirMode); err != nil {
			return err
		}
	}
	if n, ok := nameMax(root); ok && n < minNameMax {
		return fmt.Errorf("%s allows file names of only %d bytes, and stored files need %d. Put it on a file system such as ext4 or XFS", root, n, minNameMax)
	}
	data, err := os.ReadFile(l.ObjectsLayout())
	if err == nil {
		if !bytes.Equal(bytes.TrimSpace(data), []byte(spec.ObjectsLayoutContents)) {
			return fmt.Errorf("%s holds %q, and this release only knows %s", l.ObjectsLayout(), bytes.TrimSpace(data), spec.ObjectsLayoutContents)
		}
		return nil
	}
	if !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Join(staging, spec.ObjectsStagingDirs[0]), ".layout-*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	_, err = tmp.WriteString(spec.ObjectsLayoutContents)
	if err == nil {
		err = tmp.Chmod(0o660)
	}
	if err == nil {
		err = tmp.Sync()
	}
	if cerr := tmp.Close(); err == nil {
		err = cerr
	}
	if err != nil {
		return err
	}
	if err := os.Rename(tmp.Name(), l.ObjectsLayout()); err != nil {
		return err
	}
	d, err := os.Open(root)
	if err != nil {
		return err
	}
	defer d.Close()
	return d.Sync()
}

func ObjectsWarning(l spec.Layout) string {
	dir := l.ObjectsDir()
	if _, err := os.Stat(dir); err != nil {
		dir = l.Data
	}
	if kind := networkFS(dir); kind != "" {
		return fmt.Sprintf("%s is on %s. Stored files need atomic renames and fsync, which network file systems don't always give", dir, kind)
	}
	return ""
}
