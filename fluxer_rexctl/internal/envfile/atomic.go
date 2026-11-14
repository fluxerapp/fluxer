// SPDX-License-Identifier: AGPL-3.0-or-later

package envfile

import (
	"os"
	"path/filepath"
)

func WriteFileAtomic(path string, data []byte, private bool) error {
	dir := filepath.Dir(path)
	old, statErr := os.Stat(path)
	mode := os.FileMode(0o644)
	if private {
		mode = 0o600
	} else if statErr == nil {
		mode = old.Mode().Perm()
	}
	tmp, err := os.CreateTemp(dir, "."+filepath.Base(path)+".tmp-*")
	if err != nil {
		return err
	}
	name := tmp.Name()
	done := false
	defer func() {
		if !done {
			tmp.Close()
			os.Remove(name)
		}
	}()
	if err := tmp.Chmod(mode); err != nil {
		return err
	}
	if private {
		if err := restrictToOwner(name); err != nil {
			return err
		}
	}
	if statErr == nil {
		keepOwner(tmp, old)
	}
	if _, err := tmp.Write(data); err != nil {
		return err
	}
	if err := tmp.Sync(); err != nil {
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	if err := os.Rename(name, path); err != nil {
		os.Remove(name)
		done = true
		return err
	}
	done = true
	syncDir(dir)
	return nil
}
