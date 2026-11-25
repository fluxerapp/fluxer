// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"errors"
	"os"
)

var (
	ErrNativeWithoutUploads = errors.New("backup: a native instance restores only backups with uploads")
	ErrNativeExternalDB     = errors.New("backup: a native instance restores only backups that hold the database")
)

func CheckNative(path, dir string, m *Manifest) error {
	switch {
	case m.WithoutUploads():
		return ErrNativeWithoutUploads
	case m.HasSkipped(SkippedDatabase):
		return ErrNativeExternalDB
	case dir == "":
		return nil
	}
	fi, err := os.Stat(path)
	if err != nil {
		return err
	}
	return needFree(dir, uint64(max(fi.Size(), 0))+restoreDataNeed(m)+DiskReserve)
}
