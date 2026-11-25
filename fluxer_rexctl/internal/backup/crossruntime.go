// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"errors"
)

func extractInstance(path, dir string) error {
	na, err := FromNative(path)
	if err != nil {
		return err
	}
	if na.Native {
		return ErrIntoLegacy
	}
	return extractFiles(path, dir)
}

var ErrIntoLegacy = errors.New("backup: a backup from one container or systemd can't go into the older layout")
