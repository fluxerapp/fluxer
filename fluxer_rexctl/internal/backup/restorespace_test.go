// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"errors"
	"testing"
)

func TestRestoreRefusesWhenTheDiskIsShort(t *testing.T) {
	dir := t.TempDir()
	if err := checkRestoreSpace(dir, &Manifest{Entries: []Entry{{Path: "db.sql", Size: 1 << 20}}}); err != nil {
		t.Fatalf("a 1 MB backup was refused: %v", err)
	}
	var space *RestoreSpaceError
	err := checkRestoreSpace(dir, &Manifest{Entries: []Entry{{Path: "vol/uploads.tar", Size: 1 << 60}}})
	if !errors.As(err, &space) || space.Need != 1<<60+DiskReserve {
		t.Fatalf("got %v, want a refusal that needs %d", err, uint64(1<<60+DiskReserve))
	}
}
