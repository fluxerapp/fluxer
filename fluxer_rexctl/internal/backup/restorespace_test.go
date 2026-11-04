// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"errors"
	"testing"
)

func TestRestoreRefusesWhenTheDiskIsShort(t *testing.T) {
	dir := t.TempDir()
	m := &Manifest{Database: DumpName, Entries: []Entry{{Path: DumpName, Size: 1 << 20}, {Path: "vol/uploads.tar", Size: 1 << 20}}}
	if got, want := restoreDataNeed(m), uint64(3<<20); got != want {
		t.Fatalf("data need %d, want %d with room to load the database dump", got, want)
	}
	if err := checkRestoreSpace(dir, dir, restoreDataNeed(m)); err != nil {
		t.Fatalf("a small backup was refused: %v", err)
	}
	var space *RestoreSpaceError
	err := checkRestoreSpace(dir, dir, 1<<60)
	if !errors.As(err, &space) || space.Need != 1<<60+DiskReserve {
		t.Fatalf("got %v, want a refusal that needs %d", err, uint64(1<<60+DiskReserve))
	}
}
