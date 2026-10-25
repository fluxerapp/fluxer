// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"path/filepath"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

func TestSpaceAddsNeedsOnOneDisk(t *testing.T) {
	dir := t.TempDir()
	e := &Env{Dir: dir, Instance: &instance.Instance{Dir: dir, State: &instance.State{BackupDir: filepath.Join(dir, "backups")}}}
	got := e.Space(3<<30, 2<<30)
	if len(got) != 1 {
		t.Fatalf("got %d disks, want 1", len(got))
	}
	if want := uint64(5<<30 + diskCritical); got[0].Need != want || !got[0].Images {
		t.Fatalf("got %+v, want need %d on the image disk", got[0], want)
	}
}
