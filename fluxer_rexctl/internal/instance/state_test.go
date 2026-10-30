// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"slices"
	"testing"
)

func TestStateKnowsPreviousBackup(t *testing.T) {
	inst, err := Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.PreviousRelease = "2026.1020.100000"
	inst.State.PreviousBackup = "/opt/fluxer/backups/fluxer-chat.example.com-20261025-040000.tar.zst"
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	again, err := Find(inst.Dir)
	if err != nil {
		t.Fatal(err)
	}
	if again.State.PreviousBackup != inst.State.PreviousBackup {
		t.Errorf("PreviousBackup = %q, want %q", again.State.PreviousBackup, inst.State.PreviousBackup)
	}
	if slices.Contains(again.State.Unknown, "previous_backup") {
		t.Errorf("previous_backup reads as an unknown setting in %v", again.State.Unknown)
	}
	s, err := ParseState([]byte(`{"schema":1,"project":"fluxer","previous_release":"2026.1020.100000"}`), inst.Dir)
	if err != nil || s.PreviousBackup != "" {
		t.Errorf("older state = %+v, %v", s, err)
	}
}
