// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"errors"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func TestBackupCompressErrorKeepsInstanceUp(t *testing.T) {
	err := backupCreateErr(&backup.CompressError{Err: errors.New("no space left on device")})
	var ee *ui.ExitError
	if !errors.As(err, &ee) {
		t.Fatalf("backupCreateErr returned %T, want a ui.ExitError", err)
	}
	if ee.Code == ui.ExitInterrupted {
		t.Fatal("a compression failure is not a cancellation")
	}
	if !strings.Contains(ee.Msg, "running again") {
		t.Fatalf("message = %q, want it to say Fluxer is running again", ee.Msg)
	}
}

func TestBackupCompressingStageHasTitle(t *testing.T) {
	if backupStageTitle(backup.StageCompressing) == "" {
		t.Fatal("the compressing stage has no progress title")
	}
}
