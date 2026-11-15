// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"
	"fmt"
	"os"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
)

const backupMaxAge = 7 * 24 * time.Hour

func init() {
	Register(Check{ID: "backup-age", Phases: Installed, Run: runBackupAge})
}

func runBackupAge(ctx context.Context, e *Env) Result {
	if e.Instance == nil {
		return skip("")
	}
	list, err := backup.List(e.Instance.BackupDir())
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return skip("")
	}
	var newest time.Time
	for _, b := range list {
		if b.CreatedAt.After(newest) {
			newest = b.CreatedAt
		}
	}
	if newest.IsZero() {
		if e.Instance.State != nil && time.Since(e.Instance.State.InstalledAt) < backupMaxAge {
			return skip("There are no backups of your instance yet.")
		}
		return warn("There are no backups of your instance yet.", "Run rexctl backup.")
	}
	if time.Since(newest) > backupMaxAge {
		return warn(fmt.Sprintf("Your last backup is from %s.", day(newest)), "Run rexctl backup.")
	}
	return pass(fmt.Sprintf("Your last backup is from %s.", day(newest)))
}
