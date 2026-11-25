// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime/upgrade"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func backupWords(policy string) string {
	switch policy {
	case settings.BackupOff:
		return "none"
	case settings.BackupWithoutUploads:
		return "everything except uploads"
	}
	return "everything"
}

func databaseWords(from, policy string) string {
	if policy == settings.BackupOff {
		return "upgraded, and with no backup Fluxer " + from + " can't run on it again"
	}
	return "upgraded, so going back to Fluxer " + from + " needs the backup"
}

func upgradeCard(ctx context.Context, app *App, inst *instance.Instance, eng *upgrade.Engine, rel *release.Release, from, to, policy string) {
	var rows []ui.KV
	if release.Valid(inst.State.Release) {
		rows = append(rows, ui.KV{Key: "Now on", Value: "Fluxer " + from})
	}
	download, saved := eng.Sizes(ctx, rel, policy)
	switch {
	case download == 0:
		rows = append(rows, ui.KV{Key: "Download", Value: "already on this machine"})
	case download > 0:
		rows = append(rows, ui.KV{Key: "Disk", Value: "about " + ui.Bytes(download) + " for the new release"})
	}
	if policy != "" {
		words := backupWords(policy)
		if saved > 0 {
			words += ", about " + ui.Bytes(saved)
		}
		rows = append(rows, ui.KV{Key: "Backup", Value: words})
	}
	if free, err := host.FreeDisk(inst.Dir); err == nil {
		rows = append(rows, ui.KV{Key: "Free space", Value: ui.Bytes(int64(free))})
	}
	rows = append(rows, ui.KV{Key: "Database", Value: databaseWords(from, policy)})
	app.UI.Card(ui.Card{Title: "Upgrade to Fluxer " + to, Rows: rows})
}

func upgradeCheckCard(app *App, cur string, rel *release.Release) {
	from, to := cur, rel.Version
	var rows []ui.KV
	if release.Valid(cur) {
		rows = append(rows, ui.KV{Key: "Now on", Value: "Fluxer " + from})
	}
	rows = append(rows, ui.KV{Key: "Database", Value: databaseWords(from, "")})
	app.UI.Card(ui.Card{Title: "Fluxer " + to + " is available", Rows: rows})
}
