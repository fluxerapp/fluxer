// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime/upgrade"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type rollbackChoice struct {
	to     string
	backup *backup.Info
}

func (c rollbackChoice) value() string {
	if c.backup == nil {
		return "keep:" + c.to
	}
	return "restore:" + c.backup.Path
}

func (c rollbackChoice) label() string {
	if c.backup == nil {
		return "Fluxer " + c.to
	}
	return fmt.Sprintf("Fluxer %s with the backup from %s", c.to, backupTime(c.backup.CreatedAt))
}

func (c rollbackChoice) hint(now time.Time) string {
	if c.backup == nil {
		return "keeps your database"
	}
	return "restores the database, loses " + lostSince(now.Sub(c.backup.CreatedAt)) + " of messages"
}

func lostSince(d time.Duration) string {
	switch {
	case d < 2*time.Minute:
		return "a minute"
	case d < 2*time.Hour:
		return fmt.Sprintf("%d minutes", int(d.Minutes()))
	case d < 48*time.Hour:
		return fmt.Sprintf("%d hours", int(d.Hours()))
	}
	return fmt.Sprintf("%d days", int(d.Hours()/24))
}

func rollbackChoices(eng *upgrade.Engine, inst *instance.Instance, list []backup.Info, backupsCarryRelease bool) []rollbackChoice {
	st := inst.State
	cur, prev := st.Release, st.PreviousRelease
	var out []rollbackChoice
	if prev != "" {
		have := eng.Has(prev)
		if have && !eng.DataNewer(prev) {
			out = append(out, rollbackChoice{to: prev})
		}
		if b := rollbackTarget(inst, list, prev); b != nil && (have || backupsCarryRelease) {
			out = append(out, rollbackChoice{to: prev, backup: b})
		}
	}
	if st.FailedRelease != "" || prev == "" {
		if b := backup.NewestOf(list, cur); b != nil {
			out = append(out, rollbackChoice{to: cur, backup: b})
		}
	}
	return out
}

func defaultRollback(inst *instance.Instance, choices []rollbackChoice, restoreDB bool) (rollbackChoice, bool) {
	var best *rollbackChoice
	for i := range choices {
		c := &choices[i]
		if (c.backup != nil) != restoreDB {
			continue
		}
		if best == nil || (restoreDB && inst.State.FailedRelease != "" && c.to == inst.State.Release) {
			best = c
		}
	}
	if best == nil {
		return rollbackChoice{}, false
	}
	return *best, true
}

func rollbackRefusal(eng *upgrade.Engine, inst *instance.Instance, choices []rollbackChoice, restoreDB, interactive bool) error {
	prev := inst.State.PreviousRelease
	switch {
	case restoreDB:
		return ui.Refused("There's no backup on this machine to go back to.", "Run rexctl restore with a backup file you kept somewhere else.")
	case prev != "" && eng.DataNewer(prev):
		return ui.Refused(fmt.Sprintf("Fluxer %s has changed your data, so Fluxer %s can't start on it.", inst.State.Release, prev), restoreDatabaseFix(inst, choices, interactive))
	case prev == "" && release.HasSnapshot(inst, ""):
		return ui.Refused("rexctl can't go back to the release from before it took over this instance.", "Run rexctl backup list, then rexctl restore with the backup from before the upgrade.")
	case prev == "":
		return ui.Refused("There's no earlier release to go back to.", "")
	case len(choices) > 0:
		return ui.Refused(fmt.Sprintf("The files for Fluxer %s aren't on this machine anymore.", prev), "Run "+restoreDatabaseCommand(inst, interactive)+" to go back with a backup.")
	}
	return ui.Refused(fmt.Sprintf("The files for Fluxer %s aren't on this machine anymore, and there's no backup to go back to.", prev), "Run rexctl restore with a backup file you kept somewhere else.")
}

func restoreDatabaseCommand(inst *instance.Instance, interactive bool) string {
	if interactive {
		return "rexctl rollback --restore-database"
	}
	return "rexctl rollback --restore-database --confirm " + shellWord(inst.Domain())
}

func restoreDatabaseFix(inst *instance.Instance, choices []rollbackChoice, interactive bool) string {
	cmd := restoreDatabaseCommand(inst, interactive)
	c, ok := defaultRollback(inst, choices, true)
	made := inst.State.PreviousBackup != ""
	switch {
	case !ok && made:
		return "The backup from before the upgrade isn't on this machine anymore. Run rexctl restore with a backup file you kept somewhere else."
	case !ok:
		return "The upgrade made no backup, and there's no older one on this machine. Run rexctl restore with a backup file you kept somewhere else."
	case made && c.backup.Path == inst.State.PreviousBackup:
		return "Run " + cmd + " to go back with the backup from before the upgrade."
	case made:
		return fmt.Sprintf("Run %s to go back with the backup from %s, which loses everything since then.", cmd, backupTime(c.backup.CreatedAt))
	}
	return fmt.Sprintf("The upgrade made no backup. Run %s to go back with the older backup from %s, which loses everything since then.", cmd, backupTime(c.backup.CreatedAt))
}

func pickRollback(app *App, eng *upgrade.Engine, inst *instance.Instance, choices []rollbackChoice, restoreDB bool) (rollbackChoice, error) {
	def, ok := defaultRollback(inst, choices, restoreDB)
	if !app.UI.Interactive() || len(choices) < 2 {
		if !ok {
			return rollbackChoice{}, rollbackRefusal(eng, inst, choices, restoreDB, app.UI.Interactive())
		}
		return def, nil
	}
	if !ok {
		def = choices[0]
	}
	now := time.Now()
	items := make([]ui.PickItem, len(choices))
	for i, c := range choices {
		items[i] = ui.PickItem{Value: c.value(), Label: c.label(), Hint: c.hint(now)}
	}
	v, err := app.UI.Pick("Where do you want to go back to?", items, def.value())
	if err != nil {
		return rollbackChoice{}, err
	}
	for _, c := range choices {
		if c.value() == v {
			return c, nil
		}
	}
	return def, nil
}

func restoreWhat(c rollbackChoice, now time.Time) string {
	lost := "Messages, uploads and changes since then will be lost."
	if c.backup.WithoutUploads() {
		lost = "Messages and changes since then will be lost. Uploads stay as they are."
	}
	return fmt.Sprintf("This restores the backup from %s, %s ago, on Fluxer %s. %s", backupTime(c.backup.CreatedAt), lostSince(now.Sub(c.backup.CreatedAt)), c.to, lost)
}

func restoreInPlace(ctx context.Context, app *App, eng *upgrade.Engine, c rollbackChoice) error {
	if _, err := eng.RT.Backups().Verify(ctx, c.backup.Path); err != nil {
		return ui.Wrap(err, "The backup from "+backupTime(c.backup.CreatedAt)+" can't be read.", "Run rexctl backup list to see your other backups.")
	}
	res, err := eng.RestoreHere(ctx, c.backup.Path)
	if err != nil {
		return err
	}
	if res.Stopped {
		app.UI.OK("Restored the backup from %s. Fluxer stays stopped until you run rexctl start.", backupTime(c.backup.CreatedAt))
		return nil
	}
	app.UI.OK("Restored the backup from %s. Your instance is running.", backupTime(c.backup.CreatedAt))
	return nil
}

func reportRollback(app *App, res upgrade.Result, when string) error {
	fromShort, toShort := res.From, res.To
	switch res.Outcome {
	case upgrade.Upgraded:
		switch {
		case res.Stopped:
			app.UI.OK("Went back to Fluxer %s. It stays stopped until you run rexctl start.", toShort)
		case res.Backup != "" && when != "":
			app.UI.OK("Went back to Fluxer %s with the backup from %s. Your instance is running.", toShort, when)
		case res.Backup != "":
			app.UI.OK("Went back to Fluxer %s with the backup. Your instance is running.", toShort)
		default:
			app.UI.OK("Went back to Fluxer %s. Your instance is running.", toShort)
		}
	case upgrade.WentBack:
		app.UI.Fail(wentBackLine(res.Cause, toShort, fromShort), "")
		app.UI.Detail("%v", res.Cause)
		if res.Log != "" {
			fixLine(app, fmt.Sprintf("Read %s to see why.", res.Log))
		} else {
			fixLine(app, "Run rexctl logs to see why.")
		}
		app.SetExit(ui.ExitFailed)
	}
	return nil
}
