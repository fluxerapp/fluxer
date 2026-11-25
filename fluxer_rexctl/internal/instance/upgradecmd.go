// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

func UpgradeCommand(dir string) string {
	return command(dir, "upgrade")
}

func RestoreCommand(dir, archive string) string {
	return command(dir, "restore "+ShellQuote(archive))
}

func (i *Instance) RestoreUnfinished() (msg, fix string, ok bool) {
	if i == nil || i.State == nil || i.State.Restoring == "" {
		return "", "", false
	}
	return "A restore didn't finish, so Fluxer's data may be only partly restored.",
		"Run " + RestoreCommand(i.Dir, i.State.Restoring) + " again to finish it.", true
}

func (s *State) RestoreDatabaseFix() string {
	if s != nil && s.PreviousBackup != "" {
		return "Run rexctl rollback --restore-database to go back with the backup from before the upgrade."
	}
	return "The upgrade made no backup. Run rexctl rollback --restore-database --dry-run to see which older backup it would restore."
}

func command(dir, args string) string {
	cmd := "rexctl " + args
	if def, err := DefaultDir(); err != nil || RealPath(def) != RealPath(dir) {
		cmd += " --dir " + ShellQuote(dir)
	}
	if asRoot() {
		return "sudo " + cmd
	}
	return cmd
}
