// SPDX-License-Identifier: AGPL-3.0-or-later

package updates

import (
	"fmt"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const (
	BackupBuiltIn = "Fluxer backs up its database before it upgrades. Going back needs that backup."
	BackupRexctl  = "rexctl backs up Fluxer before it upgrades, and goes back by itself when the upgrade fails."
	BackupOff     = "FLUXER_BACKUP_ON_UPGRADE is false, so run fluxerd backup before you upgrade. Going back needs that backup."
	BackupOwnDB   = "Fluxer doesn't back up a database it doesn't run, so back up your PostgreSQL before you upgrade."
)

func BackupNote(method string, backupOnUpgrade, ownDatabase bool) string {
	switch {
	case method == MethodRexctl:
		return BackupRexctl
	case ownDatabase:
		return BackupOwnDB
	case !backupOnUpgrade:
		return BackupOff
	}
	return BackupBuiltIn
}

func Day(t time.Time) string { return t.UTC().Format("2 January 2006") }

func Ago(d time.Duration) string {
	if d < time.Minute {
		return "just now"
	}
	return Span(d) + " ago"
}

func Span(d time.Duration) string {
	switch {
	case d < time.Hour:
		return plural(max(int(d/time.Minute), 1), "minute")
	case d < 48*time.Hour:
		return plural(int(d/time.Hour), "hour")
	}
	return plural(int(d/(24*time.Hour)), "day")
}

func plural(n int, unit string) string {
	if n == 1 {
		return "1 " + unit
	}
	return fmt.Sprintf("%d %ss", n, unit)
}

const outdatedMark = "outdated"

func (r Report) announce() string {
	switch {
	case r.Outdated:
		return outdatedMark
	case len(r.Newer) > 0:
		return r.Newer[0].Version
	}
	return r.Announced
}

func (r Report) Announce() []string {
	if r.Outdated {
		if r.Announced == outdatedMark {
			return nil
		}
		return []string{"A newer Fluxer is out, and it lists releases in a format this one doesn't read. " + r.Then}
	}
	if len(r.Newer) == 0 || r.Newer[0].Version == r.Announced {
		return nil
	}
	return []string{fmt.Sprintf("Fluxer %s is available, and this is %s.", r.Newer[0].Version, r.Running), r.upgradeLine()}
}

func (r Report) upgradeLine() string {
	switch {
	case r.Command == "" && r.Then != "":
		return r.Then
	case r.Command == "":
		return "Upgrade with the steps at " + DocsURL
	case r.Then == "":
		return "Upgrade with " + r.Command
	}
	return "Upgrade with " + r.Command + ". " + r.Then
}

func (r Report) Text(now time.Time) string {
	var b strings.Builder
	fmt.Fprintf(&b, "Fluxer   %s\n", r.Running)
	switch {
	case r.Outdated:
		b.WriteString("Newest   newer than this one, in a list format this Fluxer doesn't read\n")
	case len(r.Newer) > 0:
		top := r.Newer[0]
		line := top.Version
		if !top.Date.IsZero() {
			line += ", out on " + Day(top.Date)
		}
		fmt.Fprintf(&b, "Newest   %s\n", line)
	case r.Latest != "":
		fmt.Fprintf(&b, "Newest   %s, which is this one\n", r.Running)
	}
	if len(r.Newer) > 0 || r.Outdated {
		b.WriteString("\nUpgrade\n")
		if r.Command != "" {
			fmt.Fprintf(&b, "  %s\n", r.Command)
		}
		if r.Then != "" {
			fmt.Fprintf(&b, "  %s\n", r.Then)
		}
		fmt.Fprintf(&b, "  %s\n", r.Backup)
	}
	b.WriteString("\n")
	switch {
	case !r.Enabled && r.CheckedAt.IsZero():
		b.WriteString("Checked  never\n")
		b.WriteString("         Fluxer looks for new releases only when " + spec.UpdateCheckEnv + " is true or when you run fluxerd version --check\n")
	case r.Failures > 0 && r.Enabled:
		fmt.Fprintf(&b, "Checked  failed for the last %s (%s), from %s\n", Span(now.Sub(r.FailedAt)), r.Error, r.URL)
	case r.CheckedAt.IsZero():
		b.WriteString("Checked  not yet, from " + r.URL + "\n")
	default:
		fmt.Fprintf(&b, "Checked  %s, from %s\n", Ago(now.Sub(r.CheckedAt)), r.URL)
	}
	if !r.Enabled && !r.CheckedAt.IsZero() {
		b.WriteString("         " + spec.UpdateCheckEnv + " is off, so Fluxer only checks when you run fluxerd version --check\n")
	}
	return b.String()
}
