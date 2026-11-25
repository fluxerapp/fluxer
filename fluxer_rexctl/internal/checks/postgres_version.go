// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
)

func init() {
	Register(Check{ID: "postgres-version", Phases: Installed, Run: nativeOnly(runPostgresVersion)})
}

func runPostgresVersion(ctx context.Context, e *Env) Result {
	if e.Instance == nil || e.Container || e.value("FLUXER_POSTGRES_URL") != "" {
		return skip("")
	}
	return postgresVersion(e.stack().Postgres())
}

func postgresVersion(p native.Postgres) Result {
	switch {
	case !p.HasData():
		return skip("")
	case p.Broken:
		return fail(fmt.Sprintf("pg_upgrade stopped part way through moving the PostgreSQL %d database, so it can't start again.", p.Data),
			"Restore the backup from before the move with rexctl restore.")
	case p.Data < native.MinPostgres:
		return fail(fmt.Sprintf("The database is PostgreSQL %d, and Fluxer needs %d or later.", p.Data, native.MinPostgres),
			"Restore your latest backup into a new install on a machine with a newer PostgreSQL.")
	case !p.DataInstalled():
		return fail(fmt.Sprintf("The database is PostgreSQL %d, which isn't installed any more, so Fluxer can't start.", p.Data),
			fmt.Sprintf("Install %s (from apt.postgresql.org if this release no longer has it), then run rexctl postgres upgrade. Or restore your latest backup with rexctl restore.", native.PackageName(p.Data)))
	case p.Upgradable():
		return warn(fmt.Sprintf("The database is PostgreSQL %d, and PostgreSQL %d is installed too.", p.Data, p.Newest()),
			fmt.Sprintf("Run rexctl postgres upgrade to move to %d.", p.Newest()))
	case p.Data <= native.PostgresEOL:
		return warn(fmt.Sprintf("The database is PostgreSQL %d, which the PostgreSQL project no longer fixes.", p.Data),
			"Install a newer PostgreSQL, from apt.postgresql.org or a newer release of this system, then run rexctl postgres upgrade.")
	}
	return pass(fmt.Sprintf("The database is PostgreSQL %d.", p.Data))
}
