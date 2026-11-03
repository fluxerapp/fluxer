// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"
	"strconv"
	"strings"
)

const maxSCRAMIterations = 100000

func init() {
	Register(Check{ID: "postgres-password", Phases: Installed, Run: runPostgresPassword, Fix: fixPostgresRole, SafeFix: true})
	Register(Check{ID: "postgres-scram", Phases: Installed, Run: runPostgresSCRAM, Fix: fixPostgresRole, SafeFix: true})
}

const psqlLocal = `exec psql -X -q -v ON_ERROR_STOP=1 -U "$POSTGRES_USER" -d "$POSTGRES_DB" "$@"`

const psqlPasswordTest = `IFS= read -r P
h=$(hostname -i 2>/dev/null | cut -d' ' -f1)
PGPASSWORD="$P" PGCONNECT_TIMEOUT=5 exec psql -X -q -tA -h "${h:-postgres}" -U "$POSTGRES_USER" -d "$POSTGRES_DB" -c 'select 1'`

func (e *Env) bundledPostgres(ctx context.Context) bool {
	if e.Instance == nil {
		return false
	}
	switch strings.ToLower(e.value("FLUXER_DATABASE_BACKEND")) {
	case "cassandra", "scylla":
		return false
	}
	if e.value("FLUXER_POSTGRES_URL") != "" {
		return false
	}
	if h := e.value("FLUXER_POSTGRES_HOST"); h != "" && h != "postgres" {
		return false
	}
	return e.running(ctx, "postgres")
}

func runPostgresPassword(ctx context.Context, e *Env) Result {
	if !e.bundledPostgres(ctx) {
		return skip("")
	}
	password := e.value("POSTGRES_PASSWORD")
	if password == "" {
		return skip("")
	}
	out, err := e.exec(ctx, "postgres", password+"\n", "sh", "-c", psqlPasswordTest)
	if err == nil {
		return pass("Fluxer can sign in to its database.")
	}
	if strings.Contains(out, "password authentication failed") || strings.Contains(out, "28P01") {
		return fixable(fail("Fluxer can't sign in to its database with the password in your settings.",
			"Run rexctl doctor --fix to set the database password to the one in your settings."))
	}
	r := skip("Couldn't test the database password.")
	r.Details = []string{out}
	return r
}

func runPostgresSCRAM(ctx context.Context, e *Env) Result {
	if !e.bundledPostgres(ctx) {
		return skip("")
	}
	out, err := e.exec(ctx, "postgres", "", "sh", "-c", psqlLocal, "psql", "-tA", "-c",
		"select rolpassword from pg_authid where rolname = current_user")
	if err != nil {
		return skip("Couldn't read how the database password is stored.")
	}
	iterations, ok := scramIterations(out)
	if !ok {
		return pass("The database password works with this release.")
	}
	if iterations > maxSCRAMIterations {
		return fixable(fail("The database password is stored in a way Fluxer can't read.",
			"Run rexctl doctor --fix to store it again."))
	}
	return pass("The database password works with this release.")
}

func scramIterations(rolpassword string) (int, bool) {
	s := strings.TrimSpace(rolpassword)
	rest, ok := strings.CutPrefix(s, "SCRAM-SHA-256$")
	if !ok {
		return 0, false
	}
	iter, _, ok := strings.Cut(rest, ":")
	if !ok {
		return 0, false
	}
	n, err := strconv.Atoi(iter)
	if err != nil {
		return 0, false
	}
	return n, true
}

func psqlQuote(s string) string {
	return "'" + strings.NewReplacer(`\`, `\\`, `'`, `''`).Replace(s) + "'"
}

func alterRoleScript(password string) string {
	return `\set pw ` + psqlQuote(password) + "\n" +
		"SET password_encryption = 'scram-sha-256';\n" +
		"SELECT set_config('scram_iterations', '4096', false) WHERE current_setting('server_version_num')::int >= 160000;\n" +
		"ALTER ROLE CURRENT_USER PASSWORD :'pw';\n"
}

func fixPostgresRole(ctx context.Context, e *Env) error {
	password := e.value("POSTGRES_PASSWORD")
	if password == "" {
		return errors.New("checks: no database password in the settings")
	}
	if strings.ContainsAny(password, "\n\r") {
		return errors.New("checks: the database password has a line break")
	}
	_, err := e.exec(ctx, "postgres", alterRoleScript(password), "sh", "-c", psqlLocal, "psql", "-f", "-")
	return err
}
