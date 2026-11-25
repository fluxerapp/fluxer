// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"bytes"
	"cmp"
	"context"
	"errors"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"syscall"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/config"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/lock"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/pg"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

func cmdPostgresUpgrade(args []string) error {
	fs := flag.NewFlagSet("postgres-upgrade", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	to := fs.Int("to", 0, "the PostgreSQL major to move to (default the newest installed)")
	cleanup := fs.Bool("cleanup", false, "delete the database folders an earlier upgrade left behind")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return err
	}
	if cfg.Env["FLUXER_POSTGRES_URL"] != "" {
		return errors.New("the database is external, so upgrade it where it runs")
	}
	if *cleanup {
		return cleanupOldClusters(cfg.Layout.PostgresDir())
	}
	held, err := lock.Acquire(cfg.Layout.LockFile())
	if err != nil {
		return fmt.Errorf("stop Fluxer first: %w", err)
	}
	defer held.Release()
	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	from, target, err := upgradePlan(cfg, *to)
	if errors.Is(err, errAlreadyOn) {
		fmt.Fprintf(os.Stderr, "the database is PostgreSQL %d already, so there is nothing to upgrade\n", from.Major)
		fmt.Println(from.Major)
		return nil
	}
	if err != nil {
		return err
	}
	work := cfg.Layout.Tmp("pg-upgrade")
	o := pg.UpgradeOptions{
		DataDir:  cfg.Layout.PostgresDir(),
		WorkDir:  work,
		User:     cmp.Or(cfg.Env["FLUXER_POSTGRES_USERNAME"], "fluxer"),
		Password: cfg.Env["POSTGRES_PASSWORD"],
		From:     from,
		To:       target,
		Log:      os.Stderr,
	}
	if err := pg.Upgrade(ctx, o); err != nil {
		return err
	}
	if err := analyze(ctx, cfg, o); err != nil {
		return fmt.Errorf("the database is PostgreSQL %d now, but updating its extensions or statistics failed. Fluxer runs without them, slower until autovacuum catches up: %w", target.Major, err)
	}
	fmt.Fprintf(os.Stderr, "the database is PostgreSQL %d now. The PostgreSQL %d files are in %s and can't start again, so your backup is the way back. Delete them with fluxerd postgres-upgrade --cleanup once Fluxer runs well\n",
		target.Major, from.Major, pg.OldDir(o.DataDir, from.Major))
	fmt.Println(target.Major)
	return nil
}

var errAlreadyOn = errors.New("the database is on that PostgreSQL already")

func upgradePlan(cfg *config.Config, to int) (pg.Install, pg.Install, error) {
	dir := cfg.Layout.PostgresDir()
	if err := pg.FinishUpgrade(dir); err != nil {
		return pg.Install{}, pg.Install{}, err
	}
	major, ok, err := pg.DataMajor(dir)
	if err != nil {
		return pg.Install{}, pg.Install{}, err
	}
	if !ok {
		return pg.Install{}, pg.Install{}, fmt.Errorf("%s holds no database yet, so there is nothing to upgrade", dir)
	}
	return pickUpgrade(major, to, pg.Installed(pg.LibRoot, ""))
}

func pickUpgrade(major, to int, installed []pg.Install) (pg.Install, pg.Install, error) {
	find := func(m int) (pg.Install, bool) {
		i := slices.IndexFunc(installed, func(in pg.Install) bool { return in.Major == m })
		if i < 0 {
			return pg.Install{}, false
		}
		return installed[i], true
	}
	from, ok := find(major)
	if !ok {
		return pg.Install{}, pg.Install{}, fmt.Errorf("the database is PostgreSQL %d and its programs aren't installed. Install postgresql-%d (from apt.postgresql.org if this release no longer has it) and run this again", major, major)
	}
	if to == 0 && len(installed) > 0 {
		to = installed[0].Major
	}
	if to < major {
		return pg.Install{}, pg.Install{}, fmt.Errorf("the database is PostgreSQL %d, and pg_upgrade can't move it back to PostgreSQL %d", major, to)
	}
	if to == major {
		return from, from, errAlreadyOn
	}
	target, ok := find(to)
	if !ok {
		return pg.Install{}, pg.Install{}, fmt.Errorf("PostgreSQL %d isn't installed. Install postgresql-%d first", to, to)
	}
	return from, target, nil
}

func analyze(ctx context.Context, cfg *config.Config, o pg.UpgradeOptions) error {
	t := backup.Target{Layout: cfg.Layout, Env: cfg.Env, PGBinDir: o.To.BinDir}
	srv, err := t.StartPostgres(ctx, o.WorkDir)
	if err != nil {
		return err
	}
	conn := []string{"-h", srv.Sock, "-p", strconv.Itoa(spec.PortPostgres), "-U", o.User}
	runErr := func() error {
		updates := filepath.Join(o.WorkDir, "update_extensions.sql")
		if _, err := os.Stat(updates); err == nil {
			fmt.Fprintln(os.Stderr, "updating the database extensions")
			if err := pgClient(ctx, o, "psql", slices.Concat(conn, []string{"-X", "-q", "-v", "ON_ERROR_STOP=1", "-d", "postgres", "-f", updates})...); err != nil {
				return err
			}
			os.Remove(updates)
		}
		args := slices.Concat(conn, []string{"--all", "--analyze-in-stages"})
		if o.To.Major >= 18 {
			args = append(args, "--missing-stats-only")
		}
		fmt.Fprintln(os.Stderr, "updating the database statistics")
		return pgClient(ctx, o, "vacuumdb", args...)
	}()
	os.Remove(filepath.Join(o.WorkDir, "delete_old_cluster.sh"))
	return errors.Join(runErr, srv.Stop())
}

func pgClient(ctx context.Context, o pg.UpgradeOptions, tool string, args ...string) error {
	cmd := exec.CommandContext(ctx, filepath.Join(o.To.BinDir, tool), args...)
	cmd.Env = append(os.Environ(), "PGPASSWORD="+o.Password)
	var out bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &out
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("%s: %w: %s", tool, err, strings.TrimSpace(out.String()))
	}
	return nil
}

func cleanupOldClusters(dir string) error {
	old := pg.OldDirs(dir)
	majors := make([]int, 0, len(old))
	for m := range old {
		majors = append(majors, m)
	}
	slices.Sort(majors)
	for _, m := range majors {
		if err := os.RemoveAll(old[m]); err != nil {
			return err
		}
		fmt.Fprintf(os.Stderr, "deleted the PostgreSQL %d files in %s\n", m, old[m])
		fmt.Println(m)
	}
	return nil
}
