// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"os"
	"slices"
	"strconv"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newPostgresCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:     "postgres",
		Short:   "Manage the PostgreSQL of a systemd install",
		Example: "rexctl postgres upgrade\nrexctl postgres upgrade --clean-up",
	}
	var to int
	var cleanup bool
	upgrade := &cobra.Command{
		Use:   "upgrade",
		Short: "Move the database to a newer PostgreSQL",
		Long: "Move Fluxer's database to the newest PostgreSQL installed on this machine, or the one --to picks. " +
			"rexctl backs up first, stops Fluxer, moves the database with pg_upgrade --link and starts Fluxer again. " +
			"The old files stay next to the database until you run rexctl postgres upgrade --clean-up. " +
			"That also lets apt remove the old PostgreSQL when rexctl installed it and nothing else on this machine uses it. " +
			"Only a systemd install uses the PostgreSQL installed on this machine. A Docker or Podman install uses the PostgreSQL that comes in its container image.",
		Example: "rexctl postgres upgrade\nrexctl postgres upgrade --to 17\nrexctl postgres upgrade --clean-up",
		Args:    noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			if cleanup {
				return runPostgresCleanup(c.Context(), app)
			}
			return runPostgresUpgrade(c.Context(), app, to)
		},
	}
	upgrade.Flags().IntVar(&to, "to", 0, "the PostgreSQL major to move to (default the newest installed)")
	upgrade.Flags().BoolVar(&cleanup, "clean-up", false, "delete the old database files an earlier upgrade kept")
	upgrade.Flags().BoolVar(&cleanup, "cleanup", false, "delete the old database files an earlier upgrade kept")
	_ = upgrade.Flags().MarkHidden("cleanup")
	upgrade.MarkFlagsMutuallyExclusive("to", "clean-up", "cleanup")
	c.AddCommand(upgrade)
	return c
}

func postgresInstance(ctx context.Context, app *App) (*instance.Instance, rt.Runtime, *native.Stack, bool, error) {
	inst, r, ok, err := app.instanceRuntime(ctx, true)
	if err != nil || !ok {
		return nil, nil, nil, false, err
	}
	if r.Kind() != rt.Native {
		return nil, nil, nil, false, ui.Refused("Only a systemd install uses the PostgreSQL installed on this machine.", "The container image brings its own PostgreSQL, and rexctl upgrade moves to the image that comes next.")
	}
	if inst.State.Settings["FLUXER_POSTGRES_URL"] != "" {
		return nil, nil, nil, false, ui.Refused("Fluxer uses your own PostgreSQL database.", "Upgrade it where it runs.")
	}
	return inst, r, native.New(inst), true, nil
}

func upgradeTarget(p native.Postgres, to int) (int, error) {
	switch {
	case !p.HasData():
		return 0, ui.Refused("Fluxer hasn't made its database yet.", "Run rexctl start, and Fluxer makes it with PostgreSQL "+fmt.Sprint(p.Newest())+".")
	case !p.DataInstalled():
		return 0, ui.Refused(fmt.Sprintf("The database is PostgreSQL %d, and PostgreSQL %d isn't installed.", p.Data, p.Data),
			fmt.Sprintf("Install %s (from apt.postgresql.org if this release no longer has it), then run rexctl postgres upgrade again. Or restore your latest backup with rexctl restore.", native.PackageName(p.Data)))
	case to == 0:
		return p.Newest(), nil
	case to <= p.Data:
		return 0, ui.Usage(fmt.Sprintf("The database is PostgreSQL %d already, so --to needs a newer major.", p.Data))
	case !slices.Contains(p.Installed, to):
		return 0, ui.Refused(fmt.Sprintf("PostgreSQL %d isn't installed.", to), fmt.Sprintf("Install %s (from apt.postgresql.org if this release doesn't have it), then run rexctl postgres upgrade --to %d again.", native.PackageName(to), to))
	}
	return to, nil
}

func runPostgresUpgrade(ctx context.Context, app *App, to int) error {
	inst, r, n, ok, err := postgresInstance(ctx, app)
	if err != nil || !ok {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		p := n.Postgres()
		target, err := upgradeTarget(p, to)
		if err != nil {
			return err
		}
		if target == p.Data {
			app.UI.OK("The database is PostgreSQL %d, the newest installed on this machine.", p.Data)
			return nil
		}
		set, err := deploy.ReleaseSet(inst)
		if err != nil {
			return err
		}
		path, err := backupWith(ctx, app, inst, r, rt.BackupOptions{Reason: "postgres-upgrade"}, nil, "Backing up your instance")
		if err != nil {
			return err
		}
		wasRunning := n.OwnsUnit(ctx) && n.ActiveState(ctx) == "active"
		var partial error
		err = app.UI.Step(ctx, fmt.Sprintf("Moving the database to PostgreSQL %d", target), func(ctx context.Context, s *ui.Step) error {
			if wasRunning {
				s.Title("Stopping Fluxer")
				if err := n.Halt(ctx); err != nil {
					return rt.NativeError(err, "Couldn't stop Fluxer.")
				}
				s.Title(fmt.Sprintf("Moving the database to PostgreSQL %d", target))
			}
			if _, err := n.UpgradePostgres(context.WithoutCancel(ctx), set, target, detailWriter{app.UI}); err != nil {
				after := n.Postgres()
				switch {
				case after.Data == target:
					partial = err
				case after.Broken:
					wasRunning = false
					return ui.Wrap(err, fmt.Sprintf("pg_upgrade stopped part way, so the PostgreSQL %d files can't start again.", p.Data), "Run rexctl restore "+shellWord(path)+" to put back the backup from just now.")
				default:
					return ui.Wrap(err, fmt.Sprintf("Couldn't move the database to PostgreSQL %d, so it is still PostgreSQL %d.", target, p.Data), upgradeFailureFix(n))
				}
			}
			s.Done("Moved the database from PostgreSQL %d to %d.", p.Data, target)
			return nil
		})
		if err != nil {
			if wasRunning {
				_ = app.UI.Step(context.WithoutCancel(ctx), "Starting Fluxer again", func(ctx context.Context, s *ui.Step) error {
					return r.Start(ctx, s, rt.StartOptions{})
				})
			}
			return err
		}
		if partial != nil {
			app.UI.Detail("%v", partial)
			app.UI.Warn(fmt.Sprintf("The database is PostgreSQL %d now, but updating its extensions and statistics after the move failed.", target), "Fluxer runs and speeds up as autovacuum catches up. Run rexctl logs if it reports database errors.")
		}
		if err := n.KeepPostgres(ctx, target); err != nil {
			app.UI.Warn(fmt.Sprintf("Couldn't mark %s as installed by hand, so apt autoremove could take it away.", native.PackageName(target)), "Run apt-mark manual "+native.PackageName(target)+".")
		}
		if wasRunning {
			if err := app.UI.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
				return r.Start(ctx, s, rt.StartOptions{})
			}); err != nil {
				return err
			}
		}
		app.UI.Info("The PostgreSQL %d files are in %s. They can't start again after the move, so going back means restoring %s.", p.Data, n.OldPostgresDir(p.Data), path)
		app.UI.Info("Run rexctl postgres upgrade --clean-up once Fluxer runs well, to delete them. apt can then remove PostgreSQL %d if rexctl installed it and nothing else uses it.", p.Data)
		return nil
	})
}

func upgradeFailureFix(n *native.Stack) string {
	fix := "Run rexctl postgres upgrade --verbose to see why."
	if _, err := os.Stat(n.PostgresUpgradeLogs()); err == nil {
		fix += " pg_upgrade's logs are in " + n.PostgresUpgradeLogs() + "."
	}
	return fix
}

func runPostgresCleanup(ctx context.Context, app *App) error {
	inst, _, n, ok, err := postgresInstance(ctx, app)
	if err != nil || !ok {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		set, err := deploy.ReleaseSet(inst)
		if err != nil {
			return err
		}
		if len(n.OldPostgresDirs()) == 0 {
			app.UI.OK("No old database files are left to delete.")
			return nil
		}
		var removed []int
		err = app.UI.Step(ctx, "Deleting the old database files", func(ctx context.Context, s *ui.Step) error {
			var err error
			if removed, err = n.CleanupPostgres(ctx, set, detailWriter{app.UI}); err != nil {
				return rt.NativeError(err, "Couldn't delete the old database files.")
			}
			s.Done("Deleted the old database files of PostgreSQL %s.", ui.JoinAnd(majorWords(removed)))
			return nil
		})
		if err != nil {
			return err
		}
		if unused := unusedPostgres(removed, n.Postgres(), inst.Dir); len(unused) > 0 {
			if err := n.ReleasePostgres(ctx, unused...); err != nil {
				app.UI.Detail("%v", err)
			}
		}
		return nil
	})
}

func majorWords(majors []int) []string {
	out := make([]string, 0, len(majors))
	for _, m := range majors {
		out = append(out, strconv.Itoa(m))
	}
	return out
}

func unusedPostgres(removed []int, p native.Postgres, dir string) []int {
	var out []int
	for _, m := range removed {
		if m == p.Data || m == p.Newest() || slices.Contains(otherNativeMajors(dir), m) {
			continue
		}
		out = append(out, m)
	}
	return out
}

func otherNativeMajors(dir string) []int {
	var out []int
	for _, o := range others(dir) {
		if o.State.IsNative() {
			out = append(out, native.New(o).Postgres().Data)
		}
	}
	return out
}
