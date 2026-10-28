// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"slices"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type statusService struct {
	Name   string `json:"name"`
	State  string `json:"state"`
	Health string `json:"health"`
}

type statusJSON struct {
	URL      string          `json:"url"`
	Domain   string          `json:"domain"`
	Release  string          `json:"release"`
	Engine   string          `json:"engine"`
	Project  string          `json:"project"`
	Dir      string          `json:"dir"`
	Running  bool            `json:"running"`
	Healthy  bool            `json:"healthy"`
	Services []statusService `json:"services"`
}

const statusLowDisk = 10 << 30

func newStatusCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "status",
		Short: "Show whether your instance is running",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runStatus(c.Context(), app)
		},
	}
	app.jsonFlag(c)
	return c
}

func runStatus(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	out := statusJSON{
		URL:      inst.Origin(),
		Domain:   inst.Domain(),
		Release:  inst.State.Release,
		Engine:   inst.State.Engine.Kind,
		Project:  inst.Project(),
		Dir:      inst.Dir,
		Services: []statusService{},
	}
	e, engineErr := app.Engine(ctx, inst)
	var p *engine.Project
	var containers []engine.Container
	var psErr error
	if engineErr == nil {
		p = e.Named(inst.Project(), inst.Dir)
		out.Engine = string(e.Kind)
		if containers, psErr = p.PS(ctx); psErr != nil {
			app.UI.Detail("%v", psErr)
		}
		if loaded, err := deploy.Load(ctx, inst, e); err == nil && psErr == nil && len(containers) > 0 {
			containers = withMissing(loaded.Services(), containers)
		}
	}
	running := 0
	var down, unhealthy, never []string
	missing := 0
	for _, c := range containers {
		out.Services = append(out.Services, statusService{Name: c.Service, State: c.State, Health: c.Health})
		switch {
		case c.State == "running":
			running++
			if c.Health == "unhealthy" || c.Health == "starting" {
				unhealthy = append(unhealthy, c.Service)
			}
		case stack.OneShot[c.Service] && c.State == "exited" && c.ExitCode == 0:
		case c.State == "created" || c.State == "missing":
			down = append(down, c.Service)
			if c.State == "created" {
				never = append(never, c.Service)
			} else {
				missing++
			}
		default:
			down = append(down, c.Service)
		}
	}
	out.Running = running > 0
	out.Healthy = len(containers) > 0 && len(down) == 0 && len(unhealthy) == 0

	healthy := engineErr == nil && psErr == nil && out.Running && len(down) == 0
	if !healthy {
		app.SetExit(ui.ExitRefused)
	}
	if app.Flags.JSON {
		return app.UI.JSON(out)
	}
	start := "Run rexctl start."
	var ej *instance.EjectedError
	if errors.As(inst.Managed(), &ej) {
		start = ej.Start()
	}
	unfinished := ej == nil && inst.State.InstallUnfinished() && (len(containers) == 0 || len(never)+missing > 0)
	locked := inst.Busy()
	busy := locked && (!out.Running || len(down) > 0 || len(unhealthy) > 0)
	var ee *ui.ExitError
	switch {
	case errors.As(engineErr, &ee):
		app.UI.Fail(ee.Msg, ee.Fix)
	case engineErr != nil:
		app.UI.Fail(engineErr.Error(), "")
	case psErr != nil:
		app.UI.Fail("rexctl can't see which Fluxer services are running.", "Run rexctl doctor to see why.")
	case busy && inst.State.Upgrade != nil:
		app.UI.Warn(fmt.Sprintf("Fluxer is being upgraded to %s.", inst.State.Upgrade.To), "Run rexctl status again when the upgrade is done.")
	case busy:
		app.UI.Warn("Another rexctl command is changing this instance.", "Run rexctl status again when it's done.")
	case unfinished:
		app.UI.Fail("The install didn't finish.", "Run rexctl install again to finish it.")
	case !out.Running && inst.State.Stopped:
		app.UI.Fail("Fluxer is stopped.", start)
	case !out.Running:
		app.UI.Fail("Fluxer isn't running.", start)
	case len(down) > 0 && len(never) == len(down):
		app.UI.Fail(checks.Services(never, "was never started.", "were never started."), start)
	case len(down) > 0 && len(never)+missing == len(down):
		app.UI.Fail(checks.Services(down, "isn't running.", "aren't running."), start)
	case len(down) > 0:
		app.UI.Fail(checks.Services(down, "isn't running.", "aren't running."), "Run rexctl doctor to find out why.")
	case len(unhealthy) > 0:
		app.UI.Warn(checks.Services(unhealthy, "isn't ready yet.", "aren't ready yet."), "Run rexctl doctor if this doesn't clear up in a few minutes.")
	default:
		app.UI.OK("Fluxer is running.")
	}
	if ej != nil {
		app.UI.Info("%s %s", ej.Error(), ej.Fix())
	}
	if u := inst.State.Upgrade; u != nil && ej == nil && !busy {
		if locked {
			app.UI.Info("Fluxer is being upgraded to %s.", u.To)
		} else {
			app.UI.Warn(fmt.Sprintf("The upgrade to %s didn't finish.", u.To), release.UnfinishedFix(u))
		}
	}
	if p != nil && out.Running && !inst.State.SetupDone {
		client := app.adminClient(inst, p)
		if done, err := setup.Done(ctx, client); err == nil && !done {
			fix := "Run rexctl setup to finish it here, or rexctl setup token to finish it in your browser."
			if admin, exists, err := setup.AccountsExist(ctx, client); err == nil && exists {
				fix = browserSetupFix(inst.Origin(), admin)
			}
			app.UI.Warn("Setup isn't finished yet.", fix)
		}
	}
	if free, err := host.FreeDisk(inst.Dir); err == nil && free < statusLowDisk {
		app.UI.Warn(fmt.Sprintf("This machine has only %s of free disk space left.", diskGB(free)), "Free up space on this machine before you upgrade.")
	}
	warnUnusedRootFiles(ctx, app, inst)
	if len(instance.UnseenEdits(inst)) > 0 {
		app.UI.Warn(fmt.Sprintf("rexctl put back its own version of files you changed. Your copies are in %s.", inst.Path(instance.StateDirName, instance.EditedDir)), "Run rexctl doctor to see them.")
	}
	for _, s := range out.Services {
		health := s.Health
		if health == "" {
			health = "-"
		}
		app.UI.Detail("%-20s %-10s %s", s.Name, s.State, health)
	}
	rows := [][]string{{"Address", out.URL}}
	if out.Release != "" {
		rows = append(rows, []string{"Version", out.Release})
	}
	rows = append(rows, []string{"Folder", out.Dir})
	for _, r := range rows {
		app.UI.Result(fmt.Sprintf("%-9s %s", r[0], r[1]))
	}
	return nil
}

func withMissing(expected []string, containers []engine.Container) []engine.Container {
	for _, name := range expected {
		if !slices.ContainsFunc(containers, func(c engine.Container) bool { return c.Service == name }) {
			containers = append(containers, engine.Container{Service: name, State: "missing"})
		}
	}
	return containers
}

func diskGB(b uint64) string {
	return fmt.Sprintf("%d GB", (b+(1<<29))>>30)
}

func warnUnusedRootFiles(ctx context.Context, app *App, inst *instance.Instance) {
	if app.Flags.JSON {
		return
	}
	for _, name := range inst.UnusedRootFiles(ctx) {
		app.UI.Warn(fmt.Sprintf("Fluxer no longer uses %s, which you changed after rexctl moved its files. Put your Caddy directives in a .caddy file in %s/ instead.", inst.Path(name), instance.CaddyDropInDir), "")
	}
}
