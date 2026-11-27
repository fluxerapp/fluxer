// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"slices"
	"sync/atomic"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	bootLockWait  = 10 * time.Minute
	bootRouteWait = 2 * time.Minute
)

func readyFailure(ctx context.Context, inst *instance.Instance, p *engine.Project, err error) error {
	if err == nil {
		return nil
	}
	if errors.Is(err, context.Canceled) {
		return err
	}
	var nr *engine.NotReadyError
	if errors.As(err, &nr) {
		msg, fix := "Fluxer didn't become ready in time.", "Run rexctl doctor to see what's wrong."
		switch {
		case nr.Fatal:
			msg, fix = "Fluxer's file storage didn't start.", "Run rexctl logs storage to see why."
		case len(nr.Crashing) > 0:
			msg, fix = checks.Services(nr.Crashing, "keeps stopping.", "keep stopping."), "Run rexctl doctor to find out why."
		}
		if path := keepStartLog(ctx, inst, p, err, nr); path != "" {
			fix = "Read " + path + " to see why."
		}
		return ui.Wrap(err, msg, fix)
	}
	if engine.ClassOf(err) != engine.ClassOther {
		return rt.EngineError("", err)
	}
	var ee *ui.ExitError
	if errors.As(err, &ee) {
		return err
	}
	fix := "Run rexctl logs to see why."
	if inst != nil && p != nil {
		if path := writeFailureLog(ctx, inst, p, "start.log", err, nil); path != "" {
			fix = "Read " + path + " to see why."
		}
	}
	return ui.Wrap(err, "Fluxer didn't start.", fix)
}

func keepStartLog(ctx context.Context, inst *instance.Instance, p *engine.Project, err error, nr *engine.NotReadyError) string {
	if inst == nil || p == nil {
		return ""
	}
	parts := slices.Concat(nr.Crashing, nr.Readiness.Failed, nr.Readiness.NotReady)
	slices.Sort(parts)
	return writeFailureLog(ctx, inst, p, "start.log", err, slices.Compact(parts))
}

func (a *App) progress(s *ui.Step, verb string) func(engine.Event) {
	return rt.Progress(s, verb, a.UI)
}

func (a *App) instanceRuntime(ctx context.Context, root bool) (*instance.Instance, rt.Runtime, bool, error) {
	inst, err := a.Instance()
	if err != nil {
		return nil, nil, false, err
	}
	r, err := a.Runtime(ctx, inst)
	if err != nil {
		return nil, nil, false, err
	}
	if root && r.Info(ctx).Root {
		ok, err := a.asRoot(inst.Dir)
		if err != nil || !ok {
			return nil, nil, false, err
		}
	}
	return inst, r, true, nil
}

func (a *App) prepare(inst *instance.Instance) error {
	if err := managed(inst); err != nil {
		return err
	}
	if err := a.drift(inst, true, true); err != nil {
		return err
	}
	if msg := deploy.Interrupted(inst); msg != "" {
		a.UI.Warn(msg, "")
	}
	return nil
}

func saveState(inst *instance.Instance, again string) error {
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Run "+again+" again.")
	}
	return nil
}

func newLogsCmd(app *App) *cobra.Command {
	var follow bool
	var since string
	var tail int
	c := &cobra.Command{
		Use:   "logs [part]",
		Short: "Show logs from your instance",
		Long: "Show logs from your instance. Every line starts with the part it comes from.\n\nThe parts are " + ui.JoinAnd(rt.PartNames()) +
			".\nWithout a part, rexctl shows every part.",
		Example: "rexctl logs\nrexctl logs api --since 10m\nrexctl logs gateway -f",
		Args:    maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			o := rt.LogOptions{Parts: args, Follow: follow, Since: since, Tail: tail}
			if inst.State.IsVoice() {
				o.Parts = nil
				return voiceMachineLogs(ctx, app, inst, args, o)
			}
			if _, err := rt.ServicesOf(args); err != nil {
				return err
			}
			r, err := app.Runtime(ctx, inst)
			if err != nil {
				return err
			}
			err = showLogs(ctx, app, r, o)
			if errors.Is(err, rt.ErrNeedsRoot) {
				return app.retryAsRoot(inst.Dir, err)
			}
			return err
		},
	}
	c.Flags().BoolVarP(&follow, "follow", "f", false, "keep printing new lines")
	c.Flags().StringVar(&since, "since", "", "show lines newer than this, like 10m or 2h")
	c.Flags().IntVarP(&tail, "tail", "n", 200, "how many lines to show from the end of each log")
	return c
}

func showLogs(ctx context.Context, app *App, r rt.Runtime, o rt.LogOptions) error {
	if app.UI.Tier() != ui.TierLive {
		return r.Logs(ctx, app.UI.Stdout(), o)
	}
	pr, pw := io.Pipe()
	done := make(chan error, 1)
	var ended atomic.Bool
	lctx, cancel := context.WithCancel(ctx)
	defer cancel()
	go func() {
		err := r.Logs(lctx, pw, o)
		ended.Store(true)
		pw.CloseWithError(err)
		done <- err
	}()
	verr := app.UI.LogView(ctx, pr, ui.LogOptions{Title: "Logs", Parts: o.Parts, Follow: o.Follow})
	quit := !ended.Load()
	cancel()
	pr.Close()
	if err := <-done; err != nil && !quit && !errors.Is(err, io.ErrClosedPipe) {
		return err
	}
	return verr
}

func newStartCmd(app *App) *cobra.Command {
	c := newBootableCmd(app, "start", "Start your instance", runStart, runBootStart)
	c.Long = "Start Fluxer and wait until every part is ready. A stopped instance stays stopped, also after this machine restarts, until you run rexctl start."
	c.Example = "rexctl start\nrexctl start --dir /opt/fluxer"
	return c
}

func newBootableCmd(app *App, use, short string, run, bootRun func(context.Context, *App) error) *cobra.Command {
	var boot bool
	c := &cobra.Command{
		Use:                   use,
		Short:                 short,
		Args:                  noArgs,
		DisableFlagsInUseLine: true,
		RunE: func(c *cobra.Command, _ []string) error {
			if boot {
				return bootRun(c.Context(), app)
			}
			return run(c.Context(), app)
		},
	}
	c.Flags().BoolVar(&boot, "boot", false, "")
	_ = c.Flags().MarkHidden("boot")
	return c
}

func runStart(ctx context.Context, app *App) error {
	inst, r, ok, err := app.instanceRuntime(ctx, true)
	if err != nil || !ok {
		return err
	}
	return startInstance(ctx, app, inst, r)
}

func startInstance(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime) error {
	return app.LockedWaiting(ctx, inst, func() error {
		if msg, fix, ok := inst.RestoreUnfinished(); ok {
			return ui.Refused(msg, fix)
		}
		if err := app.prepare(inst); err != nil {
			return err
		}
		if legacyCompose(inst) {
			if err := guardOldMounts(ctx, app, inst); err != nil {
				return err
			}
		}
		if r.Kind() == rt.Native {
			if err := fetchMissingRelease(ctx, app, inst); err != nil {
				return err
			}
		}
		err := app.UI.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
			if err := r.Start(ctx, s, rt.StartOptions{}); err != nil {
				return err
			}
			if err := r.Voice().StartAll(ctx, s); err != nil {
				s.Warn("The voice servers on this machine didn't start.", "Run rexctl voice status to see why.")
				app.UI.Detail("%v", err)
			}
			s.Done("Started Fluxer at %s.", inst.Origin())
			return nil
		})
		if err != nil {
			return err
		}
		if inst.State.Stopped || inst.State.Installing {
			inst.State.Stopped, inst.State.Installing = false, false
			if err := saveState(inst, "rexctl start"); err != nil {
				return err
			}
		}
		if c, err := app.runtimeAdmin(ctx, inst, r); err != nil {
			app.UI.Detail("%v", err)
		} else {
			app.importAdmins(ctx, inst, c)
		}
		return nil
	})
}

func bootInstance(app *App) (*instance.Instance, error) {
	dir, err := app.Dir()
	if err != nil {
		return nil, err
	}
	if _, err := instance.Find(dir); errors.Is(err, instance.ErrNotFound) || errors.Is(err, instance.ErrLegacy) {
		return nil, nil
	}
	return app.Instance()
}

func bootCompose(ctx context.Context, app *App) (*instance.Instance, error) {
	inst, err := bootInstance(app)
	if err != nil || inst == nil {
		return nil, err
	}
	if rt.KindOf(inst) != rt.Compose || inst.State.Ejected != nil {
		return nil, nil
	}
	return inst, nil
}

func runBootStart(ctx context.Context, app *App) error {
	inst, err := bootCompose(ctx, app)
	if err != nil || inst == nil || inst.State.Stopped {
		return err
	}
	if !bootStackUp(ctx, app, inst) {
		waitForRoute(ctx, bootRouteWait)
		if err := guardOldMounts(ctx, app, inst); err != nil {
			return err
		}
		if err := bootStack(ctx, app, inst); err != nil {
			return err
		}
		if err := reload(inst); err != nil {
			return err
		}
		if inst.State.Stopped || inst.State.Ejected != nil {
			return nil
		}
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	startVoiceAtBoot(ctx, app, inst, e)
	return nil
}

var (
	bootStackUp      = alreadyUp
	startVoiceAtBoot = startLocalVoiceServers
)

func bootStack(ctx context.Context, app *App, inst *instance.Instance) error {
	return bootLocked(ctx, app, inst, func() error {
		if err := reload(inst); err != nil {
			return err
		}
		if inst.State.Stopped || inst.State.Ejected != nil {
			return nil
		}
		e, err := app.Engine(ctx, inst)
		if err != nil {
			return err
		}
		if e.TooOld {
			return rt.EngineError("", e.Named(inst.Project(), inst.Dir).Start(ctx))
		}
		if u := inst.State.Upgrade; u != nil && phaseIndex(u.Phase) >= phaseIndex(release.PhaseRecreated) {
			next, err := release.SnapshotState(inst, u.To)
			if err != nil {
				return ui.Wrap(err, fmt.Sprintf("Couldn't read the state of Fluxer %s.", u.To), "Run rexctl doctor to see what's wrong.")
			}
			next.Engine = inst.State.Engine
			p, err := deploy.Load(ctx, &instance.Instance{Dir: inst.Dir, State: next}, e)
			if err != nil {
				return rt.EngineError("", err)
			}
			if err := p.Up(context.WithoutCancel(ctx), engine.UpOptions{RemoveOrphans: true, Wait: true, WaitTimeout: engine.DefaultReadyTimeout}); err != nil {
				return readyFailure(ctx, inst, p, err)
			}
			return inst.DropRootCopies(ctx, true)
		}
		if p, err := deploy.Load(ctx, inst, e); err == nil && stackRunning(ctx, p) {
			app.importAdmins(ctx, inst, app.adminClient(inst, p))
			return nil
		}
		r, err := app.Runtime(ctx, inst)
		if err != nil {
			return err
		}
		if err := r.Start(ctx, nil, rt.StartOptions{Boot: true}); err != nil {
			return err
		}
		app.importAdmins(ctx, inst, app.adminClient(inst, e.Named(inst.Project(), inst.Dir)))
		return nil
	})
}

func alreadyUp(ctx context.Context, app *App, inst *instance.Instance) bool {
	if inst.State.Upgrade != nil || (inst.State.AdminImport != nil && inst.State.AdminImport.SentAt == "") {
		return false
	}
	e, err := app.Engine(ctx, inst)
	if err != nil || e.TooOld {
		return false
	}
	p, err := deploy.Load(ctx, inst, e)
	return err == nil && stackRunning(ctx, p)
}

func stackRunning(ctx context.Context, p *engine.Project) bool {
	containers, err := p.PS(ctx)
	if err != nil {
		return false
	}
	r, fatal := engine.Assess(p.Services(), containers)
	return !fatal && len(r.NotReady) == 0 && len(r.Failed) == 0
}

func reload(inst *instance.Instance) error {
	fresh, err := instance.Find(inst.Dir)
	if err != nil {
		return err
	}
	inst.State = fresh.State
	return nil
}

func bootLocked(ctx context.Context, app *App, inst *instance.Instance, fn func() error) error {
	deadline := time.Now().Add(bootLockWait)
	for {
		l, err := inst.Lock(ctx)
		if err == nil {
			return app.holding(ctx, inst, l, fn)
		}
		if !errors.Is(err, instance.ErrLocked) || time.Now().After(deadline) {
			return app.lockedNow(ctx, inst, fn)
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(2 * time.Second):
		}
	}
}

func waitForRoute(ctx context.Context, limit time.Duration) {
	deadline := time.Now().Add(limit)
	for !defaultRoute() && time.Now().Before(deadline) {
		select {
		case <-ctx.Done():
			return
		case <-time.After(time.Second):
		}
	}
}

func defaultRoute() bool {
	for _, t := range [][2]string{{"udp4", "192.0.2.1:9"}, {"udp6", "[2001:db8::1]:9"}} {
		if c, err := net.Dial(t[0], t[1]); err == nil {
			c.Close()
			return true
		}
	}
	return false
}

func newStopCmd(app *App) *cobra.Command {
	c := newBootableCmd(app, "stop", "Stop your instance", runStop, runBootStop)
	c.Long = "Stop Fluxer but keep everything it runs with. It stays stopped, also after a restart, until you run rexctl start. To also remove its containers or runtime files, run rexctl down."
	c.Example = "rexctl stop\nrexctl stop --dir /opt/fluxer"
	return c
}

func announceVoice(s *ui.Step, inst *instance.Instance, verb string) {
	switch n := len(inst.State.Voice); {
	case n == 1:
		s.Info("%s the voice server %s too. Calls on it end.", verb, inst.State.Voice[0].ID)
	case n > 1:
		s.Info("%s %d voice servers too. Calls on them end.", verb, n)
	}
}

func runStop(ctx context.Context, app *App) error {
	inst, r, ok, err := app.instanceRuntime(ctx, true)
	if err != nil || !ok {
		return err
	}
	return app.LockedWaiting(ctx, inst, func() error {
		if err := managed(inst); err != nil {
			return err
		}
		err := app.UI.Step(ctx, "Stopping Fluxer", func(ctx context.Context, s *ui.Step) error {
			announceVoice(s, inst, "Stopping")
			if err := r.Stop(ctx, s); err != nil {
				return err
			}
			if err := r.Voice().StopAll(ctx, s); err != nil {
				app.UI.Detail("%v", err)
			}
			s.Done("Stopped Fluxer.")
			return nil
		})
		if err != nil {
			return err
		}
		inst.State.Stopped = true
		if err := saveState(inst, "rexctl stop"); err != nil {
			return err
		}
		app.UI.Info("Run rexctl start to start it again.")
		return nil
	})
}

func newDownCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:     "down",
		Short:   "Stop your instance and remove what it runs in",
		Long:    "Stop Fluxer and remove its containers and networks, or on a systemd install its runtime files. Your data, settings, backups and images stay. Fluxer stays stopped until you run rexctl start, which sets everything up again.",
		Example: "rexctl down\nrexctl down --dir /opt/fluxer",
		Args:    noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runDown(c.Context(), app)
		},
	}
}

func runDown(ctx context.Context, app *App) error {
	inst, r, ok, err := app.instanceRuntime(ctx, true)
	if err != nil || !ok {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		if err := managed(inst); err != nil {
			return err
		}
		title, done := "Removing Fluxer's containers", "Stopped Fluxer and removed its containers and networks."
		switch r.Kind() {
		case rt.Native:
			title, done = "Stopping Fluxer", "Stopped Fluxer and cleared its runtime files."
		case rt.Container:
			title, done = "Stopping Fluxer", "Stopped Fluxer's container."
		}
		err := app.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
			announceVoice(s, inst, "Removing")
			if err := r.Voice().StopAll(ctx, s); err != nil {
				app.UI.Detail("%v", err)
			}
			if err := r.Down(ctx, s); err != nil {
				return err
			}
			s.Done("%s", done)
			return nil
		})
		if err != nil {
			return err
		}
		inst.State.Stopped = true
		if err := saveState(inst, "rexctl down"); err != nil {
			return err
		}
		app.UI.Info("Your data and settings are kept. Run rexctl start to start Fluxer again.")
		return nil
	})
}

func runBootStop(ctx context.Context, app *App) error {
	inst, err := bootCompose(ctx, app)
	if err != nil || inst == nil {
		return err
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	p, err := deploy.Load(ctx, inst, e)
	if err != nil {
		app.UI.Detail("%v", err)
		p = e.Named(inst.Project(), inst.Dir)
	}
	return rt.EngineError("", p.Stop(context.WithoutCancel(ctx)))
}

func newRestartCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:     "restart [part...]",
		Short:   "Restart your instance or some of its parts",
		Long:    "Restart Fluxer, or only the parts you pass, and wait until each one is ready.\n\nThe parts are " + ui.JoinAnd(rt.PartNames()) + ".",
		Example: "rexctl restart\nrexctl restart api\nrexctl restart web gateway",
		RunE: func(c *cobra.Command, args []string) error {
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			if inst.State.IsVoice() {
				if len(args) > 0 {
					return ui.Usage("On a voice machine, rexctl restart restarts every voice server. Run rexctl voice restart <id> to restart one.")
				}
				return voiceMachineRestart(ctx, app, inst)
			}
			if _, err := rt.ServicesOf(args); err != nil {
				return err
			}
			return runRestart(ctx, app, args)
		},
	}
}

func runRestart(ctx context.Context, app *App, parts []string) error {
	inst, r, ok, err := app.instanceRuntime(ctx, true)
	if err != nil || !ok {
		return err
	}
	return app.LockedWaiting(ctx, inst, func() error {
		if err := app.prepare(inst); err != nil {
			return err
		}
		title, done := "Restarting Fluxer", "Restarted Fluxer."
		if len(parts) > 0 {
			title, done = "Restarting "+ui.JoinAnd(parts), "Restarted "+ui.JoinAnd(parts)+"."
		}
		err := app.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
			if err := r.Restart(ctx, s, parts); err != nil {
				return err
			}
			if len(parts) == 0 {
				if err := r.Voice().StartAll(ctx, s); err != nil {
					app.UI.Detail("%v", err)
				}
			}
			s.Done("%s", done)
			return nil
		})
		if err != nil || len(parts) > 0 || !inst.State.Stopped {
			return err
		}
		inst.State.Stopped = false
		return saveState(inst, "rexctl restart")
	})
}

func fetchMissingRelease(ctx context.Context, app *App, inst *instance.Instance) error {
	n := native.New(inst)
	if _, err := os.Stat(n.Fluxerd()); err == nil {
		return nil
	}
	rel, err := nativeRelease(ctx, inst.State.Release, "Run rexctl start on a machine Fluxer has a native build for.")
	if err != nil {
		return err
	}
	return app.UI.Step(ctx, "Downloading Fluxer", func(ctx context.Context, s *ui.Step) error {
		if _, err := n.Fetch(ctx, rel, s.Progress); err != nil {
			return rt.NativeError(err, "Couldn't download Fluxer.")
		}
		if err := n.Flip(rel.Version); err != nil {
			return rt.NativeError(err, "Couldn't unpack Fluxer.")
		}
		if _, err := n.WriteSettings(rel.Version); err != nil {
			return rt.NativeError(err, "Couldn't read the settings of Fluxer "+rel.Version+".")
		}
		s.Done("Downloaded Fluxer %s.", rel.Version)
		return nil
	})
}
