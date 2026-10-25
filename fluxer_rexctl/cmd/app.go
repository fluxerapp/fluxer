// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"net/url"
	"os"
	"os/exec"
	"os/user"
	"reflect"
	"runtime"
	"strconv"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type GlobalFlags struct {
	Dir     string
	Yes     bool
	JSON    bool
	Quiet   bool
	Verbose bool
}

type App struct {
	Version string
	Commit  string
	Flags   GlobalFlags
	UI      *ui.UI
	exit    int
	held    *instance.Instance
	engine  *engine.Engine
	warned  bool
}

func (a *App) init(c *cobra.Command) {
	a.UI = ui.New(ui.Options{
		Context: c.Context(),
		Yes:     a.Flags.Yes,
		JSON:    a.Flags.JSON,
		Quiet:   a.Flags.Quiet,
		Verbose: a.Flags.Verbose,
		Stdin:   c.InOrStdin(),
		Stdout:  c.OutOrStdout(),
		Stderr:  c.ErrOrStderr(),
	})
	var commands []string
	for _, sub := range c.Root().Commands() {
		commands = append(commands, sub.Name())
	}
	a.UI.CallSelf(selfCommand(), commands)
	host.SetTerminalHold(a.UI.Hold)
	if a.Flags.Verbose {
		engine.SetDetail(detailWriter{a.UI})
	}
}

func selfCommand() string {
	if runtime.GOOS == "windows" {
		return ""
	}
	exe, err := os.Executable()
	if err != nil {
		return ""
	}
	if found, err := exec.LookPath("rexctl"); err == nil {
		onPath, perr := os.Stat(found)
		running, rerr := os.Stat(exe)
		if perr == nil && rerr == nil && os.SameFile(onPath, running) {
			return ""
		}
	}
	if home, err := os.UserHomeDir(); err == nil {
		if rel, ok := strings.CutPrefix(exe, home+"/"); ok {
			return "~/" + rel
		}
	}
	return exe
}

type detailWriter struct{ u *ui.UI }

func (w detailWriter) Write(p []byte) (int, error) {
	for _, line := range strings.Split(strings.TrimRight(string(p), "\n"), "\n") {
		w.u.Detail("%s", line)
	}
	return len(p), nil
}

func (a *App) SetExit(code int) { a.exit = code }

func (a *App) Dir() (string, error) {
	if a.Flags.Dir != "" {
		return a.Flags.Dir, nil
	}
	return instance.Locate()
}

func (a *App) takeOverCommand() string {
	if a.Flags.Dir != "" {
		return "rexctl takeover --dir " + a.Flags.Dir
	}
	return "rexctl takeover"
}

func (a *App) Instance() (*instance.Instance, error) {
	dir, err := a.Dir()
	if err != nil {
		return nil, err
	}
	inst, err := instance.Find(dir)
	var se *instance.SettingError
	switch {
	case err == nil:
		if err := rootlessGuard(inst); err != nil {
			return nil, err
		}
		a.warnUnknown(inst)
		return inst, nil
	case errors.Is(err, instance.ErrNotFound):
		return nil, ui.Refused(fmt.Sprintf("There's no Fluxer instance in %s.", dir), "Run rexctl install, or pass --dir.")
	case errors.Is(err, instance.ErrLegacy):
		return nil, ui.Refused(fmt.Sprintf("rexctl doesn't manage the instance in %s yet.", dir), "Run "+a.takeOverCommand()+" to hand it to rexctl.")
	case errors.Is(err, instance.ErrNewerState):
		return nil, ui.Refused(fmt.Sprintf("A newer rexctl manages the instance in %s.", dir), "Download the latest rexctl, then try again.")
	case errors.As(err, &se):
		return nil, &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("The setting %s in %s isn't valid.", se.Key, dir), Fix: se.Fix(), Err: err}
	}
	return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't read the instance in %s.", dir), "Run rexctl as the account that installed Fluxer.")
}

func (a *App) warnUnknown(inst *instance.Instance) {
	if a.warned || inst.State == nil || len(inst.State.Unknown) == 0 {
		return
	}
	a.warned = true
	a.UI.Warn("This instance has settings this rexctl doesn't know: "+ui.JoinAnd(inst.State.Unknown)+".", "rexctl keeps them. Run rexctl upgrade if a newer rexctl wrote them.")
}

func rootlessGuard(inst *instance.Instance) error {
	if inst == nil || inst.State == nil || os.Geteuid() != 0 {
		return nil
	}
	uid, ok := inst.OwnerUID()
	if !ok || uid == 0 {
		return nil
	}
	pin := inst.State.Engine
	if pin.Host == "" {
		if pin.Kind != string(engine.Podman) {
			return nil
		}
	} else if owner, ok := socketOwner(pin.Host); !ok || owner != uid {
		return nil
	}
	name := strconv.Itoa(uid)
	if u, err := user.LookupId(name); err == nil {
		name = u.Username
	}
	return ui.Refused(fmt.Sprintf("Your instance runs on %s under the account %s, and root can't manage it.", engine.Kind(pin.Kind).Name(), name), fmt.Sprintf("Run rexctl as %s, without sudo.", name))
}

func socketOwner(host string) (int, bool) {
	u, err := url.Parse(host)
	if err != nil || u.Scheme != "unix" {
		return 0, false
	}
	info, err := os.Stat(u.Path)
	if err != nil {
		return 0, false
	}
	v := reflect.Indirect(reflect.ValueOf(info.Sys()))
	if v.Kind() != reflect.Struct {
		return 0, false
	}
	f := v.FieldByName("Uid")
	if !f.IsValid() || !f.CanUint() {
		return 0, false
	}
	return int(f.Uint()), true
}

func (a *App) Engine(ctx context.Context, inst *instance.Instance) (*engine.Engine, error) {
	if err := rootlessGuard(inst); err != nil {
		return nil, err
	}
	if a.engine != nil {
		return a.engine, nil
	}
	opts := engine.ConnectOptions{AllowOld: true}
	if inst != nil && inst.State != nil {
		pin := inst.State.Engine
		opts.Host, opts.ID, opts.Prefer = pin.Host, pin.ID, engine.Kind(pin.Kind)
	}
	e, err := engine.Connect(ctx, opts)
	if err != nil {
		if inst != nil && inst.State != nil {
			return nil, engineFailureFor(engine.Kind(inst.State.Engine.Kind), err)
		}
		return nil, engineFailure(err)
	}
	a.engine = e
	if a.held != nil && a.held == inst {
		if err := a.pin(inst, e); err != nil {
			return nil, err
		}
	}
	return e, nil
}

func (a *App) pin(inst *instance.Instance, e *engine.Engine) error {
	cur := inst.State.Engine
	if cur.ID != "" && cur.ID != e.ID {
		return nil
	}
	next := instance.EngineRef{Kind: string(e.Kind), Host: e.Host, ID: e.ID}
	if cur == next {
		return nil
	}
	inst.State.Engine = next
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	if cur.Host != "" && cur.Host != e.Host {
		a.UI.Detail("The engine moved from %s to %s.", cur.Host, e.Host)
	}
	return nil
}

func (a *App) Named(ctx context.Context, inst *instance.Instance) (*engine.Project, error) {
	e, err := a.Engine(ctx, inst)
	if err != nil {
		return nil, err
	}
	return e.Named(inst.Project(), inst.Dir), nil
}

func (a *App) Admin(ctx context.Context) (*adminapi.Client, *instance.Instance, error) {
	inst, err := a.Instance()
	if err != nil {
		return nil, nil, err
	}
	p, err := a.Named(ctx, inst)
	if err != nil {
		return nil, nil, err
	}
	return a.adminClient(inst, p), inst, nil
}

func (a *App) adminClient(inst *instance.Instance, p *engine.Project) *adminapi.Client {
	c := adminapi.New(adminapi.NewExec(p), a.Version)
	c.OnSkew = func(apiVersion string) {
		if inst.State != nil && (inst.State.Release == a.Version || inst.State.Upgrade != nil && inst.State.Upgrade.To == a.Version) {
			return
		}
		if inst.State != nil && inst.State.FailedRelease == a.Version {
			a.UI.Warn(fmt.Sprintf("rexctl is %s and Fluxer is still %s. The upgrade to %s didn't start.", a.Version, apiVersion, a.Version), "")
			return
		}
		fix := "Run rexctl upgrade."
		if inst.State != nil && inst.State.Ejected != nil {
			fix = "Run rexctl takeover, then rexctl upgrade."
		}
		a.UI.Warn(fmt.Sprintf("rexctl is %s and Fluxer is %s.", a.Version, apiVersion), fix)
	}
	return c
}

func (a *App) Locked(ctx context.Context, inst *instance.Instance, fn func() error) error {
	l, err := inst.Lock(ctx)
	if errors.Is(err, instance.ErrLocked) {
		return ui.Refused("Another rexctl command is changing this instance.", "Wait for it to finish, then try again.")
	}
	if err != nil {
		return err
	}
	return a.holding(inst, l, fn)
}

func (a *App) holding(inst *instance.Instance, l *instance.Lock, fn func() error) error {
	defer l.Release()
	a.held = inst
	defer func() { a.held = nil }()
	if a.engine != nil {
		if err := a.pin(inst, a.engine); err != nil {
			return err
		}
	}
	return fn()
}

func (a *App) Change(ctx context.Context, inst *instance.Instance, fn func(e *engine.Engine) error) error {
	return a.change(ctx, inst, true, fn)
}

func (a *App) ChangeSettings(ctx context.Context, inst *instance.Instance, fn func(e *engine.Engine) error) error {
	return a.change(ctx, inst, false, fn)
}

func (a *App) change(ctx context.Context, inst *instance.Instance, withEnv bool, fn func(e *engine.Engine) error) error {
	return a.Locked(ctx, inst, func() error {
		e, err := a.guard(ctx, inst, withEnv)
		if err != nil {
			return err
		}
		return fn(e)
	})
}

func (a *App) guard(ctx context.Context, inst *instance.Instance, withEnv bool) (*engine.Engine, error) {
	if err := managed(inst); err != nil {
		return nil, err
	}
	if err := a.drift(inst, true, withEnv); err != nil {
		return nil, err
	}
	e, err := a.Engine(ctx, inst)
	if err != nil {
		return nil, err
	}
	if err := a.replacePodmanCompose(ctx, inst, e); err != nil {
		return nil, err
	}
	interrupted := deploy.Interrupted(inst)
	if err := deploy.Resolve(ctx, inst, e); err != nil {
		return nil, engineFailure(err)
	}
	if interrupted != "" {
		a.UI.Warn(interrupted, "")
	}
	return e, nil
}

func managed(inst *instance.Instance) error {
	err := inst.Managed()
	var ej *instance.EjectedError
	if errors.As(err, &ej) {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: ej.Error(), Fix: ej.Fix(), Err: err}
	}
	return err
}

func (a *App) drift(inst *instance.Instance, clean, withEnv bool) error {
	r, err := instance.Drift(inst)
	if err != nil {
		return ui.Wrap(err, "rexctl can't check Fluxer's files.", "Check that you can read "+inst.Dir+", then try again.")
	}
	if r.Env != nil && !r.Env.Same && !withEnv {
		r.Env = nil
	}
	if r.Env != nil && r.Env.Same {
		if clean {
			if err := instance.RemoveEnv(inst); err != nil {
				return ui.Wrap(err, "Couldn't remove the .env file rexctl doesn't read.", "Delete "+r.Env.Path+" yourself, then try again.")
			}
			a.UI.Info("Removed %s, which only held settings rexctl already keeps.", r.Env.Path)
		}
		r.Env = nil
	}
	var de *instance.DriftError
	if err := r.Err(); errors.As(err, &de) {
		fix := de.Fix
		if len(de.Lines) > 0 {
			fix += "\n    " + strings.Join(de.Lines, "\n    ")
		}
		return &ui.ExitError{Code: ui.ExitRefused, Msg: de.Msg, Fix: fix, Err: err}
	} else if err != nil {
		return err
	}
	return nil
}

func (a *App) replacePodmanCompose(ctx context.Context, inst *instance.Instance, e *engine.Engine) error {
	if e.Kind != engine.Podman {
		return nil
	}
	p := e.Named(inst.Project(), inst.Dir)
	containers, err := p.PS(ctx)
	if err != nil {
		return engineFailure(err)
	}
	found := false
	for _, c := range containers {
		if c.Labels[engine.PodmanComposeLabel] != "" {
			found = true
			break
		}
	}
	if !found {
		return nil
	}
	if !a.UI.Interactive() && !a.UI.Yes() {
		return &ui.ExitError{Code: ui.ExitUsage, Msg: "Fluxer restarts once while rexctl replaces podman-compose's containers. Your data stays.", Fix: "Pass --yes to go ahead."}
	}
	ok, err := a.UI.Confirm("Fluxer restarts once while rexctl replaces podman-compose's containers. Your data stays. Go ahead?", true)
	if err != nil {
		return err
	}
	if !ok {
		return ui.Refused("Stopped without changing anything.", "")
	}
	return a.UI.Step(ctx, "Replacing podman-compose's containers", func(ctx context.Context, s *ui.Step) error {
		ctx = context.WithoutCancel(ctx)
		if err := p.RemovePodmanCompose(ctx); err != nil {
			return engineFailure(err)
		}
		if err := p.Down(ctx, false); err != nil {
			return engineFailure(err)
		}
		s.Done("Replaced podman-compose's containers.")
		return nil
	})
}

func (a *App) sendAdminImport(ctx context.Context, inst *instance.Instance, p *engine.Project) {
	if a.held != inst {
		return
	}
	if err := deploy.SendAdminImport(ctx, inst, a.adminClient(inst, p)); err != nil {
		a.UI.Detail("%v", err)
	}
}

func (a *App) jsonFlag(c *cobra.Command) {
	c.Flags().BoolVar(&a.Flags.JSON, "json", false, "print JSON")
}

func confirmFlag(c *cobra.Command, v *string) {
	c.Flags().StringVar(v, "confirm", "", "your instance's domain, to confirm without a prompt")
}

func noArgs(c *cobra.Command, args []string) error {
	if len(args) == 0 {
		return nil
	}
	if c.HasAvailableSubCommands() {
		return &ui.ExitError{
			Code: ui.ExitUsage,
			Msg:  fmt.Sprintf("%s has no %s command.", c.CommandPath(), args[0]),
			Fix:  fmt.Sprintf("Run %s --help to see every command.", c.CommandPath()),
		}
	}
	return ui.Usage(fmt.Sprintf("%s takes no arguments.", c.CommandPath()))
}

func exactArgs(names ...string) cobra.PositionalArgs {
	return func(c *cobra.Command, args []string) error {
		if len(args) < len(names) {
			return ui.Usage(fmt.Sprintf("Pass %s.", names[len(args)]))
		}
		if len(args) > len(names) {
			if len(names) == 1 {
				return ui.Usage(fmt.Sprintf("%s takes one argument.", c.CommandPath()))
			}
			return ui.Usage(fmt.Sprintf("%s takes %d arguments.", c.CommandPath(), len(names)))
		}
		for i, a := range args {
			if strings.TrimSpace(a) == "" && names[i] != valueArg {
				return ui.Usage(fmt.Sprintf("Pass %s.", names[i]))
			}
		}
		return nil
	}
}

const valueArg = "a value"

func maxArgs(n int) cobra.PositionalArgs {
	return func(c *cobra.Command, args []string) error {
		if len(args) > n {
			if n == 1 {
				return ui.Usage(fmt.Sprintf("%s takes at most one argument.", c.CommandPath()))
			}
			return ui.Usage(fmt.Sprintf("%s takes at most %d arguments.", c.CommandPath(), n))
		}
		return nil
	}
}
