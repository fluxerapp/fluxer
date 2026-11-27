// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
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
	Version    string
	Commit     string
	Flags      GlobalFlags
	UI         *ui.UI
	exit       int
	outcome    string
	moveNotice string
	putBack    []string
	held       *instance.Instance
	engine     *engine.Engine
	warned     bool
	located    *instance.Located
	command    string
	errorFile  string

	keepLayout bool
}

func (a *App) init(c *cobra.Command) {
	a.command = c.CommandPath()
	a.UI.SetOptions(ui.Options{
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
	if ranWithSudo() {
		return sudoCommand(exe, nil, "")
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
	l, err := a.Locate()
	return l.Dir, err
}

func (a *App) Locate() (instance.Located, error) {
	first := a.located == nil
	l, err := a.locate()
	if err == nil && first && l.Stale != "" {
		a.warnStale(l)
	}
	if err == nil && first {
		a.warnShared(l)
	}
	return l, err
}

func (a *App) warnShared(l instance.Located) {
	if l.Shared == "" {
		return
	}
	a.UI.Warn(fmt.Sprintf("rexctl uses the instance in %s because you picked it, but other accounts can change %s.", l.Dir, l.Shared), "If other people have accounts on this machine, keep Fluxer in a folder only root can change, such as /opt/fluxer.")
}

func (a *App) warnStale(l instance.Located) {
	a.UI.Warn(fmt.Sprintf("rexctl remembers Fluxer in %s as the default, but there's no instance there now. Using %s.", l.Stale, l.Dir), "Run rexctl use <folder> to pick another default.")
}

func (a *App) locate() (instance.Located, error) {
	if a.located != nil {
		return *a.located, nil
	}
	l, err := instance.Resolve(a.Flags.Dir)
	var several *instance.SeveralError
	switch {
	case errors.As(err, &several):
		if several.Stale != "" {
			a.UI.Warn(fmt.Sprintf("rexctl remembers Fluxer in %s as the default, but there's no instance there now.", several.Stale), "")
		}
		if !a.UI.Interactive() {
			return l, &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("This machine has Fluxer instances in %s.", ui.JoinAnd(several.Dirs)), Fix: "Pass --dir with the one you mean, or run rexctl use <folder> to make one the default.", Err: err}
		}
		opts := make([]ui.Option, len(several.Dirs))
		for i, d := range several.Dirs {
			opts[i] = ui.Option{Label: d, Value: d}
		}
		var dir string
		if err := a.UI.Ask(nil, ui.Field{Title: "Which Fluxer instance?", Value: &dir, Options: opts}); err != nil {
			return l, err
		}
		a.UI.Info("Run rexctl use %s to make it the default.", dir)
		l = instance.Located{Dir: dir, From: instance.FromPicked}
	case err != nil:
		return l, untrustedRefusal(err)
	}
	a.located = &l
	return l, nil
}

func untrustedRefusal(err error) error {
	var untrusted *instance.UntrustedError
	if !errors.As(err, &untrusted) {
		return err
	}
	switch {
	case untrusted.Link != "":
		return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("%s is a link in %s, which other accounts can change, so root doesn't use the instance in %s.", untrusted.Link, untrusted.Path, untrusted.Dir), Fix: "Pass --dir " + shellWord(instance.RealPath(untrusted.Dir)) + ", the folder the link leads to.", Err: err}
	case untrusted.Dir != "" && untrusted.Shared && untrusted.Inside:
		msg := fmt.Sprintf("Other accounts can change %s, so root doesn't use the instance in %s.", untrusted.Path, untrusted.Dir)
		if instance.RealPath(untrusted.Path) == instance.RealPath(untrusted.Dir) {
			msg = fmt.Sprintf("Other accounts can change %s, so root doesn't use the instance in it.", untrusted.Dir)
		}
		return &ui.ExitError{Code: ui.ExitRefused, Msg: msg, Fix: fmt.Sprintf("Make sure only its owner can write to it, for example with chmod go-w %s, then run the command again.", shellWord(untrusted.Path)), Err: err}
	case untrusted.Dir != "" && untrusted.Shared:
		return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("rexctl found Fluxer in %s, but other accounts can change %s, so root doesn't use it on its own.", untrusted.Dir, untrusted.Path), Fix: "If you mean that instance, pass --dir " + shellWord(untrusted.Dir) + ".", Err: err}
	case untrusted.Dir != "":
		return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("rexctl found Fluxer in %s, but another account owns %s, so root doesn't use it on its own.", untrusted.Dir, untrusted.Path), Fix: "If you trust that account, pass --dir " + shellWord(untrusted.Dir) + ".", Err: err}
	}
	return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("Accounts other than root can change %s, so root doesn't use it.", untrusted.Path), Fix: "Make root its owner and the only account that can write to it, or pass --dir.", Err: err}
}

func (a *App) pickedDir() string {
	if a.located == nil || a.located.From == instance.FromFlag || a.located.From == instance.FromWorkdir {
		return ""
	}
	return a.located.Dir
}

func (a *App) remember(dir string) {
	if err := instance.Remember(instance.OwnScope(), dir, false); err != nil {
		a.UI.Detail("recording the instance in %s: %v", instance.RecordPath(instance.OwnScope()), err)
	}
}

func (a *App) rememberFirst(inst *instance.Instance) {
	if inst.State == nil || inst.State.Ejected != nil || (a.located != nil && a.located.From == instance.FromSudoer) {
		return
	}
	if euid() == 0 && instance.RootPath(inst.Dir, false) != "" {
		return
	}
	if err := instance.RememberFirst(instance.OwnScope(), inst.Dir); err != nil {
		a.UI.Detail("recording the instance in %s: %v", instance.RecordPath(instance.OwnScope()), err)
	}
}

func (a *App) forget(dir string) {
	s := instance.OwnScope()
	if err := instance.Forget(s, dir); err != nil {
		a.UI.Detail("%v", err)
		a.UI.Warn(fmt.Sprintf("rexctl still lists %s in %s.", dir, instance.RecordPath(s)), "Remove it from that file.")
	}
}

func (a *App) Instance() (*instance.Instance, error) {
	return a.InstanceRepairing(nil)
}

func (a *App) InstanceRepairing(keys []string) (*instance.Instance, error) {
	dir, err := a.Dir()
	if err != nil {
		return nil, err
	}
	inst, err := instance.FindRepairing(dir, keys)
	var se *instance.SettingError
	switch {
	case err == nil:
		if err := rootlessGuard(inst); err != nil {
			return nil, err
		}
		if inst.State.IsVoice() && !voiceCommandAllowed(a.command) {
			return nil, voiceMachineRefusal(dir, inst)
		}
		if legacyCompose(inst) && legacyCommandRefused(a.command) {
			return nil, legacyRefusal()
		}
		a.warnUnknown(inst)
		if len(keys) == 0 {
			a.saveRepaired(inst)
		}
		a.rememberFirst(inst)
		return inst, nil
	case errors.Is(err, instance.ErrNotFound):
		if other := ownersInstance(); other != "" && a.located != nil && a.located.From == instance.FromFallback {
			return nil, ui.Refused(fmt.Sprintf("There's no Fluxer instance in %s, and there's one in %s.", dir, other), "Pass --dir "+shellWord(other)+" if you mean that one.")
		}
		if tool := handMadeFluxer(context.Background(), dir); tool != "" {
			return nil, ui.Refused(fmt.Sprintf("There's no Fluxer instance from rexctl in %s. The Fluxer on this machine runs on %s without rexctl, and rexctl can't manage it.", dir, tool), "Keep managing it with the same "+strings.ToLower(tool)+" commands you set it up with.")
		}
		return nil, ui.Refused(fmt.Sprintf("There's no Fluxer instance in %s.", dir), "Run rexctl install, or pass --dir.")
	case errors.Is(err, instance.ErrLegacy):
		return nil, preRexctlRefusal(context.Background(), dir)
	case errors.Is(err, instance.ErrNewerState):
		return nil, ui.Refused(fmt.Sprintf("A newer rexctl manages the instance in %s.", dir), "Download the latest rexctl, then try again.")
	case errors.As(err, &se):
		return nil, settingRefusal(dir, se)
	}
	if uid, ok := (&instance.Instance{Dir: dir}).OwnerUID(); ok && uid == 0 && euid() != 0 {
		if !a.UI.Interactive() {
			return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't read the instance in %s, because root manages it.", dir), "Run "+sudoAgain(dir)+".")
		}
		if _, err := a.asRoot(dir); err != nil {
			return nil, err
		}
		return nil, errDeclined
	}
	return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't read the instance in %s.", dir), "Run rexctl as the account that installed Fluxer.")
}

func settingRefusal(dir string, se *instance.SettingError) error {
	msg := fmt.Sprintf("The setting %s in %s isn't valid.", se.Key, dir)
	if se.Remedy != "" {
		msg = se.Problem
	}
	return &ui.ExitError{Code: ui.ExitRefused, Msg: msg, Fix: se.Fix(), Err: se}
}

func (a *App) saveRepaired(inst *instance.Instance) {
	if len(inst.State.Repaired) == 0 {
		return
	}
	l, err := inst.Lock(context.Background())
	if err != nil {
		return
	}
	defer l.Release()
	if err := inst.Save(); err != nil {
		a.UI.Detail("%v", err)
		return
	}
	a.UI.Info("rexctl moved the address in %s to %s=%s.", ui.JoinAnd(inst.State.Repaired), settings.EdgeListen, inst.State.Settings[settings.EdgeListen])
	inst.State.Repaired = nil
}

func (a *App) warnUnknown(inst *instance.Instance) {
	if a.warned || inst.State == nil || len(inst.State.Unknown) == 0 {
		return
	}
	a.warned = true
	a.UI.Warn("This instance has settings this rexctl doesn't know: "+ui.JoinAnd(inst.State.Unknown)+".", "rexctl keeps them. Run rexctl upgrade if a newer rexctl wrote them.")
}

var euid = os.Geteuid

func rootlessGuard(inst *instance.Instance) error {
	if euid() != 0 {
		return nil
	}
	kind, name, ok := rootlessEngine(inst)
	if !ok {
		return nil
	}
	return ui.Refused(fmt.Sprintf("Your instance runs on %s under the account %s, and root can't manage it.", kind, name), fmt.Sprintf("Run rexctl as %s, without sudo.", name))
}

func rootlessEngine(inst *instance.Instance) (string, string, bool) {
	if inst == nil || inst.State == nil {
		return "", "", false
	}
	uid, ok := inst.OwnerUID()
	if !ok || uid == 0 {
		return "", "", false
	}
	pin := inst.State.Engine
	if pin.Host == "" {
		if pin.Kind != string(engine.Podman) {
			return "", "", false
		}
	} else if owner, ok := socketOwner(pin.Host); !ok || owner != uid {
		return "", "", false
	}
	return engine.Kind(pin.Kind).Name(), accountName(uid), true
}

func socketOwner(addr string) (int, bool) {
	u, err := url.Parse(addr)
	if err != nil || u.Scheme != "unix" {
		return 0, false
	}
	uid, _, ok := host.FileOwner(u.Path)
	return uid, ok
}

func (a *App) Engine(ctx context.Context, inst *instance.Instance) (*engine.Engine, error) {
	switch rt.KindOf(inst) {
	case rt.Native:
		return nil, rt.NeedsContainers(a.command)
	case rt.Container:
		return nil, rt.NeedsCompose(a.command)
	}
	return a.anyEngine(ctx, inst)
}

func (a *App) anyEngine(ctx context.Context, inst *instance.Instance) (*engine.Engine, error) {
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
			return nil, rt.EngineError(engine.Kind(inst.State.Engine.Kind), err)
		}
		return nil, rt.EngineError("", err)
	}
	a.engine = e
	if a.held != nil && a.held == inst {
		if err := a.pin(inst, e); err != nil {
			return nil, err
		}
	}
	return e, nil
}

func (a *App) Runtime(ctx context.Context, inst *instance.Instance) (rt.Runtime, error) {
	if err := rootlessGuard(inst); err != nil {
		return nil, err
	}
	r, err := rt.OpenWith(ctx, inst, a.UI, func(ctx context.Context) (*engine.Engine, error) {
		switch {
		case rt.KindOf(inst) == rt.Container && inst.State.IsVoice():
			return rt.VoiceEngine(ctx, inst)
		case rt.KindOf(inst) == rt.Container:
			return a.anyEngine(ctx, inst)
		}
		return a.Engine(ctx, inst)
	})
	if err != nil || r.Kind() != rt.Compose {
		return r, err
	}
	return composeRuntime{Runtime: r, app: a, inst: inst}, nil
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
	if err := saveInstance(inst); err != nil {
		return err
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
	r, err := a.Runtime(ctx, inst)
	if err != nil {
		return nil, nil, err
	}
	t, err := r.Admin(ctx)
	if errors.Is(err, rt.ErrNeedsRoot) {
		ok, rerr := a.asRoot(inst.Dir)
		switch {
		case rerr != nil:
			return nil, nil, rerr
		case ok:
			return nil, nil, err
		}
		return nil, nil, errDeclined
	}
	if err != nil {
		return nil, nil, err
	}
	return a.adminFor(inst, t), inst, nil
}

func (a *App) adminClient(inst *instance.Instance, p *engine.Project) *adminapi.Client {
	return a.adminFor(inst, adminapi.NewExec(p))
}

func (a *App) instanceAdmin(ctx context.Context, inst *instance.Instance) (*adminapi.Client, error) {
	r, err := a.Runtime(ctx, inst)
	if err != nil {
		return nil, err
	}
	return a.runtimeAdmin(ctx, inst, r)
}

func (a *App) runtimeAdmin(ctx context.Context, inst *instance.Instance, r rt.Runtime) (*adminapi.Client, error) {
	t, err := r.Admin(ctx)
	if err != nil {
		return nil, err
	}
	return a.adminFor(inst, t), nil
}

func (a *App) adminFor(inst *instance.Instance, t adminapi.Transport) *adminapi.Client {
	c := adminapi.New(t, a.Version)
	c.OnSkew = func(apiVersion string) {
		if inst.State != nil && (inst.State.Release == a.Version || inst.State.Upgrade != nil && inst.State.Upgrade.To == a.Version) || legacyCompose(inst) {
			return
		}
		if inst.State != nil && inst.State.Release != "" {
			apiVersion = inst.State.Release
		}
		if inst.State != nil && inst.State.FailedRelease == a.Version {
			a.UI.Warn(fmt.Sprintf("rexctl is %s and Fluxer is still %s. The upgrade to %s didn't start.", a.Version, apiVersion, a.Version), "")
			return
		}
		if inst.State != nil && inst.State.RolledBackFrom == a.Version {
			a.UI.Info("Fluxer went back from %s to %s. Once the problem is fixed, rexctl upgrade goes forward again.", a.Version, apiVersion)
			return
		}
		fix := "Run rexctl upgrade."
		if inst.State != nil && inst.State.Ejected != nil {
			fix = takeoverFixOffline
		}
		a.UI.Warn(fmt.Sprintf("rexctl is %s and Fluxer is %s.", a.Version, apiVersion), fix)
	}
	return c
}

func (a *App) Locked(ctx context.Context, inst *instance.Instance, fn func() error) error {
	l, err := inst.Lock(ctx)
	if errors.Is(err, instance.ErrLocked) && host.BootUnitStarting(ctx, bootUnitName(inst)) {
		l, err = a.waitForBootStart(ctx, inst)
	}
	if errors.Is(err, instance.ErrLocked) {
		return ui.Refused("Another rexctl command is changing this instance.", "Wait for it to finish, then try again.")
	}
	if err != nil {
		return err
	}
	return a.holding(ctx, inst, l, fn)
}

func bootUnitName(inst *instance.Instance) string {
	if inst.State.IsNative() {
		return native.New(inst).Unit()
	}
	return inst.Project() + ".service"
}

func (a *App) lockedNow(ctx context.Context, inst *instance.Instance, fn func() error) error {
	l, err := inst.Lock(ctx)
	if errors.Is(err, instance.ErrLocked) {
		return ui.Refused("Another rexctl command is changing this instance.", "Wait for it to finish, then try again.")
	}
	if err != nil {
		return err
	}
	return a.holding(ctx, inst, l, fn)
}

func (a *App) waitForBootStart(ctx context.Context, inst *instance.Instance) (*instance.Lock, error) {
	deadline := time.Now().Add(bootLockWait)
	return a.waitForLock(ctx, inst, "Waiting for Fluxer to finish starting", "Fluxer finished starting.", func() bool {
		return time.Now().Before(deadline) && host.BootUnitStarting(ctx, bootUnitName(inst))
	})
}

func (a *App) LockedWaiting(ctx context.Context, inst *instance.Instance, fn func() error) error {
	l, err := inst.Lock(ctx)
	if errors.Is(err, instance.ErrLocked) {
		was := inst.State.Runtime
		l, err = a.waitForLock(ctx, inst, "Waiting for another rexctl command to finish", "The other rexctl command finished.", func() bool { return true })
		if err == nil {
			err = reload(inst)
			if err == nil && inst.State.Runtime != was {
				err = ui.Refused("The other rexctl command moved this instance to another runtime.", "Run the same command again.")
			}
			if err != nil {
				l.Release()
			}
		}
	}
	if err != nil {
		return err
	}
	return a.holding(ctx, inst, l, fn)
}

func (a *App) waitForLock(ctx context.Context, inst *instance.Instance, title, done string, keepWaiting func() bool) (*instance.Lock, error) {
	var l *instance.Lock
	err := a.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
		for {
			got, err := inst.Lock(ctx)
			if err == nil {
				l = got
				s.Done("%s", done)
				return nil
			}
			if !errors.Is(err, instance.ErrLocked) || !keepWaiting() {
				return err
			}
			select {
			case <-ctx.Done():
				return ctx.Err()
			case <-time.After(2 * time.Second):
			}
		}
	})
	return l, err
}

func (a *App) holding(ctx context.Context, inst *instance.Instance, l *instance.Lock, fn func() error) error {
	defer l.Release()
	if err := rootlessGuard(inst); err != nil {
		return err
	}
	if err := checkOwnership(a, inst); err != nil {
		return err
	}
	defer giveFilesBack(a, inst)
	a.held = inst
	defer func() { a.held = nil }()
	a.migrate(ctx, inst)
	if a.engine != nil {
		if err := a.pin(inst, a.engine); err != nil {
			return err
		}
	}
	return fn()
}

var errUnitBinariesDeclined = errors.New("the services that start Fluxer still need their copy of rexctl updated")

func refreshUnitBinaries(ctx context.Context, a *App, inst *instance.Instance) error {
	bin, err := host.RunningBinary()
	if err != nil {
		return err
	}
	steps, err := host.RefreshUnitBinarySteps(inst.Project(), bin)
	if err != nil || len(steps) == 0 {
		return err
	}
	ok, err := confirmHostSteps(a, steps, "Update the copy of rexctl that starts Fluxer?")
	if err != nil {
		return err
	}
	if !ok {
		return errUnitBinariesDeclined
	}
	return host.RunSteps(ctx, steps)
}

func (a *App) migrate(ctx context.Context, inst *instance.Instance) {
	if a.keepLayout || !inst.OldLayout() {
		return
	}
	err := inst.Migrate(func() error { return refreshUnitBinaries(ctx, a, inst) })
	switch {
	case errors.Is(err, errUnitBinariesDeclined):
		a.UI.Warn("rexctl keeps Fluxer's files in "+inst.Dir+" for now, because the services that start Fluxer still need their copy of rexctl updated.", "Run rexctl doctor --fix in a terminal, or run it with --yes, to update it.")
	case err != nil:
		a.UI.Detail("rexctl keeps Fluxer's files in %s for now: %v", inst.Dir, err)
	}
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
	switch rt.KindOf(inst) {
	case rt.Native:
		return nil, rt.NeedsContainers(a.command)
	case rt.Container:
		return nil, rt.NeedsCompose(a.command)
	}
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
		return nil, rt.EngineError("", err)
	}
	if interrupted != "" {
		a.UI.Warn(interrupted, "")
	}
	return e, nil
}

func managed(inst *instance.Instance) error {
	err := inst.Managed()
	var ej *instance.EjectedError
	var moved *instance.MovedError
	switch {
	case errors.As(err, &moved):
		return movedRefusal(inst, moved)
	case errors.As(err, &ej):
		return &ui.ExitError{Code: ui.ExitRefused, Msg: ej.Error(), Fix: ej.Fix(), Err: err}
	}
	return err
}

func movedStatus(moved *instance.MovedError) (string, bool) {
	return runThere(moved.To, "status"), withoutSudo(moved.To)
}

func withoutSudo(dir string) bool {
	to, err := instance.Find(dir)
	return euid() == 0 || (err == nil && !rootOwned(to))
}

func runThere(dir string, args ...string) string {
	if withoutSudo(dir) {
		return strings.Join(append(append([]string{"rexctl"}, args...), "--dir", shellWord(dir)), " ")
	}
	return sudoRun(dir, args...)
}

func movedRefusal(old *instance.Instance, moved *instance.MovedError) *ui.ExitError {
	status, reachable := movedStatus(moved)
	fix := "To manage it, run " + status + "."
	if reachable && !moved.CleanedUp && !oneWayMove(old, moved.Release) {
		fix += " To go back, run rexctl migrate --undo there."
	}
	return &ui.ExitError{Code: ui.ExitRefused, Msg: moved.Error(), Fix: fix, Err: moved}
}

func (a *App) drift(inst *instance.Instance, clean, withEnv bool) error {
	if clean {
		edits, err := instance.KeepEdits(inst)
		if err != nil {
			return ui.Wrap(err, "rexctl can't keep a copy of the files you changed.", "Check that you can write to "+inst.Dir+", then try again.")
		}
		for _, ed := range edits {
			a.showEdit(ed)
		}
		if len(edits) > 0 && a.UI.Interactive() {
			instance.SeenEdits(inst)
		}
	}
	r, err := instance.Drift(inst)
	if err != nil {
		return ui.Wrap(err, "rexctl can't check Fluxer's files.", "Check that you can read "+inst.Dir+", then try again.")
	}
	if !clean {
		for _, name := range r.Changed {
			a.UI.Warn(name+" was changed by hand. rexctl puts its own version back and keeps a copy of yours.", editFix(name))
		}
	}
	if r.Env != nil && !r.Env.Same && !withEnv {
		r.Env = nil
	}
	if r.Env != nil && r.Env.Same {
		if clean {
			if err := instance.RemoveEnv(inst); err != nil {
				return ui.Wrap(err, "Couldn't remove the .env file rexctl doesn't read.", "Check that you can write to "+inst.Dir+", or run the command again with sudo.")
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

func (a *App) showEdit(ed instance.Edit) {
	a.UI.Warn(fmt.Sprintf("%s was changed by hand. rexctl puts its own version back and keeps your copy in %s.", ed.Name, ed.Copy), editFix(ed.Name))
	for _, line := range ed.Diff {
		a.UI.Info("    %s", line)
	}
	a.putBack = append(a.putBack, fmt.Sprintf("rexctl put back its own %s. Your copy is in %s.", ed.Name, ed.Copy))
}

func editFix(name string) string {
	if name == stack.Caddyfile {
		return "Put extra Caddy directives in a .caddy file in " + instance.CaddyDropInDir + ", which rexctl keeps."
	}
	return "Put Compose changes in " + instance.CustomCompose + " with rexctl config edit, and change settings with rexctl config set."
}

func (a *App) replacePodmanCompose(ctx context.Context, inst *instance.Instance, e *engine.Engine) error {
	if e.Kind != engine.Podman {
		return nil
	}
	p := e.Named(inst.Project(), inst.Dir)
	containers, err := p.PS(ctx)
	if err != nil {
		return rt.EngineError("", err)
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
			return rt.EngineError("", err)
		}
		if err := p.Down(ctx, false); err != nil {
			return rt.EngineError("", err)
		}
		s.Done("Replaced podman-compose's containers.")
		return nil
	})
}

func (a *App) importAdmins(ctx context.Context, inst *instance.Instance, c *adminapi.Client) {
	if a.held != inst {
		return
	}
	if err := deploy.SendAdminImport(ctx, inst, c); err != nil {
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
		fix := fmt.Sprintf("Run %s --help to see every command.", c.CommandPath())
		if near := nearestCommand(c, args[0]); near != "" {
			fix = fmt.Sprintf("Did you mean %s %s? Run %s --help to see every command.", c.CommandPath(), near, c.CommandPath())
		}
		return &ui.ExitError{
			Code: ui.ExitUsage,
			Msg:  fmt.Sprintf("%s has no %s command.", c.CommandPath(), args[0]),
			Fix:  fix,
		}
	}
	return ui.Usage(fmt.Sprintf("%s takes no arguments.", c.CommandPath()))
}

func nearestCommand(c *cobra.Command, typed string) string {
	best, bestDist := "", 3
	for _, sub := range c.Commands() {
		if !sub.IsAvailableCommand() {
			continue
		}
		if d := editDistance(strings.ToLower(typed), sub.Name()); d < bestDist {
			best, bestDist = sub.Name(), d
		}
	}
	return best
}

func editDistance(a, b string) int {
	x, y := []rune(a), []rune(b)
	d := make([][]int, len(x)+1)
	for i := range d {
		d[i] = make([]int, len(y)+1)
		d[i][0] = i
	}
	for j := range d[0] {
		d[0][j] = j
	}
	for i := 1; i <= len(x); i++ {
		for j := 1; j <= len(y); j++ {
			cost := 1
			if x[i-1] == y[j-1] {
				cost = 0
			}
			d[i][j] = min(d[i-1][j]+1, d[i][j-1]+1, d[i-1][j-1]+cost)
			if i > 1 && j > 1 && x[i-1] == y[j-2] && x[i-2] == y[j-1] {
				d[i][j] = min(d[i][j], d[i-2][j-2]+1)
			}
		}
	}
	return d[len(x)][len(y)]
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

func handMadeFluxer(ctx context.Context, dir string) string {
	ctx, cancel := context.WithTimeout(ctx, 3*time.Second)
	defer cancel()
	for tool, name := range map[string]string{"docker": "Docker", "podman": "Podman"} {
		if _, err := exec.LookPath(tool); err != nil {
			continue
		}
		out, err := exec.CommandContext(ctx, tool, "ps", "-a", "--format", `{{.Image}}	{{.Label "`+rt.LabelManagedBy+`"}}	{{.Label "`+composeWorkingDir+`"}}`).Output()
		if err != nil {
			continue
		}
		if unmanagedFluxer(string(out), insideRexctl) {
			return name
		}
	}
	if _, err := os.Stat(filepath.Join(dir, "fluxer.env")); err == nil {
		return "Docker or Podman"
	}
	return ""
}

const composeWorkingDir = "com.docker.compose.project.working_dir"

func unmanagedFluxer(ps string, fromRexctl func(string) bool) bool {
	for _, line := range strings.Split(ps, "\n") {
		fields := append(strings.Split(strings.TrimSpace(line), "\t"), "", "")
		image, label, wd := fields[0], fields[1], fields[2]
		if strings.HasPrefix(image, rt.ImageRepo) && label == "" && (wd == "" || !fromRexctl(wd)) {
			return true
		}
	}
	return false
}

func insideRexctl(dir string) bool {
	for d := filepath.Clean(dir); filepath.IsAbs(d); d = filepath.Dir(d) {
		_, err := os.Stat(filepath.Join(d, instance.StateDirName, instance.StateFileName))
		if !errors.Is(err, fs.ErrNotExist) {
			return true
		}
		if d == filepath.Dir(d) {
			return false
		}
	}
	return false
}
