// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func nativeSupported() bool {
	return runtime.GOOS == "linux" && host.BootUnitSupported()
}

func (a *App) nativeRoot(dir string) error {
	if !nativeSupported() {
		return ui.Refused("Fluxer runs as a systemd service only on Linux with systemd.", "Run rexctl install --runtime docker to use one container.")
	}
	if euid() == 0 {
		return nil
	}
	if !a.UI.Interactive() {
		return ui.Refused("A systemd install is managed as root.", "Run "+sudoAgain(dir)+".")
	}
	if _, err := a.asRootFor(dir, "Fluxer runs here as a systemd service, which only root can set up. To install it in a container under this account, pass --runtime docker or --runtime podman."); err != nil {
		return err
	}
	return errDeclined
}

func nativeFolder(dir, flag string) error {
	if p := instance.RootPath(dir, false); p != "" {
		fix := "Pass " + flag + " with a folder that root owns and only root can change, such as /opt/fluxer."
		if p == "/opt/fluxer" || strings.HasPrefix("/opt/fluxer", p+"/") || p == "/" {
			fix = "Pass " + flag + " with a folder that root owns and only root can change."
		}
		return ui.Refused(fmt.Sprintf("A systemd install can't go in %s, because accounts other than root can change %s.", dir, p), fix)
	}
	if p := instance.RootPath(dir, true); p != "" {
		return ui.Refused(fmt.Sprintf("A systemd install can't go in %s, because Fluxer's service account can't open %s.", dir, p), "Pass "+flag+" with another folder, or let every account open "+p+" with sudo chmod o+x "+shellWord(p)+".")
	}
	return nil
}

func nativeRelease(ctx context.Context, version, fix string) (*release.Release, error) {
	if !release.Valid(version) {
		version = ""
	}
	rel, err := resolveRelease(ctx, version)
	if err != nil {
		return nil, err
	}
	if _, err := rel.DistFor(runtime.GOARCH); err != nil {
		return nil, &ui.ExitError{Code: ui.ExitRefused, Msg: err.Error(), Fix: fix, Err: err}
	}
	return rel, nil
}

func saveInstance(inst *instance.Instance) error {
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	return nil
}

type composeRuntime struct {
	rt.Runtime
	app  *App
	inst *instance.Instance
}

func bare(r rt.Runtime) rt.Runtime {
	if c, ok := r.(composeRuntime); ok {
		return c.Runtime
	}
	return r
}

func (c composeRuntime) ready(ctx context.Context, err error) error {
	var nr *engine.NotReadyError
	if !errors.As(err, &nr) || errors.Is(err, context.Canceled) {
		return err
	}
	var p *engine.Project
	if e, eerr := c.app.Engine(ctx, c.inst); eerr == nil {
		p = e.Named(c.inst.Project(), c.inst.Dir)
	}
	return readyFailure(ctx, c.inst, p, err)
}

func (c composeRuntime) replaceOld(ctx context.Context) error {
	e, err := c.app.Engine(ctx, c.inst)
	if err != nil || e.TooOld {
		return nil
	}
	return c.app.replacePodmanCompose(ctx, c.inst, e)
}

func (c composeRuntime) Start(ctx context.Context, s *ui.Step, o rt.StartOptions) error {
	if !o.Boot {
		if err := c.replaceOld(ctx); err != nil {
			return err
		}
	}
	return c.ready(ctx, c.Runtime.Start(ctx, s, o))
}

func (c composeRuntime) Restart(ctx context.Context, s *ui.Step, parts []string) error {
	if len(parts) == 0 {
		if err := c.replaceOld(ctx); err != nil {
			return err
		}
	}
	return c.ready(ctx, c.Runtime.Restart(ctx, s, parts))
}

func (c composeRuntime) Down(ctx context.Context, s *ui.Step) error {
	if e, err := c.app.Engine(ctx, c.inst); err == nil && !e.TooOld {
		dctx := context.WithoutCancel(ctx)
		eachVoiceServer(c.app, c.inst, "removing", func(srv *instance.VoiceServer) error {
			return deploy.VoiceProject(e, c.inst, srv).DownWithin(dctx, false, deploy.VoiceStopTimeout)
		})
	}
	return c.Runtime.Down(ctx, s)
}

func (c composeRuntime) Uninstall(ctx context.Context, s *ui.Step, o rt.UninstallOptions) error {
	if e, err := c.app.Engine(ctx, c.inst); err == nil {
		removeLocalVoiceServersForUninstall(context.WithoutCancel(ctx), c.app, c.inst, e)
	}
	return c.Runtime.Uninstall(ctx, s, o)
}

func (c composeRuntime) Voice() rt.Voice {
	return composeVoice{VoiceServers: rt.VoiceOf(c.Runtime), c: c}
}

type composeVoice struct {
	rt.VoiceServers
	c composeRuntime
}

func (v composeVoice) StartAll(ctx context.Context, s *ui.Step) error {
	e, err := v.c.app.Engine(ctx, v.c.inst)
	if err != nil {
		return err
	}
	startLocalVoiceServers(ctx, v.c.app, v.c.inst, e)
	moveVoiceEndpoints(ctx, v.c.app, v.c.inst)
	return nil
}

func (v composeVoice) StopAll(ctx context.Context, s *ui.Step) error {
	e, err := v.c.app.Engine(ctx, v.c.inst)
	if err != nil {
		return err
	}
	stopLocalVoiceServers(ctx, v.c.app, v.c.inst, e)
	return nil
}
