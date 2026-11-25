// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"cmp"
	"context"
	"errors"
	"io"
	"maps"
	"os"
	"path/filepath"
	goruntime "runtime"
	"slices"
	"strings"

	"github.com/compose-spec/compose-go/v2/types"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type compose struct {
	inst     *instance.Instance
	u        *ui.UI
	connect  Connect
	e        *engine.Engine
	dataLeft bool
	planned  *backup.Sizes
}

func newCompose(inst *instance.Instance, u *ui.UI, connect Connect) *compose {
	c := &compose{inst: inst, u: u, connect: connect}
	if c.connect == nil {
		c.connect = c.dial
	}
	return c
}

func (c *compose) dial(ctx context.Context) (*engine.Engine, error) {
	pin := c.inst.State.Engine
	e, err := engine.Connect(ctx, engine.ConnectOptions{Host: pin.Host, ID: pin.ID, Prefer: engine.Kind(pin.Kind), AllowOld: true})
	if err != nil {
		return nil, EngineError(engine.Kind(pin.Kind), err)
	}
	return e, nil
}

func (c *compose) engine(ctx context.Context) (*engine.Engine, error) {
	if c.e != nil {
		return c.e, nil
	}
	e, err := c.connect(ctx)
	if err != nil {
		return nil, err
	}
	c.e = e
	return e, nil
}

func (c *compose) project(ctx context.Context) (*engine.Project, error) {
	e, err := c.engine(ctx)
	if err != nil {
		return nil, err
	}
	if p, err := deploy.Load(ctx, c.inst, e); err == nil {
		return p, nil
	}
	return e.Named(c.inst.Project(), c.inst.Dir), nil
}

func (c *compose) engineError(err error, msg string) error {
	if err == nil || errors.Is(err, context.Canceled) {
		return err
	}
	kind := engine.Kind(c.inst.State.Engine.Kind)
	if c.e != nil {
		kind = c.e.Kind
	}
	var ee *ui.ExitError
	if mapped := EngineError(kind, err); errors.As(mapped, &ee) {
		return mapped
	}
	return ui.Wrap(err, msg, "Run rexctl doctor to see what's wrong.")
}

func (c *compose) readyFailure(err error) error {
	if err == nil || errors.Is(err, context.Canceled) {
		return err
	}
	var nr *engine.NotReadyError
	if errors.As(err, &nr) {
		return err
	}
	return c.engineError(err, "Fluxer didn't start.")
}

func (c *compose) Kind() Kind { return Compose }

func EngineLabel(e *engine.Engine) string {
	if e == nil {
		return "containers"
	}
	if e.Rootless {
		return e.DisplayName() + ", rootless"
	}
	return e.DisplayName()
}

const NativeLabel = "Linux, systemd"

func composeLabel(e *engine.Engine, inst *instance.Instance) string {
	kind := engine.Kind(inst.State.Engine.Kind)
	if e != nil {
		kind = e.Kind
	}
	if kind == "" {
		kind = engine.Docker
	}
	return kind.Name() + " Compose, older layout"
}

func (c *compose) Info(ctx context.Context) Info {
	info := Info{Kind: Compose, Label: EngineLabel(c.e), DataDir: c.inst.Dir}
	if !c.inst.State.IsVoice() {
		info.Label = composeLabel(c.e, c.inst)
	}
	if host.BootUnitSupported() {
		info.Unit = c.inst.Project() + ".service"
	}
	return info
}

func (c *compose) Status(ctx context.Context) (Status, error) {
	st := Status{Release: c.inst.State.Release}
	if c.inst.Busy() {
		st.Busy = "Another rexctl command is changing this instance."
	}
	p, err := c.project(ctx)
	if err != nil {
		return st, err
	}
	containers, err := p.PS(ctx)
	if err != nil {
		return st, c.engineError(err, "rexctl can't list Fluxer's containers.")
	}
	st.Parts = foldParts(containers)
	st.RunningRelease = c.runningRelease(containers)
	for _, ct := range containers {
		st.Restarts += ct.RestartCount
		if ct.State == "running" {
			st.Running = true
			if st.StartedAt.IsZero() || ct.Created.Before(st.StartedAt) {
				st.StartedAt = ct.Created
			}
		}
	}
	return st, nil
}

func (c *compose) runningRelease(containers []engine.Container) string {
	st := c.inst.State
	matched := false
	for _, ct := range containers {
		if _, ok := st.Images.Refs[ct.Service]; !ok {
			continue
		}
		if ct.Image != settings.ImageRef(st.Settings, st.Images.Refs, ct.Service) {
			return ""
		}
		matched = true
	}
	if !matched {
		return ""
	}
	return cmp.Or(st.Images.Release, st.Release)
}

func (c *compose) Start(ctx context.Context, s *ui.Step, o StartOptions) error {
	if err := c.inst.Managed(); err != nil {
		return err
	}
	e, err := c.engine(ctx)
	if err != nil {
		return err
	}
	p := e.Named(c.inst.Project(), c.inst.Dir)
	if e.TooOld {
		if err := p.Start(ctx); err != nil {
			return c.engineError(err, "Couldn't start Fluxer.")
		}
	} else {
		if err := deploy.Resolve(ctx, c.inst, e); err != nil {
			return c.engineError(err, "Couldn't start Fluxer.")
		}
		on, stop := watch(s, "Starting", c.u)
		_, err := deploy.Apply(ctx, c.inst, e, deploy.ApplyOptions{Pull: !o.Boot, Wait: o.Boot, OnEvent: on})
		stop()
		if err != nil {
			return c.readyFailure(err)
		}
	}
	if o.NoWait || o.Boot {
		return nil
	}
	return c.wait(ctx, s, p, "Starting")
}

func (c *compose) wait(ctx context.Context, s *ui.Step, p *engine.Project, verb string) error {
	return c.readyFailure(p.WaitReady(ctx, engine.DefaultReadyTimeout, ShowReadiness(s, verb+" Fluxer")))
}

func (c *compose) Stop(ctx context.Context, s *ui.Step) error {
	p, err := c.project(ctx)
	if err != nil {
		return err
	}
	return c.engineError(p.Stop(context.WithoutCancel(ctx)), "Couldn't stop Fluxer.")
}

func (c *compose) Restart(ctx context.Context, s *ui.Step, parts []string) error {
	services, err := ServicesOf(parts)
	if err != nil {
		return err
	}
	e, err := c.engine(ctx)
	if err != nil {
		return err
	}
	if len(services) == 0 {
		if err := deploy.Resolve(ctx, c.inst, e); err != nil {
			return c.engineError(err, "Couldn't restart Fluxer.")
		}
		on, stop := watch(s, "Restarting", c.u)
		_, err := deploy.Apply(ctx, c.inst, e, deploy.ApplyOptions{Pull: true, OnEvent: on})
		stop()
		if err != nil && engine.ClassOf(err) != engine.ClassDependencyFailed {
			return c.readyFailure(err)
		}
	}
	p, err := c.project(ctx)
	if err != nil {
		return err
	}
	if len(services) > 0 {
		if services = inProject(p, services); len(services) == 0 {
			return unsupported("This instance runs without "+ui.JoinAnd(parts)+".", "Run rexctl status to see what runs.")
		}
	}
	if err := p.RestartWatched(context.WithoutCancel(ctx), Progress(s, "Restarting", c.u), services...); err != nil {
		return c.engineError(err, "Couldn't restart Fluxer.")
	}
	return c.wait(ctx, s, p, "Restarting")
}

func inProject(p *engine.Project, services []string) []string {
	known := p.Services()
	return slices.DeleteFunc(slices.Clone(services), func(s string) bool { return !slices.Contains(known, s) })
}

func (c *compose) Down(ctx context.Context, s *ui.Step) error {
	p, err := c.project(ctx)
	if err != nil {
		return err
	}
	if p.Engine.TooOld {
		return unsupported("rexctl can't create Fluxer's containers again with this version of Podman.", "Run rexctl stop to stop Fluxer and keep its containers.")
	}
	return c.engineError(p.Down(context.WithoutCancel(ctx), false), "Couldn't remove Fluxer's containers.")
}

func (c *compose) Logs(ctx context.Context, w io.Writer, o LogOptions) error {
	lp, err := planLogs(c.inst, o.Parts)
	if err != nil {
		return err
	}
	p, err := c.project(ctx)
	if err != nil {
		return err
	}
	opts := engine.LogsOptions{Follow: o.Follow, Since: o.Since, Tail: o.Tail, Stdout: &partWriter{w: w, lp: lp}}
	if err := p.Logs(ctx, opts, lp.services...); err != nil && ctx.Err() == nil {
		return c.engineError(err, "Couldn't read the logs.")
	}
	return nil
}

func (c *compose) plan(ctx context.Context, o UninstallOptions) Removal {
	var r Removal
	var binds []string
	if _, err := c.engine(ctx); err != nil {
		r.Stays = append(r.Stays, ui.KV{Key: "Containers", Value: "Fluxer's containers, data volumes and images, because rexctl can't connect to Docker or Podman"})
		if !o.KeepData {
			r.Goes = append(r.Goes, folderRow(c.inst.Dir))
		}
		r.Kept = "Your data is still in Docker or Podman volumes."
		if o.KeepData {
			r.Kept = "Your data is still in Docker or Podman volumes and your settings are still in " + c.inst.Dir + "."
		}
	} else {
		binds = c.dataBinds(ctx)
		if o.KeepData {
			r.Goes = append(r.Goes, ui.KV{Key: "Containers", Value: "Fluxer's containers and networks"})
			r.Stays = append(r.Stays, ui.KV{Key: "Data", Value: "Every account, message and file, in " + c.e.DisplayName() + " volumes"}, ui.KV{Key: "Settings", Value: c.inst.Dir})
			r.Kept = "Your data is still in " + c.e.DisplayName() + " volumes and your settings are still in " + c.inst.Dir + "."
		} else {
			r.Goes = append(r.Goes,
				ui.KV{Key: "Containers", Value: "Fluxer's containers and networks"},
				ui.KV{Key: "Data", Value: "Every account, message and file, in " + c.e.DisplayName() + " volumes"},
				ui.KV{Key: "Images", Value: "Fluxer's images"},
				folderRow(c.inst.Dir))
		}
	}
	if len(binds) > 0 {
		r.Stays = append(r.Stays, ui.KV{Key: "Data elsewhere", Value: ui.JoinAnd(binds)})
		r.Kept = strings.TrimSpace(r.Kept + " Some of your data is still in " + ui.JoinAnd(binds) + ", outside the instance folder.")
	}
	r.Goes, r.Stays = backupRows(c.inst, o, r.Goes, r.Stays)
	if host.BootUnitSupported() {
		if path, err := c.unitFor(c.e).Path(); err == nil && exists(path) {
			r.Goes = append(r.Goes, ui.KV{Key: "Service", Value: path})
		}
	}
	r.Goes = append(r.Goes, firewallRows(c.inst)...)
	r.Stays = append(r.Stays, proxyRows()...)
	return r
}

func (c *compose) dataBinds(ctx context.Context) []string {
	p, err := c.project(ctx)
	if err != nil || p.Model == nil {
		return nil
	}
	var out []string
	for _, dm := range stack.DataMounts {
		svc, ok := p.Model.Services[dm.Service]
		if !ok {
			continue
		}
		for _, v := range svc.Volumes {
			if v.Type != types.VolumeTypeBind || v.Target != dm.Target || v.Source == "" {
				continue
			}
			src := v.Source
			if !filepath.IsAbs(src) {
				src = filepath.Join(c.inst.Dir, src)
			}
			src = filepath.Clean(src)
			if !inside(src, c.inst.Dir) && !slices.Contains(out, src) {
				out = append(out, src)
			}
		}
	}
	slices.Sort(out)
	return out
}

func (c *compose) Uninstall(ctx context.Context, s *ui.Step, o UninstallOptions) error {
	var left []string
	ctx = context.WithoutCancel(ctx)
	e, err := c.engine(ctx)
	if err != nil {
		c.dataLeft = true
		left = append(left, "Fluxer's containers, data volumes and images are still on this machine, because rexctl couldn't connect to Docker or Podman.")
	} else {
		c.dataLeft = len(c.dataBinds(ctx)) > 0
		p, err := c.project(ctx)
		if err != nil {
			return err
		}
		if err := p.Down(ctx, !o.KeepData); err != nil {
			return c.engineError(err, "Couldn't remove Fluxer.")
		}
		if !o.KeepData {
			if err := RemoveProjectVolumes(ctx, e, p.Name); err != nil {
				c.u.Detail("%v", err)
				c.dataLeft = true
				left = append(left, "Some of Fluxer's data volumes are still on this machine.")
			}
			if line := NetworksLeft(ctx, e, p.Name); line != "" {
				left = append(left, line)
			}
			images := slices.Sorted(maps.Values(c.inst.State.Images.Refs))
			for _, ref := range release.KeptImages(c.inst) {
				if !slices.Contains(images, ref) {
					images = append(images, ref)
				}
			}
			if err := e.RemoveImages(ctx, images); err != nil {
				c.u.Detail("%v", err)
				left = append(left, "Some of Fluxer's images are still on this machine, because other containers use them.")
			}
		}
	}
	if err := removeComposeUnit(ctx, c.unitFor(e)); err != nil {
		c.u.Detail("%v", err)
		left = append(left, "The service that starts Fluxer with this machine is still there.")
	}
	stillThere(s, append(left, closeFirewall(ctx, c.inst)...))
	return nil
}

func NetworksLeft(ctx context.Context, e *engine.Engine, project string) string {
	nets, err := e.Networks(ctx, project)
	if err != nil || len(nets) == 0 {
		return ""
	}
	return networksLeft(nets)
}

func networksLeft(nets []string) string {
	if len(nets) == 1 {
		return "The network " + nets[0] + " is still there, because containers outside Fluxer use it."
	}
	return "The networks " + ui.JoinAnd(nets) + " are still there, because containers outside Fluxer use them."
}

func RemoveProjectVolumes(ctx context.Context, e *engine.Engine, project string) error {
	left, err := e.Volumes(ctx, project)
	if err != nil {
		return err
	}
	var errs []error
	for _, name := range ownVolumes(left, project) {
		errs = append(errs, e.RemoveVolume(ctx, name))
	}
	return errors.Join(errs...)
}

func ownVolumes(vols []engine.Volume, project string) []string {
	var out []string
	for _, v := range vols {
		if strings.HasPrefix(v.Name, project+"_") {
			out = append(out, v.Name)
		}
	}
	return out
}

func (c *compose) unitFor(e *engine.Engine) host.BootUnit {
	u := host.BootUnit{Project: c.inst.Project(), Dir: c.inst.Dir, Rootless: goruntime.GOOS == "linux" && os.Geteuid() != 0}
	if e != nil {
		u.Podman, u.Rootless = e.Kind == engine.Podman, e.Rootless
	} else if c.inst.State.Engine.Kind == string(engine.Podman) {
		u.Podman = true
	}
	return u
}

func RemoveComposeUnit(ctx context.Context, inst *instance.Instance, e *engine.Engine) error {
	if inst.State.RunsFluxerd() {
		return unsupported("This instance has no compose service to remove.", "Run rexctl boot off to stop Fluxer starting with this machine.")
	}
	return removeComposeUnit(ctx, (&compose{inst: inst}).unitFor(e))
}

func removeComposeUnit(ctx context.Context, u host.BootUnit) error {
	if !host.BootUnitSupported() {
		return nil
	}
	steps, err := u.RemoveSteps(ctx)
	if err != nil {
		return err
	}
	return host.RunSteps(ctx, append(steps, host.LibexecRemoveSteps(ctx, u)...))
}

func (c *compose) Boot() Boot { return composeBoot{c} }

func (c *compose) Admin(ctx context.Context) (adminapi.Transport, error) {
	p, err := c.project(ctx)
	if err != nil {
		return nil, err
	}
	return adminapi.NewExec(p), nil
}

func (c *compose) Settings() Settings { return composeSettings{c} }
func (c *compose) Backups() Backups   { return composeBackups{c} }
func (c *compose) Upgrades() Upgrades { return composeUpgrades{c} }

type composeBoot struct{ c *compose }

func noSystemd() error {
	return unsupported("This machine doesn't run systemd, so rexctl can't add a service that starts Fluxer with it.", "Run rexctl start after this machine restarts.")
}

func (b composeBoot) unit(ctx context.Context) (host.BootUnit, error) {
	if !host.BootUnitSupported() {
		return host.BootUnit{}, noSystemd()
	}
	e, err := b.c.engine(ctx)
	if err != nil {
		return host.BootUnit{}, err
	}
	var p *engine.Project
	if loaded, err := deploy.Load(ctx, b.c.inst, e); err == nil {
		p = loaded
	}
	return checks.BootUnitFor(b.c.inst, e, p)
}

func (b composeBoot) State(ctx context.Context) (BootState, error) {
	if !host.BootUnitSupported() {
		return BootState{}, noSystemd()
	}
	u, err := b.unit(ctx)
	if err != nil {
		return BootState{}, err
	}
	s, err := host.CheckBootUnit(ctx, u)
	if err != nil {
		return BootState{}, err
	}
	detail := "Docker starts Fluxer again after this machine restarts."
	if b.c.e != nil && b.c.e.Kind == engine.Podman {
		detail = "Podman doesn't start Fluxer by itself after this machine restarts."
	}
	return BootState{Enabled: !s.Missing && !s.Disabled, Unit: s.Path, Detail: detail}, nil
}

func (b composeBoot) steps(ctx context.Context, enable bool) ([]host.Step, error) {
	u, err := b.unit(ctx)
	if err != nil {
		return nil, err
	}
	if enable {
		return u.InstallSteps(ctx)
	}
	steps, err := u.RemoveSteps(ctx)
	if err != nil || len(steps) == 0 {
		return nil, err
	}
	return append(steps, host.LibexecRemoveSteps(ctx, u)...), nil
}

func (b composeBoot) Enable(ctx context.Context, s *ui.Step) error {
	steps, err := b.steps(ctx, true)
	if err != nil {
		return err
	}
	return host.RunSteps(ctx, steps)
}

func (b composeBoot) Disable(ctx context.Context, s *ui.Step) error {
	steps, err := b.steps(ctx, false)
	if err != nil {
		return err
	}
	return host.RunSteps(ctx, steps)
}
