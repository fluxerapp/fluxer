// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const adoptedDir = "adopted"

func newAdoptCmd(app *App) *cobra.Command {
	var dryRun bool
	c := &cobra.Command{
		Use:     "adopt",
		Short:   "Let rexctl run a Fluxer you run with docker compose",
		Long:    "Takes a Fluxer that runs from compose.yaml and fluxer.env, such as the files each release publishes or a folder rexctl detach wrote, into rexctl on the same volume. rexctl backs it up, stops it, starts the same release in its own container and checks it. If that doesn't work, rexctl starts it again with docker compose. rexctl detach hands it back.",
		Example: "rexctl adopt --dry-run\nrexctl adopt --dir /opt/fluxer",
		Args:    noArgs,
		RunE: func(c *cobra.Command, args []string) error {
			return runAdopt(c.Context(), app, dryRun)
		},
	}
	c.Flags().BoolVar(&dryRun, "dry-run", false, "show what rexctl would do without changing anything")
	return c
}

func newTakeoverCmd() *cobra.Command {
	return &cobra.Command{
		Use:    "takeover",
		Hidden: true,
		Args:   cobra.ArbitraryArgs,
		RunE: func(c *cobra.Command, args []string) error {
			return ui.Refused("rexctl takeover is gone.", "For the older layout with one container per service, run rexctl migrate --to docker in its folder. For a Fluxer you run from compose.yaml, run rexctl adopt there.")
		},
	}
}

type adoption struct {
	dir      string
	compose  *rt.ComposeInstall
	existing *instance.Instance
	inst     *instance.Instance
	e        *engine.Engine
	composed rt.Composed
	tool     string
	unknown  []string
	config   bool
}

func runAdopt(ctx context.Context, app *App, dryRun bool) error {
	dir, err := app.Dir()
	if err != nil {
		return err
	}
	if dir, err = filepath.Abs(dir); err != nil {
		return err
	}
	a, err := readAdoption(ctx, app, dir)
	if err != nil {
		return err
	}
	if len(a.unknown) > 0 {
		app.UI.Info("%s %s new to rexctl, and rexctl keeps %s as %s.", ui.JoinAnd(a.unknown), oneOrMany(len(a.unknown), "is", "are"), oneOrMany(len(a.unknown), "it", "them"), oneOrMany(len(a.unknown), "it is", "they are"))
	}
	backups := a.inst.Path(instance.BackupsDir)
	items := []ui.PlanItem{
		{Verb: ui.PlanCreate, Text: "Back up Fluxer", Detail: "inside its volume, while it runs"},
		{Verb: ui.PlanChange, Text: "Stop Fluxer with " + a.tool},
		{Verb: ui.PlanCreate, Text: "Back up the stopped volume", Detail: backups},
		{Verb: ui.PlanCreate, Text: fmt.Sprintf("Start Fluxer %s in rexctl's own container on the volume %s", a.inst.State.Release, a.compose.Volume)},
		{Verb: ui.PlanRun, Text: "Check that it works", Detail: "if it doesn't, rexctl removes its container and starts Fluxer again with " + a.tool},
		{Verb: ui.PlanChange, Text: "Move compose.yaml and fluxer.env to " + filepath.Join(dir, instance.StateDirName, adoptedDir)},
	}
	if dryRun {
		app.UI.Plan("rexctl would do this", items...)
		app.UI.Info("Dry run. Nothing changed.")
		return nil
	}
	app.UI.Plan("rexctl will do this", items...)
	ok, err := app.UI.ConfirmWith(ui.Question{
		Text:   fmt.Sprintf("Let rexctl run Fluxer in %s?", dir),
		Detail: fmt.Sprintf("rexctl backs up Fluxer, stops it, starts the same release in its own container on the volume %s and checks it. compose.yaml and fluxer.env move to %s/.rexctl/adopted. rexctl detach hands it back.", a.compose.Volume, dir),
		Flag:   "yes",
	})
	if err != nil {
		return err
	}
	if !ok {
		app.UI.Info("Nothing changed.")
		return nil
	}
	return a.run(ctx, app)
}

func readAdoption(ctx context.Context, app *App, dir string) (*adoption, error) {
	a := &adoption{dir: dir}
	switch instance.Classify(dir) {
	case instance.KindHandRunNew:
	case instance.KindManaged:
		inst, err := instance.Find(dir)
		if err != nil {
			return nil, err
		}
		if !inst.State.Detached() || inst.State.MovedFrom != nil {
			return nil, ui.Refused(fmt.Sprintf("rexctl already runs Fluxer in %s.", dir), "Run rexctl status to check it.")
		}
		a.existing = inst
	default:
		return nil, ui.Refused(fmt.Sprintf("There's no Fluxer from compose.yaml in %s.", dir), "Run rexctl adopt in the folder with compose.yaml and fluxer.env.")
	}
	c, err := rt.ReadComposeInstall(dir)
	if err != nil {
		return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't read the compose files in %s.", dir), "Check that compose.yaml and fluxer.env are there and readable, then run this again.")
	}
	a.compose = c
	if err := a.check(app); err != nil {
		return nil, err
	}
	if err := a.connect(ctx); err != nil {
		return nil, err
	}
	if err := a.build(); err != nil {
		return nil, err
	}
	return a, nil
}

func (a *adoption) check(app *App) error {
	c := a.compose
	if err := problemsRefusal(c.Problems, nil); err != nil {
		return err
	}
	if c.Multiline != "" {
		return ui.Refused(fmt.Sprintf("The setting %s in fluxer.env holds more than one line, and rexctl keeps one line per setting.", c.Multiline), "Put it on one line, then run this again.")
	}
	tag, ok := c.ReleaseTag()
	if !ok {
		return ui.Refused(fmt.Sprintf("rexctl can't tell which files belong to %s, because it isn't a Fluxer release.", cmpDir(c.Image, "the image in compose.yaml")), "Set the image in compose.yaml to a release, such as "+rt.ImageRepo+":2026.1124.80040, then run this again.")
	}
	if release.Valid(app.Version) && release.Compare(tag, app.Version) > 0 {
		return ui.Refused(fmt.Sprintf("Fluxer in %s runs %s, which is newer than this rexctl.", a.dir, tag), "Update rexctl, then run this again.")
	}
	if c.Volume == "" {
		return ui.Refused(fmt.Sprintf("compose.yaml in %s keeps no volume at %s.", a.dir, rt.ContainerData), "Check the file against the one each release publishes, then run this again.")
	}
	tool := "docker compose"
	name, ok := c.InstanceName()
	if !ok || !instance.ValidProject(name) {
		return ui.Refused(fmt.Sprintf("rexctl takes the instance name from the volume, and %s doesn't end in -data.", c.Volume), fmt.Sprintf("Back up with %s exec %s fluxerd backup, then restore into a new rexctl install.", tool, c.Service))
	}
	for _, o := range instance.Others(a.dir) {
		if o.Project() == name && o.Moved() == nil {
			return ui.Refused(fmt.Sprintf("The Fluxer in %s already uses the name %s.", o.Dir, name), "Remove it with rexctl uninstall --dir "+shellWord(o.Dir)+" first.")
		}
	}
	return nil
}

func problemsRefusal(problems map[string][]string, fixes []string) error {
	if len(problems) == 0 {
		return nil
	}
	var msg []string
	for _, file := range slices.Sorted(maps.Keys(problems)) {
		msg = append(msg, fmt.Sprintf("rexctl can't take over these lines of %s:", file))
		msg = append(msg, problems[file]...)
	}
	fix := "Change or remove those lines, then run this again."
	if len(fixes) > 0 {
		fix = strings.Join(fixes, " ")
	}
	return ui.Refused(strings.Join(msg, " "), fix)
}

func (a *adoption) connect(ctx context.Context) error {
	var lastErr error
	for _, kind := range []engine.Kind{engine.Docker, engine.Podman} {
		if !engineOnMachine(kind) {
			continue
		}
		e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: kind, AllowOld: true})
		if err != nil || e.Kind != kind {
			continue
		}
		composed, err := rt.FindComposed(ctx, e, a.compose)
		if err == nil {
			a.e, a.composed, a.tool = e, composed, instance.ComposeTool(string(kind))
			return nil
		}
		lastErr = err
	}
	tool := "docker compose"
	if lastErr != nil && !errors.Is(lastErr, rt.ErrAdoptUnhealthy) {
		return rt.ContainerError(lastErr, fmt.Sprintf("rexctl couldn't read how Fluxer in %s runs.", a.dir))
	}
	return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("Fluxer in %s isn't healthy, and taking it over would hide why.", a.dir), Fix: fmt.Sprintf("Check %s logs, fix it, then run this again.", tool), Err: lastErr}
}

func (a *adoption) build() error {
	c := a.compose
	name, _ := c.InstanceName()
	tag, _ := c.ReleaseTag()
	values := map[string]string{}
	known := instance.EnvNames()
	for k, v := range c.Env {
		if key, ok := settings.NamedByVar(k); ok && len(key.Vars) > 0 && key.Vars[0] == k {
			values[key.Name] = v
		} else {
			values[k] = v
		}
		if strings.HasPrefix(k, "FLUXER_") && k != rt.EdgeModeVar && !slices.Contains(known, k) {
			a.unknown = append(a.unknown, k)
		}
	}
	slices.Sort(a.unknown)
	if mode, ok := values[rt.EdgeModeVar]; ok {
		values[settings.EdgeMode] = mode
		delete(values, rt.EdgeModeVar)
	}
	if mode := values[settings.EdgeMode]; mode != "" && mode != settings.EdgeBundled {
		delete(values, settings.EdgeListen)
		delete(values, settings.EdgeBind)
		if bind, ok := rt.ProxyBind(c.Ports); ok {
			values[settings.EdgeBind] = bind
		}
	}
	for _, k := range settings.Named() {
		if v, ok := values[k.Name]; ok && k.Validate != nil {
			if err := k.Validate(v); err != nil {
				return ui.Refused(fmt.Sprintf("The setting %s in fluxer.env isn't valid. %s", k.Vars[0], sentence(err.Error())), "Change it in fluxer.env, then run this again.")
			}
		}
	}
	now := time.Now().UTC().Truncate(time.Second)
	st := &instance.State{
		Project:     name,
		Runtime:     instance.RuntimeContainer,
		Engine:      instance.EngineRef{Kind: string(a.e.Kind), Host: a.e.Host, ID: a.e.ID},
		Release:     tag,
		Settings:    values,
		Secrets:     map[string]string{},
		ExtraHosts:  slices.Clone(c.ExtraHosts),
		InstalledAt: now,
		UpdatedAt:   now,
	}
	if c.Restart == "no" {
		st.BootService = instance.BootServiceOff
	}
	if a.existing != nil {
		kept := *a.existing.State
		kept.Settings, kept.Release, kept.Engine, kept.ExtraHosts, kept.Ejected = values, tag, st.Engine, st.ExtraHosts, nil
		kept.BootService = st.BootService
		st = &kept
	}
	a.inst = &instance.Instance{Dir: a.dir, State: st}
	var fixes []string
	problems := map[string][]string{}
	for _, p := range c.ForeignPorts(rt.ContainerPorts(a.inst, c.Env)) {
		problems[p.File] = append(problems[p.File], fmt.Sprintf("line %d: the port %s can't be made from a rexctl setting.", p.Line, p.Value))
		if to, set := rt.PortFix(p.Value); to != "" {
			fixes = append(fixes, fmt.Sprintf("Change it to %s and set %s in fluxer.env.", to, set))
		}
	}
	if len(fixes) > 0 {
		fixes = append(fixes, "Then run this again.")
	}
	return problemsRefusal(problems, fixes)
}

func (a *adoption) run(ctx context.Context, app *App) error {
	u := app.UI
	err := u.Step(ctx, "Backing up Fluxer inside its volume", func(ctx context.Context, s *ui.Step) error {
		if err := rt.LiveBackup(ctx, a.e, a.composed.Name); err != nil {
			return rt.ContainerError(err, "Couldn't back up Fluxer. rexctl didn't change anything.")
		}
		s.Done("Backed up Fluxer inside its volume.")
		return nil
	})
	if err != nil {
		return err
	}
	restored, err := rt.ReadComposedSecrets(ctx, a.e, a.composed.Name)
	if err != nil {
		return rt.ContainerError(err, "Couldn't read Fluxer's secrets from its volume. rexctl didn't change anything.")
	}
	for _, k := range append([]string{setup.TokenKey}, secretKeys()...) {
		if v := restored[k]; v != "" {
			a.inst.State.Secrets[k] = v
		}
	}
	modes, err := rt.ReadComposedModes(ctx, a.e, a.composed.Name)
	if err != nil {
		return rt.ContainerError(err, "Couldn't read how Fluxer keeps its data from its volume. rexctl didn't change anything.")
	}
	for _, m := range migrate.Modes {
		if _, set := a.inst.State.Settings[m.Key]; !set && strings.TrimSpace(modes[m.Env]) != "" {
			a.inst.State.Settings[m.Key] = strings.TrimSpace(modes[m.Env])
		}
	}
	bg := context.WithoutCancel(ctx)
	if err := a.composed.Project.Stop(bg); err != nil {
		return rt.ContainerError(err, "Couldn't stop Fluxer with "+a.tool+". rexctl didn't change anything.")
	}
	if err := a.takeOver(ctx, app); err != nil {
		return a.rollBack(ctx, app, err)
	}
	if res, err := a.composed.Project.DownReport(bg, false); err != nil {
		u.Detail("%v", err)
	} else if line := rt.ForeignLine(res.Foreign); line != "" {
		u.Info("%s", line)
	}
	kept := filepath.Join(a.dir, instance.StateDirName, adoptedDir)
	if err := os.MkdirAll(kept, 0o700); err != nil {
		u.Detail("%v", err)
	}
	for _, f := range append(slices.Clone(a.compose.Files), filepath.Join(a.dir, rt.EnvFileName)) {
		if err := os.Rename(f, filepath.Join(kept, filepath.Base(f))); err != nil && !errors.Is(err, os.ErrNotExist) {
			u.Detail("%v", err)
		}
	}
	app.remember(a.dir)
	u.OK("rexctl runs Fluxer in %s now. Your old files are in %s/.rexctl/adopted.", a.dir, a.dir)
	return nil
}

func secretKeys() []string {
	out := make([]string, len(secrets.Required))
	for i, s := range secrets.Required {
		out[i] = s.Key
	}
	return out
}

func (a *adoption) takeOver(ctx context.Context, app *App) error {
	_, err := os.Stat(rt.ConfigDir(a.inst))
	a.config = err == nil
	for _, d := range []string{instance.StateDirName, instance.BackupsDir} {
		if err := os.MkdirAll(a.inst.Path(d), 0o700); err != nil {
			return err
		}
		if err := envfile.RestrictToOwner(a.inst.Path(d)); err != nil {
			return err
		}
	}
	if err := a.inst.Save(); err != nil {
		return err
	}
	r, err := app.Runtime(ctx, a.inst)
	if err != nil {
		return err
	}
	return app.UI.Step(ctx, "Starting Fluxer in rexctl's own container", func(ctx context.Context, s *ui.Step) error {
		if err := rt.PrepareContainer(ctx, r); err != nil {
			return err
		}
		if _, err := r.Backups().Create(ctx, s, rt.BackupOptions{Reason: "adopt", Dir: a.inst.Path(instance.BackupsDir)}); err != nil {
			return err
		}
		if err := r.Start(ctx, s, rt.StartOptions{}); err != nil {
			return err
		}
		if err := answers(ctx, a.inst, handOverCheckTimeout); err != nil {
			return err
		}
		s.Done("Fluxer %s runs in rexctl's own container.", a.inst.State.Release)
		return nil
	})
}

func (a *adoption) rollBack(ctx context.Context, app *App, cause error) error {
	u := app.UI
	u.Detail("%v", cause)
	bg := context.WithoutCancel(ctx)
	if r, err := app.Runtime(bg, a.inst); err == nil {
		if err := rt.RemoveContainer(bg, r); err != nil {
			u.Detail("removing rexctl's container: %v", err)
		}
	}
	if a.existing != nil {
		if err := a.existing.Save(); err != nil {
			u.Detail("%v", err)
		}
	} else if err := os.RemoveAll(a.inst.StateDir()); err != nil {
		u.Detail("%v", err)
	}
	if !a.config {
		if err := os.RemoveAll(rt.ConfigDir(a.inst)); err != nil {
			u.Detail("%v", err)
		}
	}
	if err := a.composed.Project.Start(bg); err != nil {
		return rt.ContainerError(errors.Join(cause, err), fmt.Sprintf("rexctl couldn't start Fluxer in its own container, and couldn't start it again with %s. Run %s up -d in %s.", a.tool, a.tool, a.dir))
	}
	return &ui.ExitError{Code: ui.ExitFailed, Msg: fmt.Sprintf("rexctl couldn't start Fluxer in its own container and started it again with %s. Nothing changed.", a.tool), Fix: "Run rexctl adopt --verbose to see why.", Err: cause}
}
