// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/spf13/pflag"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func (in *installer) machineChecks(ctx context.Context, c *ui.Checklist) error {
	in.failed = nil
	if err := in.askRuntime(ctx, c); err != nil {
		return err
	}
	in.machineRuns++
	in.planFor("machine")
	c.Check("host", "This machine", func(ctx context.Context) (ui.State, string, error) {
		h, err := host.Detect(ctx, in.dir)
		if err != nil {
			return ui.StateFail, "", err
		}
		in.host = h
		return ui.StateOK, in.hostLine(), nil
	})
	if in.host == nil {
		return notReady()
	}
	if in.native {
		in.nativeMachine(ctx, c)
	} else {
		in.containerMachine(ctx, c)
	}
	return in.failed
}

func (in *installer) hostLine() string {
	hw := settings.Hardware()
	parts := []string{plural(hw.CPUs, "CPU", "CPUs"), ui.Memory(int64(in.host.MemoryBytes)) + " memory"}
	if in.host.FreeDiskBytes > 0 {
		parts = append(parts, ui.Bytes(int64(in.host.FreeDiskBytes))+" free")
	}
	return strings.Join(parts, ", ")
}

func (in *installer) machineSummary() string {
	if in.host == nil {
		return ""
	}
	if in.native {
		return in.hostLine() + ", " + runtimeWord(runtimeSystemd)
	}
	return in.hostLine() + ", " + rt.ContainerLabel(engine.Kind(in.f.Engine))
}

func (in *installer) checkEnv() *checks.Env {
	env := &checks.Env{Dir: in.dir, Domain: normaliseDomain(in.f.Domain), Engine: in.engine, Host: in.host, Behind: in.f.BehindProxy, Native: in.native, Name: in.f.Project, Bind: in.f.Bind, Settings: in.settings(), Prefer: engine.Kind(in.f.Engine)}
	if in.engine == nil && !in.native {
		env.SetEngine(in.found, in.findErr)
	}
	return env
}

func (in *installer) checkRows(ctx context.Context, c *ui.Checklist, ids ...string) {
	env := in.checkEnv()
	for _, id := range ids {
		if in.ran[id] {
			continue
		}
		chk, ok := checks.Get(id)
		if !ok || chk.Phases&checks.Preinstall == 0 {
			continue
		}
		in.ran[id] = true
		r := checks.RunIn(ctx, env, checks.Preinstall, chk)
		if r.Status == checks.Skip && r.Message == "" {
			continue
		}
		label := checkLabel(id)
		if rest, ok := strings.CutPrefix(r.Message, label+" "); ok && r.Status == checks.Pass {
			r.Message = rest
		}
		c.Check(id, label, func(context.Context) (ui.State, string, error) {
			return in.resultRow(r)
		})
	}
}

func checkLabel(id string) string {
	switch id {
	case "memory":
		return "Memory"
	case "disk":
		return "Disk"
	case "engine":
		return "Engine"
	case "ports":
		return "Ports"
	case "dns":
		return "DNS"
	case "low-ports":
		return "Low ports"
	case "memory-overcommit":
		return "Overcommit"
	case "objects-filesystem":
		return "Upload folder"
	case "podman-subids":
		return "User IDs"
	case "windows-linux-containers":
		return "Containers"
	}
	return id
}

func (in *installer) resultRow(r checks.Result) (ui.State, string, error) {
	switch r.Status {
	case checks.Pass:
		return ui.StateOK, r.Message, nil
	case checks.Warn:
		if r.Fix != "" {
			return ui.StateWarn, r.Message + " " + r.Fix, nil
		}
		return ui.StateWarn, r.Message, nil
	case checks.Fail:
		if in.failed == nil {
			in.failed = ui.Refused(r.Message, in.checkFix(r.Fix))
		}
		return ui.StateFail, r.Message, nil
	}
	return ui.StateSkipped, r.Message, nil
}

func (in *installer) checkFix(fix string) string {
	if rest, ok := strings.CutPrefix(fix, "Run rexctl install "); ok {
		return "Run " + in.rerun() + " " + rest
	}
	return strings.TrimSpace(fix + " Then run " + in.rerun() + " again.")
}

func (in *installer) rerun() string {
	if in.f.ForMigrate {
		return migrateCommand
	}
	args := []string{"rexctl", "install"}
	if in.native {
		args = []string{"sudo", "rexctl", "install"}
	}
	in.c.Flags().Visit(func(fl *pflag.Flag) {
		if fl.Name == "confirm" || strings.HasSuffix(fl.Name, "-file") && fl.Value.String() == "-" {
			return
		}
		if fl.Value.Type() == "bool" {
			if fl.Value.String() == "true" {
				args = append(args, "--"+fl.Name)
			} else {
				args = append(args, "--"+fl.Name+"=false")
			}
			return
		}
		args = append(args, "--"+fl.Name, shellWord(fl.Value.String()))
	})
	return strings.Join(args, " ")
}

func (in *installer) findEngine(ctx context.Context) (ui.State, string, error) {
	want := engine.Kind(in.f.Engine)
	e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: want, AllowOld: true})
	if err == nil && want != "" && e.Kind != want {
		e.Close()
		e, err = nil, &engine.Error{Op: "connect", Class: engine.ClassUnreachable, Err: fmt.Errorf("%s does not respond", want)}
	}
	in.found, in.findErr = e, err
	if err == nil {
		if e.TooOld {
			return in.offerDocker(e)
		}
		in.engine = e
		if e.DisplayName() == want.Name() {
			return ui.StateOK, "found", nil
		}
		return ui.StateOK, e.DisplayName(), nil
	}
	if engine.ClassOf(err) != engine.ClassUnreachable {
		in.failed = rt.EngineError(want, err)
		return ui.StateFail, "isn't working", nil
	}
	if runtime.GOOS != "linux" && in.f.Engine == string(engine.Podman) {
		in.failed = ui.Refused("rexctl runs Fluxer on Podman only on Linux.", "Start Docker Desktop, then run rexctl install without --engine podman.")
		return ui.StateFail, "only on Linux", nil
	}
	_, podmanErr := exec.LookPath("podman")
	docker := dockerInstalled()
	switch {
	case podmanErr == nil && runtime.GOOS == "linux" && in.f.Engine != string(engine.Docker) && (!docker || in.f.Engine == string(engine.Podman)):
		return in.planPodmanSocket(ctx)
	case podmanErr != nil && !docker:
		return in.planDocker()
	}
	in.failed = rt.EngineError(want, err)
	return ui.StateFail, "doesn't respond", nil
}

func dockerInstalled() bool {
	if path, err := exec.LookPath("docker"); err == nil {
		return !engine.PodmanDockerWrapper(path)
	}
	switch runtime.GOOS {
	case "windows":
		dir := os.Getenv("ProgramFiles")
		return dir != "" && fileExists(filepath.Join(dir, "Docker", "Docker", "Docker Desktop.exe"))
	case "darwin":
		home, _ := os.UserHomeDir()
		return fileExists("/Applications/Docker.app") || (home != "" && fileExists(filepath.Join(home, "Applications", "Docker.app")))
	}
	return false
}

func (in *installer) allowed(flag string, value bool, refusal error) error {
	if fl := in.c.Flags().Lookup(flag); fl != nil && fl.Changed {
		if value {
			return nil
		}
		return refusal
	}
	if in.app.UI.Interactive() || in.app.UI.Yes() || in.f.DryRun {
		return nil
	}
	return ui.MissingFlag(flag)
}

type planned struct {
	item ui.PlanItem
	task *ui.Task
}

var planStages = []string{"machine", "ready", "admin"}

func (in *installer) planFor(stage string) {
	if in.plans == nil {
		in.plans = map[string][]planned{}
	}
	in.planStage = stage
	delete(in.plans, stage)
}

func (in *installer) change(verb ui.PlanVerb, text string, t *ui.Task) {
	if in.plans == nil {
		in.plans = map[string][]planned{}
	}
	in.plans[in.planStage] = append(in.plans[in.planStage], planned{item: ui.PlanItem{Verb: verb, Text: text}, task: t})
}

func (in *installer) planned() ([]ui.PlanItem, []ui.Task) {
	var items []ui.PlanItem
	var tasks []ui.Task
	for _, stage := range planStages {
		for _, p := range in.plans[stage] {
			items = append(items, p.item)
			if p.task != nil {
				tasks = append(tasks, *p.task)
			}
		}
	}
	return items, tasks
}

func (in *installer) planDocker() (ui.State, string, error) {
	if in.f.Engine == string(engine.Podman) {
		in.failed = ui.Refused("Podman isn't installed.", "Install Podman, then run rexctl install again.")
		return ui.StateFail, "isn't installed", nil
	}
	if !host.CanInstallDocker(in.host) {
		fix := "Install Docker Desktop from https://docs.docker.com/desktop/, then run rexctl install again."
		switch {
		case in.defaulted && in.host.OS == "linux" && !in.host.WSL:
			fix = noRuntimeFix
		case in.host.OS == "linux" && !in.host.WSL:
			fix = "Install Docker from https://docs.docker.com/engine/install/, then run rexctl install again."
		}
		in.failed = ui.Refused("Docker isn't installed.", fix)
		return ui.StateFail, "isn't installed", nil
	}
	if err := in.allowed("install-docker", in.f.InstallDocker, ui.Refused("Fluxer needs Docker.", "Install Docker, then run rexctl install again.")); err != nil {
		in.failed = err
		return ui.StateFail, "isn't installed", nil
	}
	in.change(ui.PlanRun, "Run the Docker installer", &ui.Task{ID: "docker", Title: "Installing Docker", Run: in.installDocker})
	return ui.StateWarn, "Docker isn't installed. rexctl installs it for you.", nil
}

func (in *installer) offerDocker(e *engine.Engine) (ui.State, string, error) {
	need := strings.TrimSuffix(engine.MinPodman, ".0")
	msg := fmt.Sprintf("Podman %s is too old for Fluxer, which needs %s or newer.", e.Version, need)
	refusal := ui.Refused(msg, fmt.Sprintf("Upgrade Podman to %s or newer, or install Docker, then run rexctl install again.", need))
	if in.f.Engine == string(engine.Podman) || !host.CanInstallDocker(in.host) {
		in.failed = refusal
		return ui.StateFail, msg, nil
	}
	if err := in.allowed("install-docker", in.f.InstallDocker, refusal); err != nil {
		in.failed = err
		return ui.StateFail, msg, nil
	}
	e.Close()
	in.found, in.findErr = nil, nil
	in.change(ui.PlanRun, "Run the Docker installer, so Fluxer runs on Docker", &ui.Task{ID: "docker", Title: "Installing Docker", Run: in.installDocker})
	return ui.StateWarn, msg + " rexctl installs Docker instead.", nil
}

func (in *installer) installDocker(ctx context.Context, s *ui.Step) error {
	if err := host.InstallDocker(ctx, in.host, s.Log); err != nil {
		return in.dockerFailure(err)
	}
	e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: engine.Docker})
	if err != nil {
		return in.dockerFailure(err)
	}
	in.engine = e
	s.Done("Installed Docker %s.", e.Version)
	return nil
}

func (in *installer) dockerFailure(err error) error {
	switch {
	case errors.Is(err, host.ErrRelogin) || engine.ClassOf(err) == engine.ClassDenied:
		return ui.Refused("Docker is installed, and your account needs a new login to use it.", "Log out and back in, then run rexctl install again.")
	case errors.Is(err, context.Canceled):
		return err
	}
	return ui.Wrap(err, "Couldn't install Docker.", "Install Docker from https://docs.docker.com/engine/install/, then run rexctl install again.")
}

func (in *installer) planPodmanSocket(ctx context.Context) (ui.State, string, error) {
	rootless := os.Geteuid() != 0
	if runtime.GOOS == "linux" && rootless && !host.HasSubIDs(ctx) {
		in.failed = ui.Refused("Your account has no subordinate user IDs, and Podman needs them to unpack Fluxer.", "Add your account to /etc/subuid and /etc/subgid, run podman system migrate, then run rexctl install again.")
		return ui.StateFail, "no subordinate user IDs", nil
	}
	if missing := host.MissingRootlessTools(); runtime.GOOS == "linux" && rootless && len(missing) > 0 {
		in.failed = ui.Refused("Podman without root needs newuidmap, newgidmap and pasta, and this machine has no "+ui.JoinOr(missing)+".", host.RootlessToolsFix("rexctl install again"))
		return ui.StateFail, "no " + ui.JoinOr(missing), nil
	}
	s := host.CheckPodman(ctx, rootless, true)
	if !s.Systemd {
		in.failed = ui.Refused("Podman's API socket isn't running.", "Start podman.socket, then run rexctl install again.")
		return ui.StateFail, "API socket isn't running", nil
	}
	if err := in.allowed("setup-podman", in.f.SetupPodman, ui.Refused("Fluxer needs Podman's API socket.", "Start podman.socket, then run rexctl install again.")); err != nil {
		in.failed = err
		return ui.StateFail, "API socket isn't running", nil
	}
	in.change(ui.PlanRun, "Start Podman's API socket", &ui.Task{ID: "podman-socket", Title: "Starting Podman's API socket", Run: func(ctx context.Context, st *ui.Step) error {
		if err := host.RunSteps(ctx, s.Steps()); err != nil {
			if errors.Is(err, host.ErrPodmanServiceFailed) {
				return ui.Wrap(err, "Podman's API service stopped with an error.", "Fix what it reports, then run rexctl install again.")
			}
			if errors.Is(err, host.ErrNoUserSession) {
				msg, fix := host.NoUserSession("rexctl install again")
				return ui.Wrap(err, msg, fix)
			}
			return ui.Wrap(err, "Couldn't start Podman's API socket.", "Run rexctl install --verbose to see why.")
		}
		e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: engine.Podman, AllowOld: true})
		if err != nil {
			return rt.EngineError(engine.Podman, err)
		}
		if e.TooOld {
			e.Close()
			return ui.Refused(fmt.Sprintf("Podman %s is too old for Fluxer.", e.Version), "Upgrade Podman, or install Docker, then run rexctl install again.")
		}
		in.engine = e
		if t := in.podmanSetup(ctx); t != nil {
			if err := t.Run(ctx, st); err != nil {
				return err
			}
		}
		st.Done("Started Podman's API socket.")
		return nil
	}})
	return ui.StateWarn, "Podman's API socket isn't running. Rex starts it.", nil
}

func (in *installer) podmanSetup(ctx context.Context) *ui.Task {
	if in.engine == nil || in.engine.Kind != engine.Podman {
		return nil
	}
	s := host.CheckPodman(ctx, in.engine.Rootless, false)
	steps := s.Steps()
	if in.f.Runtime == runtimePodman && in.f.BootService != instance.BootServiceOff && !host.PodmanRestartOn(ctx, in.engine.Rootless) {
		steps = append(steps, host.PodmanRestartStep(in.engine.Rootless))
	}
	if len(steps) == 0 || !in.f.SetupPodman || in.allowed("setup-podman", true, nil) != nil {
		return nil
	}
	return &ui.Task{ID: "podman", Title: "Setting up Podman for Fluxer", Run: func(ctx context.Context, st *ui.Step) error {
		if err := host.RunSteps(ctx, steps); err != nil {
			return ui.Wrap(err, "Couldn't set up Podman for Fluxer.", "Run rexctl install --verbose to see why.")
		}
		st.Info("Set up Podman for Fluxer.")
		return nil
	}}
}

func (in *installer) rootlessPodman() bool {
	if in.engine != nil {
		return in.engine.Kind == engine.Podman && in.engine.Rootless
	}
	return !in.native && in.f.Engine != string(engine.Docker) && os.Geteuid() != 0 && runtime.GOOS == "linux" && !dockerInstalled()
}
