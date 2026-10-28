// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"os/exec"
	"path"
	"runtime"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	bootLockWait   = 10 * time.Minute
	bootRouteWait  = 2 * time.Minute
	readyHintAfter = 3 * time.Minute
)

type logPart struct {
	name     string
	services []string
}

var logParts = []logPart{
	{"web", []string{"app-proxy", "static-proxy"}},
	{"api", []string{"api"}},
	{"worker", []string{"worker"}},
	{"gateway", []string{"gateway"}},
	{"voice", []string{"livekit"}},
	{"media", []string{"media-proxy"}},
	{"edge", []string{"edge"}},
	{"admin", []string{"admin"}},
	{"push", []string{"push"}},
	{"database", []string{"postgres"}},
	{"search", []string{"meilisearch"}},
	{"storage", []string{"seaweedfs", "seaweedfs-init"}},
	{"cache", []string{"valkey"}},
	{"queue", []string{"nats"}},
	{"internal", []string{"snowflakes", "users", "gifs", "messages", "unfurl"}},
}

func logServiceNames() []string {
	names := make([]string, 0, len(logParts))
	for _, p := range logParts {
		names = append(names, p.name)
	}
	return names
}

func servicesForName(name string) ([]string, error) {
	name = strings.ToLower(strings.TrimSpace(name))
	for _, p := range logParts {
		if p.name == name {
			return p.services, nil
		}
	}
	if slices.Contains(stack.Services, name) {
		return []string{name}, nil
	}
	return nil, &ui.ExitError{
		Code: ui.ExitUsage,
		Msg:  fmt.Sprintf("There's no service called %s.", name),
		Fix:  "Pick " + ui.JoinOr(logServiceNames()) + ".",
	}
}

func engineFailure(err error) error {
	return engineFailureFor("", err)
}

func engineFailureFor(kind engine.Kind, err error) error {
	if err == nil {
		return nil
	}
	var ee *ui.ExitError
	if errors.As(err, &ee) || errors.Is(err, context.Canceled) {
		return err
	}
	refused := func(msg, fix string) error { return &ui.ExitError{Code: ui.ExitRefused, Msg: msg, Fix: fix, Err: err} }
	failed := func(msg, fix string) error { return &ui.ExitError{Code: ui.ExitFailed, Msg: msg, Fix: fix, Err: err} }
	desktop := kind != engine.Podman && runtime.GOOS != "linux"
	name := kind.Name()
	var e *engine.Error
	errors.As(err, &e)
	switch engine.ClassOf(err) {
	case engine.ClassUnreachable:
		if desktop {
			return refused("Docker Desktop isn't running.", "Open Docker Desktop and wait until its engine is running, then try again.")
		}
		if fix := podmanSocketFix(kind); fix != "" {
			return refused("Podman is installed, but its API socket isn't running.", fix)
		}
		if kind == "" {
			return refused("Neither Docker nor Podman answers on this machine.", "Start Docker or Podman, then try again.")
		}
		return refused(name+" isn't running.", "Start "+name+", then try again.")
	case engine.ClassDenied:
		switch {
		case runtime.GOOS == "windows":
			return refused("This account isn't allowed to use Docker.", "Add your account to docker-users, then sign out and back in.")
		case kind == engine.Podman:
			return refused("This account isn't allowed to use Podman.", "Run rexctl as the account that runs Podman.")
		}
		return refused("This account isn't allowed to use Docker.", "Add your account to the docker group with sudo usermod -aG docker $USER, then sign out and back in.")
	case engine.ClassWindowsContainers:
		return refused("Docker Desktop is set to Windows containers.", "Switch Docker Desktop to Linux containers, then try again.")
	case engine.ClassPodmanTooOld:
		return refused(fmt.Sprintf("Fluxer keeps running, but this needs Podman %s or newer.", strings.TrimSuffix(engine.MinPodman, ".0")),
			"Upgrade Podman, or move to Docker with rexctl backup and then rexctl restore <file> --engine docker.")
	case engine.ClassEngineChanged:
		other := "another engine"
		switch {
		case e == nil || e.Other == "":
		case e.Other == name || desktop && e.Other == "Docker Desktop":
			other = "a different " + e.Other + " engine"
		default:
			other = e.Other
		}
		if e != nil && e.Replaced {
			return refused(fmt.Sprintf("The engine Fluxer uses is now %s, which doesn't have this instance's data.", other),
				"Start the engine Fluxer was installed on. If it was reset or reinstalled, restore a backup with rexctl restore <file>.")
		}
		if desktop {
			return refused("Docker Desktop isn't running, and "+other+" answers in its place.", "Open Docker Desktop and wait until its engine is running, then try again.")
		}
		return refused(fmt.Sprintf("Fluxer runs on %s, but %s answers in its place.", name, other), "Start the engine Fluxer runs on, or point rexctl at it with rexctl config set engine.host.")
	case engine.ClassRateLimit:
		registry := "The image registry"
		if e == nil || len(e.Images) == 0 || dockerHub(e.Images) {
			registry = "Docker Hub"
		}
		msg := registry + " is limiting downloads from this machine."
		if e != nil && len(e.Images) > 0 {
			msg = fmt.Sprintf("%s is limiting downloads from this machine and %s didn't download. Images already on this machine weren't downloaded again.", registry, ui.JoinAnd(imageNames(e.Images)))
		}
		if registry == "Docker Hub" {
			return failed(msg, "Wait for Docker Hub's limit to reset, which takes up to 6 hours, then try again.")
		}
		return failed(msg, "Wait a while, then try again.")
	case engine.ClassCredentials:
		return failed("Docker's credential helper on this machine doesn't work.", "Remove credsStore from your Docker config.json, then try again.")
	case engine.ClassAuth:
		return failed("The image registry refused to send Fluxer's images.", "Sign in to the registry with docker login, or check images.registry with rexctl config get images.registry.")
	case engine.ClassImageNotFound:
		return failed("The image registry doesn't have an image Fluxer needs.", "Check the images.* settings with rexctl config list, then try again.")
	case engine.ClassDNS:
		return failed("This machine can't look up the image registry.", "Check this machine's DNS settings, then try again.")
	case engine.ClassTLS:
		return failed("rexctl couldn't make a secure connection to the image registry.", "Check this machine's clock and any proxy that inspects HTTPS, then try again.")
	case engine.ClassProxy:
		return failed("The proxy the engine uses refused the connection.", "Check the proxy settings of "+name+", then try again.")
	case engine.ClassTimeout:
		return failed("The connection timed out.", "Check this machine's internet connection, then try again.")
	case engine.ClassDiskFull:
		return failed("This machine ran out of disk space.", "Free up space, then try again.")
	case engine.ClassPortInUse:
		return failed("A port Fluxer needs is already in use on this machine.", "Run rexctl doctor to see which one.")
	case engine.ClassAddressPools:
		return failed("Docker has no free network addresses left for Fluxer.", "Remove the Docker networks you don't use with docker network prune, then try again.")
	case engine.ClassSubIDs:
		return failed("Your account has no subordinate user IDs, and Podman needs them to run Fluxer.", "Add your account to /etc/subuid and /etc/subgid, run podman system migrate, then try again.")
	case engine.ClassDependencyFailed:
		return failed("A Fluxer service didn't start, and the services that need it didn't start either.", "Run rexctl logs to see why.")
	case engine.ClassVolumeChanged:
		return refused("A data volume on this machine doesn't match what Fluxer expects, and rexctl left it alone.", "Run rexctl doctor to see what's wrong.")
	case engine.ClassInvalidConfig:
		return failed("rexctl can't load Fluxer's compose files.", "Run rexctl doctor to see what's wrong.")
	case engine.ClassForeignProject:
		return refused("Another Fluxer instance on this machine uses the same name.", "Run rexctl status --dir with that instance's folder to check it.")
	}
	return err
}

func dockerHub(refs []string) bool {
	for _, ref := range refs {
		first, _, found := strings.Cut(ref, "/")
		if !found || first == "docker.io" || (!strings.ContainsAny(first, ".:") && first != "localhost") {
			return true
		}
	}
	return false
}

func imageNames(refs []string) []string {
	var out []string
	for _, ref := range refs {
		name, _, _ := strings.Cut(ref, "@")
		name = path.Base(name)
		name, _, _ = strings.Cut(name, ":")
		if !slices.Contains(out, name) {
			out = append(out, name)
		}
	}
	return out
}

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
		return engineFailure(err)
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

func waitForFluxer(ctx context.Context, inst *instance.Instance, p *engine.Project, s *ui.Step, verb string) error {
	err := p.WaitReady(ctx, engine.DefaultReadyTimeout, showReadiness(s, verb+" Fluxer"))
	return readyFailure(ctx, inst, p, err)
}

func showReadiness(s *ui.Step, prefix string) func(engine.Readiness) {
	start, hinted := time.Now(), false
	return func(r engine.Readiness) {
		waiting := slices.Concat(r.NotReady, r.Failed)
		slices.Sort(waiting)
		showWaiting(s, prefix, len(r.Ready), len(r.Ready)+len(waiting), "ready", waiting)
		if !hinted && len(waiting) > 0 && time.Since(start) >= readyHintAfter {
			hinted = true
			s.Info("%s", notReadyHint(waiting))
		}
	}
}

func notReadyHint(waiting []string) string {
	if len(waiting) == 1 && slices.Contains(stack.Services, waiting[0]) {
		return fmt.Sprintf("%s isn't ready after %d minutes. Run rexctl logs %s in another terminal to see why.", waiting[0], int(readyHintAfter.Minutes()), waiting[0])
	}
	verb := "aren't"
	if len(waiting) == 1 {
		verb = "isn't"
	}
	return fmt.Sprintf("%s %s ready after %d minutes. Run rexctl logs in another terminal to see why.", waitingFor(waiting), verb, int(readyHintAfter.Minutes()))
}

func showWaiting(s *ui.Step, prefix string, done, total int, state string, waiting []string) {
	if len(waiting) == 0 {
		s.Title(fmt.Sprintf("%s, %d of %d services %s", prefix, done, total, state))
		return
	}
	s.Waiting(fmt.Sprintf("%s: %d of %d services %s", prefix, done, total, state), waitingFor(waiting))
}

func waitingFor(names []string) string {
	if len(names) > 3 {
		return fmt.Sprintf("%s and %d more", strings.Join(names[:3], ", "), len(names)-3)
	}
	return ui.JoinAnd(names)
}

type progress struct {
	mu         sync.Mutex
	app        *App
	step       *ui.Step
	verb       string
	images     map[string]bool
	layers     map[string][2]int64
	states     map[string]string
	samples    []byteSample
	containers map[string]engine.Event
	verbose    bool
}

type byteSample struct {
	at    time.Time
	bytes int64
}

const rateWindow = 5 * time.Second

func (a *App) progress(s *ui.Step, verb string) func(engine.Event) {
	pr := &progress{app: a, step: s, verb: verb, images: map[string]bool{}, layers: map[string][2]int64{}, states: map[string]string{}, containers: map[string]engine.Event{}, verbose: a.UI.Verbose()}
	return pr.on
}

func (pr *progress) showContainers() {
	started := 0
	var waiting []string
	for _, ev := range pr.containers {
		switch ev.Text {
		case "Started", "Running", "Healthy", "Exited", "Restarted":
			started++
		case "Waiting":
			started++
			if !slices.Contains(waiting, ev.Service) {
				waiting = append(waiting, ev.Service)
			}
		}
	}
	slices.Sort(waiting)
	showWaiting(pr.step, pr.verb+" Fluxer", started, len(pr.containers), "started", waiting)
}

func (pr *progress) on(ev engine.Event) {
	pr.mu.Lock()
	defer pr.mu.Unlock()
	if pr.verbose {
		pr.app.UI.Detail("%s %s", ev.ID, strings.TrimSpace(ev.Text+" "+string(ev.Status)))
	}
	done := ev.Status == engine.EventDone
	switch {
	case ev.Kind == "container":
		pr.containers[ev.ID] = ev
		pr.step.Progress(0, 0)
		pr.step.Rate(0)
		pr.showContainers()
	case ev.Parent != "":
		pr.states[ev.ID] = ev.Text
		l, known := pr.layers[ev.ID]
		switch {
		case ev.Text == "Downloading" && ev.Total > 0:
			pr.layers[ev.ID] = [2]int64{ev.Current, ev.Total}
		case known && ev.Text != "Downloading":
			pr.layers[ev.ID] = [2]int64{l[1], l[1]}
		case !known && ev.Text != "Extracting" && ev.Total > 0:
			pr.layers[ev.ID] = [2]int64{ev.Current, ev.Total}
		}
		pr.showPull()
	case ev.Kind == "image" || ev.Parent == "" && ev.Total > 0:
		pr.images[ev.ID] = pr.images[ev.ID] || done
		if ev.Total > 0 {
			pr.step.Title(pr.pullTitle())
			pr.step.Progress(ev.Current, ev.Total)
			return
		}
		pr.showPull()
	}
}

func (pr *progress) pullTitle() string {
	title := fmt.Sprintf("Downloading Fluxer, %d of %d images", count(pr.images), len(pr.images))
	if pr.unpacking() {
		title += ", unpacking"
	}
	return title
}

func (pr *progress) unpacking() bool {
	extracting := false
	for _, state := range pr.states {
		switch state {
		case "Pulling fs layer", "Waiting", "Downloading", "Verifying Checksum":
			return false
		case "Extracting":
			extracting = true
		}
	}
	return extracting
}

func (pr *progress) showPull() {
	var cur, total int64
	for _, l := range pr.layers {
		cur, total = cur+l[0], total+l[1]
	}
	pr.step.Title(pr.pullTitle())
	pr.step.Progress(cur, total)
	if pr.unpacking() {
		pr.samples = nil
		pr.step.Rate(0)
		return
	}
	now := time.Now()
	pr.samples = append(pr.samples, byteSample{at: now, bytes: cur})
	for len(pr.samples) > 1 && now.Sub(pr.samples[0].at) > rateWindow {
		pr.samples = pr.samples[1:]
	}
	first := pr.samples[0]
	if elapsed := now.Sub(first.at); elapsed >= time.Second && cur > first.bytes {
		pr.step.Rate(int64(float64(cur-first.bytes) / elapsed.Seconds()))
		return
	}
	pr.step.Rate(0)
}

func count(m map[string]bool) int {
	n := 0
	for _, v := range m {
		if v {
			n++
		}
	}
	return n
}

func newLogsCmd(app *App) *cobra.Command {
	var follow bool
	var since string
	c := &cobra.Command{
		Use:   "logs [service]",
		Short: "Show logs from your instance",
		Long: "Show logs from your instance.\n\nThe services are " + ui.JoinAnd(logServiceNames()) +
			".\nWithout a service, rexctl shows every service.",
		Args: maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			var services []string
			if len(args) == 1 {
				var err error
				if services, err = servicesForName(args[0]); err != nil {
					return err
				}
			}
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			p, err := app.Named(ctx, inst)
			if err != nil {
				return err
			}
			opts := engine.LogsOptions{Follow: follow, Since: since, Stdout: app.UI.Stdout()}
			if since == "" {
				opts.Tail = 200
			}
			if err := p.Logs(ctx, opts, services...); err != nil {
				if ctx.Err() != nil {
					return nil
				}
				return ui.Wrap(engineFailure(err), "Couldn't read the logs.", "Run rexctl status to check your instance.")
			}
			return nil
		},
	}
	c.Flags().BoolVarP(&follow, "follow", "f", false, "keep printing new lines")
	c.Flags().StringVar(&since, "since", "", "show lines newer than this, like 10m or 2h")
	return c
}

func newStartCmd(app *App) *cobra.Command {
	return newBootableCmd(app, "start", "Start your instance", runStart, runBootStart)
}

func newBootableCmd(app *App, use, short string, run, bootRun func(context.Context, *App) error) *cobra.Command {
	var boot bool
	c := &cobra.Command{
		Use:   use,
		Short: short,
		Args:  noArgs,
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
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	return app.Change(ctx, inst, func(e *engine.Engine) error {
		if inst.State.Stopped {
			inst.State.Stopped = false
			if err := inst.Save(); err != nil {
				return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
			}
		}
		p := e.Named(inst.Project(), inst.Dir)
		err := app.UI.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
			if e.TooOld {
				if err := p.Start(ctx); err != nil {
					return engineFailure(err)
				}
			} else if _, err := deploy.Apply(ctx, inst, e, deploy.ApplyOptions{Pull: true, OnEvent: app.progress(s, "Starting")}); err != nil {
				return readyFailure(ctx, inst, p, err)
			}
			if err := waitForFluxer(ctx, inst, p, s, "Starting"); err != nil {
				return err
			}
			s.Done("Started Fluxer at %s.", inst.Origin())
			return nil
		})
		if err != nil {
			return err
		}
		if inst.State.Installing {
			inst.State.Installing = false
			if err := inst.Save(); err != nil {
				return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
			}
		}
		app.sendAdminImport(ctx, inst, p)
		return nil
	})
}

func bootInstance(app *App) (*instance.Instance, error) {
	dir, err := app.Dir()
	if err != nil {
		return nil, nil
	}
	if _, err := instance.Find(dir); errors.Is(err, instance.ErrNotFound) || errors.Is(err, instance.ErrLegacy) {
		return nil, nil
	}
	return app.Instance()
}

func runBootStart(ctx context.Context, app *App) error {
	inst, err := bootInstance(app)
	if err != nil || inst == nil {
		return err
	}
	if inst.State.Stopped || inst.State.Ejected != nil || alreadyUp(ctx, app, inst) {
		return nil
	}
	waitForRoute(ctx, bootRouteWait)
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
			return engineFailure(e.Named(inst.Project(), inst.Dir).Start(ctx))
		}
		if u := inst.State.Upgrade; u != nil && phaseIndex(u.Phase) >= phaseIndex(release.PhaseRecreated) {
			next, err := release.SnapshotState(inst, u.To)
			if err != nil {
				return ui.Wrap(err, fmt.Sprintf("Couldn't read the state of Fluxer %s.", release.Short(u.To)), "Run rexctl doctor to see what's wrong.")
			}
			next.Engine = inst.State.Engine
			p, err := deploy.Load(ctx, &instance.Instance{Dir: inst.Dir, State: next}, e)
			if err != nil {
				return engineFailure(err)
			}
			if err := p.Up(context.WithoutCancel(ctx), engine.UpOptions{RemoveOrphans: true, Wait: true, WaitTimeout: engine.DefaultReadyTimeout}); err != nil {
				return readyFailure(ctx, inst, p, err)
			}
			return inst.DropRootCopies(ctx, true)
		}
		if p, err := deploy.Load(ctx, inst, e); err == nil && stackRunning(ctx, p) {
			app.sendAdminImport(ctx, inst, p)
			return nil
		}
		if _, err := deploy.Apply(ctx, inst, e, deploy.ApplyOptions{Wait: true}); err != nil {
			return readyFailure(ctx, inst, e.Named(inst.Project(), inst.Dir), err)
		}
		app.sendAdminImport(ctx, inst, e.Named(inst.Project(), inst.Dir))
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
			return app.Locked(ctx, inst, fn)
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
	return newBootableCmd(app, "stop", "Stop your instance", runStop, runBootStop)
}

func runStop(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		if err := managed(inst); err != nil {
			return err
		}
		p, err := app.Named(ctx, inst)
		if err != nil {
			return err
		}
		err = app.UI.Step(ctx, "Stopping Fluxer", func(ctx context.Context, s *ui.Step) error {
			if err := p.Stop(ctx); err != nil {
				return ui.Wrap(engineFailure(err), "Couldn't stop Fluxer.", "Run rexctl status to see what's still running.")
			}
			s.Done("Stopped Fluxer.")
			return nil
		})
		if err != nil {
			return err
		}
		inst.State.Stopped = true
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, "Couldn't save your instance's state.", "Run rexctl stop again.")
		}
		app.UI.Info("Run rexctl start to start it again.")
		return nil
	})
}

func runBootStop(ctx context.Context, app *App) error {
	inst, err := bootInstance(app)
	if err != nil || inst == nil {
		return err
	}
	if inst.State.Ejected != nil {
		return nil
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
	return engineFailure(p.Stop(context.WithoutCancel(ctx)))
}

func newRestartCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:   "restart",
		Short: "Restart your instance",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			return app.Change(ctx, inst, func(e *engine.Engine) error {
				return app.UI.Step(ctx, "Restarting Fluxer", func(ctx context.Context, s *ui.Step) error {
					if _, err := deploy.Apply(ctx, inst, e, deploy.ApplyOptions{Pull: true, OnEvent: app.progress(s, "Restarting")}); err != nil && engine.ClassOf(err) != engine.ClassDependencyFailed {
						return readyFailure(ctx, inst, e.Named(inst.Project(), inst.Dir), err)
					}
					p, err := deploy.Load(ctx, inst, e)
					if err != nil {
						return engineFailure(err)
					}
					if err := p.RestartWatched(context.WithoutCancel(ctx), app.progress(s, "Restarting")); err != nil {
						return ui.Wrap(engineFailure(err), "Couldn't restart Fluxer.", "Run rexctl logs to see why.")
					}
					if err := waitForFluxer(ctx, inst, p, s, "Restarting"); err != nil {
						return err
					}
					if inst.State.Stopped {
						inst.State.Stopped = false
						if err := inst.Save(); err != nil {
							return ui.Wrap(err, "Couldn't save your instance's state.", "Run rexctl restart again.")
						}
					}
					s.Done("Restarted Fluxer.")
					return nil
				})
			})
		},
	}
}

func podmanSocketFix(kind engine.Kind) string {
	if runtime.GOOS != "linux" || (kind != "" && kind != engine.Podman) {
		return ""
	}
	if _, err := exec.LookPath("podman"); err != nil {
		return ""
	}
	if os.Geteuid() == 0 {
		return "Run systemctl enable --now podman.socket, then try again."
	}
	return "Run systemctl --user enable --now podman.socket, then try again."
}
