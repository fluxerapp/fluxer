// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"maps"
	"net"
	"net/netip"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const defaultProject = "fluxer"

type installFlags struct {
	Domain        string
	Engine        string
	BehindProxy   bool
	Bind          string
	VoiceIP       string
	MemoryProfile string
	InstallDocker bool
	OpenFirewall  bool
	SetupPodman   bool
	SetupWhere    string
	NoStart       bool
	DryRun        bool
	TakeOver      bool
	DeleteOld     bool
	Confirm       string
	Project       string
	VoiceTCPPort  int
	VoiceUDPPort  int
	Setup         setupFlags
	Storage       storageFlags
}

type installer struct {
	answers      *setup.Answers
	storage      *storageChoice
	app          *App
	c            *cobra.Command
	f            *installFlags
	dir          string
	host         *host.Info
	engine       *engine.Engine
	found        *engine.Engine
	findErr      error
	ran          map[string]bool
	plan         []string
	offered      bool
	lowPortsSaid bool
}

func newInstallCmd(app *App) *cobra.Command {
	var f installFlags
	c := &cobra.Command{
		Use:   "install",
		Short: "Install Fluxer on this machine, or take over an existing install",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runInstall(c.Context(), app, c, &f)
		},
	}
	fl := c.Flags()
	fl.StringVar(&f.Domain, "domain", "", "address people will use to open your instance, like chat.example.com")
	fl.StringVar(&f.Engine, "engine", "", "container engine to use (docker or podman)")
	fl.BoolVar(&f.BehindProxy, "behind-proxy", false, "serve plain HTTP for your own reverse proxy")
	fl.StringVar(&f.Bind, "bind", "127.0.0.1:8080", "address your proxy connects to, with --behind-proxy")
	fl.StringVar(&f.VoiceIP, "voice-ip", "", "public IP address for voice and video (found automatically)")
	fl.StringVar(&f.MemoryProfile, "memory-profile", "", "pin small, medium or large instead of sizing Fluxer to this machine on every start")
	fl.BoolVar(&f.InstallDocker, "install-docker", true, "install Docker if it's missing")
	fl.BoolVar(&f.OpenFirewall, "open-firewall", true, "open the ports Fluxer needs in ufw or firewalld")
	fl.BoolVar(&f.SetupPodman, "setup-podman", true, "start Podman's API socket and start Fluxer with the machine")
	fl.StringVar(&f.SetupWhere, "setup", "", "where to create your admin account (terminal or browser)")
	fl.BoolVar(&f.NoStart, "no-start", false, "write the files without starting Fluxer")
	fl.BoolVar(&f.DryRun, "dry-run", false, "show what would happen without changing anything")
	fl.BoolVar(&f.TakeOver, "take-over", false, "take over Fluxer data found on this machine")
	fl.BoolVar(&f.DeleteOld, "delete-old-data", false, "delete Fluxer data found on this machine and start fresh")
	fl.StringVar(&f.Project, "project", defaultProject, "name for this instance's containers and volumes")
	fl.IntVar(&f.VoiceTCPPort, "voice-tcp-port", 0, "TCP port for voice and video, for running more than one instance on a host")
	fl.IntVar(&f.VoiceUDPPort, "voice-udp-port", 0, "UDP port for voice and video, for running more than one instance on a host")
	_ = fl.MarkHidden("project")
	confirmFlag(c, &f.Confirm)
	f.Setup.bind(c)
	f.Storage.bind(c)
	return c
}

func runInstall(ctx context.Context, app *App, c *cobra.Command, f *installFlags) error {
	if err := validateInstallFlags(f); err != nil {
		return err
	}
	if err := f.Storage.validate(c); err != nil {
		return err
	}
	if err := oneStdinFlag(c); err != nil {
		return err
	}
	if names := f.Setup.passedNames(c); f.SetupWhere == "browser" && len(names) > 0 {
		return &ui.ExitError{Code: ui.ExitUsage, Msg: fmt.Sprintf("--setup browser doesn't take %s.", ui.JoinAnd(names)), Fix: "Pass --setup terminal instead, or leave out the setup options and enter them in the browser."}
	}
	l, err := app.locate()
	if err != nil {
		return err
	}
	dir := l.Dir
	switch {
	case l.Stale != "" && l.Chosen:
		dir = l.Stale
		app.located = &instance.Located{Dir: dir, From: instance.FromRecord, Scope: l.Scope}
		app.UI.Info("Installing Fluxer in %s, the folder you picked with rexctl use.", dir)
	case l.Stale != "":
		app.warnStale(l)
	}
	dir, err = filepath.Abs(dir)
	if err != nil {
		return err
	}
	in := &installer{app: app, c: c, f: f, dir: dir, ran: map[string]bool{}}

	inst, err := instance.Find(dir)
	switch {
	case errors.Is(err, instance.ErrLegacy):
		return in.takeOver(ctx, dir)
	case errors.Is(err, instance.ErrNotFound):
	case err != nil:
		return err
	case inst.State != nil && inst.State.SetupDone:
		return ui.Refused(fmt.Sprintf("Fluxer is already installed in %s.", dir), "Run rexctl status to check it, or rexctl upgrade to upgrade it.")
	default:
		return in.resume(ctx, inst)
	}

	if err := in.missingFlags(); err != nil {
		return err
	}
	app.UI.Welcome()
	if in.host, err = host.Detect(ctx, dir); err != nil {
		return err
	}
	in.findEngine(ctx)
	if in.runChecks(ctx, checks.Preinstall, "memory", "disk") {
		return notReady()
	}
	if err := in.ensureEngine(ctx); err != nil {
		return err
	}
	if in.engine != nil {
		if in.runChecks(ctx, checks.Preinstall, "engine", "podman-subids", "windows-linux-containers") {
			return notReady()
		}
	} else {
		in.ran["engine"], in.ran["podman-subids"], in.ran["windows-linux-containers"] = true, true, true
	}
	if err := in.offerProxy(); err != nil {
		return err
	}
	if in.engine != nil {
		if err := in.preparePodman(ctx); err != nil {
			return err
		}
	}
	if err := app.UI.Ask(c, ui.Field{
		Flag:     "domain",
		Title:    "What address will people use to open your instance?",
		Value:    &f.Domain,
		Validate: validateDomain,
	}); err != nil {
		return err
	}
	f.Domain = normaliseDomain(f.Domain)
	in.resolveVoicePorts(ctx)
	if in.runChecks(ctx, checks.Preinstall) {
		return notReady()
	}
	if err := in.openFirewall(ctx); err != nil {
		return err
	}
	if err := in.askStorage(); err != nil {
		return err
	}
	if err := in.checkStorage(ctx); err != nil {
		return err
	}
	if err := in.askSetupWhere(); err != nil {
		return err
	}
	if err := in.collectAnswers(f.Domain); err != nil {
		return err
	}
	deleteOld, err := in.checkOldData(ctx)
	if err != nil || deleteOld == nil {
		return err
	}
	if f.DryRun {
		return in.printPlan()
	}

	inst, err = instance.Create(dir, f.Project)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't create %s.", dir), "Check that you can write to it, or pass --dir.")
	}
	inst.State.Installing = true
	return app.Locked(ctx, inst, func() error {
		if err := in.write(ctx, inst, *deleteOld); err != nil {
			return err
		}
		if err := in.start(ctx, inst); err != nil {
			return err
		}
		in.proxyNote()
		return nil
	})
}

func (in *installer) missingFlags() error {
	if in.app.UI.Interactive() {
		return nil
	}
	f := in.f
	var missing []string
	if f.Domain == "" {
		missing = append(missing, "--domain")
	}
	missing = append(missing, f.Storage.missing(in.c)...)
	terminal := f.SetupWhere == "terminal" || (f.SetupWhere == "" && f.Setup.passed(in.c))
	if terminal && !f.NoStart {
		s := f.Setup
		if s.SignIn == "" {
			missing = append(missing, "--sign-in email or --sign-in username")
		}
		if s.SignIn == string(setup.SignInEmail) && s.Email == "" {
			missing = append(missing, "--admin-email")
		}
		if s.Username == "" {
			missing = append(missing, "--admin-username")
		}
		if s.PasswordFile == "" {
			missing = append(missing, "--admin-password-file")
		}
		if s.Name == "" {
			missing = append(missing, "--name")
		}
		if s.SignIn == string(setup.SignInEmail) && !s.SkipEmail && s.SMTPHost == "" {
			missing = append(missing, "--smtp-host or --skip-email")
		}
	}
	if len(missing) == 0 {
		return nil
	}
	return ui.Usage("Pass " + ui.JoinAnd(missing) + ".")
}

func notReady() error {
	return ui.Refused("This machine isn't ready for Fluxer yet.", "Fix the problems above, then run rexctl install again.")
}

func validateInstallFlags(f *installFlags) error {
	if f.Engine != "" && f.Engine != string(engine.Docker) && f.Engine != string(engine.Podman) {
		return ui.Usage("Pass --engine docker or --engine podman.")
	}
	switch stack.MemoryProfile(f.MemoryProfile) {
	case "", stack.ProfileSmall, stack.ProfileMedium, stack.ProfileLarge:
	default:
		return ui.Usage("Pass --memory-profile small, medium or large.")
	}
	if f.SetupWhere != "" && f.SetupWhere != "terminal" && f.SetupWhere != "browser" {
		return ui.Usage("Pass --setup terminal or --setup browser.")
	}
	if f.VoiceIP != "" {
		if _, err := netip.ParseAddr(f.VoiceIP); err != nil {
			return ui.Usage(fmt.Sprintf("%s isn't an IP address.", f.VoiceIP))
		}
	}
	if f.BehindProxy && validateBind(f.Bind) != nil {
		return ui.Usage("Pass --bind as an address and port, like 127.0.0.1:8080.")
	}
	for _, p := range []int{f.VoiceTCPPort, f.VoiceUDPPort} {
		if p < 0 || p > 65535 {
			return ui.Usage("Pass a voice port from 1 to 65535.")
		}
	}
	if !projectPattern.MatchString(f.Project) {
		return ui.Usage("Pass --project with lowercase letters, numbers, dashes or underscores.")
	}
	if err := validTLS(f.Setup.SMTPTLS, "smtp-tls"); err != nil {
		return err
	}
	if f.TakeOver && f.DeleteOld {
		return ui.Usage("Pass --take-over or --delete-old-data, not both.")
	}
	if f.Domain != "" {
		if err := validateDomain(f.Domain); err != nil {
			return ui.Usage(err.Error())
		}
	}
	return nil
}

func validateBind(v string) error {
	if _, _, err := net.SplitHostPort(v); err != nil {
		return setupInputError("Enter an address and port, like 127.0.0.1:8080.")
	}
	return nil
}

var projectPattern = regexp.MustCompile(`^[a-z0-9][a-z0-9_-]*$`)

var labelPattern = regexp.MustCompile(`^[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$`)

func normaliseDomain(v string) string {
	v = strings.ToLower(strings.TrimSpace(v))
	v = strings.TrimPrefix(v, "https://")
	v = strings.TrimPrefix(v, "http://")
	return strings.TrimSuffix(v, "/")
}

func validateDomain(v string) error {
	d := normaliseDomain(v)
	if _, err := netip.ParseAddr(d); err == nil {
		return setupInputError("Enter a domain name, like chat.example.com. An IP address won't get a certificate.")
	}
	labels := strings.Split(d, ".")
	if len(d) > 253 || len(labels) < 2 {
		return setupInputError("Enter a domain name, like chat.example.com.")
	}
	for _, l := range labels {
		if !labelPattern.MatchString(l) {
			return setupInputError("Enter a domain name, like chat.example.com.")
		}
	}
	return nil
}

func (in *installer) runChecks(ctx context.Context, phase checks.Phase, ids ...string) bool {
	env := &checks.Env{Dir: in.dir, Domain: normaliseDomain(in.f.Domain), Engine: in.engine, Host: in.host, Behind: in.f.BehindProxy, Bind: in.f.Bind, Settings: in.settings()}
	if in.engine == nil {
		env.SetEngine(in.found, in.findErr)
	}
	list := ids
	if len(list) == 0 {
		for _, c := range checks.All() {
			list = append(list, c.ID)
		}
	}
	failed := false
	for _, id := range list {
		if in.ran[id] {
			continue
		}
		c, ok := checks.Get(id)
		if !ok || c.Phases&phase == 0 {
			continue
		}
		in.ran[id] = true
		r := checks.RunIn(ctx, env, checks.Preinstall, c)
		in.printResult(r)
		failed = failed || r.Status == checks.Fail
	}
	return failed
}

func (in *installer) printResult(r checks.Result) {
	if r.Message == "" {
		return
	}
	switch r.Status {
	case checks.Pass:
		in.app.UI.OK("%s", r.Message)
	case checks.Warn:
		in.app.UI.Warn(r.Message, r.Fix)
	case checks.Fail:
		in.app.UI.Fail(r.Message, r.Fix)
	}
}

func (in *installer) consent(flag string, value bool, question string) (bool, error) {
	if fl := in.c.Flags().Lookup(flag); fl != nil && fl.Changed {
		return value, nil
	}
	if in.app.UI.Interactive() {
		return in.app.UI.Confirm(question, true)
	}
	if in.app.UI.Yes() {
		return value, nil
	}
	return false, ui.MissingFlag(flag)
}

func (in *installer) findEngine(ctx context.Context) {
	want := engine.Kind(in.f.Engine)
	in.findErr = in.app.UI.Step(ctx, "Checking "+want.Name(), func(ctx context.Context, _ *ui.Step) error {
		e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: want, AllowOld: true})
		if err != nil {
			return err
		}
		if want != "" && e.Kind != want {
			e.Close()
			return &engine.Error{Op: "connect", Class: engine.ClassUnreachable, Err: fmt.Errorf("%s does not answer", want)}
		}
		in.found = e
		return nil
	})
	if in.findErr != nil {
		in.found = nil
	}
}

func (in *installer) ensureEngine(ctx context.Context) error {
	e, err := in.found, in.findErr
	if err == nil {
		if e.TooOld {
			return in.offerDocker(ctx, e)
		}
		in.engine = e
		return nil
	}
	if engine.ClassOf(err) != engine.ClassUnreachable {
		return engineFailureFor(engine.Kind(in.f.Engine), err)
	}
	if runtime.GOOS != "linux" && in.f.Engine == string(engine.Podman) {
		return ui.Refused("rexctl runs Fluxer on Podman only on Linux.", "Start Docker Desktop, then run rexctl install without --engine podman.")
	}
	_, podmanErr := exec.LookPath("podman")
	docker := dockerInstalled()
	switch {
	case podmanErr == nil && runtime.GOOS == "linux" && in.f.Engine != string(engine.Docker) && (!docker || in.f.Engine == string(engine.Podman)):
		return in.startPodman(ctx)
	case podmanErr != nil && !docker:
		return in.installDocker(ctx)
	}
	return engineFailureFor(engine.Kind(in.f.Engine), err)
}

func dockerInstalled() bool {
	if path, err := exec.LookPath("docker"); err == nil {
		return !podmanDockerWrapper(path)
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

func podmanDockerWrapper(path string) bool {
	if runtime.GOOS != "linux" {
		return false
	}
	if target, err := filepath.EvalSymlinks(path); err == nil && filepath.Base(target) == "podman" {
		return true
	}
	f, err := os.Open(path)
	if err != nil {
		return false
	}
	defer f.Close()
	head := make([]byte, 512)
	n, _ := io.ReadFull(f, head)
	return bytes.HasPrefix(head[:n], []byte("#!")) && bytes.Contains(head[:n], []byte("podman"))
}

func (in *installer) startPodman(ctx context.Context) error {
	rootless := os.Geteuid() != 0
	if runtime.GOOS == "linux" && rootless && !host.HasSubIDs(ctx) {
		return ui.Refused("Your account has no subordinate user IDs, and Podman needs them to unpack Fluxer.", "Add your account to /etc/subuid and /etc/subgid, run podman system migrate, then run rexctl install again.")
	}
	s := host.CheckPodman(ctx, rootless, true)
	if !s.Systemd {
		return ui.Refused("Podman's API socket isn't running.", "Start podman.socket, then run rexctl install again.")
	}
	ok, err := in.consent("setup-podman", in.f.SetupPodman, "Start Podman's API socket, which Fluxer needs?")
	if err != nil {
		return err
	}
	if !ok {
		return ui.Refused("Fluxer needs Podman's API socket.", "Start podman.socket, then run rexctl install again.")
	}
	if in.f.DryRun {
		in.plan = append(in.plan, "Start Podman's API socket.")
		return nil
	}
	if err := host.FixPodman(ctx, s); err != nil {
		return ui.Wrap(err, "Couldn't start Podman's API socket.", "Run rexctl install --verbose to see why.")
	}
	e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: engine.Podman, AllowOld: true})
	if err != nil {
		return engineFailureFor(engine.Podman, err)
	}
	if e.TooOld {
		return in.offerDocker(ctx, e)
	}
	in.engine = e
	in.app.UI.OK("Started Podman's API socket.")
	return nil
}

func (in *installer) offerDocker(ctx context.Context, e *engine.Engine) error {
	need := strings.TrimSuffix(engine.MinPodman, ".0")
	msg := fmt.Sprintf("Podman %s is too old for Fluxer, which needs %s or newer.", e.Version, need)
	if in.f.Engine == string(engine.Podman) || !host.CanInstallDocker(in.host) {
		return ui.Refused(msg, fmt.Sprintf("Upgrade Podman to %s or newer, or install Docker, then run rexctl install again.", need))
	}
	in.app.UI.Warn(msg, "")
	ok, err := in.consent("install-docker", in.f.InstallDocker, "Install Docker and run Fluxer on it instead?")
	if err != nil {
		return err
	}
	if !ok {
		return ui.Refused(msg, fmt.Sprintf("Upgrade Podman to %s or newer, or install Docker, then run rexctl install again.", need))
	}
	e.Close()
	return in.dockerInstall(ctx)
}

func (in *installer) preparePodman(ctx context.Context) error {
	if in.engine.Kind != engine.Podman {
		return nil
	}
	if err := in.setupPodman(ctx); err != nil {
		return err
	}
	return in.allowLowPorts(ctx)
}

func (in *installer) setupPodman(ctx context.Context) error {
	s := host.CheckPodman(ctx, in.engine.Rootless, false)
	if !s.Needed() {
		return nil
	}
	ok, err := in.consent("setup-podman", in.f.SetupPodman, "Start Fluxer with this machine and turn off Podman's health check logging?")
	if err != nil || !ok {
		return err
	}
	if in.f.DryRun {
		in.plan = append(in.plan, "Set up Podman for Fluxer.")
		return nil
	}
	if err := host.FixPodman(ctx, s); err != nil {
		return ui.Wrap(err, "Couldn't set up Podman for Fluxer.", "Run rexctl install --verbose to see why.")
	}
	in.app.UI.OK("Set up Podman for Fluxer.")
	return nil
}

func (in *installer) allowLowPorts(ctx context.Context) error {
	if in.f.BehindProxy || !host.LowPortsBlocked(in.engine.Rootless) {
		return nil
	}
	if !in.lowPortsSaid {
		in.app.UI.Info("Rootless Podman needs this to serve Fluxer on ports 80 and 443.")
	}
	ok, err := in.consent("setup-podman", in.f.SetupPodman, "Let programs that don't run as root use ports 80 and up on this machine?")
	if err != nil {
		return err
	}
	if !ok {
		return ui.Refused("Fluxer can't serve HTTPS from rootless Podman without ports 80 and 443.", "Run rexctl install --behind-proxy to use your own reverse proxy instead.")
	}
	if in.f.DryRun {
		in.plan = append(in.plan, "Let programs that don't run as root use ports 80 and up.")
		return nil
	}
	if err := host.AllowLowPorts(ctx); err != nil {
		return ui.Wrap(err, "Couldn't open ports 80 and up to programs that don't run as root.", "Run rexctl install --verbose to see why.")
	}
	in.app.UI.OK("Programs that don't run as root can use ports 80 and up.")
	return nil
}

type proxyHint struct {
	Interactive bool
	Decided     bool
	Busy        []int
	Owner       string
	LowPorts    string
}

func (h proxyHint) offer() (reason string, def bool, ok bool) {
	if !h.Interactive || h.Decided {
		return "", false, false
	}
	if len(h.Busy) > 0 {
		nums := make([]string, len(h.Busy))
		for i, n := range h.Busy {
			nums[i] = strconv.Itoa(n)
		}
		subject := fmt.Sprintf("Port %s on this machine is", nums[0])
		if len(nums) > 1 {
			subject = fmt.Sprintf("Ports %s on this machine are", ui.JoinAnd(nums))
		}
		switch {
		case checks.ContainerOwner(h.Owner):
			return subject + " already in use by a container.", true, true
		case h.Owner != "":
			return fmt.Sprintf("%s already in use by %s.", subject, h.Owner), true, true
		}
		return subject + " already in use.", true, true
	}
	if h.LowPorts != "" {
		return fmt.Sprintf("Rootless %s can't use ports 80 and 443 without a change to this machine.", h.LowPorts), false, true
	}
	return "", false, false
}

func (in *installer) proxyHint() proxyHint {
	h := proxyHint{
		Interactive: in.app.UI.Interactive(),
		Decided:     in.f.BehindProxy || in.c.Flags().Changed("behind-proxy") || in.c.Flags().Changed("bind"),
	}
	if !h.Interactive || h.Decided {
		return h
	}
	sameOwner := true
	for _, port := range []int{80, 443} {
		used, owner, err := host.PortInUse("tcp", port)
		if err != nil || !used {
			continue
		}
		if len(h.Busy) == 0 {
			h.Owner = owner
		} else if owner != h.Owner {
			sameOwner = false
		}
		h.Busy = append(h.Busy, port)
	}
	if !sameOwner {
		h.Owner = ""
	}
	if in.engine != nil && host.LowPortsBlocked(in.engine.Rootless) {
		h.LowPorts = in.engine.Kind.Name()
	}
	return h
}

func (in *installer) offerProxy() error {
	h := in.proxyHint()
	reason, def, ok := h.offer()
	if !ok {
		return nil
	}
	in.offered, in.lowPortsSaid = true, len(h.Busy) == 0
	in.app.UI.Info("%s", reason)
	yes, err := in.app.UI.Confirm("Use your own reverse proxy in front of Fluxer?", def)
	if err != nil || !yes {
		return err
	}
	if err := in.app.UI.Ask(in.c, ui.Field{
		Flag:     "bind",
		Title:    "Which address should your proxy connect to?",
		Value:    &in.f.Bind,
		Validate: validateBind,
	}); err != nil {
		return err
	}
	in.f.BehindProxy = true
	in.plan = append(in.plan, fmt.Sprintf("Serve Fluxer over plain HTTP on %s for your reverse proxy.", in.f.Bind))
	return nil
}

func (in *installer) proxyNote() {
	if !in.offered || !in.f.BehindProxy {
		return
	}
	in.app.UI.Info("Set up your proxy to forward https://%s to http://%s.", normaliseDomain(in.f.Domain), in.f.Bind)
	in.app.UI.Info("See %sreverse-proxy/ for what your proxy needs to do.", docsURL)
}

func (in *installer) installDocker(ctx context.Context) error {
	if in.f.Engine == string(engine.Podman) {
		return ui.Refused("Podman isn't installed.", "Install Podman, then run rexctl install again.")
	}
	if !host.CanInstallDocker(in.host) {
		if in.host.OS == "linux" && !in.host.WSL {
			return ui.Refused("Docker isn't installed.", "Install Docker from https://docs.docker.com/engine/install/, then run rexctl install again.")
		}
		return ui.Refused("Docker isn't installed.", "Install Docker Desktop from https://docs.docker.com/desktop/, then run rexctl install again.")
	}
	in.app.UI.Warn("Docker isn't installed.", "")
	ok, err := in.consent("install-docker", in.f.InstallDocker, "Install Docker?")
	if err != nil {
		return err
	}
	if !ok {
		return ui.Refused("Fluxer needs Docker.", "Install Docker, then run rexctl install again.")
	}
	return in.dockerInstall(ctx)
}

func (in *installer) dockerInstall(ctx context.Context) error {
	if in.f.DryRun {
		in.plan = append(in.plan, "Install Docker.")
		return nil
	}
	err := in.app.UI.Step(ctx, "Installing Docker", func(ctx context.Context, s *ui.Step) error {
		if err := host.InstallDocker(ctx, in.host, s.Log); err != nil {
			return err
		}
		e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: engine.Docker})
		if err != nil {
			return err
		}
		in.engine = e
		s.Done("Installed Docker %s.", e.Version)
		return nil
	})
	switch {
	case err == nil:
		return nil
	case errors.Is(err, host.ErrRelogin) || engine.ClassOf(err) == engine.ClassDenied:
		return ui.Refused("Docker is installed, and your account needs a new login to use it.", "Log out and back in, then run rexctl install again.")
	case errors.Is(err, context.Canceled):
		return err
	}
	return ui.Wrap(err, "Couldn't install Docker.", "Install Docker from https://docs.docker.com/engine/install/, then run rexctl install again.")
}

func (in *installer) settings() map[string]string {
	out := map[string]string{}
	if in.f.VoiceTCPPort > 0 {
		out[stack.VoiceTCPPortKey] = strconv.Itoa(in.f.VoiceTCPPort)
	}
	if in.f.VoiceUDPPort > 0 {
		out[stack.VoiceUDPPortKey] = strconv.Itoa(in.f.VoiceUDPPort)
	}
	return out
}

func (in *installer) resolveVoicePorts(ctx context.Context) {
	tcpSet := in.f.VoiceTCPPort > 0
	udpSet := in.f.VoiceUDPPort > 0
	if tcpSet && udpSet {
		return
	}
	pub := in.publishedPorts(ctx)
	free := func(number int, proto string) bool {
		if number < 1 || number > 65535 {
			return false
		}
		used, _, err := host.PortInUse(proto, number)
		if err != nil || used {
			return false
		}
		return !pub[host.Port{Number: number, Proto: proto}]
	}
	switch {
	case !tcpSet && !udpSet:
		tcp, udp := pickVoicePair(free)
		if tcp == 0 || tcp == 7881 {
			return
		}
		in.f.VoiceTCPPort, in.f.VoiceUDPPort = tcp, udp
		in.announceVoicePorts(tcp, udp)
	case !tcpSet:
		for tcp := 7881; tcp <= 65535; tcp += 2 {
			if tcp != in.f.VoiceUDPPort && free(tcp, "tcp") {
				in.f.VoiceTCPPort = tcp
				return
			}
		}
	case !udpSet:
		for udp := 7882; udp <= 65535; udp += 2 {
			if udp != in.f.VoiceTCPPort && free(udp, "udp") {
				in.f.VoiceUDPPort = udp
				return
			}
		}
	}
}

func pickVoicePair(free func(number int, proto string) bool) (int, int) {
	for tcp, udp := 7881, 7882; tcp <= 65533 && udp <= 65534; tcp, udp = tcp+2, udp+2 {
		if free(tcp, "tcp") && free(udp, "udp") {
			return tcp, udp
		}
	}
	return 0, 0
}

func (in *installer) announceVoicePorts(tcp, udp int) {
	msg := fmt.Sprintf("Ports 7881 and 7882 are already in use, so this instance will use %d and %d for voice and video.", tcp, udp)
	in.plan = append(in.plan, msg)
	if !in.f.DryRun {
		in.app.UI.Info("%s", msg)
	}
}

func (in *installer) publishedPorts(ctx context.Context) map[host.Port]bool {
	out := map[host.Port]bool{}
	e := in.engine
	if e == nil {
		e = in.found
	}
	if e == nil {
		return out
	}
	owners, err := e.PublishedHostPorts(ctx)
	if err != nil {
		return out
	}
	for _, o := range owners {
		out[host.Port{Number: o.Number, Proto: o.Proto}] = true
	}
	return out
}

func (in *installer) firewallPorts() []host.Port {
	var out []host.Port
	for _, p := range stack.Ports(in.settings()) {
		if in.f.BehindProxy && (p.Number == 80 || p.Number == 443) {
			continue
		}
		out = append(out, host.Port{Number: p.Number, Proto: p.Proto})
	}
	return out
}

func portList(ports []host.Port) string {
	var nums []int
	for _, p := range ports {
		if !slices.Contains(nums, p.Number) {
			nums = append(nums, p.Number)
		}
	}
	slices.Sort(nums)
	parts := make([]string, len(nums))
	for i, n := range nums {
		parts[i] = strconv.Itoa(n)
	}
	if len(parts) == 1 {
		return parts[0]
	}
	return strings.Join(parts[:len(parts)-1], ", ") + " and " + parts[len(parts)-1]
}

func (in *installer) openFirewall(ctx context.Context) error {
	fw, err := host.DetectFirewall(ctx)
	if err != nil || fw == host.FirewallNone {
		return nil
	}
	closed, err := host.ClosedPorts(ctx, fw, in.firewallPorts())
	if err != nil || len(closed) == 0 {
		return nil
	}
	ok, err := in.consent("open-firewall", in.f.OpenFirewall, fmt.Sprintf("Open ports %s in the firewall on this machine?", portList(closed)))
	if err != nil {
		return err
	}
	if !ok {
		in.app.UI.Warn(fmt.Sprintf("Ports %s are closed in the firewall on this machine.", portList(closed)), "Open them before people connect.")
		return nil
	}
	if in.f.DryRun {
		in.plan = append(in.plan, fmt.Sprintf("Open ports %s in the firewall.", portList(closed)))
		return nil
	}
	if err := host.OpenPorts(ctx, fw, closed); err != nil {
		return ui.Wrap(err, "Couldn't open the ports.", fmt.Sprintf("Open ports %s in the firewall yourself, then run rexctl install again.", portList(closed)))
	}
	in.app.UI.OK("Opened the ports.")
	return nil
}

func (in *installer) askSetupWhere() error {
	def := "browser"
	if in.f.Setup.passed(in.c) {
		def = "terminal"
	}
	return in.app.UI.Ask(in.c, ui.Field{
		Flag:    "setup",
		Title:   "Where do you want to create your admin account?",
		Value:   &in.f.SetupWhere,
		Default: def,
		Options: []ui.Option{
			{Label: "Here in the terminal", Value: "terminal"},
			{Label: "In my browser", Value: "browser"},
		},
	})
}

func (in *installer) collectAnswers(domain string) error {
	if in.f.SetupWhere != "terminal" || in.app.UI.Interactive() || in.f.NoStart {
		return nil
	}
	a, err := askSetup(in.app, in.c, domain, &in.f.Setup)
	if err != nil {
		return err
	}
	in.answers = &a
	return nil
}

func (in *installer) checkOldData(ctx context.Context) (*bool, error) {
	no := false
	if in.engine == nil {
		return &no, nil
	}
	vols, err := in.engine.Volumes(ctx, in.f.Project)
	if err != nil {
		return nil, ui.Wrap(err, "Couldn't check for Fluxer data on this machine.", "Run rexctl install --verbose to see why.")
	}
	if len(vols) == 0 {
		return &no, nil
	}
	legacy := in.findLegacy(ctx)
	choice := ""
	switch {
	case in.f.TakeOver:
		choice = "take-over"
	case in.f.DeleteOld:
		choice = "delete"
	case in.app.UI.Interactive():
		kept := ""
		if dir := in.keptDir(); dir != "" {
			kept = fmt.Sprintf("It belongs to the instance in %s. Run rexctl start --dir %s to start it again.", dir, dir)
		}
		in.app.UI.Warn("Fluxer data from an earlier install is on this machine.", kept)
		opts := []ui.Option{{Label: "Delete it and start fresh", Value: "delete"}, {Label: "Stop here", Value: "stop"}}
		if legacy != nil {
			opts = append([]ui.Option{{Label: "Keep it and take over that install", Value: "take-over"}}, opts...)
		}
		if err := in.app.UI.Ask(nil, ui.Field{Title: "What do you want to do with it?", Value: &choice, Options: opts}); err != nil {
			return nil, err
		}
	default:
		fix := in.keptFix()
		if legacy != nil {
			fix = fmt.Sprintf("Run rexctl install --take-over --dir %s to take it over, or pass --delete-old-data to start fresh.", legacy.Dir)
		}
		return nil, ui.Refused("Fluxer data from an earlier install is on this machine.", fix)
	}
	switch choice {
	case "take-over":
		if legacy == nil {
			return nil, ui.Refused("Couldn't find the settings that belong to the Fluxer data on this machine.", in.keptFix())
		}
		return nil, in.takeOver(ctx, legacy.Dir)
	case "delete":
		if err := in.app.UI.ConfirmDestructive("This deletes every account, message and file in the old data.", normaliseDomain(in.f.Domain), in.f.Confirm); err != nil {
			return nil, err
		}
		if in.f.DryRun {
			in.plan = append(in.plan, "Delete the Fluxer data from the earlier install.")
		}
		yes := true
		return &yes, nil
	}
	return nil, ui.Refused("Stopped without changing anything.", "")
}

func (in *installer) keptFix() string {
	if dir := in.keptDir(); dir != "" {
		return fmt.Sprintf("It belongs to the instance in %s. Run rexctl start --dir %s to start it again, or pass --delete-old-data to delete the data and start fresh.", dir, dir)
	}
	return "If you removed Fluxer with rexctl uninstall --keep-data, run rexctl start --dir with the folder it was in. To delete the data and start fresh, pass --delete-old-data."
}

func (in *installer) keptDir() string {
	var dirs []string
	if d, err := instance.DefaultDir(); err == nil {
		dirs = append(dirs, d)
	}
	if h, err := os.UserHomeDir(); err == nil {
		dirs = append(dirs, filepath.Join(h, "fluxer"))
	}
	if wd, err := os.Getwd(); err == nil {
		dirs = append(dirs, wd)
	}
	own, _ := filepath.Abs(in.dir)
	for _, d := range dirs {
		if abs, _ := filepath.Abs(d); abs == own {
			continue
		}
		if inst, err := instance.Find(d); err == nil && inst.Project() == in.f.Project {
			return d
		}
	}
	return ""
}

func (in *installer) findLegacy(ctx context.Context) *instance.Legacy {
	projects, err := in.engine.Projects(ctx)
	if err != nil {
		return nil
	}
	for _, p := range projects {
		if p.Name != in.f.Project || p.ConfigFiles == "" {
			continue
		}
		first := strings.Split(p.ConfigFiles, ",")[0]
		if l, err := instance.DetectLegacy(stack.ProjectDir(first)); err == nil && l != nil {
			return l
		}
	}
	return nil
}

func (in *installer) takeOver(ctx context.Context, dir string) error {
	if in.f.Storage.passed(in.c) {
		return &ui.ExitError{Code: ui.ExitUsage, Msg: "Taking over an install doesn't use the storage options.", Fix: "Leave them out, then change the object store after the take-over with rexctl config set."}
	}
	legacy, err := instance.DetectLegacy(dir)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't read the Fluxer install in %s.", dir), "Run rexctl install --verbose to see why.")
	}
	if !in.f.TakeOver {
		in.app.UI.Info("Fluxer is already installed in %s.", dir)
		ok, err := in.consent("take-over", in.f.TakeOver || !in.c.Flags().Changed("take-over"), "Take it over with rexctl?")
		if err != nil {
			return err
		}
		if !ok {
			return ui.Refused("Stopped without changing anything.", "")
		}
	}
	inst, _, err := takeoverDir(ctx, in.app, legacy.Dir, takeoverOptions{DryRun: in.f.DryRun})
	if err != nil || in.f.DryRun || inst == nil {
		return err
	}
	in.app.UI.OK("rexctl looks after your instance in %s.", inst.Dir)
	return nil
}

func (in *installer) printPlan() error {
	u := in.app.UI
	domain := normaliseDomain(in.f.Domain)
	u.Info("rexctl would do this:")
	steps := append([]string{}, in.plan...)
	steps = append(steps, fmt.Sprintf("Save Fluxer %s and its settings in %s.", stack.Embedded().Release, in.dir))
	if in.f.NoStart {
		steps = append(steps, "Leave Fluxer stopped.")
	} else {
		steps = append(steps, fmt.Sprintf("Download Fluxer and start it at https://%s.", domain))
		if in.answers != nil {
			steps = append(steps, fmt.Sprintf("Create the admin account %s and finish setup.", in.answers.Username))
		} else if in.f.SetupWhere == "terminal" {
			steps = append(steps, "Ask you for the admin account details and finish setup here.")
		} else {
			steps = append(steps, "Show the setup token for your browser.")
		}
	}
	for _, s := range steps {
		u.Info("  %s", s)
	}
	return nil
}

func (in *installer) write(ctx context.Context, inst *instance.Instance, deleteOld bool) error {
	f := in.f
	st := inst.State
	set := stack.Embedded()
	images, err := stack.EmbeddedImages()
	if err != nil {
		return ui.Wrap(err, "This copy of rexctl has no Fluxer release in it.", "Download rexctl again.")
	}
	st.Settings[settings.Domain] = normaliseDomain(f.Domain)
	if f.BehindProxy {
		st.Settings[settings.EdgeMode] = settings.EdgeProxy
		st.Settings[settings.EdgeBind] = f.Bind
	}
	if f.VoiceTCPPort > 0 {
		st.Settings[settings.VoiceTCPPort] = strconv.Itoa(f.VoiceTCPPort)
	}
	if f.VoiceUDPPort > 0 {
		st.Settings[settings.VoiceUDPPort] = strconv.Itoa(f.VoiceUDPPort)
	}
	if f.VoiceIP != "" {
		st.Settings[settings.VoicePublicIP] = f.VoiceIP
	}
	if f.MemoryProfile != "" {
		st.Settings[settings.MemoryProfile] = f.MemoryProfile
	}
	if in.storage != nil {
		maps.Copy(st.Settings, in.storage.values())
	}
	if _, err := secrets.Mint(st.Secrets, false); err != nil {
		return ui.Wrap(err, "Couldn't create your instance's secrets.", "Run rexctl install again.")
	}
	if _, err := setup.EnsureToken(st.Secrets); err != nil {
		return ui.Wrap(err, "Couldn't create a setup token.", "Run rexctl install again.")
	}
	st.Engine = instance.EngineRef{Kind: string(in.engine.Kind), Host: in.engine.Host, ID: in.engine.ID}
	st.Release = set.Release
	st.Images = instance.Images{Release: set.Release, Refs: maps.Clone(images.Images)}
	if st.Images.Refs == nil {
		st.Images.Refs = map[string]string{}
	}
	if err := release.WriteRelease(inst, set); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't write Fluxer's files to %s.", inst.Dir), "Check the free disk space, then run rexctl install again.")
	}
	r, err := inst.Render(set, stack.OverlaysFor(st.Settings), st.Images.Refs)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't write Fluxer's files to %s.", inst.Dir), "Check the free disk space, then run rexctl install again.")
	}
	if err := inst.WriteCustomTemplate(); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't write %s to %s.", instance.CustomCompose, inst.Dir), "Check the free disk space, then run rexctl install again.")
	}
	st.Rendered = r.Hashes
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then run rexctl install again.")
	}
	if deleteOld {
		p := in.engine.Named(inst.Project(), inst.Dir)
		err := in.app.UI.Step(ctx, "Deleting the old Fluxer data", func(ctx context.Context, s *ui.Step) error {
			if err := p.Down(context.WithoutCancel(ctx), true); err != nil {
				return err
			}
			s.Done("Deleted the old Fluxer data.")
			return nil
		})
		if err != nil {
			return ui.Wrap(engineFailure(err), "Couldn't delete the old Fluxer data.", "Run rexctl install again.")
		}
	}
	return nil
}

func (in *installer) resume(ctx context.Context, inst *instance.Instance) error {
	if err := rootlessGuard(inst); err != nil {
		return err
	}
	in.app.UI.Info("Continuing the install in %s.", inst.Dir)
	e, err := in.app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	in.engine = e
	in.f.BehindProxy = inst.State.Settings[settings.EdgeMode] == settings.EdgeProxy || inst.State.Settings[settings.EdgeMode] == settings.EdgeTunnel
	if in.f.DryRun {
		if in.f.Storage.passed(in.c) {
			return &ui.ExitError{Code: ui.ExitUsage, Msg: "--dry-run doesn't use the storage options when it continues an unfinished install.", Fix: "Leave out --dry-run to check and save them, or leave out the storage options."}
		}
		in.app.UI.Info("rexctl would start Fluxer at %s and finish setup.", inst.Origin())
		return nil
	}
	if err := in.resumeStorage(ctx, inst); err != nil {
		return err
	}
	if err := in.askSetupWhere(); err != nil {
		return err
	}
	if err := in.collectAnswers(inst.Domain()); err != nil {
		return err
	}
	return in.app.Locked(ctx, inst, func() error { return in.start(ctx, inst) })
}

func (in *installer) start(ctx context.Context, inst *instance.Instance) error {
	u := in.app.UI
	in.app.remember(inst.Dir)
	origin := inst.Origin()
	token := setup.Token(inst.State.Secrets)
	if token == "" && !inst.State.SetupDone {
		t, err := setup.EnsureToken(inst.State.Secrets)
		if err != nil {
			return err
		}
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then run rexctl install again.")
		}
		token = t
	}
	if in.f.NoStart {
		inst.State.Stopped = true
		inst.State.Installing = false
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then run rexctl install again.")
		}
		in.installBootUnit(ctx, inst)
		u.OK("Saved Fluxer and its settings in %s.", inst.Dir)
		u.Info("Run rexctl start to start it.")
		if in.f.SetupWhere != "terminal" {
			u.Info("Then open %s and enter this setup token to create your admin account.", origin)
			u.Highlight(token)
		} else {
			u.Info("Then run rexctl setup to create your admin account.")
		}
		return nil
	}
	p := in.engine.Named(inst.Project(), inst.Dir)
	var err error
	if in.alreadyRunning(ctx, inst) {
		u.OK("Fluxer is already running.")
	} else {
		err = in.startFluxer(ctx, inst, p)
	}
	if err != nil {
		return err
	}
	if inst.State.Stopped || inst.State.Installing {
		inst.State.Stopped = false
		inst.State.Installing = false
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then run rexctl install again.")
		}
	}
	in.installBootUnit(ctx, inst)
	if !in.f.BehindProxy {
		in.waitCertificate(ctx, inst, p)
	}

	client := in.app.adminClient(inst, p)
	if done, err := setup.Done(ctx, client); err == nil && done {
		inst.State.SetupDone = true
		if err := inst.Save(); err != nil {
			return err
		}
		in.app.UI.Finish(origin)
		return nil
	}
	if in.f.SetupWhere != "terminal" {
		u.Info("Your instance is ready at %s.", origin)
		u.Info("Open it and enter this setup token to create your admin account.")
		u.Highlight(token)
		return nil
	}
	adminID, err := runSetup(ctx, in.app, in.c, inst, p, &in.f.Setup, in.answers)
	if err != nil {
		return err
	}
	in.app.UI.Finish(origin)
	signInAfterSetup(ctx, in.app, inst, p, adminID)
	return nil
}

func (in *installer) installBootUnit(ctx context.Context, inst *instance.Instance) {
	if !host.BootUnitSupported() {
		return
	}
	var p *engine.Project
	if loaded, err := deploy.Load(ctx, inst, in.engine); err == nil {
		p = loaded
	}
	if err := installBootUnit(ctx, inst, in.engine, p); err != nil {
		in.app.UI.Detail("%v", err)
		in.app.UI.Warn("Fluxer won't start by itself when this machine restarts.", "Run rexctl doctor --fix to set that up.")
	}
}

func installBootUnit(ctx context.Context, inst *instance.Instance, e *engine.Engine, p *engine.Project) error {
	u, err := checks.BootUnitFor(inst, e, p)
	if err != nil {
		return err
	}
	return host.InstallBootUnit(ctx, u)
}

func refreshBootUnit(ctx context.Context, inst *instance.Instance, e *engine.Engine, p *engine.Project) error {
	u, err := checks.BootUnitFor(inst, e, p)
	if err != nil {
		return err
	}
	return host.RefreshBootUnit(ctx, u)
}

func (in *installer) waitCertificate(ctx context.Context, inst *instance.Instance, p *engine.Project) {
	c, ok := checks.Get("certificate")
	if !ok {
		return
	}
	env := &checks.Env{Dir: inst.Dir, Domain: inst.Domain(), Instance: inst, Engine: in.engine, Project: p, Host: in.host}
	last := checks.RunIn(ctx, env, checks.Installed, c)
	if last.Status == checks.Skip {
		return
	}
	_ = in.app.UI.Step(ctx, fmt.Sprintf("Getting a certificate for %s", inst.Domain()), func(ctx context.Context, s *ui.Step) error {
		deadline := time.Now().Add(3 * time.Minute)
		for {
			last = checks.RunIn(ctx, env, checks.Installed, c)
			if last.Status == checks.Skip {
				return nil
			}
			if last.Status == checks.Pass {
				s.Done("Got a certificate for %s.", inst.Domain())
				return nil
			}
			if time.Now().After(deadline) {
				return nil
			}
			select {
			case <-ctx.Done():
				return ctx.Err()
			case <-time.After(5 * time.Second):
			}
		}
	})
	if last.Status != checks.Pass && last.Message != "" {
		in.app.UI.Warn(last.Message, last.Fix)
	}
}

func (in *installer) startFluxer(ctx context.Context, inst *instance.Instance, p *engine.Project) error {
	return in.app.UI.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
		if _, err := deploy.Apply(ctx, inst, in.engine, deploy.ApplyOptions{Pull: true, OnEvent: in.app.progress(s, "Starting")}); err != nil {
			return readyFailure(ctx, inst, p, err)
		}
		if err := waitForFluxer(ctx, inst, p, s, "Starting"); err != nil {
			return err
		}
		s.Done("Started Fluxer.")
		return nil
	})
}

func (in *installer) alreadyRunning(ctx context.Context, inst *instance.Instance) bool {
	if inst.State.Stopped {
		return false
	}
	proj, err := deploy.Load(ctx, inst, in.engine)
	if err != nil {
		return false
	}
	plan, err := proj.Plan(ctx)
	if err != nil || len(plan.Create)+len(plan.Recreate)+len(plan.Remove) > 0 {
		return false
	}
	return proj.WaitReady(ctx, 5*time.Second, nil) == nil
}
