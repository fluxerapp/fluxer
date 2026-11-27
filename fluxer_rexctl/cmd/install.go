// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"net/netip"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/proxyconf"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const defaultProject = "fluxer"

type installFlags struct {
	Domain        string
	Engine        string
	Runtime       string
	Native        bool
	BehindProxy   bool
	Bind          string
	Proxy         string
	ProxyFrom     string
	VoiceIP       string
	MemoryProfile string
	InstallDocker bool
	OpenFirewall  bool
	SetupPodman   bool
	SetupWhere    string
	NoStart       bool
	BootService   string
	DryRun        bool
	Confirm       string
	Project       string
	VoiceTCPPort  int
	VoiceUDPPort  int
	NoVoice       bool
	ForMigrate    bool
	MigrateFrom   string
	Setup         setupFlags
	Storage       storageFlags
}

type installer struct {
	app         *App
	c           *cobra.Command
	f           *installFlags
	ctx         context.Context
	dir         string
	native      bool
	container   bool
	facts       runtimeFacts
	defaulted   bool
	machineRuns int
	resuming    *instance.Instance
	inst        *instance.Instance
	release     func()
	host        *host.Info
	engine      *engine.Engine
	found       *engine.Engine
	findErr     error
	rel         *release.Release
	postgres    int
	ran         map[string]bool
	failed      error
	plans       map[string][]planned
	planStage   string
	net         netAnswer
	store       string
	backups     string
	scheduled   bool
	storage     *storageChoice
	secret      string
	admin       setupState
	answers     *setup.Answers
	started     time.Time
	done        bool
	adminID     string
	addrStage   *ui.Stage
	netStage    *ui.Stage
	adminStage  *ui.Stage
	publicIP    string
	lanIP       string
	publicDone  bool
	tailnet     *host.Tailnet
	noCert      bool
	proxyFiles  map[proxyconf.Server]string
	opened      []instance.OpenedPort
}

func newInstallCmd(app *App) *cobra.Command {
	var f installFlags
	c := &cobra.Command{
		Use:   "install",
		Short: "Install Fluxer on this machine",
		Example: "rexctl install\n" +
			"sudo rexctl install --runtime systemd --domain chat.example.com --yes\n" +
			"rexctl install --runtime docker --domain chat.example.com --behind-proxy --yes",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runInstall(c.Context(), app, c, &f)
		},
	}
	fl := c.Flags()
	fl.StringVar(&f.Domain, "domain", "", "domain or IP address people will use to open your instance, like chat.example.com or 203.0.113.7, and without it --yes uses this machine's public IP address")
	fl.StringVar(&f.Runtime, "runtime", "", "where Fluxer runs, systemd for a service on this machine or docker or podman for one container. Without it, rexctl picks systemd on Debian or Ubuntu as root and one container everywhere else")
	fl.StringVar(&f.Engine, "engine", "", "")
	_ = fl.MarkHidden("engine")
	fl.BoolVar(&f.Native, "experimental-native", false, "")
	_ = fl.MarkHidden("experimental-native")
	fl.BoolVar(&f.BehindProxy, "behind-proxy", false, "serve plain HTTP for your own reverse proxy or tunnel")
	fl.StringVar(&f.Bind, "bind", settings.DefaultBind, "address your proxy or tunnel connects to. Implies --behind-proxy")
	fl.StringVar(&f.Proxy, "proxy", "", "what is in front of Fluxer (local, remote or tunnel). Implies --behind-proxy")
	fl.StringVar(&f.ProxyFrom, "proxy-from", "", "addresses or ranges your proxy on another machine connects from, or private for any private address, with --proxy remote")
	fl.StringVar(&f.VoiceIP, "voice-ip", "", "public IP address for voice and video (found automatically)")
	fl.StringVar(&f.MemoryProfile, "memory-profile", "", "use small, medium or large on every start, without sizing Fluxer to this machine")
	fl.BoolVar(&f.InstallDocker, "install-docker", true, "install Docker if it's missing")
	fl.BoolVar(&f.OpenFirewall, "open-firewall", true, "open the ports Fluxer needs in ufw or firewalld")
	fl.BoolVar(&f.SetupPodman, "setup-podman", true, "start Podman's API socket and start Fluxer with the machine")
	fl.StringVar(&f.SetupWhere, "setup", "", "where to create your admin account (terminal or browser)")
	fl.BoolVar(&f.NoStart, "no-start", false, "write the files without starting Fluxer")
	fl.StringVar(&f.BootService, "boot-service", "", "start Fluxer when this machine starts (on or off)")
	fl.BoolVar(&f.DryRun, "dry-run", false, "show what would happen without changing anything")
	fl.StringVar(&f.Project, "project", defaultProject, "name for this instance's service, containers and folders. A second Fluxer on one machine gets a free name by itself")
	fl.IntVar(&f.VoiceTCPPort, "voice-tcp-port", 0, "TCP port for voice and video. A second Fluxer on one machine gets a free port by itself")
	fl.IntVar(&f.VoiceUDPPort, "voice-udp-port", 0, "UDP port for voice and video. A second Fluxer on one machine gets a free port by itself")
	fl.BoolVar(&f.NoVoice, "no-bundled-voice", false, "run without the bundled LiveKit and use only voice servers you add later")
	fl.BoolVar(&f.ForMigrate, "for-migrate", false, "")
	_ = fl.MarkHidden("for-migrate")
	fl.StringVar(&f.MigrateFrom, "migrate-from", "", "")
	_ = fl.MarkHidden("migrate-from")
	confirmFlag(c, &f.Confirm)
	f.Setup.bind(c)
	f.Storage.bind(c)
	return c
}

func installTarget(app *App) (string, bool, error) {
	l, err := app.locate()
	if err != nil {
		return "", false, err
	}
	dir := l.Dir
	explicit := l.From == instance.FromFlag || l.From == instance.FromWorkdir
	switch {
	case l.Stale != "" && l.Chosen:
		dir = l.Stale
		explicit = true
		app.located = &instance.Located{Dir: dir, From: instance.FromRecord, Scope: l.Scope}
		app.UI.Info("Installing Fluxer in %s, the folder you picked with rexctl use.", dir)
	case l.Stale != "":
		app.warnStale(l)
	}
	if dir, err = filepath.Abs(dir); err != nil {
		return "", false, err
	}
	inst, err := instance.Find(dir)
	if err != nil || !inst.State.IsVoice() {
		return dir, explicit, nil
	}
	refusal := ui.Refused(fmt.Sprintf("%s holds voice servers that joined %s, not a Fluxer instance.", dir, voiceInstances(inst)), "Pass --dir with another folder to install Fluxer there.")
	if explicit {
		return "", false, refusal
	}
	fallback, err := instance.DefaultDir()
	if err != nil {
		return "", false, err
	}
	if fallback, err = filepath.Abs(fallback); err != nil {
		return "", false, err
	}
	if other, err := instance.Find(fallback); err == nil && other.State.IsVoice() {
		return "", false, refusal
	}
	app.located = &instance.Located{Dir: fallback, From: instance.FromFallback}
	app.UI.Info("Installing Fluxer in %s. %s holds this machine's voice servers.", fallback, dir)
	return fallback, false, nil
}

func runInstall(ctx context.Context, app *App, c *cobra.Command, f *installFlags) error {
	err := install(ctx, app, c, f)
	switch {
	case err == nil:
		return nil
	case f.ForMigrate:
		app.handOver(err)
		app.UI.Detail("%v", err)
		return &ui.ExitError{Code: ui.CodeOf(err)}
	}
	return err
}

func install(ctx context.Context, app *App, c *cobra.Command, f *installFlags) error {
	if err := validateInstallFlags(app, c, f); err != nil {
		return err
	}
	if err := f.Storage.validate(c); err != nil {
		return err
	}
	if _, err := parseBootServiceFlag(f.BootService); err != nil {
		return err
	}
	if err := oneStdinFlag(c); err != nil {
		return err
	}
	if names := f.Setup.passedNames(c); f.SetupWhere == "browser" && len(names) > 0 {
		return &ui.ExitError{Code: ui.ExitUsage, Msg: fmt.Sprintf("--setup browser doesn't take %s.", ui.JoinAnd(names)), Fix: "Pass --setup terminal instead, or leave out the setup options and enter them in the browser."}
	}
	in := &installer{app: app, c: c, f: f, ctx: ctx, ran: map[string]bool{}}
	if f.ForMigrate {
		movingFrom = instance.RealPath(f.MigrateFrom)
	}
	dir, explicit, err := installTarget(app)
	if err != nil {
		return err
	}
	in.dir = dir
	inst, err := instance.Find(dir)
	if err == nil && inst.State != nil && !inst.State.SetupDone {
		switch {
		case f.Domain == "" || normaliseDomain(f.Domain) == inst.Domain():
			return in.resume(ctx, inst)
		case inst.State.Installing:
			return ui.Refused(fmt.Sprintf("The install of %s in %s didn't finish.", inst.Domain(), dir), "Run rexctl install --dir "+shellWord(dir)+" to finish it first, or pass --dir with an empty folder for another Fluxer.")
		}
	}
	if f.Runtime == "" {
		in.defaultRuntime(ctx)
	} else {
		in.facts = gatherFacts(ctx, app.UI.Interactive(), false)
	}
	in.native = f.Runtime == runtimeSystemd
	in.container = f.Runtime == runtimeDocker || f.Runtime == runtimePodman
	if in.native {
		if err := in.nativeFlags(ctx); err != nil {
			return err
		}
	}
	second := false
	switch {
	case errors.Is(err, instance.ErrLegacy):
		return preRexctlRefusal(ctx, dir)
	case errors.Is(err, instance.ErrNotFound):
	case err != nil:
		return err
	case explicit:
		return ui.Refused(fmt.Sprintf("Fluxer is already installed in %s.", dir), "Run rexctl status --dir "+shellWord(dir)+" to check it. For a second Fluxer next to it, pass --dir with an empty folder.")
	default:
		if err := keptData(dir); err != nil {
			return err
		}
		if in.dir, err = in.secondDir(dir); err != nil {
			return err
		}
		second = true
		app.located = &instance.Located{Dir: in.dir, From: instance.FromFlag}
	}
	if !second {
		if err := in.pickName(explicit); err != nil {
			return err
		}
	}
	if in.native {
		if err := in.nativeTarget(); err != nil {
			return err
		}
	}
	in.defaultDomain()
	if f.Domain != "" {
		if err := domainTaken(in.dir, f.Domain); err != nil {
			return err
		}
	}
	if second || !c.Flags().Changed("project") && f.Project != defaultProject {
		app.UI.Info("Fluxer already runs on this machine, so this one is called %s and goes in %s.", f.Project, in.dir)
	}
	if err := in.missingFlags(); err != nil {
		return err
	}
	ok, err := in.confirmPublicBind()
	if err != nil || !ok {
		return err
	}
	if err := in.loadSecrets(); err != nil {
		return err
	}
	return in.run(ctx)
}

func (in *installer) run(ctx context.Context) error {
	defer func() {
		if in.release != nil {
			in.release()
		}
	}()
	flow := in.flow()
	if in.f.DryRun {
		flow.Tasks = nil
		flow.Review = nil
	}
	if err := in.app.UI.RunFlow(ctx, in.c, flow); err != nil {
		return in.interrupted(ctx, err)
	}
	if in.f.DryRun {
		in.app.UI.Card(in.review())
		return nil
	}
	if !in.done {
		return nil
	}
	in.scheduleBackups(ctx)
	if err := in.finish(ctx); err != nil {
		return in.interrupted(ctx, err)
	}
	return nil
}

func (in *installer) interrupted(ctx context.Context, err error) error {
	if err == nil || ctx.Err() == nil || ui.CodeOf(err) == ui.ExitInterrupted {
		return err
	}
	return ui.Interrupted(in.resumeHint())
}

func (in *installer) resumeHint() string {
	if in.f != nil && in.f.ForMigrate {
		return ""
	}
	cmd := "rexctl install"
	if def, err := instance.DefaultDir(); err == nil && in.dir != "" && instance.RealPath(def) != instance.RealPath(in.dir) {
		cmd += " --dir " + shellWord(in.dir)
	}
	return in.asRun(cmd)
}

func thenAgain(first, resume string) string {
	switch {
	case resume == "" && first == "":
		return ""
	case resume == "":
		return first + "."
	case first == "":
		return "Run " + resume + " again."
	}
	return first + ", then run " + resume + " again."
}

func (in *installer) runtimeLabel() string {
	if in.native {
		return rt.NativeLabel
	}
	return rt.ContainerLabel(engine.Kind(in.f.Engine))
}

func (in *installer) releaseLabel() string {
	if in.rel != nil {
		return in.rel.Version
	}
	return stack.EmbeddedRelease()
}

func (in *installer) subtitle() string {
	if release := in.releaseLabel(); release != "" && release != "dev" {
		return in.runtimeLabel() + ", Fluxer " + release
	}
	return in.runtimeLabel()
}

func (in *installer) flow() *ui.Flow {
	resuming := func() bool { return in.resuming != nil }
	in.addrStage = &ui.Stage{ID: "address", Title: "Address", Key: 'a', Hide: resuming, Checks: in.addressChecks, Fields: in.addressFields(), Summary: in.addressSummary}
	in.netStage = &ui.Stage{ID: "network", Title: "Network", Key: 'n', Hide: resuming, Checks: in.networkChecks, After: in.readyChecks, Fields: in.networkFields(), Summary: in.networkSummary}
	in.adminStage = &ui.Stage{ID: "admin", Title: "Admin", Key: 'u', Hide: func() bool { return in.f.NoStart }, Checks: in.adminChecks, Fields: in.adminFields(), Summary: in.adminSummary}
	return &ui.Flow{
		Title:    "rexctl install",
		Subtitle: in.subtitle(),
		Greeting: in.resuming == nil,
		Stages: []*ui.Stage{
			{ID: "machine", Title: "Machine", Key: 'r', Hide: resuming, Checks: in.machineChecks, Summary: in.machineSummary},
			in.addrStage,
			in.netStage,
			{ID: "storage", Title: "Storage", Key: 's', Hide: resuming, Checks: in.storageChecks, Fields: in.storageFields(), Summary: in.storageSummary},
			in.adminStage,
		},
		Review: in.review,
		Tasks:  in.tasks,
	}
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
	if f.Proxy == settings.ProxyRemote && f.ProxyFrom == "" {
		missing = append(missing, "--proxy-from")
	}
	if in.terminalSetup() && !f.NoStart {
		if f.Setup.SignIn == "" && usernameDomain(normaliseDomain(f.Domain)) {
			f.Setup.SignIn = string(setup.SignInUsername)
		}
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
		if s.SignIn == string(setup.SignInEmail) && !s.SkipEmail && s.SMTP.Host == "" {
			missing = append(missing, "--smtp-host or --skip-email")
		}
	}
	if len(missing) == 0 {
		return nil
	}
	return ui.Usage("Pass " + ui.JoinAnd(missing) + ".")
}

func (in *installer) defaultDomain() {
	if in.f.Domain != "" || in.f.BehindProxy || in.app.UI.Interactive() {
		return
	}
	ip := in.detectPublicIP(in.ctx)
	if ip == "" {
		return
	}
	in.f.Domain = settings.NormalDomain(ip)
	in.app.UI.Info("Without --domain, Fluxer uses this machine's public IP address, %s.", in.f.Domain)
	if in.lanIP != "" {
		in.app.UI.Info("Behind a home router, --domain %s works for devices on the same network without forwarding ports.", in.lanIP)
	}
}

func (in *installer) terminalSetup() bool {
	f := in.f
	return f.SetupWhere == "terminal" || (f.SetupWhere == "" && f.Setup.passed(in.c))
}

func notReady() error {
	return ui.Refused("This machine isn't ready for Fluxer yet.", "Fix the problems above, then run rexctl install again.")
}

var proxyKinds = []string{settings.ProxyLocal, settings.ProxyRemote, settings.ProxyTunnel, settings.ProxyInstance}

func validateInstallFlags(app *App, c *cobra.Command, f *installFlags) error {
	if f.Engine != "" && f.Engine != string(engine.Docker) && f.Engine != string(engine.Podman) {
		return ui.Usage("Pass --runtime docker or --runtime podman.")
	}
	if f.Native {
		if f.Runtime != "" && f.Runtime != runtimeSystemd {
			return ui.Usage("Pass --runtime systemd without --experimental-native.")
		}
		f.Runtime = runtimeSystemd
		aliasLine(app, "--experimental-native", "--runtime systemd")
	}
	switch f.Runtime {
	case "":
		if f.Engine != "" {
			f.Runtime = f.Engine
			aliasLine(app, "--engine "+f.Engine, "--runtime "+f.Engine)
		}
	case runtimeSystemd:
		if f.Engine != "" {
			return ui.Usage("--runtime systemd runs Fluxer without containers, so it doesn't take --engine.")
		}
	case runtimeDocker, runtimePodman:
		if f.Engine != "" && f.Engine != f.Runtime {
			return ui.Usage("Pass --runtime " + f.Runtime + " on its own.")
		}
		f.Engine = f.Runtime
	default:
		return ui.Usage("Pass --runtime systemd, --runtime docker or --runtime podman.")
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
	bind, err := settings.NormaliseBind(f.Bind)
	if err != nil {
		return ui.Usage("Pass --bind as an address and port, like " + settings.DefaultBind + ".")
	}
	f.Bind = bind
	if f.Proxy != "" && !slices.Contains(proxyKinds, f.Proxy) {
		return ui.Usage("Pass --proxy local, --proxy remote or --proxy tunnel.")
	}
	if f.ProxyFrom != "" {
		if err := validateProxyFrom(f.ProxyFrom); err != nil {
			return ui.Usage(fmt.Sprintf("--proxy-from can't be %q. %s", f.ProxyFrom, err.Error()))
		}
		if f.Proxy == "" {
			f.Proxy = settings.ProxyRemote
		}
	}
	if c.Flags().Changed("bind") || f.Proxy != "" {
		f.BehindProxy = true
	}
	for _, p := range []int{f.VoiceTCPPort, f.VoiceUDPPort} {
		if p < 0 || p > 65535 {
			return ui.Usage("Pass a voice port from 1 to 65535.")
		}
	}
	if f.NoVoice && (f.VoiceTCPPort > 0 || f.VoiceUDPPort > 0) {
		return ui.Usage("Pass --voice-tcp-port and --voice-udp-port only with the bundled LiveKit, not with --no-bundled-voice.")
	}
	if !projectPattern.MatchString(f.Project) {
		return ui.Usage("Pass --project with lowercase letters, numbers, dashes or underscores.")
	}
	if f.Domain != "" {
		if err := validateDomain(f.Domain); err != nil {
			return ui.Usage(err.Error())
		}
		f.Domain = normaliseDomain(f.Domain)
	}
	return nil
}

func (in *installer) confirmPublicBind() (bool, error) {
	f := in.f
	if !f.BehindProxy || !in.c.Flags().Changed("bind") || settings.BindIsLoopback(f.Bind) {
		return true, nil
	}
	ok, err := confirmPublicBind(in.app, f.Bind, in.native)
	if err != nil || !ok {
		return false, err
	}
	if f.Proxy == "" {
		f.Proxy = settings.ProxyRemote
	}
	return true, nil
}

func confirmPublicBind(app *App, bind string, firewallHolds bool) (bool, error) {
	if settings.BindIsLoopback(bind) {
		return true, nil
	}
	detail := "Only your proxy should connect to it. rexctl lets only the proxy's address through the firewall."
	if !firewallHolds {
		detail = "Only your proxy should connect to it. Docker and rootful Podman publish ports past the host firewall, so use an address on a private network that only your proxy can connect to."
	}
	ok, err := app.UI.ConfirmWith(ui.Question{
		Text:   fmt.Sprintf("Serve plain HTTP on %s, where other machines can connect to it?", bind),
		Detail: detail,
		Flag:   "yes",
	})
	if err != nil {
		return false, err
	}
	if !ok {
		app.UI.Info("Nothing changed.")
	}
	return ok, nil
}

func validateBind(v string) error {
	if _, err := settings.NormaliseBind(v); err != nil {
		return setupInputError("Enter an address and port, like " + settings.DefaultBind + ".")
	}
	return nil
}

func validateProxyFrom(v string) error {
	sources := proxySources(v)
	bad := len(sources) == 0
	for _, s := range sources {
		_, addrErr := netip.ParseAddr(s)
		_, prefixErr := netip.ParsePrefix(s)
		if s != proxyFromPrivate && addrErr != nil && prefixErr != nil {
			bad = true
		}
	}
	if bad {
		return setupInputError("Enter an IP address or range, like 203.0.113.10 or 10.0.0.0/24, or private for any address on your private network. Separate several with commas.")
	}
	return nil
}

var projectPattern = regexp.MustCompile(`^[a-z0-9][a-z0-9_-]*$`)

var labelPattern = regexp.MustCompile(`^[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$`)

func normaliseDomain(v string) string {
	v = strings.ToLower(strings.TrimSpace(v))
	v = strings.TrimPrefix(v, "https://")
	v = strings.TrimPrefix(v, "http://")
	return settings.NormalDomain(strings.TrimSuffix(v, "/"))
}

func validateDomain(v string) error {
	d := normaliseDomain(v)
	if ip, ok := settings.DomainIP(d); ok {
		if settings.LocalOnlyIP(ip) {
			return setupInputError("Enter an IP address other devices can open, not a loopback or link-local one.")
		}
		return nil
	}
	bad := setupInputError("Enter a domain name or IP address, like chat.example.com or 203.0.113.7.")
	labels := strings.Split(d, ".")
	if len(d) > 253 || len(labels) < 2 {
		return bad
	}
	for _, l := range labels {
		if !labelPattern.MatchString(l) {
			return bad
		}
	}
	return nil
}

func (in *installer) loadSecrets() error {
	if file := in.f.Storage.SecretFile; file != "" {
		v, err := readSetupSecret(in.app, file)
		if err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't read %s.", file), "Check the path you passed to --storage-secret-key-file.")
		}
		in.secret = v
	}
	if err := in.admin.load(in.app, &in.f.Setup, in.f.Domain); err != nil {
		return err
	}
	if in.f.SetupWhere == "" {
		in.f.SetupWhere = defaultSetupPlace(in.f.Setup.passed(in.c), in.app.UI.Interactive())
	}
	return nil
}

func defaultSetupPlace(flags, interactive bool) string {
	desktop := os.Getenv("DISPLAY") != "" || os.Getenv("WAYLAND_DISPLAY") != ""
	if flags || interactive && (!desktop || os.Getenv("SSH_CONNECTION") != "") {
		return "terminal"
	}
	return "browser"
}

func (in *installer) resume(ctx context.Context, inst *instance.Instance) error {
	r, err := rt.Open(ctx, inst, in.app.UI)
	if err != nil {
		return err
	}
	if r.Kind() == rt.Compose {
		return legacyRefusal()
	}
	in.native = r.Kind() == rt.Native
	in.container = r.Kind() == rt.Container
	if in.container {
		in.f.Runtime = cmp.Or(inst.State.Engine.Kind, runtimeDocker)
		in.f.Engine = in.f.Runtime
	}
	if in.f.Runtime == runtimeSystemd && !in.native {
		return ui.Refused(fmt.Sprintf("Fluxer already runs in %s in %s.", engineName(inst), inst.Dir), "To move it out of "+engineName(inst)+", run "+sudoRun(inst.Dir, dockerMoveArgs("")...)+".")
	}
	if in.native {
		in.f.Runtime = runtimeSystemd
	}
	in.f.Project = inst.Project()
	in.resuming = inst
	in.inst = inst
	in.f.Domain = inst.Domain()
	if err := rootlessGuard(inst); err != nil {
		return err
	}
	in.restoreNetwork(inst)
	if in.f.DryRun {
		if in.f.Storage.passed(in.c) {
			return &ui.ExitError{Code: ui.ExitUsage, Msg: "--dry-run doesn't use the storage options when it continues an unfinished install.", Fix: "Leave out --dry-run to check and save them, or leave out the storage options."}
		}
		in.app.UI.Info("rexctl would continue the install in %s.", inst.Dir)
		in.app.UI.Card(in.review())
		return nil
	}
	if in.native {
		if err := in.app.nativeRoot(inst.Dir); err != nil {
			return err
		}
	}
	in.app.UI.Info("Continuing the install in %s.", inst.Dir)
	switch {
	case in.native:
		rel, err := nativeRelease(ctx, inst.State.Release, nativeInstallFix)
		if err != nil {
			return err
		}
		in.rel = rel
	case in.container:
		rel, err := resolveRelease(ctx, inst.State.Release)
		if err != nil {
			return err
		}
		in.rel = rel
		e, err := in.app.anyEngine(ctx, inst)
		if err != nil {
			return err
		}
		in.engine = e
	}
	if err := in.loadSecrets(); err != nil {
		return err
	}
	return in.app.Locked(ctx, inst, func() error { return in.run(ctx) })
}
