// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"cmp"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"maps"
	"net/http"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"reflect"
	"runtime/debug"
	"slices"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"text/tabwriter"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/children"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/config"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/ctl"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/edge"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/lock"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/logs"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/natsembed"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/notify"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/pg"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/seaweed"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/sizing"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/stamp"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/supervise"
)

const usage = `usage: fluxerd <command> [flags]

commands:
  init                         create the data directory and secrets, print the setup token
  setup-token                  print the token that creates the first admin account
  users create <username> [--email e] [--admin]
                               create an account and print a one-time link to set its password
  users reset-link <user>      print a one-time link to set a new password for an account
  run [--voice-only]           start every service and supervise it, or only the voice servers
  health [--live]              exit 0 when every service is ready, or with --live when fluxerd responds
  ps [--json]                  list services
  version [--json] [--check]   show this release, the newest one and how to upgrade
  ctl start|stop|restart <svc> control one service
  logs <svc> [-f]              show the recent output of one service
  exec <svc> -- <cmd...>       run a command with the environment of one service
  psql [args...]               open psql on the bundled database
  backup [--dir d | --out -] [--pre-upgrade] [--include d]
  restore [--check] <archive | ->
  postgres-upgrade [--to N] [--cleanup]
                               move the database to the newest installed PostgreSQL with pg_upgrade,
                               or delete the folders an earlier upgrade left behind
  import [--dir d | --out -] <file | ->
                               turn the kubectl get -o json output of a 0.x Helm release's
                               Secrets and ConfigMap into a backup archive
  check-config                 validate the configuration without starting anything
  print-env <svc>              print the environment one service gets
  dial control|operator|api    connect standard input and output to a control socket or the API
  print-env --catalog [--json] print every setting fluxerd reads
  voice-config --endpoint wss://host [--id id] [--region r] [--tcp-port n] [--udp-port n] [--ip a] [--lat n --long n] [--soft-limit n]
                               register a voice server and print its LiveKit config
`

func main() {
	os.Exit(dispatch(os.Args[1:]))
}

func dispatch(args []string) int {
	if len(args) == 0 {
		fmt.Fprint(os.Stderr, usage)
		return 2
	}
	commands := map[string]func([]string) error{
		"init":             cmdInit,
		"setup-token":      cmdSetupToken,
		"health":           cmdHealth,
		"ps":               cmdPs,
		"ctl":              cmdCtl,
		"logs":             cmdLogs,
		"exec":             cmdExec,
		"psql":             cmdPsql,
		"backup":           cmdBackup,
		"restore":          cmdRestore,
		"check-config":     cmdCheckConfig,
		"dial":             cmdDial,
		"print-env":        cmdPrintEnv,
		"voice-config":     cmdVoiceConfig,
		"import":           cmdImport,
		"postgres-upgrade": cmdPostgresUpgrade,
		"version":          cmdVersion,
		"users":            cmdUsers,
	}
	name, rest := args[0], args[1:]
	switch name {
	case "run":
		return cmdRun(rest)
	case seaweed.InitCommand:
		return seaweed.InitMain(context.Background(), rest, os.Getenv, os.Stdout, os.Stderr)
	case "help", "-h", "--help":
		fmt.Print(usage)
		return 0
	}
	cmd, ok := commands[name]
	if !ok {
		fmt.Fprintf(os.Stderr, "fluxerd: unknown command %q\n\n%s", name, usage)
		return 2
	}
	if err := dataOwnerCheck(name); err != nil {
		fmt.Fprintf(os.Stderr, "fluxerd %s: %v\n", name, err)
		return 1
	}
	if err := cmd(rest); err != nil {
		var exit exitError
		if errors.As(err, &exit) {
			return int(exit)
		}
		fmt.Fprintf(os.Stderr, "fluxerd %s: %v\n", name, err)
		return 1
	}
	return 0
}

type exitError int

const exitNoSpace = 4

func (e exitError) Error() string { return "exit " + strconv.Itoa(int(e)) }

type configFlags struct {
	envFile, caddyDir, certsDir, root string
	voiceOnly, daemon                 bool
	runtime                           runtimeLookup
}

func addConfigFlags(fs *flag.FlagSet) *configFlags {
	f := &configFlags{}
	fs.StringVar(&f.envFile, "config", "", "env file (default "+spec.DefaultEnvFile+")")
	fs.StringVar(&f.caddyDir, "caddy-dir", "", "Caddy drop-in directory (default "+spec.DefaultCaddyDir+")")
	fs.StringVar(&f.certsDir, "certs-dir", "", "certificate directory (default "+spec.DefaultCertsDir+")")
	fs.StringVar(&f.root, "root", "", "release directory (default: two levels above this binary)")
	fs.BoolVar(&f.voiceOnly, "voice-only", false, "run only the voice servers and the edge, the same as "+spec.VoiceOnlyEnv+"=true")
	return f
}

func (f *configFlags) children(cfg *config.Config) (children.Config, edge.Config, error) {
	root, err := children.ReleaseRoot(f.root)
	if err != nil {
		return children.Config{}, edge.Config{}, err
	}
	self, err := os.Executable()
	if err != nil {
		return children.Config{}, edge.Config{}, err
	}
	cc := children.Config{
		Layout:   cfg.Layout,
		Root:     root,
		Self:     self,
		Version:  releaseVersion(root),
		Vars:     cfg.Env,
		Host:     sizing.Detect(),
		CaddyDir: cfg.CaddyDir,
		CertsDir: cfg.CertsDir,
	}
	ec, err := children.Edge(cc)
	if err != nil {
		return cc, ec, err
	}
	if slices.Equal(strings.Fields(ec.TLS), []string{"tls", "internal"}) {
		cc.CABundle = filepath.Join(cfg.Layout.Runtime, "ca-bundle.crt")
	}
	return cc, ec, nil
}

func releaseVersion(root string) string {
	version, _ := os.ReadFile(filepath.Join(root, "share", "VERSION"))
	return strings.TrimSpace(string(version))
}

func (f *configFlags) child(name string) (spec.Child, error) {
	cfg, err := f.loadWithSecrets()
	if err != nil {
		return spec.Child{}, err
	}
	cc, _, err := f.children(cfg)
	if err != nil {
		return spec.Child{}, err
	}
	specs, err := children.Specs(cc)
	if err != nil {
		return spec.Child{}, err
	}
	var names []string
	for _, c := range specs {
		if c.Name == name {
			return c, nil
		}
		names = append(names, c.Name)
	}
	return spec.Child{}, fmt.Errorf("no service %q (services: %s)", name, strings.Join(names, ", "))
}

func pgChoice(cfg *config.Config) (pg.Choice, error) {
	choice, err := pg.Resolve(cfg.Layout.PostgresDir(), cfg.Env["FLUXER_POSTGRES_BIN_DIR"])
	if err != nil && cfg.Env["FLUXER_POSTGRES_URL"] != "" {
		return pg.Choice{}, nil
	}
	return choice, err
}

func (f *configFlags) load() (*config.Config, error) {
	cfg, err := config.Load(config.Options{EnvFile: f.envFile, CaddyDir: f.caddyDir, CertsDir: f.certsDir, Environ: os.Environ()})
	if err != nil {
		return nil, err
	}
	if d, ok := cfg.Env["FLUXER_DOMAIN"]; ok {
		cfg.Env["FLUXER_DOMAIN"] = children.NormalDomain(d)
	}
	if f.voiceOnly {
		cfg.Env[spec.VoiceOnlyEnv] = "true"
	}
	if !f.daemon {
		f.runtime = lookupRuntime(cfg.Layout, cfg.Env[spec.RuntimeDirEnv] != "")
		cfg.Layout.Runtime = f.runtime.dir
	}
	if err := children.CheckVoiceOnly(cfg.Env); err != nil {
		return nil, err
	}
	return cfg, nil
}

func voiceOnly(cfg *config.Config) bool {
	return children.Config{Vars: cfg.Env}.VoiceOnly()
}

func checkDomain(cfg *config.Config) error {
	if children.VoiceEdgeOff(cfg.Env) {
		return nil
	}
	return children.CheckDomain(cfg.Env["FLUXER_DOMAIN"])
}

func (f *configFlags) loadWithSecrets() (*config.Config, error) {
	cfg, err := f.load()
	if err != nil {
		return nil, err
	}
	return cfg, secrets.Fill(cfg.Layout, cfg.Env)
}

func socketFlag(fs *flag.FlagSet) *string {
	def := lookupRuntime(spec.LayoutFromEnv(os.Getenv), os.Getenv(spec.RuntimeDirEnv) != "").dir
	return fs.String("runtime-dir", def, "runtime directory that holds fluxerd.sock")
}

func client(runtimeDir string) *ctl.Client {
	return ctl.Dial(spec.Layout{Runtime: runtimeDir}.ControlSocket())
}

func cmdRun(args []string) int {
	out := logs.New()
	if err := notify.Detach(); err != nil {
		out.Logf(logs.PrioErr, "hide NOTIFY_SOCKET from the edge: %v", err)
		return 1
	}
	code, err := runDaemon(args, out)
	if err != nil {
		out.Logf(logs.PrioErr, "%v", err)
	}
	return code
}

const memoryHeadroom = 256 << 20

func setMemoryLimit(jetStreamMemory int64) {
	if _, set := os.LookupEnv("GOMEMLIMIT"); !set {
		debug.SetMemoryLimit(memoryHeadroom + max(jetStreamMemory, 0))
	}
}

func runDaemon(args []string, out *logs.Output) (int, error) {
	setMemoryLimit(0)
	fs := flag.NewFlagSet("run", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	cf.daemon = true
	if err := fs.Parse(args); err != nil {
		return 2, nil
	}
	cfg, err := cf.load()
	if err != nil {
		return 1, err
	}
	if err := checkDomain(cfg); err != nil {
		return 1, err
	}
	voice := voiceOnly(cfg)
	root, err := children.ReleaseRoot(cf.root)
	if err != nil {
		return 1, err
	}
	release := releaseVersion(root)
	if _, err := parseSchedule(cfg.Env); err != nil {
		return 1, err
	}
	l := cfg.Layout
	if err := os.MkdirAll(l.Data, 0o700); err != nil {
		return 1, err
	}
	if err := os.MkdirAll(l.Runtime, 0o750); err != nil {
		return 1, err
	}
	if !voice {
		stop, err := waitWhileDataIsNewer(l, release, out)
		if err != nil {
			return 1, err
		}
		if stop {
			return 0, nil
		}
	}
	lk, err := lock.Acquire(l.LockFile())
	if err != nil {
		return 1, err
	}
	defer lk.Release()
	forget, err := recordRuntime(l)
	if err != nil {
		return 1, err
	}
	defer forget()
	ln, err := ctl.Listen(l.ControlSocket())
	if err != nil {
		return 1, err
	}
	control := &ctl.Swap{}
	control.Set(ctl.Waiting("starting"))
	srv := controlServer(control)
	go srv.Serve(ln)
	defer srv.Close()
	status := func(line string) {
		control.Set(ctl.Waiting(line))
		notify.Status(line)
	}
	if voice {
		out.Logf(logs.PrioInfo, "voice-only mode, running only the voice servers in %s", children.VoiceDir(children.Config{CaddyDir: cfg.CaddyDir}))
	} else {
		if err := checkBundled(cfg, filepath.Join(root, "bin")); err != nil {
			return 1, err
		}
		if err := prepareData(cfg, release, out, status); err != nil {
			return 1, err
		}
	}
	status("starting")
	cc, edgeCfg, err := cf.children(cfg)
	if err != nil {
		return 1, err
	}
	specs, err := children.Specs(cc)
	if err != nil {
		return 1, err
	}
	_, skipped := children.VoiceServers(cc)
	for _, err := range skipped {
		out.Logf(logs.PrioWarning, "skipping %v", err)
	}
	held, err := otherInstance(specs)
	if err != nil {
		return 1, err
	}
	for _, w := range held {
		out.Logf(logs.PrioWarning, "%s", w)
	}
	for _, w := range children.Ignored(cfg.Env) {
		out.Logf(logs.PrioWarning, "%s", w)
	}
	front := &edgeState{cfg: edgeCfg, out: out}
	front.dropInternalCert()
	if err := front.apply(); err != nil {
		return 1, err
	}
	defer front.stop()
	if err := supervise.ClearAmbientCaps(); err != nil {
		return 1, fmt.Errorf("clear ambient capabilities: %w", err)
	}
	if cc.CABundle != "" {
		if err := edge.TrustBundle(edgeCfg, edge.SystemBundle, cc.CABundle); err != nil {
			return 1, err
		}
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	if err := children.Prepare(ctx, cc, specs); err != nil {
		return 1, err
	}
	if hint := children.PostgresHint(cc); hint != "" {
		out.Logf(logs.PrioWarning, "%s", hint)
	}

	var readyErr atomic.Value
	var notified atomic.Bool
	sup, err := supervise.New(specs, supervise.Options{
		Out: out,
		OnReady: func(ready bool) {
			if err := front.setReady(ready); err != nil {
				readyErr.Store(err)
				cancel()
				return
			}
			if ready && !notified.Swap(true) {
				if err := notify.Ready(); err != nil {
					out.Logf(logs.PrioWarning, "sd_notify READY: %v", err)
				}
			}
		},
	})
	if err != nil {
		return 1, err
	}

	hooks := ctl.Hooks{Maintenance: front.setMaintenance, JetStream: func(bool) error { return errors.New("voice-only mode runs no JetStream") }}
	checker := newUpdateChecker(cf, root, l, out)
	var natsOpts natsembed.Options
	if !voice {
		natsOpts, err = children.NATS(cc)
		if err != nil {
			return 1, err
		}
		nats, err := natsembed.Start(natsOpts)
		if err != nil {
			return 1, err
		}
		setMemoryLimit(nats.MaxMemory())
		defer func() {
			nats.Shutdown()
			nats.WaitForShutdown()
		}()
		hooks.JetStream = nats.SetJetStream
		hooks.Update = checker.Hook
	}

	var reloadMu sync.Mutex
	reload := func(ctx context.Context) (spec.Reloaded, error) {
		reloadMu.Lock()
		defer reloadMu.Unlock()
		return reloadDaemon(ctx, cf, sup, front, natsOpts)
	}
	hooks.Reload = reload
	control.Set(ctl.Handler(sup, hooks))
	go notify.Watchdog(context.WithoutCancel(ctx), func() { sup.Health() })
	go reportStatus(ctx, sup)
	if !voice {
		go runSchedule(ctx, cf, root, sup, out)
		go runUpdateCheck(ctx, checker, release)
	}

	sigs := make(chan os.Signal, 4)
	signal.Notify(sigs, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT)
	defer signal.Stop(sigs)
	go func() {
		for sig := range sigs {
			if sig == syscall.SIGHUP {
				out.Logf(logs.PrioInfo, "SIGHUP: reloading the configuration")
				res, err := reload(ctx)
				switch {
				case err != nil:
					out.Logf(logs.PrioErr, "reload: %v", err)
					if err := front.refresh(); err != nil {
						out.Logf(logs.PrioErr, "edge reload: %v", err)
					}
				case len(res.Restarted) > 0:
					out.Logf(logs.PrioInfo, "reload restarted %s", strings.Join(res.Restarted, ", "))
				}
				continue
			}
			out.Logf(logs.PrioInfo, "%s: shutting down", sig)
			front.stop()
			cancel()
		}
	}()

	runErr := sup.Run(ctx)
	if err, ok := readyErr.Load().(error); ok {
		return 1, err
	}
	if errors.Is(runErr, supervise.ErrCrashloop) {
		return spec.ExitCrashloop, runErr
	}
	if runErr == nil {
		notify.Status("stopped")
	}
	return 0, runErr
}

type edgeState struct {
	mu          sync.Mutex
	out         *logs.Output
	cfg         edge.Config
	ready       bool
	maintenance bool
	stopped     bool
}

func (e *edgeState) mode() edge.Mode {
	switch {
	case e.maintenance:
		return edge.Maintenance
	case e.ready:
		return edge.Ready
	}
	return edge.Starting
}

func checkBundled(cfg *config.Config, binDir string) error {
	pending, err := config.PendingModes(cfg, !secrets.HasData(cfg.Layout))
	if err != nil {
		return err
	}
	env := maps.Clone(cfg.Env)
	maps.Copy(env, pending)
	return spec.CheckBundled(binDir, env)
}

func prepareData(cfg *config.Config, release string, out *logs.Output, status func(string)) error {
	l := cfg.Layout
	modes, err := config.EnsureModes(cfg, !secrets.HasData(l))
	if err != nil {
		return err
	}
	if len(modes) > 0 {
		out.Logf(logs.PrioInfo, "new data directory, wrote %s to %s", modeList(modes), l.ModesFile())
	}
	minted, err := secrets.Ensure(l, cfg.Env)
	if err != nil {
		return err
	}
	if len(minted) > 0 {
		out.Logf(logs.PrioInfo, "created %d secrets in %s", len(minted), l.SecretsFile())
	}
	if slices.Contains(minted, secrets.SetupToken) {
		switch command := children.SetupCommand(cfg.Env); {
		case command != "" && command != "fluxerd setup-token":
			out.Logf(logs.PrioInfo, "To create the first admin, run %s and open https://%s", command, cfg.Env["FLUXER_DOMAIN"])
		case cfg.Env[spec.ContainerEnv] == "1":
			out.Logf(logs.PrioInfo, "To create the first admin, run fluxerd setup-token in this container, for example with docker compose exec fluxer fluxerd setup-token, and open https://%s", cfg.Env["FLUXER_DOMAIN"])
		default:
			out.Logf(logs.PrioInfo, "created %s for the first admin account in %s", secrets.SetupToken, l.SecretsFile())
		}
	}
	return settleVersion(context.Background(), l.Data, release, preUpgradeBackup(cfg, out, status))
}

func (e *edgeState) apply() error {
	if e.stopped {
		return nil
	}
	return edge.Load(e.cfg, e.mode())
}

func (e *edgeState) refresh() error {
	e.mu.Lock()
	defer e.mu.Unlock()
	return e.apply()
}

func (e *edgeState) setReady(ready bool) error {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.ready = ready
	return e.apply()
}

func (e *edgeState) setMaintenance(on bool) error {
	e.mu.Lock()
	defer e.mu.Unlock()
	if e.maintenance == on {
		return nil
	}
	e.maintenance = on
	if on {
		e.out.Logf(logs.PrioInfo, "the edge shows the maintenance page")
	} else {
		e.out.Logf(logs.PrioInfo, "the edge serves Fluxer again")
	}
	return e.apply()
}

func (e *edgeState) setConfig(c edge.Config) (bool, error) {
	e.mu.Lock()
	defer e.mu.Unlock()
	same := reflect.DeepEqual(e.cfg, c)
	e.cfg = c
	e.dropInternalCert()
	if e.stopped {
		return !same, nil
	}
	return !same, edge.Reload(e.cfg, e.mode())
}

func (e *edgeState) dropInternalCert() {
	if err := e.cfg.DropInternalCert(); err != nil {
		e.out.Logf(logs.PrioWarning, "couldn't delete the internal certificate for %s: %v", e.cfg.ACMEFirst, err)
	}
}

func (e *edgeState) stop() {
	e.mu.Lock()
	defer e.mu.Unlock()
	if e.stopped {
		return
	}
	e.stopped = true
	notify.Stopping()
	if err := edge.Stop(); err != nil {
		e.out.Logf(logs.PrioWarning, "edge stop: %v", err)
	}
}

func reloadDaemon(ctx context.Context, cf *configFlags, sup *supervise.Supervisor, front *edgeState, nats natsembed.Options) (spec.Reloaded, error) {
	var res spec.Reloaded
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return res, err
	}
	cc, edgeCfg, err := cf.children(cfg)
	if err != nil {
		return res, err
	}
	specs, err := children.Specs(cc)
	if err != nil {
		return res, err
	}
	if cc.VoiceOnly() != (nats == natsembed.Options{}) {
		return res, fmt.Errorf("%w: %s changed", supervise.ErrRestart, spec.VoiceOnlyEnv)
	}
	if !cc.VoiceOnly() {
		natsOpts, err := children.NATS(cc)
		if err != nil {
			return res, err
		}
		if natsOpts != nats {
			return res, fmt.Errorf("%w: the embedded NATS server's settings changed", supervise.ErrRestart)
		}
	}
	if cc.CABundle != "" {
		if res.Edge, err = front.setConfig(edgeCfg); err != nil {
			return res, err
		}
		if err := edge.TrustBundle(edgeCfg, edge.SystemBundle, cc.CABundle); err != nil {
			return res, err
		}
	}
	force, err := children.Refresh(cc, specs)
	if err != nil {
		return res, err
	}
	if res.Restarted, err = sup.Reload(ctx, specs, force); err != nil {
		return res, err
	}
	changed, err := front.setConfig(edgeCfg)
	res.Edge = res.Edge || changed
	return res, err
}

func cmdInit(args []string) error {
	fs := flag.NewFlagSet("init", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	cfg, err := cf.load()
	if err != nil {
		return err
	}
	if err := os.MkdirAll(cfg.Layout.Data, 0o700); err != nil {
		return err
	}
	lk, err := lock.Acquire(cfg.Layout.LockFile())
	if err != nil {
		return err
	}
	defer lk.Release()
	modes, err := config.EnsureModes(cfg, !secrets.HasData(cfg.Layout))
	if err != nil {
		return err
	}
	minted, err := secrets.Ensure(cfg.Layout, cfg.Env)
	if err != nil {
		return err
	}
	if cfg.Env[spec.StorageBackendEnv] == string(spec.StorageFilesystem) {
		if err := children.PrepareObjects(cfg.Layout); err != nil {
			return err
		}
		if w := children.ObjectsWarning(cfg.Layout); w != "" {
			fmt.Printf("Warning: %s\n", w)
		}
	}
	fmt.Printf("Data directory: %s\n", cfg.Layout.Data)
	if len(modes) > 0 {
		fmt.Printf("Wrote %s to %s\n", modeList(modes), cfg.Layout.ModesFile())
	}
	if len(minted) > 0 {
		fmt.Printf("Created %d secrets in %s\n", len(minted), cfg.Layout.SecretsFile())
	}
	if slices.Contains(minted, secrets.SetupToken) {
		fmt.Printf("Setup token for the first admin account: %s\n", cfg.Env[secrets.SetupToken])
	}
	return nil
}

func cmdSetupToken(args []string) error {
	fs := flag.NewFlagSet("setup-token", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return err
	}
	token := strings.TrimSpace(cfg.Env[secrets.SetupToken])
	if token == "" {
		if starting(cfg.Layout.LockFile()) {
			return errors.New("the setup token isn't made yet, because Fluxer is still starting. Run this again in a minute")
		}
		return fmt.Errorf("there is no setup token in %s. Fluxer makes one when it first starts, so start Fluxer and check that --config and %s point at this instance", cfg.Layout.SecretsFile(), spec.DataDirEnv)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	switch setupProgress(ctx, spec.URL(spec.PortAPI)) {
	case setupAdminMade:
		return errors.New("the admin account exists, so the setup token no longer works. Sign in with that account to finish setup")
	case setupDone:
		return errors.New("setup is finished, so the setup token no longer works. Sign in with the admin account")
	}
	fmt.Println(token)
	return nil
}

func starting(lockFile string) bool {
	held, err := lock.Held(lockFile)
	return err == nil && held
}

const (
	setupOpen = iota
	setupAdminMade
	setupDone
)

func setupProgress(ctx context.Context, api string) int {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, api+"/.well-known/fluxer", nil)
	if err != nil {
		return setupOpen
	}
	req.Header.Set("X-Forwarded-For", spec.Loopback)
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return setupOpen
	}
	defer resp.Body.Close()
	var doc struct {
		AppPublic struct {
			Setup struct {
				Configured bool   `json:"configured"`
				LockReason string `json:"account_identity_lock_reason"`
			} `json:"setup"`
		} `json:"app_public"`
	}
	if resp.StatusCode != http.StatusOK || json.NewDecoder(resp.Body).Decode(&doc) != nil {
		return setupOpen
	}
	switch {
	case doc.AppPublic.Setup.Configured || doc.AppPublic.Setup.LockReason == "setup_configured":
		return setupDone
	case doc.AppPublic.Setup.LockReason == "admin_exists":
		return setupAdminMade
	}
	return setupOpen
}

func cmdHealth(args []string) error {
	fs := flag.NewFlagSet("health", flag.ContinueOnError)
	runtimeDir := socketFlag(fs)
	live := fs.Bool("live", false, "exit 0 when fluxerd responds, ready or not")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	h, err := client(*runtimeDir).Health(ctx)
	if err != nil {
		return err
	}
	if h.Ready {
		line := "ready"
		if len(h.Starting) > 0 {
			line += ", still starting " + strings.Join(h.Starting, ", ")
		}
		if len(h.Degraded) > 0 {
			line += ", not healthy " + strings.Join(h.Degraded, ", ")
		}
		fmt.Println(line)
		return nil
	}
	var parts []string
	if h.Status != "" {
		parts = append(parts, h.Status)
	}
	if h.Stopping {
		parts = append(parts, "stopping")
	}
	if len(h.Pending) > 0 {
		parts = append(parts, "waiting for "+strings.Join(h.Pending, ", "))
	}
	if len(h.Failed) > 0 {
		parts = append(parts, "failing: "+strings.Join(h.Failed, ", "))
	}
	fmt.Println("not ready: " + strings.Join(parts, "; "))
	if *live {
		return nil
	}
	return exitError(1)
}

func modeList(modes map[string]string) string {
	var parts []string
	for _, k := range slices.Sorted(maps.Keys(modes)) {
		parts = append(parts, k+"="+modes[k])
	}
	return strings.Join(parts, " ")
}

type psRow struct {
	spec.PsEntry
	UnitRestarts int `json:"unit_restarts"`
}

func cmdPs(args []string) error {
	fs := flag.NewFlagSet("ps", flag.ContinueOnError)
	place := lookupRuntime(spec.LayoutFromEnv(os.Getenv), os.Getenv(spec.RuntimeDirEnv) != "")
	runtimeDir := fs.String("runtime-dir", place.dir, "runtime directory that holds fluxerd.sock")
	asJSON := fs.Bool("json", false, "print JSON")
	unit := fs.String("unit", place.unit, "systemd unit whose restart count to report")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	entries, err := client(*runtimeDir).PS(ctx)
	if err != nil {
		return err
	}
	unitRestarts, unitKnown := systemdRestarts(ctx, *unit)
	if *asJSON {
		rows := make([]psRow, 0, len(entries))
		for _, e := range entries {
			rows = append(rows, psRow{PsEntry: e, UnitRestarts: unitRestarts})
		}
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		return enc.Encode(rows)
	}
	tw := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	fmt.Fprintln(tw, "NAME\tSTATE\tHEALTH\tRESTARTS\tPID\tUPTIME")
	for _, e := range entries {
		uptime := "-"
		if e.State == spec.StateRunning && !e.StartedAt.IsZero() {
			uptime = time.Since(e.StartedAt).Truncate(time.Second).String()
		}
		pid := "-"
		if e.PID != 0 {
			pid = strconv.Itoa(e.PID)
		}
		fmt.Fprintf(tw, "%s\t%s\t%s\t%d\t%s\t%s\n", e.Name, e.State, cmp.Or(string(e.Health), "-"), e.Restarts, pid, uptime)
		for _, w := range e.Watch {
			fmt.Fprintf(tw, "%s/%s\t-\t%s\t-\t-\t-\n", e.Name, w.Name, cmp.Or(string(w.Health), "-"))
		}
	}
	if err := tw.Flush(); err != nil {
		return err
	}
	if unitKnown {
		fmt.Printf("\n%s restarts: %d\n", *unit, unitRestarts)
	}
	printUpdateFooter(ctx, client(*runtimeDir))
	return nil
}

func systemdRestarts(ctx context.Context, unit string) (int, bool) {
	if unit == "" {
		return 0, false
	}
	path, err := exec.LookPath("systemctl")
	if err != nil {
		return 0, false
	}
	out, err := exec.CommandContext(ctx, path, "show", "-p", "NRestarts", "--value", unit).Output()
	if err != nil {
		return 0, false
	}
	n, err := strconv.Atoi(strings.TrimSpace(string(out)))
	return n, err == nil
}

func cmdCtl(args []string) error {
	fs := flag.NewFlagSet("ctl", flag.ContinueOnError)
	runtimeDir := socketFlag(fs)
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	if fs.NArg() != 2 {
		return errors.New("usage: fluxerd ctl start|stop|restart <svc>")
	}
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	return client(*runtimeDir).Service(ctx, fs.Arg(1), fs.Arg(0))
}

func cmdLogs(args []string) error {
	fs := flag.NewFlagSet("logs", flag.ContinueOnError)
	runtimeDir := socketFlag(fs)
	follow := fs.Bool("f", false, "keep printing new lines")
	var names []string
	for len(args) > 0 {
		if err := fs.Parse(args); err != nil {
			return exitError(2)
		}
		if fs.NArg() == 0 {
			break
		}
		names = append(names, fs.Arg(0))
		args = fs.Args()[1:]
	}
	if len(names) != 1 {
		return errors.New("usage: fluxerd logs <svc> [-f]")
	}
	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	return client(*runtimeDir).Logs(ctx, names[0], *follow, os.Stdout)
}

func cmdExec(args []string) error {
	fs := flag.NewFlagSet("exec", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	rest := fs.Args()
	if len(rest) > 1 && rest[1] == "--" {
		rest = append(rest[:1:1], rest[2:]...)
	}
	if len(rest) < 2 {
		return errors.New("usage: fluxerd exec <svc> -- <cmd...>")
	}
	c, err := cf.child(rest[0])
	if err != nil {
		return err
	}
	if c.Cwd != "" {
		if err := os.Chdir(c.Cwd); err != nil {
			return err
		}
	}
	return execWith(rest[1:], c.Env)
}

func execWith(argv []string, env spec.Env) error {
	if path, ok := env["PATH"]; ok {
		os.Setenv("PATH", path)
	}
	bin, err := exec.LookPath(argv[0])
	if err != nil {
		return err
	}
	return syscall.Exec(bin, argv, env.List())
}

func cmdPrintEnv(args []string) error {
	fs := flag.NewFlagSet("print-env", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	catalog := fs.Bool("catalog", false, "print every setting fluxerd reads instead")
	asJSON := fs.Bool("json", false, "print the catalog as JSON")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	if *catalog {
		return printCatalog(os.Stdout, *asJSON)
	}
	if fs.NArg() != 1 {
		return errors.New("usage: fluxerd print-env <svc>")
	}
	c, err := cf.child(fs.Arg(0))
	if err != nil {
		return err
	}
	for _, kv := range c.Env.List() {
		fmt.Println(kv)
	}
	return nil
}

func printCatalog(w io.Writer, asJSON bool) error {
	settings := children.Catalog()
	if asJSON {
		data, err := json.MarshalIndent(settings, "", "  ")
		if err != nil {
			return err
		}
		_, err = w.Write(append(data, '\n'))
		return err
	}
	for _, s := range settings {
		if _, err := fmt.Fprintf(w, "%s=%s\n", s.Name, s.Default); err != nil {
			return err
		}
	}
	return nil
}

func cmdPsql(args []string) error {
	fs := flag.NewFlagSet("psql", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return err
	}
	choice, _ := pgChoice(cfg)
	bin := choice.BinDir
	psql := filepath.Join(bin, "psql")
	if _, err := os.Stat(psql); bin == "" || err != nil {
		psql = "psql"
	}
	env := spec.Env{}
	for _, kv := range os.Environ() {
		if k, v, ok := strings.Cut(kv, "="); ok {
			env[k] = v
		}
	}
	if url := cfg.Env["FLUXER_POSTGRES_URL"]; url != "" {
		return execWith(append([]string{psql, url}, fs.Args()...), env)
	}
	env["PGHOST"] = cfg.Layout.PGSocketDir()
	env["PGPORT"] = strconv.Itoa(spec.PortPostgres)
	env["PGUSER"] = cmp.Or(cfg.Env["FLUXER_POSTGRES_USERNAME"], "fluxer")
	env["PGDATABASE"] = cmp.Or(cfg.Env["FLUXER_POSTGRES_DATABASE"], "fluxer")
	env["PGPASSWORD"] = cfg.Env["POSTGRES_PASSWORD"]
	return execWith(append([]string{psql}, fs.Args()...), env)
}

func backupTarget(cfg *config.Config) (backup.Target, error) {
	choice, err := pgChoice(cfg)
	return backup.Target{Layout: cfg.Layout, Env: cfg.Env, PGBinDir: choice.BinDir, PGMajor: choice.Major}, err
}

func cmdBackup(args []string) error {
	fs := flag.NewFlagSet("backup", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	dir := fs.String("dir", "", "directory for the archive (default <data>/backups)")
	out := fs.String("out", "", "- writes the archive to standard output")
	preUpgrade := fs.Bool("pre-upgrade", false, "mark the archive as taken before an upgrade")
	reason := fs.String("reason", "", "reason recorded in the manifest")
	include := fs.String("include", "", "directory whose files are added to the archive next to the env file")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return err
	}
	if voiceOnly(cfg) {
		return errors.New("a voice server keeps no data, so there is nothing to back up")
	}
	root, err := children.ReleaseRoot(cf.root)
	if err != nil {
		return err
	}
	if *out != "" && *out != "-" {
		return errors.New("--out takes only -. Use --dir for a folder")
	}
	if *out == "-" && isTerminal(os.Stdout) {
		return errors.New("--out - writes the archive to standard output, so redirect it to a file")
	}
	if *preUpgrade && *reason == "" {
		*reason = backup.ReasonPreUpgrade
	}
	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	var sup backup.Supervisor
	c := client(cfg.Layout.Runtime)
	if _, err := c.PS(ctx); err == nil {
		sup = c
	}
	opts := backup.CreateOptions{
		Dir:      *dir,
		EnvFile:  cfg.EnvFile,
		Include:  *include,
		Release:  dataVersion(cfg.Layout, root),
		Reason:   *reason,
		Progress: func(stage string) { fmt.Fprintf(os.Stderr, "backup: %s\n", stage) },
	}
	if *out == "-" {
		opts.Out = os.Stdout
	}
	t, err := backupTarget(cfg)
	if err != nil {
		return err
	}
	path, err := backup.Create(ctx, t, sup, opts)
	var space *backup.SpaceError
	if errors.As(err, &space) {
		fmt.Fprintf(os.Stderr, "fluxerd backup: %v\n", err)
		if opts.Out != nil {
			return exitError(exitNoSpace)
		}
		if err := json.NewEncoder(os.Stdout).Encode(space); err != nil {
			return err
		}
		return exitError(exitNoSpace)
	}
	if err != nil {
		return err
	}
	if opts.Out == nil {
		fmt.Println(path)
	}
	return nil
}

func isTerminal(f *os.File) bool {
	fi, err := f.Stat()
	return err == nil && fi.Mode()&os.ModeCharDevice != 0
}

func cmdRestore(args []string) error {
	fs := flag.NewFlagSet("restore", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	check := fs.Bool("check", false, "verify the archive and that it fits this instance, then exit without changing anything")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	if fs.NArg() != 1 {
		return errors.New("usage: fluxerd restore [--check] <archive | ->")
	}
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return err
	}
	root, err := children.ReleaseRoot(cf.root)
	if err != nil {
		return err
	}
	release := releaseVersion(root)
	archive := fs.Arg(0)
	if archive == "-" {
		if isTerminal(os.Stdin) {
			return errors.New("restore - reads the archive from standard input, so redirect a file into it")
		}
		if !*check {
			if err := os.MkdirAll(cfg.Layout.Data, 0o700); err != nil {
				return err
			}
			held, err := lock.Acquire(cfg.Layout.LockFile())
			if err != nil {
				return err
			}
			held.Release()
		}
		if *check {
			fmt.Fprintln(os.Stderr, "restore: checking the archive")
		} else {
			fmt.Fprintf(os.Stderr, "restore: reading the archive into %s\n", cfg.Layout.BackupsDir())
		}
		spooled, err := backup.Spool(os.Stdin, cfg.Layout)
		if err != nil {
			return err
		}
		defer spooled.Close()
		archive = spooled.Path
	}
	manifest, manifestErr := backup.ReadManifest(archive)
	hasDatabase := manifestErr == nil && manifest.Database != ""
	dumpMajor := 0
	if hasDatabase {
		dumpMajor = manifest.Dump.Major()
	}
	target, aside, pgErr := restoreTarget(cfg, dumpMajor)
	target.BinDir = filepath.Join(root, "bin")
	target.SetModes = cfg.SetModes
	if manifestErr == nil {
		if err := backup.CheckRelease(manifest, release); err != nil {
			return err
		}
		if err := backup.CheckModes(target, manifest, archive); err != nil {
			return err
		}
		if hasDatabase && pgErr != nil {
			return pgErr
		}
	}
	if !hasDatabase {
		aside = 0
	}
	if *check {
		m, err := backup.Check(target, archive)
		if err != nil {
			return err
		}
		fmt.Printf("the backup of %s from %s can be restored here\n", cmp.Or(m.Domain, m.Project), m.CreatedAt.Format(time.RFC3339))
		return nil
	}
	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	staging, err := stageCluster(ctx, cf, cfg, &target, archive, aside)
	if err != nil {
		return err
	}
	if err := prepareFreshRestore(ctx, cf, cfg); err != nil {
		return errors.Join(err, removeStaging(staging))
	}
	m, envCopy, err := backup.Restore(ctx, target, archive, backup.RestoreOptions{
		EnvFile:  cfg.EnvFile,
		Release:  release,
		Progress: func(stage string) { fmt.Fprintf(os.Stderr, "restore: %s\n", stage) },
	})
	if err != nil {
		return errors.Join(err, removeStaging(staging))
	}
	if staging != "" {
		dir := cfg.Layout.PostgresDir()
		if err := pg.Swap(dir, aside); err != nil {
			return err
		}
		fmt.Fprintf(os.Stderr, "restore: the PostgreSQL %d database that was in %s is in %s now. Delete it with sudo rexctl postgres upgrade --cleanup, or fluxerd postgres-upgrade --cleanup without rexctl, once Fluxer runs well\n", aside, dir, pg.OldDir(dir, aside))
	}
	if err := stamp.Write(cfg.Layout.Data, m.Release); err != nil {
		return err
	}
	if envCopy != "" {
		fmt.Fprintf(os.Stderr, "restore: %s differs from the backup and this user can't write it, so it was left alone. The backup's copy is in %s\n", cfg.EnvFile, envCopy)
	}
	fmt.Printf("restored the backup of %s from %s\n", cmp.Or(m.Domain, m.Project), m.CreatedAt.Format(time.RFC3339))
	return nil
}

func restoreTarget(cfg *config.Config, dumpMajor int) (backup.Target, int, error) {
	t, err := backupTarget(cfg)
	var missing *pg.MissingError
	var broken *pg.BrokenError
	data := 0
	switch {
	case errors.As(err, &missing):
		data = missing.Major
	case errors.As(err, &broken):
		data = broken.Major
	case err == nil && t.PGMajor > 0 && dumpMajor > t.PGMajor:
		data = t.PGMajor
	default:
		return t, 0, err
	}
	installed := pg.Installed(pg.LibRoot, cfg.Env["FLUXER_POSTGRES_BIN_DIR"])
	if len(installed) == 0 || installed[0].Major < pg.MinMajor || err == nil && installed[0].Major < dumpMajor {
		return t, 0, err
	}
	t.PGBinDir, t.PGMajor = installed[0].BinDir, installed[0].Major
	return t, data, nil
}

func stageCluster(ctx context.Context, cf *configFlags, cfg *config.Config, target *backup.Target, archive string, aside int) (string, error) {
	if aside == 0 {
		return "", nil
	}
	if _, err := backup.Check(*target, archive); err != nil {
		return "", err
	}
	dir := cfg.Layout.PostgresDir()
	old := pg.OldDir(dir, aside)
	if _, err := os.Stat(old); err == nil {
		return "", fmt.Errorf("restore: %s from an earlier move is in the way. Delete it with sudo rexctl postgres upgrade --cleanup, or fluxerd postgres-upgrade --cleanup without rexctl, and try again", old)
	}
	staging := pg.StagingDir(dir)
	if err := os.RemoveAll(staging); err != nil {
		return "", err
	}
	cc, _, err := cf.children(cfg)
	if err != nil {
		return "", err
	}
	if err := children.PreparePostgresIn(ctx, cc, staging); err != nil {
		return "", errors.Join(err, removeStaging(staging))
	}
	target.PGDataDir = staging
	fmt.Fprintf(os.Stderr, "restore: the PostgreSQL %d database in %s can't take this backup here, so the backup goes into a new PostgreSQL %d database, and the old one moves to %s once the restore succeeds\n", aside, dir, target.PGMajor, old)
	return staging, nil
}

func removeStaging(staging string) error {
	if staging == "" {
		return nil
	}
	return os.RemoveAll(staging)
}

func prepareFreshRestore(ctx context.Context, cf *configFlags, cfg *config.Config) error {
	if secrets.HasData(cfg.Layout) {
		return nil
	}
	if _, err := secrets.Ensure(cfg.Layout, cfg.Env); err != nil {
		return err
	}
	cc, _, err := cf.children(cfg)
	if err != nil {
		return err
	}
	return children.PreparePostgres(ctx, cc)
}

func printReloadPlan(cf configFlags, against string, cc children.Config, specs []spec.Child, ec edge.Config) error {
	cf.envFile = against
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return err
	}
	oldCC, oldEdge, err := cf.children(cfg)
	if err != nil {
		return err
	}
	oldSpecs, err := children.Specs(oldCC)
	if err != nil {
		return err
	}
	oldNATS, err := children.NATS(oldCC)
	if err != nil {
		return err
	}
	nats, err := children.NATS(cc)
	if err != nil {
		return err
	}
	running, err := supervise.New(oldSpecs, supervise.Options{})
	if err != nil {
		return err
	}
	names, err := running.Plan(specs)
	restart := oldNATS != nats || errors.Is(err, supervise.ErrRestart)
	if err != nil && !restart {
		return err
	}
	fmt.Printf("reload: restart=%t edge=%t children=%s\n", restart, !reflect.DeepEqual(oldEdge, ec), strings.Join(names, ","))
	return nil
}

func cmdCheckConfig(args []string) error {
	fs := flag.NewFlagSet("check-config", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	against := fs.String("against", "", "env file of the running configuration, to print what a reload to this one restarts")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return err
	}
	if err := checkDomain(cfg); err != nil {
		return err
	}
	if _, err := parseSchedule(cfg.Env); err != nil {
		return err
	}
	if missing := secrets.Missing(cfg.Env); len(missing) > 0 && !voiceOnly(cfg) {
		if _, err := secrets.Mint(maps.Clone(cfg.Env), secrets.HasData(cfg.Layout)); err != nil {
			return err
		}
		fmt.Printf("the first run creates: %s\n", strings.Join(missing, ", "))
	}
	cc, ec, err := cf.children(cfg)
	if err != nil {
		return err
	}
	specs, err := children.Specs(cc)
	if err != nil {
		return err
	}
	if _, err := supervise.New(specs, supervise.Options{}); err != nil {
		return err
	}
	if *against != "" {
		if err := printReloadPlan(*cf, *against, cc, specs, ec); err != nil {
			return err
		}
	}
	for _, m := range edge.Modes {
		body, err := edge.Render(ec, m)
		if err != nil {
			return err
		}
		if _, err := edge.Adapt(body); err != nil {
			return err
		}
	}
	m, err := spec.ResolveModes(cfg.Env, cfg.Layout.Data)
	if err != nil {
		return err
	}
	for _, w := range children.Ignored(cfg.Env) {
		fmt.Printf("warning: %s\n", w)
	}
	if m.Storage == spec.StorageFilesystem {
		if w := children.ObjectsWarning(cfg.Layout); w != "" {
			fmt.Printf("warning: %s\n", w)
		}
	}
	fmt.Printf("configuration is valid: %d services, data in %s, %s=%s %s=%s %s=%s\n", len(specs), cfg.Layout.Data,
		spec.RustProcessEnv, m.Rust, spec.StorageBackendEnv, m.Storage, spec.KVModeEnv, m.KV)
	return nil
}
