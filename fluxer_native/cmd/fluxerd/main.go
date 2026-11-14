// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"cmp"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"maps"
	"net/http"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
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
	"github.com/fluxerapp/fluxer/fluxer_native/internal/supervise"
)

const usage = `usage: fluxerd <command> [flags]

commands:
  init                         create the data directory and secrets, print the setup token
  run                          start every service and supervise it
  health                       exit 0 when every service is ready
  ps [--json]                  list services
  ctl start|stop|restart <svc> control one service
  logs <svc> [-f]              show the recent output of one service
  exec <svc> -- <cmd...>       run a command with the environment of one service
  psql [args...]               open psql on the bundled database
  backup [--dir d] [--pre-upgrade] [--include d]
  restore <archive>
  check-config                 validate the configuration without starting anything
  print-env <svc>              print the environment one service gets
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
		"init":         cmdInit,
		"health":       cmdHealth,
		"ps":           cmdPs,
		"ctl":          cmdCtl,
		"logs":         cmdLogs,
		"exec":         cmdExec,
		"psql":         cmdPsql,
		"backup":       cmdBackup,
		"restore":      cmdRestore,
		"check-config": cmdCheckConfig,
		"print-env":    cmdPrintEnv,
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

func (e exitError) Error() string { return "exit " + strconv.Itoa(int(e)) }

type configFlags struct {
	envFile, caddyDir, certsDir, root string
}

func addConfigFlags(fs *flag.FlagSet) *configFlags {
	f := &configFlags{}
	fs.StringVar(&f.envFile, "config", "", "env file (default "+spec.DefaultEnvFile+")")
	fs.StringVar(&f.caddyDir, "caddy-dir", "", "Caddy drop-in directory (default "+spec.DefaultCaddyDir+")")
	fs.StringVar(&f.certsDir, "certs-dir", "", "certificate directory (default "+spec.DefaultCertsDir+")")
	fs.StringVar(&f.root, "root", "", "release directory (default: two levels above this binary)")
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

func pgBinDir(cfg *config.Config) string {
	return cmp.Or(cfg.Env["FLUXER_POSTGRES_BIN_DIR"], pg.DefaultBinDir)
}

func (f *configFlags) load() (*config.Config, error) {
	return config.Load(config.Options{EnvFile: f.envFile, CaddyDir: f.caddyDir, CertsDir: f.certsDir, Environ: os.Environ()})
}

func (f *configFlags) loadWithSecrets() (*config.Config, error) {
	cfg, err := f.load()
	if err != nil {
		return nil, err
	}
	return cfg, secrets.Fill(cfg.Layout, cfg.Env)
}

func socketFlag(fs *flag.FlagSet) *string {
	def := spec.LayoutFromEnv(os.Getenv).Runtime
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

func runDaemon(args []string, out *logs.Output) (int, error) {
	fs := flag.NewFlagSet("run", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	if err := fs.Parse(args); err != nil {
		return 2, nil
	}
	cfg, err := cf.load()
	if err != nil {
		return 1, err
	}
	l := cfg.Layout
	if err := os.MkdirAll(l.Data, 0o700); err != nil {
		return 1, err
	}
	if err := os.MkdirAll(l.Runtime, 0o750); err != nil {
		return 1, err
	}
	lk, err := lock.Acquire(l.LockFile())
	if err != nil {
		return 1, err
	}
	defer lk.Release()
	modes, err := config.EnsureModes(cfg, !secrets.HasData(l))
	if err != nil {
		return 1, err
	}
	if len(modes) > 0 {
		out.Logf(logs.PrioInfo, "new data directory, wrote %s to %s", modeList(modes), l.ModesFile())
	}
	minted, err := secrets.Ensure(l, cfg.Env)
	if err != nil {
		return 1, err
	}
	if len(minted) > 0 {
		out.Logf(logs.PrioInfo, "created %d secrets in %s", len(minted), l.SecretsFile())
	}
	if slices.Contains(minted, secrets.SetupToken) {
		out.Logf(logs.PrioInfo, "setup token for the first admin account: %s", cfg.Env[secrets.SetupToken])
	}
	cc, edgeCfg, err := cf.children(cfg)
	if err != nil {
		return 1, err
	}
	specs, err := children.Specs(cc)
	if err != nil {
		return 1, err
	}
	if err := otherInstance(l, specs); err != nil {
		return 1, err
	}
	if err := edge.Load(edgeCfg, false); err != nil {
		return 1, err
	}
	var edgeMu sync.Mutex
	edgeStopped := false
	stopEdge := func() {
		edgeMu.Lock()
		defer edgeMu.Unlock()
		if edgeStopped {
			return
		}
		edgeStopped = true
		notify.Stopping()
		if err := edge.Stop(); err != nil {
			out.Logf(logs.PrioWarning, "edge stop: %v", err)
		}
	}
	defer stopEdge()
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

	var ready atomic.Bool
	var readyErr atomic.Value
	sup, err := supervise.New(specs, supervise.Options{
		Out: out,
		OnReady: func() {
			edgeMu.Lock()
			defer edgeMu.Unlock()
			if edgeStopped {
				return
			}
			if err := edge.Load(edgeCfg, true); err != nil {
				readyErr.Store(err)
				cancel()
				return
			}
			ready.Store(true)
			if err := notify.Ready(); err != nil {
				out.Logf(logs.PrioWarning, "sd_notify READY: %v", err)
			}
		},
	})
	if err != nil {
		return 1, err
	}

	natsOpts, err := children.NATS(cc)
	if err != nil {
		return 1, err
	}
	nats, err := natsembed.Start(natsOpts)
	if err != nil {
		return 1, err
	}
	defer func() {
		nats.Shutdown()
		nats.WaitForShutdown()
	}()

	ln, err := ctl.Listen(l.ControlSocket())
	if err != nil {
		return 1, err
	}
	srv := &http.Server{Handler: ctl.Handler(sup, nats.SetJetStream)}
	go srv.Serve(ln)
	defer srv.Close()

	sigs := make(chan os.Signal, 4)
	signal.Notify(sigs, syscall.SIGHUP, syscall.SIGTERM, syscall.SIGINT)
	defer signal.Stop(sigs)
	go func() {
		for sig := range sigs {
			if sig == syscall.SIGHUP {
				out.Logf(logs.PrioInfo, "SIGHUP: reloading the edge configuration")
				edgeMu.Lock()
				if !edgeStopped {
					if err := edge.Load(edgeCfg, ready.Load()); err != nil {
						out.Logf(logs.PrioErr, "edge reload: %v", err)
					}
				}
				edgeMu.Unlock()
				continue
			}
			out.Logf(logs.PrioInfo, "%s: shutting down", sig)
			stopEdge()
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
	return 0, runErr
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

func cmdHealth(args []string) error {
	fs := flag.NewFlagSet("health", flag.ContinueOnError)
	runtimeDir := socketFlag(fs)
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	h, err := client(*runtimeDir).Health(ctx)
	if err != nil {
		return err
	}
	if h.Ready && len(h.Degraded) > 0 {
		fmt.Println("ready, but not healthy inside the Rust process: " + strings.Join(h.Degraded, ", "))
		return nil
	}
	if h.Ready {
		fmt.Println("ready")
		return nil
	}
	var parts []string
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
	runtimeDir := socketFlag(fs)
	asJSON := fs.Bool("json", false, "print JSON")
	unit := fs.String("unit", "fluxerd.service", "systemd unit whose restart count to report")
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
	return nil
}

func systemdRestarts(ctx context.Context, unit string) (int, bool) {
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
		os.Chdir(c.Cwd)
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
	if err := fs.Parse(args); err != nil {
		return exitError(2)
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
	psql := filepath.Join(pgBinDir(cfg), "psql")
	if _, err := os.Stat(psql); err != nil {
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

func backupTarget(cfg *config.Config) backup.Target {
	return backup.Target{Layout: cfg.Layout, Env: cfg.Env, PGBinDir: pgBinDir(cfg)}
}

func cmdBackup(args []string) error {
	fs := flag.NewFlagSet("backup", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	dir := fs.String("dir", "", "directory for the archive (default <data>/backups)")
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
	root, err := children.ReleaseRoot(cf.root)
	if err != nil {
		return err
	}
	if *preUpgrade && *reason == "" {
		*reason = "pre-upgrade"
	}
	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	var sup backup.Supervisor
	c := client(cfg.Layout.Runtime)
	if _, err := c.PS(ctx); err == nil {
		sup = c
	}
	path, err := backup.Create(ctx, backupTarget(cfg), sup, backup.CreateOptions{
		Dir:      *dir,
		EnvFile:  cfg.EnvFile,
		Include:  *include,
		Release:  releaseVersion(root),
		Reason:   *reason,
		Progress: func(stage string) { fmt.Fprintf(os.Stderr, "backup: %s\n", stage) },
	})
	if err != nil {
		return err
	}
	fmt.Println(path)
	return nil
}

func cmdRestore(args []string) error {
	fs := flag.NewFlagSet("restore", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	if fs.NArg() != 1 {
		return errors.New("usage: fluxerd restore <archive>")
	}
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return err
	}
	ctx, cancel := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer cancel()
	if err := prepareFreshRestore(ctx, cf, cfg); err != nil {
		return err
	}
	m, envCopy, err := backup.Restore(ctx, backupTarget(cfg), fs.Arg(0), backup.RestoreOptions{
		EnvFile:  cfg.EnvFile,
		Progress: func(stage string) { fmt.Fprintf(os.Stderr, "restore: %s\n", stage) },
	})
	if err != nil {
		return err
	}
	if envCopy != "" {
		fmt.Fprintf(os.Stderr, "restore: %s differs from the backup and this user can't write it, so it was left alone. The backup's copy is in %s\n", cfg.EnvFile, envCopy)
	}
	fmt.Printf("restored the backup of %s from %s\n", cmp.Or(m.Domain, m.Project), m.CreatedAt.Format(time.RFC3339))
	return nil
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

func cmdCheckConfig(args []string) error {
	fs := flag.NewFlagSet("check-config", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	cfg, err := cf.loadWithSecrets()
	if err != nil {
		return err
	}
	if missing := secrets.Missing(cfg.Env); len(missing) > 0 {
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
	for _, ready := range []bool{false, true} {
		body, err := edge.Render(ec, ready)
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
	if m.Storage == spec.StorageFilesystem {
		if w := children.ObjectsWarning(cfg.Layout); w != "" {
			fmt.Printf("warning: %s\n", w)
		}
	}
	fmt.Printf("configuration is valid: %d services, data in %s, %s=%s %s=%s %s=%s\n", len(specs), cfg.Layout.Data,
		spec.RustProcessEnv, m.Rust, spec.StorageBackendEnv, m.Storage, spec.KVModeEnv, m.KV)
	return nil
}
