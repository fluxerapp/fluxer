// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"cmp"
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/compose-spec/compose-go/v2/types"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func noNativeEquivalent(msg, fix string) error {
	return &ui.ExitError{Code: ui.ExitRefused, Msg: msg, Fix: fix, Err: migrate.ErrNoNativeEquivalent}
}

type moveBase struct {
	app       *App
	inst      *instance.Instance
	live      *engine.Project
	geoip     string
	geoipName string
	dir       string
	runtime   string
	chosen    map[string]string
	units     []migrate.SavedUnit
	mounts    []migrate.DataMount
	dryRun    bool
}

func (t *moveBase) Dir() string                 { return t.dir }
func (t *moveBase) Runtime() string             { return t.runtime }
func (t *moveBase) Units() []migrate.SavedUnit  { return t.units }
func (t *moveBase) Mounts() []migrate.DataMount { return t.mounts }

func (t *moveBase) preflight(ctx context.Context, chosen map[string]string) error {
	t.chosen = chosen
	if mode := chosen[settings.EdgeMode]; mode != "" && mode != settings.EdgeBundled && strings.TrimSpace(chosen[settings.EdgeBind]) == "" {
		t.chosen[settings.EdgeBind] = settings.DefaultBind
	}
	if url := backup.ExternalPostgresURL(t.inst); url != "" {
		t.chosen["FLUXER_POSTGRES_URL"] = url
	}
	if err := t.checkOldLayout(ctx); err != nil {
		return err
	}
	if len(t.inst.State.Voice) > 0 {
		ids := make([]string, len(t.inst.State.Voice))
		for i, v := range t.inst.State.Voice {
			ids[i] = v.ID
		}
		return noNativeEquivalent(fmt.Sprintf("The voice servers %s run in %s on this machine, and they can't move with it.", ui.JoinAnd(ids), engineName(t.inst)), "Remove them with rexctl voice remove first.")
	}
	if mode := chosen[settings.EdgeMode]; mode != "" && mode != settings.EdgeBundled {
		if _, err := native.ProxyListen(chosen[settings.EdgeBind]); err != nil {
			return noNativeEquivalent(fmt.Sprintf("edge.bind is %s, which isn't an address Fluxer can listen on.", chosen[settings.EdgeBind]), "Run rexctl config set edge.bind=127.0.0.1:9000 first, with the address your proxy sends to, and point your proxy there.")
		}
	}
	entries, err := os.ReadDir(t.dir)
	if err == nil && len(entries) > 0 {
		return ui.Refused(fmt.Sprintf("The folder for the moved instance, %s, already has files in it.", t.dir), "Pass --new-dir with an empty or new folder.")
	}
	if err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	for _, o := range instance.Others(t.dir) {
		if instance.RealPath(o.Dir) != instance.RealPath(t.inst.Dir) && o.Project() == t.inst.Project() && o.Moved() == nil {
			return ui.Refused(fmt.Sprintf("The Fluxer in %s already uses the name %s.", o.Dir, o.Project()), "Remove it with rexctl uninstall --dir "+shellWord(o.Dir)+" first.")
		}
	}
	return nil
}

func volumePin(short string, def any, project string) bool {
	if _, ok := stack.DataMounts[short]; !ok {
		return false
	}
	fields, ok := def.(map[string]any)
	if !ok || len(fields) != 2 || fields["external"] != true || fields["name"] != project+"_"+short {
		return false
	}
	return true
}

func (t *moveBase) checkOldLayout(ctx context.Context) error {
	custom, err := t.inst.Custom()
	if err != nil {
		return err
	}
	e, err := t.app.Engine(ctx, t.inst)
	if err == nil {
		t.live, err = deploy.Load(ctx, t.inst, e)
	}
	if err != nil {
		if t.dryRun {
			t.app.UI.Detail("reading the old containers: %v", err)
			return nil
		}
		return rt.ContainerError(err, "rexctl couldn't read how the old containers run, so it didn't change anything.")
	}
	stock, err := composeModel(ctx, e, t.inst, t.inst.State.Settings)
	if err != nil {
		return err
	}
	storage := slices.Contains(minimalModes(t.inst), migrate.OptStorage)
	r := &customReview{project: t.inst.Project(), dir: t.inst.Dir, chosen: t.inst.State.Settings, stock: stock.Model, live: t.live.Model, storage: storage,
		stockWith: func(changes map[string]string) (*types.Project, error) {
			p, err := composeModel(ctx, e, t.inst, merged(t.inst.State.Settings, changes))
			if err != nil {
				return nil, err
			}
			return p.Model, nil
		}}
	if err := r.read(custom); err != nil {
		return ui.Wrap(err, fmt.Sprintf("rexctl can't read %s.", instance.CustomCompose), "Fix the file with rexctl config edit --compose, then try again.")
	}
	r.ports()
	if err := r.refusal(); err != nil {
		return err
	}
	if t.mounts, err = checkedMounts(ctx, t.live); err != nil {
		return err
	}
	if storage {
		p, err := customComposeModel(ctx, e, t.inst, withOptions(t.inst.State.Settings, migrate.OptStorage))
		if err != nil {
			return err
		}
		if err := objectsMountProblem(p.Model); err != nil {
			return err
		}
	}
	return t.findGeoIP()
}

func (t *moveBase) findGeoIP() error {
	path := serviceEnv(t.live.Model, "api", settings.GeoIPVar)
	if path == "" || strings.HasPrefix(path, "s3://") {
		return nil
	}
	svc, _ := modelService(t.live.Model, "api")
	src := mountedFile(svc, path)
	if src == "" {
		return ui.Refused(fmt.Sprintf("%s is %s, and no folder on this machine is mounted there for the api.", settings.GeoIPVar, path),
			"Run rexctl config unset "+settings.GeoIPVar+" first, and set "+settings.GeoIPDatabase+" after the move.")
	}
	if err := checkGeoIPFile(src); err != nil {
		var ee *ui.ExitError
		if errors.As(err, &ee) {
			ee.Msg = fmt.Sprintf("%s points the api at %s on this machine. %s", settings.GeoIPVar, src, ee.Msg)
		}
		return err
	}
	name, err := geoIPName(src)
	if err != nil {
		return ui.Failed(fmt.Sprintf("Couldn't read %s, which %s points the api at.", src, settings.GeoIPVar), "Check that you can read it, then try again.")
	}
	t.geoip, t.geoipName = src, name
	t.withGeoIP(t.chosen)
	return nil
}

func (t *moveBase) withGeoIP(chosen map[string]string) {
	if t.geoip == "" || chosen == nil {
		return
	}
	delete(chosen, settings.GeoIPVar)
	chosen[settings.GeoIPDatabase] = settings.GeoIPMountPoint + t.geoipName
}

func (t *moveBase) carryGeoIP(target *instance.Instance) error {
	if t.geoip == "" {
		return nil
	}
	_, err := saveGeoIP(t.geoip, target.Path(instance.GeoIPDir), t.geoipName)
	return err
}

func (t *moveBase) savedUnitPaths() []string {
	var paths []string
	if host.BootUnitSupported() {
		if p, err := (host.BootUnit{Project: t.inst.Project(), Rootless: euid() != 0}).Path(); err == nil {
			paths = append(paths, p)
		}
	}
	if a, err := autoUpgradeFor(t.inst, nil); err == nil && a.Scheduler != "" {
		for _, list := range [][]string{must(a.Paths()), must(a.BackupPaths())} {
			paths = append(paths, list...)
		}
	}
	if r, err := rt.Open(context.Background(), t.inst, nil); err == nil {
		if s, err := backupScheduleFor(r, t.inst); err == nil && s.Scheduler != "" {
			paths = append(paths, must(s.Paths())...)
		}
	}
	return paths
}

func must(paths []string, err error) []string {
	if err != nil {
		return nil
	}
	return paths
}

func systemdUnit(path string) (string, bool) {
	name := filepath.Base(path)
	return name, strings.HasSuffix(name, ".service") || strings.HasSuffix(name, ".timer")
}

func systemctl(ctx context.Context, user bool, args ...string) error {
	if user {
		args = append([]string{"--user"}, args...)
	}
	return exec.CommandContext(ctx, "systemctl", args...).Run()
}

func (t *moveBase) saveUnits(ctx context.Context) error {
	t.units = nil
	user := euid() != 0
	for _, p := range t.savedUnitPaths() {
		data, err := os.ReadFile(p)
		if errors.Is(err, os.ErrNotExist) {
			t.units = append(t.units, migrate.SavedUnit{Path: p, User: user})
			continue
		}
		if err != nil {
			return err
		}
		enabled := false
		if name, ok := systemdUnit(p); ok {
			enabled = systemctl(ctx, user, "is-enabled", "--quiet", name) == nil
		}
		t.units = append(t.units, migrate.SavedUnit{Path: p, Data: data, Enabled: enabled, User: user})
	}
	return nil
}

func restoreUnits(ctx context.Context, units []migrate.SavedUnit) error {
	var errs []error
	reload := map[bool]bool{}
	for _, u := range units {
		if u.Data == nil {
			if err := os.Remove(u.Path); err != nil && !errors.Is(err, os.ErrNotExist) {
				errs = append(errs, err)
			}
		} else {
			errs = append(errs, envfile.WriteFileAtomic(u.Path, u.Data, false))
		}
		if _, ok := systemdUnit(u.Path); ok {
			reload[u.User] = true
		}
	}
	for user := range reload {
		errs = append(errs, systemctl(ctx, user, "daemon-reload"))
	}
	for _, u := range units {
		name, ok := systemdUnit(u.Path)
		if !u.Enabled || !ok {
			continue
		}
		args := []string{"enable", name}
		if strings.HasSuffix(name, ".timer") {
			args = []string{"enable", "--now", name}
		}
		errs = append(errs, systemctl(ctx, u.User, args...))
	}
	return errors.Join(errs...)
}

func (t *moveBase) install(ctx context.Context, runtimeFlag string) error {
	if err := t.saveUnits(ctx); err != nil {
		return ui.Wrap(err, "Couldn't keep a copy of the services that run the old containers.", "Run "+migrateCommand+" --verbose to see why.")
	}
	args := []string{"install", "--runtime", runtimeFlag, "--for-migrate", "--migrate-from", t.inst.Dir, "--dir", t.dir, "--project", t.inst.Project(), "--domain", t.inst.Domain(), "--setup", "browser", "--no-start", "--yes"}
	args = append(args, installNetworkArgs(t.inst.Domain(), t.chosen)...)
	if t.app.Flags.Verbose {
		args = append(args, "--verbose")
	}
	if err := t.selfQuiet(ctx, args...); err != nil {
		return installFailed(t.dir, err)
	}
	target, err := instance.Find(t.dir)
	if err != nil {
		return err
	}
	target.State.Settings = merged(target.State.Settings, t.chosen)
	return saveInstance(target)
}

func installNetworkArgs(domain string, chosen map[string]string) []string {
	mode := chosen[settings.EdgeMode]
	if mode == "" || mode == settings.EdgeBundled {
		return nil
	}
	bind := chosen[settings.EdgeBind]
	args := []string{"--bind", bind}
	kind := chosen[settings.EdgeProxyKind]
	if kind == "" && mode == settings.EdgeTunnel {
		kind = settings.ProxyTunnel
	}
	switch {
	case kind == settings.ProxyLocal || kind == settings.ProxyTunnel:
		return append(args, "--proxy", kind)
	case kind == settings.ProxyRemote || !settings.BindIsLoopback(bind) && !checks.OverlayDomain(domain):
		return append(args, "--proxy", settings.ProxyRemote, "--proxy-from", migratedProxyFrom(chosen[settings.EdgeTrusted]))
	}
	return args
}

func installFailed(dir string, err error) error {
	msg := fmt.Sprintf("Couldn't set up the new instance in %s.", dir)
	fix := "Run " + migrateCommand + " --verbose to see why."
	var child *childError
	if errors.As(err, &child) {
		msg += " " + child.Message
		fix = cmp.Or(child.Fix, fix)
	}
	return ui.Wrap(err, msg, fix)
}

func (t *moveBase) open(chosen map[string]string) (*instance.Instance, error) {
	target, err := instance.Find(t.dir)
	if err != nil {
		return nil, err
	}
	t.withGeoIP(chosen)
	target.State.Settings = merged(target.State.Settings, chosen)
	target.State.BackupDir = cmpDir(t.inst.State.BackupDir, t.inst.BackupDir())
	target.State.KeepBackups = t.inst.State.KeepBackups
	target.State.AdminImport = t.inst.State.AdminImport
	target.State.NotifyURL = t.inst.State.NotifyURL
	target.State.MovedFrom = &instance.MovedFrom{Dir: t.inst.Dir, At: time.Now().UTC().Truncate(time.Second), Firewall: slices.Clone(t.inst.State.Firewall), Backup: t.inst.State.MigrateBackup}
	for _, p := range t.inst.State.Firewall {
		if !slices.Contains(target.State.Firewall, p) {
			target.State.Firewall = append(target.State.Firewall, p)
		}
	}
	return target, saveInstance(target)
}

func keepRules(inst *instance.Instance, rules []instance.OpenedPort) error {
	n := len(inst.State.Firewall)
	inst.State.Firewall = slices.DeleteFunc(inst.State.Firewall, func(p instance.OpenedPort) bool { return slices.Contains(rules, p) })
	if len(inst.State.Firewall) == n {
		return nil
	}
	return saveInstance(inst)
}

func cmpDir(set, fallback string) string {
	if set != "" {
		return set
	}
	return fallback
}

func (t *moveBase) Discard(ctx context.Context) error {
	var errs []error
	if target, err := instance.Find(t.dir); err == nil {
		errs = append(errs, keepRules(target, t.inst.State.Firewall))
		if err := t.selfQuiet(ctx, "uninstall", "--dir", t.dir, "--confirm", target.Domain(), "--delete-backups"); err != nil {
			errs = append(errs, err)
		}
	}
	t.app.forget(t.dir)
	errs = append(errs, os.RemoveAll(t.dir), restoreUnits(ctx, t.units))
	return errors.Join(errs...)
}

func (t *moveBase) databaseReach(err error, who, instead, to string) error {
	if errors.Is(err, context.Canceled) {
		return err
	}
	if errors.Is(err, rt.ErrImageDigest) {
		return rt.ContainerError(err, "rexctl couldn't check your database.")
	}
	var far *rt.PostgresUnreachable
	if !errors.As(err, &far) {
		if err != nil {
			t.app.UI.Detail("checking the database: %v", err)
		}
		return nil
	}
	t.app.UI.Detail("%s", far.Reason)
	return &ui.ExitError{
		Code: ui.ExitRefused,
		Msg:  far.Sentence(who) + " Fluxer would start on it during the move, and the move can't be undone after that.",
		Fix:  "Point FLUXER_POSTGRES_URL at " + instead + ", with rexctl config set, then run " + migrateCommand + " --to " + to + " again.",
		Err:  err,
	}
}

func (t *moveBase) OneWay() bool { return oneWayMove(t.inst, stack.EmbeddedRelease()) }

func oneWayMove(old *instance.Instance, to string) bool {
	database, _ := backup.ExternalData(old)
	return database && release.Compare(to, old.State.Release) > 0
}

const selfWaitDelay = 10 * time.Minute

func (t *moveBase) selfQuiet(ctx context.Context, args ...string) error {
	var report string
	var env []string
	if f, err := os.CreateTemp("", "rexctl-error-*.json"); err == nil {
		report = f.Name()
		_ = f.Close()
		defer os.Remove(report)
		env = []string{childErrorEnv + "=" + report}
	}
	if t.app.Flags.Verbose {
		return childFailure(t.self(ctx, migrateOutput(t.app), env, args...), report)
	}
	var out bytes.Buffer
	err := t.self(ctx, &out, env, args...)
	if err != nil {
		_, _ = out.WriteTo(migrateOutput(t.app))
	}
	return childFailure(err, report)
}

func (t *moveBase) self(ctx context.Context, w io.Writer, env []string, args ...string) error {
	bin, err := os.Executable()
	if err != nil {
		return err
	}
	c := exec.CommandContext(ctx, bin, args...)
	c.Cancel = func() error { return c.Process.Signal(os.Interrupt) }
	c.WaitDelay = selfWaitDelay
	c.Stdin = nil
	c.Stdout, c.Stderr = w, w
	if len(env) > 0 {
		c.Env = append(os.Environ(), env...)
	}
	return c.Run()
}

type nativeTarget struct {
	moveBase
	rel *release.Release
}

func newNativeTarget(app *App, inst *instance.Instance, dir string, dryRun bool) *nativeTarget {
	return &nativeTarget{moveBase: moveBase{app: app, inst: inst, dir: dir, runtime: instance.RuntimeNative, dryRun: dryRun}}
}

func (t *nativeTarget) Preflight(ctx context.Context, chosen map[string]string) error {
	if !t.dryRun {
		if err := t.app.nativeRoot(t.inst.Dir); err != nil {
			return err
		}
	}
	if err := nativeFolder(t.dir, "--new-dir"); err != nil {
		return err
	}
	t.dir = instance.RealPath(t.dir)
	if err := rt.NativeAlone(ctx); err != nil {
		return err
	}
	if err := t.preflight(ctx, chosen); err != nil {
		return err
	}
	if t.chosen["FLUXER_POSTGRES_URL"] == "" {
		if err := postgresFits(postgresRef(t.inst.State), native.ExpectedPostgres(ctx), toOneContainer(t.inst)); err != nil {
			return err
		}
	} else if t.OneWay() {
		if err := t.databaseReach(rt.CheckPostgresFromHost(ctx, t.chosen["FLUXER_POSTGRES_URL"]), "This machine", "an address this machine can connect to", runtimeSystemd); err != nil {
			return err
		}
	}
	rel, err := nativeRelease(ctx, stack.EmbeddedRelease(), "Move it to one container with "+toOneContainer(t.inst)+" instead.")
	t.rel = rel
	return err
}

func downloadDir(dir string) string {
	return filepath.Join(filepath.Dir(dir), "."+filepath.Base(dir)+"-download")
}

const downloadIdle = 10 * time.Minute

func leftDownloads(inst *instance.Instance, now time.Time) []string {
	dirs := []string{downloadDir(inst.Dir)}
	if legacyCompose(inst) {
		dirs = append(dirs, downloadDir(defaultNativeDir(inst)))
	}
	var out []string
	for _, d := range dirs {
		if !slices.Contains(out, d) && idleSince(d, now.Add(-downloadIdle)) {
			out = append(out, d)
		}
	}
	return out
}

func idleSince(dir string, since time.Time) bool {
	if info, err := os.Lstat(dir); err != nil || !info.IsDir() {
		return false
	}
	busy := errors.New("written lately")
	err := filepath.WalkDir(dir, func(_ string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		info, err := d.Info()
		if err != nil {
			return err
		}
		if info.ModTime().After(since) {
			return busy
		}
		return nil
	})
	return err == nil
}

func (t *nativeTarget) download() *native.Stack {
	return native.New(&instance.Instance{Dir: downloadDir(t.dir)})
}

func (t *nativeTarget) Prepare(ctx context.Context) error {
	d, version := t.download(), t.rel.Version
	err := t.app.UI.Step(ctx, "Downloading Fluxer "+version, func(ctx context.Context, s *ui.Step) error {
		if _, err := d.Fetch(ctx, t.rel, s.Progress); err != nil {
			_ = os.RemoveAll(d.Inst.Dir)
			return rt.NativeError(err, "Couldn't download Fluxer "+version+". rexctl didn't change anything.")
		}
		s.Done("Downloaded Fluxer %s.", version)
		return nil
	})
	if err != nil || dpkgInterrupted(ctx) {
		return err
	}
	return t.app.UI.Step(ctx, "Downloading the system packages Fluxer needs", func(ctx context.Context, s *ui.Step) error {
		if err := d.DownloadPackages(ctx, version, s); err != nil {
			return rt.NativeError(err, "Couldn't download the system packages Fluxer needs. rexctl didn't change anything.")
		}
		s.Done("The system packages Fluxer needs are on this machine.")
		return nil
	})
}

func (t *nativeTarget) adoptDownload() {
	d := t.download()
	defer os.RemoveAll(d.Inst.Dir)
	version := stack.EmbeddedRelease()
	if !d.Has(version) {
		return
	}
	n := native.New(&instance.Instance{Dir: t.dir})
	err := os.MkdirAll(n.ReleasesDir(), 0o755)
	if err == nil {
		err = os.Rename(d.ReleaseDir(version), n.ReleaseDir(version))
	}
	if err != nil {
		t.app.UI.Detail("moving the download of Fluxer %s: %v", version, err)
	}
}

func postgresFits(ref string, here int, move string) error {
	from, err := stack.PostgresMajor(ref)
	if err != nil || here == 0 || from <= here {
		return nil
	}
	return ui.Refused(fmt.Sprintf("Your database is PostgreSQL %d, and Fluxer on this machine would run PostgreSQL %d, which can't restore it.", from, here),
		fmt.Sprintf("Install %s from apt.postgresql.org first, or move to one container with %s.", native.PackageName(from), move))
}

func (t *nativeTarget) Install(ctx context.Context) error {
	t.adoptDownload()
	return t.install(ctx, runtimeSystemd)
}

func (t *nativeTarget) Restore(ctx context.Context, archive string) error {
	target, err := instance.Find(t.dir)
	if err != nil {
		return err
	}
	set, err := deploy.ReleaseSet(target)
	if err != nil {
		return err
	}
	n := native.New(target)
	if err := n.Restore(ctx, set, archive, migrateOutput(t.app)); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't restore the backup into %s.", t.dir), "Run "+migrateCommand+" --verbose to see why.")
	}
	if err := t.carryGeoIP(target); err != nil {
		return err
	}
	return adoptNativeSecrets(target, n)
}

func (t *nativeTarget) Open(ctx context.Context, chosen map[string]string) (migrate.Host, error) {
	target, err := t.open(chosen)
	if err != nil {
		return nil, err
	}
	if rt.KindOf(target) != rt.Native {
		return nil, ui.Failed(fmt.Sprintf("%s holds no native instance after the restore.", t.dir), "Run "+migrateCommand+" --verbose to see why.")
	}
	return migrateHost(ctx, t.app, target)
}

type containerTarget struct {
	moveBase
	engine engine.Kind
}

func newContainerTarget(app *App, inst *instance.Instance, dir string, kind engine.Kind, dryRun bool) *containerTarget {
	return &containerTarget{moveBase: moveBase{app: app, inst: inst, dir: dir, runtime: instance.RuntimeContainer, dryRun: dryRun}, engine: kind}
}

func (t *containerTarget) Preflight(ctx context.Context, chosen map[string]string) error {
	if inst := t.inst; !t.dryRun && euid() != 0 && rootOwned(inst) {
		return ui.Refused("Root manages this instance, so the move runs as root too.", "Run "+sudoAgain(inst.Dir)+".")
	}
	if err := t.preflight(ctx, chosen); err != nil {
		return err
	}
	if !engineOnMachine(t.engine) {
		return ui.Refused(fmt.Sprintf("%s isn't on this machine.", t.engine.Name()), fmt.Sprintf("Install %s, or pick the engine the instance runs on now with --to %s.", t.engine.Name(), cmpDir(t.inst.State.Engine.Kind, string(engine.Docker))))
	}
	if _, err := resolveRelease(ctx, stack.EmbeddedRelease()); err != nil {
		return err
	}
	if !t.dryRun || !t.checksDatabase() {
		return nil
	}
	e, err := t.connect(ctx)
	if err != nil {
		t.app.UI.Detail("checking the database from a new container: %v", err)
		return nil
	}
	return t.checkDatabase(ctx, e)
}

func (t *containerTarget) checksDatabase() bool {
	return t.chosen["FLUXER_POSTGRES_URL"] != "" && t.OneWay()
}

func (t *containerTarget) connect(ctx context.Context) (*engine.Engine, error) {
	e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: t.engine, AllowOld: true})
	if err == nil && e.Kind != t.engine {
		return nil, &engine.Error{Op: "connect", Class: engine.ClassUnreachable, Err: fmt.Errorf("%s doesn't respond", t.engine.Name())}
	}
	return e, err
}

func (t *containerTarget) Prepare(ctx context.Context) error {
	e, err := t.connect(ctx)
	if err != nil {
		return rt.EngineError(t.engine, err)
	}
	version := stack.EmbeddedRelease()
	_, had, err := e.LocalImage(ctx, rt.ImageRef(version))
	had = had || err != nil
	err = t.app.UI.Step(ctx, "Downloading Fluxer "+version, func(ctx context.Context, s *ui.Step) error {
		if err := rt.FetchImage(ctx, e, version, t.app.progress(s, "Downloading")); err != nil {
			return err
		}
		s.Done("Downloaded Fluxer %s.", version)
		return nil
	})
	if err != nil {
		return rt.ContainerError(err, "Couldn't download Fluxer "+version+". rexctl didn't change anything.")
	}
	if !t.checksDatabase() {
		return nil
	}
	err = t.checkDatabase(ctx, e)
	if err != nil && !had {
		if rerr := e.RemoveImages(context.WithoutCancel(ctx), []string{rt.ImageRef(version)}); rerr != nil {
			t.app.UI.Detail("removing the download of Fluxer %s: %v", version, rerr)
		}
	}
	return err
}

func (t *containerTarget) checkDatabase(ctx context.Context, e *engine.Engine) error {
	return t.app.UI.Check(ctx, "Checking that a new container can connect to your database", func(ctx context.Context, s *ui.Step) error {
		return t.reportDatabaseCheck(s, rt.CheckPostgresFromContainer(ctx, e, stack.EmbeddedRelease(), t.chosen["FLUXER_POSTGRES_URL"]))
	})
}

func (t *containerTarget) reportDatabaseCheck(s *ui.Step, err error) error {
	if errors.Is(err, rt.ErrNoImage) {
		s.Warned("Skipped the database check, because Fluxer %s isn't on this machine yet and a dry run downloads nothing.", stack.EmbeddedRelease())
		return nil
	}
	if refused := t.databaseReach(err, "A new container", "an address containers on this machine can connect to, such as "+hostFromContainer(t.engine), string(t.engine)); refused != nil {
		return refused
	}
	if err != nil {
		s.Warned("Couldn't check that a new container can connect to your database.")
		return nil
	}
	s.Done("A new container can connect to your database.")
	return nil
}

func hostFromContainer(kind engine.Kind) string {
	if kind == engine.Podman {
		return "host.containers.internal"
	}
	return "the machine's own IP address"
}

func rootOwned(inst *instance.Instance) bool {
	uid, ok := inst.OwnerUID()
	return ok && uid == 0
}

func (t *containerTarget) Install(ctx context.Context) error {
	return t.install(ctx, string(t.engine))
}

func (t *containerTarget) opened(ctx context.Context) (*instance.Instance, rt.Runtime, error) {
	target, err := instance.Find(t.dir)
	if err != nil {
		return nil, nil, err
	}
	r, err := t.app.Runtime(ctx, target)
	return target, r, err
}

func (t *containerTarget) Restore(ctx context.Context, archive string) error {
	target, r, err := t.opened(ctx)
	if err != nil {
		return err
	}
	err = t.app.UI.Step(ctx, "Restoring the backup into one container", func(ctx context.Context, s *ui.Step) error {
		if err := r.Backups().Restore(ctx, s, archive); err != nil {
			return err
		}
		s.Done("Restored the backup into one container.")
		return nil
	})
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't restore the backup into %s.", t.dir), "Run "+migrateCommand+" --verbose to see why.")
	}
	if err := t.carryGeoIP(target); err != nil {
		return err
	}
	return adoptSecrets(ctx, target, r)
}

func (t *containerTarget) Open(ctx context.Context, chosen map[string]string) (migrate.Host, error) {
	target, err := t.open(chosen)
	if err != nil {
		return nil, err
	}
	if e, err := t.app.Engine(ctx, t.inst); err == nil {
		if err := rt.RemoveComposeUnit(ctx, t.inst, e); err != nil {
			t.app.UI.Detail("removing the service that started the old containers: %v", err)
		}
	}
	return &containerHost{app: t.app, inst: target}, nil
}
