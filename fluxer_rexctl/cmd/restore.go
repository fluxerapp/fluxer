// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

const restoreReadyTimeout = 10 * time.Minute

type restoreFlags struct {
	confirm  string
	engine   string
	runtime  string
	native   bool
	noSafety bool
	identity string
}

func newRestoreCmd(app *App) *cobra.Command {
	var f restoreFlags
	c := &cobra.Command{
		Use:   "restore [file]",
		Short: "Restore your instance from a backup",
		Long:  "Restore your instance from a backup. This works on a new machine too. Copy the backup file there and run this command. Without a file, rexctl lists your backups to pick from. Before it replaces an instance, rexctl backs it up first. A backup without uploads restores only on the instance it was made for, and the uploads there stay as they are. A backup from containers restores into a systemd service and the other way round. Pass --runtime systemd to set up the new instance as a systemd service.",
		Example: "rexctl restore\n" +
			"rexctl restore chat.example.org-20261114-1612.tar.zst\n" +
			"rexctl restore /mnt/backups/fluxer.tar.zst --confirm chat.example.org",
		Args: maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			arg := ""
			if len(args) == 1 {
				arg = args[0]
			}
			return runRestore(c.Context(), app, arg, f)
		},
	}
	confirmFlag(c, &f.confirm)
	c.Flags().StringVar(&f.engine, "engine", "", "")
	_ = c.Flags().MarkHidden("engine")
	c.Flags().StringVar(&f.runtime, "runtime", "", "run a new instance from the backup as a systemd service (systemd)")
	c.Flags().BoolVar(&f.native, "experimental-native", false, "")
	_ = c.Flags().MarkHidden("experimental-native")
	c.Flags().BoolVar(&f.noSafety, "no-safety-backup", false, "don't back up the current instance before replacing it")
	c.Flags().StringVar(&f.identity, "age-identity", "", "age key file that decrypts a backup ending in .age")
	return c
}

func ownEngine() string {
	for _, tool := range []string{runtimeDocker, runtimePodman} {
		if _, err := exec.LookPath(tool); err == nil {
			return tool
		}
	}
	return ""
}

func freshRestoreFix(path string) string {
	fix := "To restore it as a systemd service, run sudo rexctl restore " + shellWord(path) + "."
	if tool := ownEngine(); tool != "" {
		fix += fmt.Sprintf(" To restore into one container under this account, run rexctl install --runtime %s first, then rexctl restore %s.", tool, shellWord(path))
	}
	return fix
}

func keptBackupsLine(dir string) string {
	if dir == "" {
		def, err := instance.DefaultDir()
		if err != nil {
			return "Pass the backup file to restore."
		}
		dir = def
	}
	list, err := backup.List(filepath.Join(dir, instance.BackupsDir))
	if err != nil || len(list) == 0 {
		return "Pass the backup file to restore."
	}
	return fmt.Sprintf("Pass the backup file to restore. The newest in %s is %s.", filepath.Join(dir, instance.BackupsDir), list[0].Path)
}

func pickBackup(ctx context.Context, app *App) (string, error) {
	inst, err := app.Instance()
	if err != nil {
		dir, _ := app.Dir()
		return "", ui.Usage(keptBackupsLine(dir))
	}
	fetchOwnBackups(ctx, app, inst)
	list, err := backup.List(inst.BackupDir())
	if err != nil || len(list) == 0 {
		return "", ui.Refused(fmt.Sprintf("There are no backups in %s.", inst.BackupDir()), "Pass the backup file to restore.")
	}
	if !app.UI.Interactive() {
		return "", ui.Usage("Pass the backup file to restore. Run rexctl backup list to see your backups.")
	}
	items := make([]ui.PickItem, 0, len(list))
	for _, b := range list {
		hint := fmt.Sprintf("%s, %s", fluxerName(b.Release), ui.Bytes(b.SizeBytes))
		if r := backupReason(b); r != "" {
			hint += ", " + r
		}
		items = append(items, ui.PickItem{Value: b.Path, Label: backupTime(b.CreatedAt), Hint: hint})
	}
	return app.UI.Pick("Which backup do you want to restore?", items, list[0].Path)
}

func databaseCantStart(inst *instance.Instance) bool {
	if rt.KindOf(inst) != rt.Native || inst.State.Settings["FLUXER_POSTGRES_URL"] != "" {
		return false
	}
	p := native.New(inst).Postgres()
	return p.Broken || p.HasData() && !p.DataInstalled()
}

func safetyBackup(ctx context.Context, app *App, inst *instance.Instance, withoutUploads, skip bool) (string, error) {
	if skip {
		return "", nil
	}
	if inst.State.Restoring != "" {
		app.UI.Warn("A restore didn't finish, so rexctl doesn't back up the partly restored data first.", "")
		return "", nil
	}
	if databaseCantStart(inst) {
		app.UI.Warn("The database can't start, so rexctl can't back up the current instance before the restore.", "The restore replaces the database.")
		return "", nil
	}
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return "", err
	}
	o := rt.BackupOptions{Reason: "pre-restore", WithoutUploads: withoutUploads}
	plan, err := r.Backups().Plan(ctx, o)
	if err == nil && plan.FreeBytes >= 0 && plan.FreeBytes < plan.NeedBytes {
		app.UI.Warn(fmt.Sprintf("There isn't room to back up %s before the restore. It needs about %s and %s is free.", inst.Domain(), ui.Bytes(plan.NeedBytes), ui.Bytes(plan.FreeBytes)), "")
		ok, err := app.UI.ConfirmWith(ui.Question{Text: "Restore without a backup of the current instance?", Flag: "no-safety-backup"})
		if err != nil {
			return "", err
		}
		if !ok {
			return "", errDeclined
		}
		return "", nil
	}
	var known *rt.BackupPlan
	if err == nil {
		known = &plan
	}
	path, err := backupWith(ctx, app, inst, r, o, known, "Backing up your instance before the restore")
	var again *rt.StartAgainError
	var ee *ui.ExitError
	switch {
	case errors.Is(err, backup.ErrNeverStarted):
		return "", nil
	case errors.As(err, &again), errors.As(err, &ee) && ee.Code == ui.ExitInterrupted, errors.Is(err, context.Canceled):
		return "", err
	case err != nil:
		return "", ui.Wrap(err, "Couldn't back up the current instance, so rexctl didn't restore anything.", "Fix the problem above, or pass --no-safety-backup to restore without it.")
	}
	app.UI.Card(ui.Card{
		Title:    "Safety copy",
		Body:     []string{"rexctl saved how your instance looks now. If the restore goes wrong, this command puts it back."},
		Commands: []string{"rexctl restore " + shellWord(path)},
	})
	return path, nil
}

func withSafety(err error, path string) error {
	if err == nil || path == "" {
		return err
	}
	back := "To go back to how it was, run rexctl restore " + shellWord(path) + "."
	var ee *ui.ExitError
	if errors.As(err, &ee) && ee.Code == ui.ExitRefused {
		return err
	}
	if errors.As(err, &ee) {
		ee.Fix = strings.TrimSpace(ee.Fix + " " + back)
		return ee
	}
	return ui.Wrap(err, "Couldn't restore your instance.", back)
}

func runRestore(ctx context.Context, app *App, arg string, f restoreFlags) error {
	confirm, engineFlag := f.confirm, f.engine
	switch {
	case f.native:
		aliasLine(app, "--experimental-native", "--runtime systemd")
	case f.runtime == runtimeSystemd:
		f.native = true
	case f.runtime != "":
		return ui.Usage("rexctl restore takes --runtime systemd. To restore into one container, run rexctl install --runtime " + f.runtime + " first, then rexctl restore.")
	}
	if engineFlag != "" && engineFlag != string(engine.Docker) {
		return ui.Usage(fmt.Sprintf("rexctl can restore onto docker, not %s.", engineFlag))
	}
	toDocker := engineFlag == string(engine.Docker)
	if arg == "" {
		picked, err := pickBackup(ctx, app)
		if err != nil {
			return err
		}
		arg = picked
	}
	path := resolveBackupPath(app, arg)
	given := path
	if strings.HasSuffix(path, ageSuffix) {
		plain, cleanup, err := decryptBackup(path, f.identity)
		if err != nil {
			return err
		}
		defer cleanup()
		path = plain
	}
	m, err := backup.ReadManifest(path)
	if err != nil {
		return backupFileErr(path, err)
	}
	dir, err := app.Dir()
	if err != nil {
		return err
	}
	existing, err := instance.Find(dir)
	switch {
	case err == nil:
	case errors.Is(err, instance.ErrNotFound):
		existing = nil
	case errors.Is(err, instance.ErrLegacy):
		return preRexctlRefusal(ctx, dir)
	default:
		return err
	}
	if err := rootlessGuard(existing); err != nil {
		return err
	}
	if existing != nil {
		if err := managed(existing); err != nil {
			return err
		}
	}
	na, err := backup.FromNative(path)
	if err != nil {
		return backupFileErr(path, err)
	}
	if f.native && engineFlag != "" {
		return ui.Usage("Pass --runtime systemd or --engine, not both.")
	}
	if existing != nil {
		r, err := app.Runtime(ctx, existing)
		if err != nil {
			return err
		}
		if r.Kind() == rt.Container {
			if engineFlag != "" || f.native {
				return ui.Usage("An instance in one container restores without --engine or --runtime.")
			}
			return restoreContainerInstance(ctx, app, existing, r, path, m, f)
		}
		if r.Kind() == rt.Native {
			if engineFlag != "" {
				return ui.Usage("A Fluxer that runs as a systemd service restores without --engine.")
			}
			if native.New(existing).Running() != "" {
				return restoreNativeInstance(ctx, app, existing, r, path, m, f)
			}
			if nativeHasData(existing) {
				return ui.Refused(
					fmt.Sprintf("The Fluxer in %s is only partly installed as a systemd service, so rexctl can't back it up before the restore.", dir),
					"Run sudo rexctl install to finish it, then run rexctl restore again. To restore without keeping it, run rexctl uninstall first.",
				)
			}
			return runNativeRestoreFresh(ctx, app, dir, path, m, f.confirm)
		}
		if f.native {
			return ui.Refused(
				fmt.Sprintf("The instance in %s runs in containers.", dir),
				"Restore into an empty folder with --dir and --runtime systemd.",
			)
		}
	}
	switch {
	case na.Native && na.State == nil:
		return ui.Refused(
			fmt.Sprintf("This backup of %s comes from a Fluxer set up without rexctl, and it doesn't hold rexctl's settings for it.", m.Domain),
			fmt.Sprintf("Run rexctl install --runtime systemd --domain %s in %s first, then run this again.", m.Domain, dir),
		)
	case existing == nil && !f.native && na.Native && engineFlag == "" && runtime.GOOS == "linux" && euid() != 0:
		return ui.Refused(
			fmt.Sprintf("Nothing in %s to restore into, and a new instance from this backup runs as a systemd service, which needs root.", dir),
			freshRestoreFix(given),
		)
	case existing == nil && (f.native || na.Native && engineFlag == "" && runtime.GOOS == "linux"):
		return runNativeRestoreFresh(ctx, app, dir, path, m, f.confirm)
	case existing == nil && !na.Native && m.WithoutUploads():
		if !backup.ResumesRestore(dir, m) {
			return uploadsNotHere(nil, m.Domain, m.Project, backup.ErrUploadsNotHere)
		}
	case existing == nil:
		return ui.Refused(
			fmt.Sprintf("Nothing in %s to restore into.", dir),
			"To restore as a systemd service, run rexctl restore --runtime systemd. To restore into one container, run rexctl install --runtime docker first, then rexctl restore.",
		)
	case na.Native:
		return ui.Refused("This backup comes from one container or a systemd service, and this instance uses the older layout.", legacyMoveFix)
	}

	old := oldEngineRef(existing)
	if m.WithoutUploads() {
		if toDocker && existing != nil && old.Kind != string(engine.Docker) {
			return uploadsNotHere(existing, m.Domain, m.Project, backup.ErrUploadsNotHere)
		}
		e, err := app.Engine(ctx, existing)
		if err != nil {
			return err
		}
		if toDocker && e.Kind != engine.Docker {
			return uploadsNotHere(existing, m.Domain, m.Project, backup.ErrUploadsNotHere)
		}
		if err := checkUploadsInPlace(ctx, dir, existing, e, path); err != nil {
			return err
		}
	}

	made := backupTime(m.CreatedAt)
	domain := m.Domain
	what := fmt.Sprintf("This restores %s in %s from the backup made on %s.", m.Domain, dir, made)
	switch {
	case existing != nil && m.WithoutUploads():
		domain = existing.Domain()
		what = fmt.Sprintf("This replaces everything on %s except uploads with the backup made on %s. Uploads stay as they are.", domain, made)
	case existing != nil:
		domain = existing.Domain()
		what = fmt.Sprintf("This replaces everything on %s with the backup made on %s.", domain, made)
	case m.WithoutUploads():
		what = fmt.Sprintf("This finishes restoring %s in %s from the backup made on %s. Uploads stay as they are.", m.Domain, dir, made)
	}
	if toDocker && old.Kind == string(engine.Podman) {
		what += " Fluxer stops on Podman and starts on Docker. Podman keeps its copy of your data."
	}
	if err := confirmRestore(app, existing != nil, what, domain, confirm); errors.Is(err, errDeclined) {
		app.UI.Info("Nothing changed.")
		return nil
	} else if err != nil {
		return err
	}

	run := func() error {
		var safety string
		if existing != nil {
			var err error
			if safety, err = safetyBackup(ctx, app, existing, m.WithoutUploads(), f.noSafety); err != nil {
				return err
			}
			if err := noteRestore(existing, path, safety); err != nil {
				return withSafety(err, safety)
			}
		}
		e, err := restoreEngine(ctx, app, existing, dir, toDocker)
		if err != nil {
			return withSafety(err, safety)
		}
		if toDocker && old.ID != "" && old.ID != e.ID {
			stopOnOldEngine(ctx, app, existing, old)
		}
		return withSafety(restoreInto(ctx, app, e, path, dir, m, existing, old), safety)
	}
	var err2 error
	if existing != nil {
		err2 = app.Locked(ctx, existing, run)
	} else {
		err2 = run()
	}
	if errors.Is(err2, errDeclined) {
		app.UI.Info("Nothing changed.")
		return nil
	}
	return err2
}

func checkUploadsInPlace(ctx context.Context, dir string, existing *instance.Instance, e *engine.Engine, path string) error {
	m, err := backup.ReadManifest(path)
	if err != nil {
		return backupFileErr(path, err)
	}
	err = backup.CheckUploadsInPlace(ctx, dir, existing, e, m)
	switch {
	case errors.Is(err, backup.ErrUploadsNotHere):
		return uploadsNotHere(existing, m.Domain, m.Project, err)
	case err != nil:
		return rt.EngineError("", err)
	}
	return nil
}

func uploadsNotHere(existing *instance.Instance, domain, project string, err error) error {
	fix := "Restore a backup made with rexctl backup."
	if existing != nil {
		if list, lerr := backup.List(existing.BackupDir()); lerr == nil {
			same := slices.DeleteFunc(list, func(b backup.Info) bool { return b.Domain != domain || b.Project != project })
			if b := backup.NewestWithUploads(same); b != nil {
				fix = fmt.Sprintf("Restore %s, the newest backup with uploads.", b.Path)
			}
		}
	}
	return &ui.ExitError{Code: ui.ExitRefused, Msg: "This backup leaves out uploads, and this machine doesn't have the uploads it belongs with.", Fix: fix, Err: err}
}

func oldEngineRef(inst *instance.Instance) instance.EngineRef {
	if inst == nil || inst.State == nil {
		return instance.EngineRef{}
	}
	return inst.State.Engine
}

func stopOnOldEngine(ctx context.Context, app *App, inst *instance.Instance, old instance.EngineRef) {
	err := app.UI.Step(ctx, "Stopping Fluxer on "+engine.Kind(old.Kind).Name(), func(ctx context.Context, s *ui.Step) error {
		e, err := engine.Connect(ctx, engine.ConnectOptions{Host: old.Host, ID: old.ID, AllowOld: true})
		if err != nil {
			return err
		}
		defer e.Close()
		if err := e.Named(inst.Project(), inst.Dir).Stop(context.WithoutCancel(ctx)); err != nil {
			return err
		}
		s.Done("Stopped Fluxer on %s.", engine.Kind(old.Kind).Name())
		return nil
	})
	if err != nil && !errors.Is(err, context.Canceled) {
		app.UI.Detail("%v", err)
	}
}

func restoreInto(ctx context.Context, app *App, e *engine.Engine, path, dir string, m *backup.Manifest, existing *instance.Instance, old instance.EngineRef) error {
	var inst *instance.Instance
	var before []instance.VoiceServer
	if existing != nil && existing.State != nil {
		before = slices.Clone(existing.State.Voice)
	}
	err := app.UI.Step(ctx, "Checking the backup", func(ctx context.Context, s *ui.Step) error {
		var err error
		inst, err = backup.Restore(ctx, path, dir, e, backup.RestoreOptions{
			Progress: func(stage string) {
				if t := rt.RestoreStageTitle(stage); t != "" {
					s.Title(t)
				}
				if stage == backup.StageClearing {
					for i := range before {
						if err := removeVoiceServerProject(ctx, e, &before[i]); err != nil {
							app.UI.Detail("stopping voice server %s: %v", before[i].ID, err)
						}
					}
				}
			},
			OnEvent: func(ev engine.Event) {
				if ev.Total > 0 {
					s.Progress(ev.Current, ev.Total)
				}
			},
			Adopted: matchRestoredSites(app),
		})
		if err != nil {
			return err
		}
		s.Done("Restored %s from the backup made on %s.", m.Domain, backupTime(m.CreatedAt))
		return nil
	})
	if err != nil {
		return restoreErr(path, err, restoreFailureLog(ctx, e, dir, err))
	}
	warnBackupSkipped(app, m)
	if err := forgetRestoredMigrations(inst, m.CreatedAt); err != nil {
		return err
	}

	p, err := deploy.Load(ctx, inst, e)
	if err != nil {
		return rt.EngineError("", err)
	}
	restoreBootUnit(ctx, app, inst, e, p, existing, old)
	app.remember(inst.Dir)
	err = app.UI.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
		if err := p.WaitReady(ctx, restoreReadyTimeout, rt.ShowReadiness(s, "Starting Fluxer")); err != nil {
			return err
		}
		s.Done("Started Fluxer.")
		return nil
	})
	if err != nil {
		if errors.Is(err, context.Canceled) {
			return err
		}
		return ui.Wrap(err, "Fluxer didn't start after the restore.", "Run rexctl doctor to find the problem.")
	}
	restoreVoiceServers(ctx, app, e, p, inst, before)
	app.UI.Rex("Your instance is back the way it was on " + backupTime(m.CreatedAt) + ".")
	if origin := inst.Origin(); origin != "" {
		app.UI.Highlight(origin)
	}
	warnDNS(ctx, app, checkEnv(ctx, inst, e, p))
	return nil
}

func warnDNS(ctx context.Context, app *App, env *checks.Env) {
	c, ok := checks.Get("dns")
	if !ok {
		return
	}
	if r := checks.RunIn(ctx, env, checks.Installed, c); r.Status == checks.Fail || r.Status == checks.Warn {
		app.UI.Warn(r.Message, r.Fix)
	}
}

func droppedVoiceServers(before, after []instance.VoiceServer) []instance.VoiceServer {
	var out []instance.VoiceServer
	for _, b := range before {
		if !slices.ContainsFunc(after, func(a instance.VoiceServer) bool { return a.ID == b.ID && a.Project == b.Project }) {
			out = append(out, b)
		}
	}
	return out
}

func restoreVoiceServers(ctx context.Context, app *App, e *engine.Engine, p *engine.Project, inst *instance.Instance, before []instance.VoiceServer) {
	for _, gone := range droppedVoiceServers(before, inst.State.Voice) {
		closeVoiceFirewall(ctx, app, gone.Opened)
		_ = os.RemoveAll(voice.ServerDir(inst.Dir, gone.ID))
		app.UI.Info("Removed the voice server %s, which isn't in the backup.", gone.ID)
	}
	if len(inst.State.Voice) == 0 {
		return
	}
	err := app.UI.Step(ctx, "Starting the voice servers", func(ctx context.Context, s *ui.Step) error {
		started := 0
		for i := range inst.State.Voice {
			srv := &inst.State.Voice[i]
			err := writeVoiceServer(inst, srv)
			if err == nil {
				err = startVoiceServer(ctx, app, e, inst, srv, s)
			}
			if err != nil {
				app.UI.Warn(fmt.Sprintf("Voice server %s didn't start (%v).", srv.ID, err), "Run rexctl voice status "+srv.ID+" to see why.")
				continue
			}
			started++
			if len(srv.Opened) > 0 {
				for _, o := range openVoiceFirewall(ctx, app, voicePortStrings(srv), false) {
					if !slices.Contains(srv.Opened, o) {
						srv.Opened = append(srv.Opened, o)
					}
				}
			}
		}
		if err := reloadCaddy(ctx, p, "edge", "/etc/caddy/Caddyfile"); err != nil {
			app.UI.Detail("reloading the edge: %v", err)
		}
		if err := saveVoiceState(inst); err != nil {
			return err
		}
		s.Done("Started %s.", plural(started, "voice server", "voice servers"))
		return nil
	})
	if err != nil {
		app.UI.Warn(fmt.Sprintf("The voice servers didn't start (%v).", err), "Run rexctl voice status to see why.")
	}
}

func restoreBootUnit(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, p *engine.Project, existing *instance.Instance, old instance.EngineRef) {
	if !host.BootUnitSupported() {
		return
	}
	u, err := checks.BootUnitFor(inst, e, p)
	if err != nil {
		app.UI.Detail("%v", err)
		return
	}
	if existing != nil && old.Kind != "" && (old.Kind != string(e.Kind) || old.ID != e.ID) {
		prev := u
		prev.Podman = old.Kind == string(engine.Podman)
		prev.Rootless = strings.Contains(old.Host, "/run/user/")
		if prev.Rootless != u.Rootless {
			steps, err := prev.RemoveSteps(ctx)
			if err == nil {
				var ok bool
				ok, err = confirmHostSteps(app, steps, "Remove the old service that started Fluxer with this machine?")
				if err == nil && ok {
					err = host.RunSteps(ctx, steps)
				}
			}
			if err != nil {
				app.UI.Detail("%v", err)
				app.UI.Warn("The old service that started Fluxer with this machine is still there.", "")
			}
		}
	}
	if err := setupBootService(ctx, app, inst, e, p, ""); err != nil {
		app.UI.Warn("Fluxer won't start again after this machine restarts.", "Run rexctl boot on to start it with the machine.")
		app.UI.Detail("%v", err)
	}
}

func restoreEngine(ctx context.Context, app *App, existing *instance.Instance, dir string, toDocker bool) (*engine.Engine, error) {
	if toDocker {
		e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: engine.Docker})
		if err == nil && e.Kind == engine.Docker {
			return e, nil
		}
		if err == nil {
			e.Close()
		}
		if engine.ClassOf(err) == engine.ClassDenied {
			return nil, rt.EngineError(engine.Docker, err)
		}
		return installDockerForRestore(ctx, app, dir)
	}
	e, err := app.Engine(ctx, existing)
	switch {
	case err == nil:
		return e, nil
	case errors.Is(err, context.Canceled):
		return nil, err
	case existing == nil && runtime.GOOS == "linux" && engine.ClassOf(err) == engine.ClassUnreachable:
		return installDockerForRestore(ctx, app, dir)
	}
	return nil, err
}

func installDockerForRestore(ctx context.Context, app *App, dir string) (*engine.Engine, error) {
	return installDockerHere(ctx, app, dir, "run rexctl restore again", func() (bool, error) {
		return app.UI.Confirm("Install Docker?", true)
	})
}

func installDockerHere(ctx context.Context, app *App, dir, again string, confirm func() (bool, error)) (*engine.Engine, error) {
	manual := ui.Refused("Docker isn't installed or isn't running.", engine.Docker.InstallHint()+", then "+again+".")
	info, err := host.Detect(ctx, dir)
	if err != nil || !host.CanInstallDocker(info) {
		return nil, manual
	}
	app.UI.Warn("Docker isn't installed.", "")
	ok, err := confirm()
	if err != nil {
		return nil, err
	}
	if !ok {
		return nil, manual
	}
	err = app.UI.Step(ctx, "Installing Docker", func(ctx context.Context, s *ui.Step) error {
		if err := host.InstallDocker(ctx, info, s.Log); err != nil {
			return err
		}
		s.Done("Installed Docker.")
		return nil
	})
	if errors.Is(err, host.ErrRelogin) {
		return nil, ui.Refused("Docker is installed, and your account can use it after you sign in again.", "Sign out and back in, then "+again+".")
	}
	if err != nil {
		return nil, ui.Wrap(err, "Couldn't install Docker.", "Install Docker, then "+again+".")
	}
	e, err := engine.Connect(ctx, engine.ConnectOptions{Prefer: engine.Docker})
	if err != nil || e.Kind != engine.Docker {
		return nil, ui.Wrap(err, "Docker was installed but isn't responding.", "Start Docker, then "+again+".")
	}
	return e, nil
}

func restoreFailureLog(ctx context.Context, e *engine.Engine, dir string, err error) string {
	var part *backup.RestoreError
	if !errors.As(err, &part) || errors.Is(err, context.Canceled) {
		return ""
	}
	inst, ferr := instance.Find(dir)
	if ferr != nil {
		return ""
	}
	cause := err
	if part.Output != "" {
		cause = fmt.Errorf("%w\n%s", err, part.Output)
	}
	return writeFailureLog(ctx, inst, e.Named(inst.Project(), inst.Dir), "restore.log", cause, nil)
}

func restoreErr(path string, err error, logPath string) error {
	var inUse *backup.ProjectInUseError
	var part *backup.RestoreError
	var pull *backup.PullError
	var space *backup.RestoreSpaceError
	var unknown *backup.SpaceUnknownError
	partFix := "Run rexctl restore again with --verbose to see why."
	if logPath != "" {
		partFix = fmt.Sprintf("Read %s to see why, then run rexctl restore again.", logPath)
	}
	switch {
	case errors.As(err, &pull) && !errors.Is(err, context.Canceled):
		if engine.ClassOf(pull.Err) != engine.ClassOther {
			return rt.EngineError("", pull.Err)
		}
		return ui.Wrap(pull.Err, "Couldn't download Fluxer.", "Run rexctl restore again with --verbose to see why.")
	case errors.Is(err, backup.ErrUploadsNotHere):
		return uploadsNotHere(nil, "", "", err)
	case errors.Is(err, backup.ErrArchiveReplaced):
		return ui.Refused(fmt.Sprintf("%s is in a folder that the restore replaces, so rexctl didn't change anything.", path), "Move the backup out of that folder, such as into the backups folder, then run rexctl restore again.")
	case errors.Is(err, backup.ErrIntoLegacy):
		return ui.Refused("This backup comes from one container or a systemd service, and this instance uses the older layout.", legacyMoveFix)
	case errors.As(err, &space):
		return ui.Refused(
			fmt.Sprintf("Restoring %s needs about %s of free disk space in %s, and %s is free.", path, ui.Bytes(int64(space.Need)), space.Dir, ui.Bytes(int64(space.Available))),
			"Free up space there, then run rexctl restore again.",
		)
	case errors.As(err, &unknown):
		return ui.Wrap(err, fmt.Sprintf("rexctl can't read how much free disk space %s has.", unknown.Dir), "Check that the folder is on a mounted disk you can open, then run rexctl restore again.")
	case errors.Is(err, backup.ErrNativeWithoutUploads):
		return ui.Refused("This backup leaves out uploads, and a systemd install restores only backups that have them.", "Restore a backup made with rexctl backup.")
	case errors.Is(err, backup.ErrNativeExternalDB):
		return ui.Refused("This backup leaves out the database, which ran outside Fluxer, and a systemd install keeps its own database.", "Restore a backup that has the database in it.")
	case errors.As(err, &inUse):
		return ui.Refused(
			fmt.Sprintf("Another Fluxer instance on this machine is in %s.", inUse.Dir),
			"Restore into that folder with --dir, or uninstall it first.",
		)
	case errors.As(err, &part) && part.Part == "database":
		return ui.Wrap(fmt.Errorf("%w %s", err, part.Output), "Couldn't restore the database.", partFix)
	case errors.As(err, &part) && part.Part == "containers":
		return ui.Wrap(fmt.Errorf("%w %s", err, part.Output), "Couldn't set up Fluxer from the backup.", partFix)
	case errors.As(err, &part):
		return ui.Wrap(fmt.Errorf("%w %s", err, part.Output), "Couldn't restore uploads and search data.", partFix)
	case errors.Is(err, backup.ErrDamaged), errors.Is(err, backup.ErrNotBackup), errors.Is(err, backup.ErrNewer):
		return backupFileErr(path, err)
	}
	var ee *ui.ExitError
	if errors.As(err, &ee) || errors.Is(err, context.Canceled) {
		return err
	}
	if engine.ClassOf(err) != engine.ClassOther {
		return rt.EngineError("", err)
	}
	return ui.Wrap(err, "Couldn't restore your instance.", "Run rexctl restore again with --verbose to see why.")
}

func nativePackagesStep(ctx context.Context, app *App, n *native.Stack, version string) error {
	return app.UI.Step(ctx, "Installing the system packages Fluxer needs", func(ctx context.Context, s *ui.Step) error {
		if err := n.EnsurePackages(ctx, version, s); err != nil {
			return rt.NativeError(err, "Couldn't install the system packages Fluxer needs.")
		}
		if err := native.EnsureUser(ctx); err != nil {
			return rt.NativeError(err, "Couldn't create the account "+native.ServiceUser+".")
		}
		s.Done("Installed the system packages Fluxer needs.")
		return nil
	})
}

func runNativeRestoreFresh(ctx context.Context, app *App, dir, path string, m *backup.Manifest, confirm string) error {
	if err := app.nativeRoot(dir); err != nil {
		return err
	}
	dir = instance.RealPath(dir)
	if err := nativeFolder(dir, "--dir"); err != nil {
		return err
	}
	if err := rt.NativeAlone(ctx); err != nil {
		return err
	}
	_, found := instance.Find(dir)
	fresh := errors.Is(found, instance.ErrNotFound)
	if fresh && native.HoldsReleases(dir) {
		return ui.Refused(fmt.Sprintf("%s already holds a Fluxer that runs as %s.", dir, host.StandaloneNativeUnit), "Restore into another folder with --dir.")
	}
	if fresh {
		if o := sameDomain(dir, m.Domain); o != nil {
			return ui.Refused(fmt.Sprintf("The Fluxer in %s already serves %s.", o.Dir, m.Domain), "Restore into it with --dir "+shellWord(o.Dir)+".")
		}
	}
	have := stack.EmbeddedRelease()
	if release.Valid(m.Release) && release.Valid(have) && release.Compare(m.Release, have) > 0 {
		return ui.Refused(
			fmt.Sprintf("This backup comes from Fluxer %s, which is newer than Fluxer %s that this rexctl installs.", m.Release, have),
			"Download the latest rexctl, then run rexctl restore again.",
		)
	}
	if err := checkNativeBackup(ctx, app, path, dir, m); err != nil {
		return err
	}
	what := fmt.Sprintf("This restores %s in %s from the backup made on %s, and runs it without containers.", m.Domain, dir, backupTime(m.CreatedAt))
	if err := confirmRestore(app, false, what, m.Domain, confirm); errors.Is(err, errDeclined) {
		app.UI.Info("Nothing changed.")
		return nil
	} else if err != nil {
		return err
	}
	rel, err := nativeRelease(ctx, have, "Restore it on a machine Fluxer has a native build for, or into an instance that runs in containers.")
	if err != nil {
		return err
	}
	var old *release.TooOldError
	if err := release.CheckPath(m.Release, rel); errors.As(err, &old) {
		return ui.Refused(
			fmt.Sprintf("This backup comes from Fluxer %s, which is too old to restore straight onto Fluxer %s.", old.From, old.To),
			fmt.Sprintf("Restore it with rexctl %s, then run rexctl upgrade.", old.MinFrom),
		)
	}
	err = found
	if errors.Is(err, instance.ErrNotFound) {
		_, err = instance.Create(dir, cmp.Or(m.Project, "fluxer"))
	}
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't create %s.", dir), "Check that you can write to it, or pass --dir.")
	}
	if err := backup.ExtractNativeFiles(path, dir); err != nil {
		return restoreErr(path, err, "")
	}
	inst, err := instance.Find(dir)
	if err != nil {
		return ui.Wrap(err, "Couldn't read rexctl's settings from the backup.", "Run rexctl restore again with --verbose to see why.")
	}
	st := inst.State
	if st.Runtime != instance.RuntimeNative {
		st.Images = instance.Images{}
		st.Rendered = nil
		delete(st.Settings, settings.EngineHost)
		if len(st.Voice) > 0 {
			app.UI.Info("The backup's voice servers ran in containers, so rexctl left them out. Run rexctl voice add to set one up again.")
			st.Voice = nil
		}
	}
	st.Runtime = instance.RuntimeNative
	if fresh {
		st.Layout = instance.LayoutFHS
		if nameTaken(st.Project, dir, true) || native.ReservedName(st.Project) {
			if st.Project = freeName(dir, true, true); st.Project == "" {
				return ui.Refused("Every name rexctl tries is taken on this machine.", "Remove a Fluxer you no longer use, then try again.")
			}
		}
		if st.Loopback, err = pickLoopback(dir); err != nil {
			return ui.Wrap(err, "Couldn't find a free internal address for a second Fluxer on this machine.", "Remove a Fluxer you no longer use, then try again.")
		}
		if err := restoredEdge(app, inst); err != nil {
			return err
		}
	}
	st.Engine = instance.EngineRef{}
	st.Upgrade, st.Pending = nil, nil
	st.Mounted = nil
	st.Stopped = false
	st.Release = rel.Version
	if err := release.WriteRelease(inst, settings.NativeSet(rel.Version)); err != nil {
		return saveFailed(inst, err, fmt.Sprintf("Couldn't write Fluxer's files to %s.", inst.Dir), "rexctl restore")
	}
	if err := saveInstance(inst); err != nil {
		return err
	}
	app.remember(inst.Dir)
	if fresh {
		if err := syncEdgeSite(ctx, app, inst); err != nil {
			return err
		}
	}
	return app.Locked(ctx, inst, func() error {
		n := native.New(inst)
		err := app.UI.Step(ctx, "Downloading Fluxer", func(ctx context.Context, s *ui.Step) error {
			if _, err := n.Fetch(ctx, rel, s.Progress); err != nil {
				return rt.NativeError(err, "Couldn't download Fluxer.")
			}
			if err := n.Flip(rel.Version); err != nil {
				return rt.NativeError(err, "Couldn't unpack Fluxer.")
			}
			if _, err := n.WriteSettings(rel.Version); err != nil {
				return rt.NativeError(err, "Couldn't read the settings of Fluxer "+rel.Version+".")
			}
			s.Done("Downloaded Fluxer %s.", rel.Version)
			return nil
		})
		if err != nil {
			return err
		}
		if err := nativePackagesStep(ctx, app, n, rel.Version); err != nil {
			return err
		}
		nset, err := deploy.ReleaseSet(inst)
		if err != nil {
			return err
		}
		if _, err := n.Render(nset); err != nil {
			return rt.NativeError(err, "Couldn't write Fluxer's settings.")
		}
		r, err := app.Runtime(ctx, inst)
		if err != nil {
			return err
		}
		if err := noteRestore(inst, path, ""); err != nil {
			return err
		}
		return restoreNativeData(ctx, app, inst, r, path, m, false)
	})
}

func restoreNativeInstance(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime, path string, m *backup.Manifest, f restoreFlags) error {
	if err := app.nativeRoot(inst.Dir); err != nil {
		return err
	}
	if err := sameUploads(inst, m); err != nil {
		return err
	}
	what := fmt.Sprintf("This replaces everything on %s with the backup made on %s.", inst.Domain(), backupTime(m.CreatedAt))
	if err := confirmRestore(app, true, what, inst.Domain(), f.confirm); err != nil {
		return err
	}
	err := app.Locked(ctx, inst, func() error {
		if err := checkNativeBackup(ctx, app, path, native.New(inst).Root(), m); err != nil {
			return err
		}
		safety, err := safetyBackup(ctx, app, inst, false, f.noSafety)
		if err != nil {
			return err
		}
		if err := noteRestore(inst, path, safety); err != nil {
			return withSafety(err, safety)
		}
		return withSafety(restoreNativeData(ctx, app, inst, r, path, m, wasRunning(ctx, r, inst)), safety)
	})
	if errors.Is(err, errDeclined) {
		app.UI.Info("Nothing changed.")
		return nil
	}
	return err
}

func restoreContainerInstance(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime, path string, m *backup.Manifest, f restoreFlags) error {
	if err := containerCanRestore(m); err != nil {
		return err
	}
	if err := sameUploads(inst, m); err != nil {
		return err
	}
	what := fmt.Sprintf("This replaces everything on %s with the backup made on %s.", inst.Domain(), backupTime(m.CreatedAt))
	if err := confirmRestore(app, true, what, inst.Domain(), f.confirm); err != nil {
		if errors.Is(err, errDeclined) {
			app.UI.Info("Nothing changed.")
			return nil
		}
		return err
	}
	err := app.Locked(ctx, inst, func() error {
		if err := checkNativeBackup(ctx, app, path, "", m); err != nil {
			return err
		}
		safety, err := safetyBackup(ctx, app, inst, false, f.noSafety)
		if err != nil {
			return err
		}
		if err := noteRestore(inst, path, safety); err != nil {
			return withSafety(err, safety)
		}
		return withSafety(restoreNativeData(ctx, app, inst, r, path, m, wasRunning(ctx, r, inst)), safety)
	})
	if errors.Is(err, errDeclined) {
		app.UI.Info("Nothing changed.")
		return nil
	}
	return err
}

func nativeHasData(inst *instance.Instance) bool {
	size, err := backup.DirSize(native.New(inst).Root())
	if errors.Is(err, os.ErrNotExist) {
		return false
	}
	return err != nil || size > 0
}

func wasRunning(ctx context.Context, r rt.Runtime, inst *instance.Instance) bool {
	if st, err := r.Status(ctx); err == nil {
		return st.Running
	}
	switch native.New(inst).ActiveState(ctx) {
	case "active", "activating", "reloading":
		return true
	case "":
		return !inst.State.Stopped
	}
	return false
}

func containerCanRestore(m *backup.Manifest) error {
	valkey := slices.Contains(m.Volumes, stack.ValkeyVolume)
	weed := slices.Contains(m.Volumes, stack.SeaweedFSVolume) && !slices.Contains(m.Volumes, stack.ObjectsVolume)
	if valkey || weed {
		return ui.Refused(
			"This backup uses Valkey or SeaweedFS, and Fluxer's container has neither.",
			"Run rexctl migrate on the old install first, then restore a new backup of it.",
		)
	}
	return nil
}

func sameUploads(inst *instance.Instance, m *backup.Manifest) error {
	want := stack.UploadsVolume(inst.State.Settings)
	for _, v := range m.Volumes {
		if (v == stack.SeaweedFSVolume || v == stack.ObjectsVolume) && v != want {
			return ui.Refused(
				fmt.Sprintf("This backup keeps uploads in %s, and %s keeps them in %s, so restoring it would leave your uploads where Fluxer can't find them.", v, inst.Domain(), want),
				"Restore it into an empty folder with --dir, then point your domain at that instance.",
			)
		}
	}
	return nil
}

func checkNativeBackup(ctx context.Context, app *App, path, dir string, m *backup.Manifest) error {
	return app.UI.Check(ctx, "Checking the backup", func(ctx context.Context, s *ui.Step) error {
		if _, err := backup.Verify(path); err != nil {
			return backupFileErr(path, err)
		}
		if err := backup.CheckNative(path, dir, m); err != nil {
			return restoreErr(path, err, "")
		}
		s.Done("The backup file is whole and fits this instance.")
		return nil
	})
}

func restoreNativeData(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime, path string, m *backup.Manifest, wasRunning bool) error {
	err := app.UI.Step(ctx, "Restoring your instance", func(ctx context.Context, s *ui.Step) error {
		earlier := inst.State.Restoring
		if err := markRestoring(inst, path); err != nil {
			return err
		}
		if err := r.Backups().Restore(ctx, s, path); err != nil {
			var begun *rt.RestoreBegunError
			if !errors.As(err, &begun) {
				_ = markRestoring(inst, earlier)
			}
			return err
		}
		if err := adoptSecrets(ctx, inst, r); err != nil {
			return &rt.RestoreBegunError{Err: ui.Wrap(err, "Restored the backup, but couldn't read its secrets.", "Run rexctl restore again with --verbose to see why.")}
		}
		if err := forgetRestoredMigrations(inst, m.CreatedAt); err != nil {
			return &rt.RestoreBegunError{Err: err}
		}
		if err := markRestoring(inst, ""); err != nil {
			return err
		}
		s.Done("Restored %s from the backup made on %s.", inst.Domain(), backupTime(m.CreatedAt))
		return nil
	})
	var begun *rt.RestoreBegunError
	switch {
	case err == nil:
	case errors.As(err, &begun):
		fix := "Run rexctl restore again with --verbose to see why."
		if _, again, ok := inst.RestoreUnfinished(); ok && ctx.Err() != nil {
			fix = again
		}
		return rt.HalfRestored(ctx, err, fix)
	default:
		var ee *ui.ExitError
		if wasRunning && !(errors.As(err, &ee) && ee.Code == ui.ExitRefused) {
			startAfterFailedRestore(ctx, app, r)
		}
		if ctx.Err() != nil {
			return &ui.ExitError{Code: ui.ExitInterrupted, Msg: "Stopped the restore before it changed anything.", Err: err}
		}
		return err
	}
	err = app.UI.Step(ctx, "Starting Fluxer", func(ctx context.Context, s *ui.Step) error {
		if err := r.Start(ctx, s, rt.StartOptions{}); err != nil {
			return err
		}
		s.Done("Started Fluxer.")
		return nil
	})
	if err != nil {
		return err
	}
	if inst.State.Stopped {
		inst.State.Stopped = false
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then run rexctl start.")
		}
	}
	app.UI.Rex("Your instance is back the way it was on " + backupTime(m.CreatedAt) + ".")
	if origin := inst.Origin(); origin != "" {
		app.UI.Highlight(origin)
	}
	env := checkEnv(ctx, inst, nil, nil)
	env.Native = true
	warnDNS(ctx, app, env)
	return nil
}

func noteRestore(inst *instance.Instance, from, safety string) error {
	if abs, err := filepath.Abs(from); err == nil {
		from = abs
	}
	inst.State.Restored = &instance.Restored{From: from, Safety: safety}
	return saveInstance(inst)
}

func markRestoring(inst *instance.Instance, path string) error {
	if inst.State.Restoring == path {
		return nil
	}
	inst.State.Restoring = path
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	return nil
}

func startAfterFailedRestore(ctx context.Context, app *App, r rt.Runtime) {
	err := app.UI.Step(ctx, "Starting Fluxer again", func(ctx context.Context, s *ui.Step) error {
		if err := r.Start(context.WithoutCancel(ctx), s, rt.StartOptions{}); err != nil {
			return err
		}
		s.Done("Fluxer is running again.")
		return nil
	})
	if err != nil {
		app.UI.Warn("Fluxer didn't start again after the failed restore.", "Run rexctl doctor to see why.")
	}
}

func confirmRestore(app *App, replaces bool, what, domain, confirm string) error {
	if replaces {
		return app.UI.Destructive(ui.DestructiveCard{Title: "Replace " + domain + " with a backup", Body: []string{what}}, domain, confirm)
	}
	ok, err := app.UI.ConfirmWith(ui.Question{Text: "Restore this backup?", Detail: what, Default: true})
	if err != nil {
		return err
	}
	if !ok {
		return errDeclined
	}
	return nil
}
