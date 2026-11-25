// SPDX-License-Identifier: AGPL-3.0-or-later

package upgrade

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	RecoveryTimeout = 20 * time.Minute
	logTimeout      = 30 * time.Second
	logTail         = 200
)

const (
	killSwitched = "switched"
	killWentBack = "went-back"
	killRestored = "restored"
)

var ErrInterrupted = errors.New("upgrade: rexctl stopped before the upgrade finished")

type Outcome int

const (
	Nothing Outcome = iota
	Upgraded
	Prefetched
	WentBack
	Cleared
)

type Options struct {
	Prefetch bool
	Backup   string
}

type Result struct {
	Outcome  Outcome
	From     string
	To       string
	Backup   string
	Stopped  bool
	Cause    error
	Log      string
	Put      []string
	Restored bool
}

type Engine struct {
	Inst         *instance.Instance
	RT           runtime.Runtime
	UI           *ui.UI
	AfterRestore func(ctx context.Context) error
	Driver       runtime.Upgrades
	restored     bool
	sized        string
	download     int64
	saved        int64
}

type has interface{ Has(version string) bool }

type secretSource interface {
	Secrets() (map[string]string, error)
}

type abandoner interface{ Abandon(u *instance.Upgrade) }

type finisher interface {
	Finished(ctx context.Context, res Result)
}

type failureLogger interface {
	FailureLog(ctx context.Context, name string, cause error) string
}

type backChecker interface {
	CheckBack(ctx context.Context, to string) error
}

type stamper interface {
	DataNewer(version string) bool
}

type spaceChecker interface {
	CheckSpace(ctx context.Context, to *release.Release, download, saved int64) error
}

type settler interface {
	Settle(target string) (bool, error)
}

type sizer interface {
	Sizes(ctx context.Context, to *release.Release, backup string) (download, saved int64)
}

func (e *Engine) up() runtime.Upgrades {
	if e.Driver == nil {
		e.Driver = e.RT.Upgrades()
	}
	return e.Driver
}

func (e *Engine) Has(version string) bool {
	if h, ok := e.up().(has); ok {
		return h.Has(version)
	}
	return release.HasSnapshot(e.Inst, version)
}

func (e *Engine) Sizes(ctx context.Context, to *release.Release, backup string) (download, saved int64) {
	key := to.Version + "\x00" + backup
	if e.sized == key {
		return e.download, e.saved
	}
	download, saved = -1, -1
	if s, ok := e.up().(sizer); ok {
		download, saved = s.Sizes(ctx, to, backup)
	}
	e.sized, e.download, e.saved = key, download, saved
	return download, saved
}

func LogName(version string) string { return "upgrade-" + release.DirName(version) + ".log" }

func goBackLog(version string) string { return "go-back-" + release.DirName(version) + ".log" }

func Rollback(u *instance.Upgrade) bool {
	return u != nil && release.Valid(u.From) && release.Valid(u.To) && release.Compare(u.To, u.From) < 0
}

func Begin(inst *instance.Instance, from, to, phase string) error {
	if inst.State.Upgrade == nil {
		inst.State.Upgrade = &instance.Upgrade{From: from, To: to, StartedAt: time.Now().UTC()}
	}
	inst.State.Upgrade.Phase = phase
	if err := inst.Save(); err != nil {
		return saveError(inst, err)
	}
	release.KillPoint(phase)
	return nil
}

func saveError(inst *instance.Instance, err error) error {
	return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then run rexctl upgrade again.")
}

func (e *Engine) save() error {
	if err := e.Inst.Save(); err != nil {
		return saveError(e.Inst, err)
	}
	return nil
}

func (e *Engine) mark(phase string) error {
	e.Inst.State.Upgrade.Phase = phase
	if err := e.save(); err != nil {
		return err
	}
	release.KillPoint(phase)
	return nil
}

func (e *Engine) clear() {
	if a, ok := e.up().(abandoner); ok && e.Inst.State.Upgrade != nil {
		a.Abandon(e.Inst.State.Upgrade)
	}
	e.Inst.State.Upgrade = nil
	_ = e.Inst.Save()
}

func (e *Engine) imageRefs() []string {
	if len(e.Inst.State.Images.Refs) == 0 {
		return nil
	}
	return slices.Sorted(maps.Values(e.Inst.State.Images.Refs))
}

func (e *Engine) writeLog(ctx context.Context, name string, cause error) string {
	ctx, cancel := context.WithTimeout(context.WithoutCancel(ctx), logTimeout)
	defer cancel()
	if l, ok := e.up().(failureLogger); ok {
		return l.FailureLog(ctx, name, cause)
	}
	root, err := os.OpenRoot(e.Inst.Dir)
	if err != nil {
		return ""
	}
	defer root.Close()
	f, err := root.OpenFile(filepath.Join(instance.StateDirName, name), os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
	if err != nil {
		return ""
	}
	defer f.Close()
	fmt.Fprintf(f, "rexctl: %v\n\n", cause)
	if e.RT != nil {
		_ = e.RT.Logs(ctx, f, runtime.LogOptions{Tail: logTail})
	}
	return e.Inst.Path(instance.StateDirName, name)
}

func name(v string) string { return "Fluxer " + v }

func (e *Engine) Run(ctx context.Context, rel *release.Release, o Options) (Result, error) {
	u := e.Inst.State.Upgrade
	if u == nil || u.To != rel.Version {
		return Result{}, errors.New("upgrade: no upgrade to " + rel.Version + " was started")
	}
	res := Result{From: u.From, To: u.To}
	h := e.up()
	if err := h.Check(ctx, rel); err != nil {
		e.clear()
		return res, err
	}
	if sc, ok := h.(spaceChecker); ok && !o.Prefetch {
		download, saved := e.Sizes(ctx, rel, o.Backup)
		if err := sc.CheckSpace(ctx, rel, download, saved); err != nil {
			e.clear()
			return res, err
		}
	}
	if err := release.Snapshot(e.Inst, u.From, e.imageRefs()); err != nil {
		e.clear()
		return res, ui.Wrap(err, fmt.Sprintf("Couldn't save the settings of %s.", name(u.From)), "Check that you can write to "+e.Inst.Dir+", then run rexctl upgrade again.")
	}
	if err := e.mint(); err != nil {
		e.clear()
		return res, err
	}
	if err := e.save(); err != nil {
		e.clear()
		return res, err
	}
	err := e.UI.Step(ctx, "Downloading "+name(u.To), func(ctx context.Context, s *ui.Step) error {
		if err := h.Prepare(ctx, s, rel); err != nil {
			return err
		}
		s.Done("Downloaded %s.", name(u.To))
		return nil
	})
	if err != nil {
		e.clear()
		if ctx.Err() != nil {
			e.UI.Info("Stopped the upgrade. %s is still running.", name(u.From))
			return res, ctx.Err()
		}
		return res, err
	}
	if err := e.mark(release.PhasePulled); err != nil {
		e.clear()
		return res, err
	}
	if o.Prefetch {
		e.clear()
		res.Outcome = Prefetched
		return res, nil
	}
	if o.Backup != settings.BackupOff {
		path, err := e.backup(ctx, o.Backup)
		if err != nil {
			e.clear()
			if ctx.Err() != nil {
				e.UI.Info("Stopped the upgrade. %s is still running.", name(u.From))
				return res, ctx.Err()
			}
			return res, err
		}
		u.Backup = path
		if err := e.mark(release.PhaseBackedUp); err != nil {
			e.clear()
			return res, err
		}
	}
	return e.switchTo(ctx, rel, res)
}

func (e *Engine) mint() error {
	u := e.Inst.State.Upgrade
	next := maps.Clone(e.Inst.State.Secrets)
	if next == nil {
		next = map[string]string{}
	}
	if src, ok := e.up().(secretSource); ok {
		if have, err := src.Secrets(); err == nil {
			for k, v := range have {
				if secrets.IsUnset(next[k]) && !secrets.IsUnset(v) {
					next[k] = v
				}
			}
		}
	}
	_, err := secrets.Mint(next, true)
	var missing *secrets.StatefulMissingError
	switch {
	case errors.As(err, &missing):
		return &ui.ExitError{
			Code: ui.ExitRefused,
			Msg:  fmt.Sprintf("%s needs %s, and this instance has no value for it. A new value would lock Fluxer out of data it already keeps.", name(u.To), ui.JoinAnd(missing.Keys)),
			Fix:  fmt.Sprintf("Nothing changed. Stay on %s and run rexctl doctor to see what's wrong.", name(u.From)),
			Err:  err,
		}
	case err != nil:
		return ui.Wrap(err, fmt.Sprintf("Couldn't create the secrets %s needs. Nothing changed.", name(u.To)), "Run rexctl upgrade --verbose to see why.")
	}
	e.Inst.State.Secrets = next
	return nil
}

func (e *Engine) backup(ctx context.Context, scope string) (string, error) {
	u := e.Inst.State.Upgrade
	var path string
	err := e.UI.Step(ctx, "Backing up your instance", func(ctx context.Context, s *ui.Step) error {
		var err error
		path, err = e.RT.Backups().Create(ctx, s, runtime.BackupOptions{WithoutUploads: scope == settings.BackupWithoutUploads, Reason: "upgrade"})
		if err == nil {
			s.Done("Backed up your instance.")
		}
		return err
	})
	if err != nil {
		var ee *ui.ExitError
		if errors.As(err, &ee) {
			return "", &ui.ExitError{Code: ee.Code, Msg: ee.Msg + " " + name(u.From) + " is still running.", Fix: ee.Fix, Err: err, LogPath: ee.LogPath, Tail: ee.Tail}
		}
		return "", ui.Wrap(err, fmt.Sprintf("Couldn't back up your instance. %s is still running.", name(u.From)), "Run rexctl backup --verbose to see why, then run rexctl upgrade again.")
	}
	return path, nil
}

func (e *Engine) switchTo(ctx context.Context, rel *release.Release, res Result) (Result, error) {
	ctx, cancel := ui.OutliveHangup(ctx)
	defer cancel()
	u := e.Inst.State.Upgrade
	if err := e.mark(release.PhaseSwitching); err != nil {
		e.clear()
		return res, err
	}
	err := e.UI.Step(ctx, "Switching to "+name(u.To), func(ctx context.Context, s *ui.Step) error {
		if err := e.up().Switch(ctx, s, rel); err != nil {
			return err
		}
		s.Done("Switched to %s.", name(u.To))
		return nil
	})
	if err != nil {
		return e.goBack(ctx, res, err)
	}
	release.KillPoint(killSwitched)
	e.Inst.State.Release = u.To
	if err := e.mark(release.PhaseRecreated); err != nil {
		e.Inst.State.Release = u.From
		return e.goBack(ctx, res, err)
	}
	return e.verify(ctx, res)
}

func (e *Engine) forward(ctx context.Context, res Result) (Result, error) {
	ctx, cancel := ui.OutliveHangup(ctx)
	defer cancel()
	u := e.Inst.State.Upgrade
	u.BackFailed, u.NoWayBack = false, false
	err := e.UI.Step(ctx, "Switching to "+name(u.To), func(ctx context.Context, s *ui.Step) error {
		if err := e.up().Switch(ctx, s, &release.Release{Version: u.To}); err != nil {
			return err
		}
		s.Done("Switched to %s.", name(u.To))
		return nil
	})
	if err != nil {
		return e.goBack(ctx, res, err)
	}
	e.Inst.State.Release = u.To
	if err := e.mark(release.PhaseRecreated); err != nil {
		return res, err
	}
	return e.verify(ctx, res)
}

func (e *Engine) verify(ctx context.Context, res Result) (Result, error) {
	u := e.Inst.State.Upgrade
	if e.Inst.State.Stopped {
		res.Stopped = true
	} else {
		err := e.UI.Step(ctx, "Starting "+name(u.To), func(ctx context.Context, s *ui.Step) error {
			if err := e.up().StartVerify(ctx, s); err != nil {
				return err
			}
			s.Done("%s is running.", name(u.To))
			return nil
		})
		switch {
		case err != nil && u.From == u.To:
			e.clear()
			return res, ui.Wrap(err, "Restored the backup, but Fluxer didn't start.", "Run rexctl logs to see why.")
		case err != nil && ctx.Err() != nil:
			return e.goBack(ctx, res, errors.Join(ErrInterrupted, err))
		case err != nil:
			return e.goBack(ctx, res, errors.Join(ErrStart, err))
		}
	}
	if err := e.mark(release.PhaseVerified); err != nil {
		return res, err
	}
	return e.finish(ctx, res)
}

func (e *Engine) finish(ctx context.Context, res Result) (Result, error) {
	u := e.Inst.State.Upgrade
	st := e.Inst.State
	st.Release = u.To
	switch {
	case u.From == u.To:
	case Rollback(u):
		st.PreviousRelease, st.PreviousBackup = "", ""
		st.RolledBackFrom = u.From
	default:
		st.PreviousRelease, st.PreviousBackup = u.From, u.Backup
		st.FailedRelease, st.RolledBackFrom = "", ""
	}
	st.Upgrade = nil
	if err := e.save(); err != nil {
		return res, err
	}
	if err := e.up().Prune(ctx, []string{u.From, u.To}); err != nil {
		e.UI.Detail("%v", err)
	}
	res.Outcome = Upgraded
	res.Backup = u.Backup
	res.Stopped = st.Stopped
	if f, ok := e.up().(finisher); ok {
		f.Finished(ctx, res)
	}
	return res, nil
}

func (e *Engine) goBack(ctx context.Context, res Result, cause error) (Result, error) {
	ctx, cancel := context.WithTimeout(context.WithoutCancel(ctx), RecoveryTimeout)
	defer cancel()
	u := e.Inst.State.Upgrade
	res.Cause = cause
	res.Backup = u.Backup
	if err := e.mark(release.PhaseGoingBack); err != nil {
		return res, errors.Join(cause, err)
	}
	res.Log = e.writeLog(ctx, LogName(u.To), cause)
	if e.NoWayBack(u) {
		u.BackFailed, u.NoWayBack = true, true
		_ = e.Inst.Save()
		return res, &ui.ExitError{Code: ui.ExitFailed, Msg: fmt.Sprintf("%s didn't start. It already updated the data, and there's no backup from before the upgrade, so rexctl can't go back to %s.", name(u.To), name(u.From)), Fix: "Run rexctl logs to see why, fix the cause, then run rexctl upgrade to start " + name(u.To) + " again.", Err: cause, LogPath: res.Log}
	}
	switch {
	case errors.Is(cause, ErrInterrupted):
	case errors.Is(cause, ErrStart):
		e.UI.Say(ui.MoodWorried, "That release didn't start, so I'm putting the old one back.")
		e.UI.Warn(fmt.Sprintf("%s didn't start.", name(u.To)), "rexctl goes back to "+name(u.From)+".")
	default:
		e.UI.Warn(fmt.Sprintf("rexctl couldn't finish the move to %s.", name(u.To)), "rexctl goes back to "+name(u.From)+".")
	}
	if err := e.back(ctx, cause); err != nil {
		u.BackFailed = true
		_ = e.Inst.Save()
		fix := "Run rexctl upgrade to try going back again, or rexctl logs to see why."
		if log := e.writeLog(ctx, goBackLog(u.From), err); log != "" {
			fix = fmt.Sprintf("Read %s to see why, then run rexctl upgrade to try going back again.", log)
		}
		return res, &ui.ExitError{Code: ui.ExitFailed, Msg: backFailed(u, cause), Fix: fix, Err: errors.Join(cause, err), LogPath: res.Log}
	}
	res.Outcome = WentBack
	res.Stopped = e.Inst.State.Stopped
	res.Restored = e.restored
	return res, nil
}

var (
	ErrStart   = errors.New("upgrade: the new release didn't start")
	ErrRestore = errors.New("upgrade: the backup didn't restore")
)

func backFailed(u *instance.Upgrade, cause error) string {
	after := fmt.Sprintf("and going back to %s didn't work either.", name(u.From))
	switch {
	case errors.Is(cause, ErrInterrupted):
		return "The upgrade stopped while it switched over, " + after
	case errors.Is(cause, ErrStart):
		return fmt.Sprintf("%s didn't start, %s", name(u.To), after)
	case errors.Is(cause, ErrRestore):
		return "The backup didn't restore, " + after
	}
	return fmt.Sprintf("rexctl couldn't switch to %s, %s", name(u.To), after)
}

func (e *Engine) back(ctx context.Context, cause error) error {
	u := e.Inst.State.Upgrade
	tasks := []ui.Task{
		{ID: "switch", Title: "Switching back to " + name(u.From), Run: func(ctx context.Context, s *ui.Step) error {
			if err := e.up().GoBack(ctx, s, u.From); err != nil {
				return err
			}
			release.KillPoint(killWentBack)
			e.Inst.State.Release = u.From
			if Rollback(u) {
				if snap, err := release.SnapshotState(e.Inst, u.From); err == nil {
					e.Inst.State.Settings, e.Inst.State.Secrets = snap.Settings, snap.Secrets
				}
			} else if errors.Is(cause, ErrStart) {
				e.Inst.State.FailedRelease = u.To
			}
			if err := e.save(); err != nil {
				return err
			}
			s.Done("Switched back to %s.", name(u.From))
			return nil
		}},
	}
	if u.Backup != "" && e.DataNewer(u.From) {
		tasks = append(tasks, ui.Task{ID: "restore", Title: "Restoring the backup from before the upgrade", Run: func(ctx context.Context, s *ui.Step) error {
			if err := e.RT.Backups().Restore(ctx, s, u.Backup); err != nil {
				return err
			}
			if e.AfterRestore != nil {
				if err := e.AfterRestore(ctx); err != nil {
					return err
				}
			}
			e.restored = true
			s.Done("Restored the backup from before the upgrade.")
			return nil
		}})
	}
	tasks = append(tasks, ui.Task{ID: "start", Title: "Starting " + name(u.From), Skip: func() bool { return e.Inst.State.Stopped }, Run: func(ctx context.Context, s *ui.Step) error {
		if err := e.up().StartVerify(ctx, s); err != nil {
			return err
		}
		s.Done("%s is running again.", name(u.From))
		return nil
	}})
	err := e.UI.Tasks(ctx, "Going back to "+name(u.From), tasks...)
	if err != nil {
		return err
	}
	e.Inst.State.Upgrade = nil
	if err := e.save(); err != nil {
		return err
	}
	if e.Inst.State.Stopped {
		e.UI.Info("Went back to %s. It stays stopped.", name(u.From))
	}
	return nil
}

func (e *Engine) Recover(ctx context.Context) (Result, error) {
	u := e.Inst.State.Upgrade
	if u == nil {
		return Result{Outcome: Nothing}, nil
	}
	res := Result{From: u.From, To: u.To, Backup: u.Backup}
	what := "upgrade to " + name(u.To)
	if Rollback(u) {
		what = "move back to " + name(u.To)
	}
	switch u.Phase {
	case release.PhaseVerified:
		return e.finish(ctx, res)
	case release.PhaseRecreated:
		e.UI.Warn(fmt.Sprintf("The %s stopped before rexctl checked that it runs.", what), "rexctl checks it now, and goes back to "+name(u.From)+" if it doesn't start.")
		return e.verify(ctx, res)
	case release.PhaseRestoring:
		if u.From == u.To {
			e.UI.Warn("The restore stopped before it finished.", "rexctl restores the backup again from the start.")
		} else {
			e.UI.Warn(fmt.Sprintf("The %s stopped while rexctl restored the backup.", what), "rexctl restores it again from the start.")
		}
		return e.restore(ctx, res)
	case release.PhaseSwitching, release.PhaseMigrated:
		e.UI.Warn(fmt.Sprintf("The %s stopped while it switched over.", what), "rexctl goes back to "+name(u.From)+".")
		return e.goBack(ctx, res, ErrInterrupted)
	case release.PhaseGoingBack:
		if u.BackFailed && !e.Has(u.From) {
			e.UI.Warn(fmt.Sprintf("rexctl can't go back to %s, because it isn't on this machine anymore.", name(u.From)), "rexctl starts "+name(u.To)+" again instead.")
			return e.forward(ctx, res)
		}
		if e.NoWayBack(u) {
			e.UI.Warn(fmt.Sprintf("rexctl can't go back to %s, because %s already updated the data and there's no backup from before.", name(u.From), name(u.To)), "rexctl starts "+name(u.To)+" again instead.")
			return e.forward(ctx, res)
		}
		e.UI.Warn(fmt.Sprintf("rexctl was going back to %s when it stopped.", name(u.From)), "rexctl finishes going back.")
		return e.goBack(ctx, res, ErrInterrupted)
	}
	e.clear()
	res.Outcome = Cleared
	return res, nil
}

func (e *Engine) Rollback(ctx context.Context, to, archive string) (Result, error) {
	st := e.Inst.State
	if c, ok := e.up().(backChecker); ok && archive == "" {
		if err := c.CheckBack(ctx, to); err != nil {
			return Result{}, err
		}
	}
	if err := Begin(e.Inst, st.Release, to, release.PhaseResolved); err != nil {
		return Result{}, err
	}
	u := st.Upgrade
	u.Backup = archive
	res := Result{From: u.From, To: u.To, Backup: archive}
	if err := release.Snapshot(e.Inst, u.From, e.imageRefs()); err != nil {
		e.clear()
		return res, ui.Wrap(err, fmt.Sprintf("Couldn't save the settings of %s.", name(u.From)), "Check that you can write to "+e.Inst.Dir+", then try again.")
	}
	if err := e.mark(release.PhaseSwitching); err != nil {
		e.clear()
		return res, err
	}
	if snap, err := release.SnapshotState(e.Inst, to); err == nil {
		for k, v := range st.Secrets {
			if _, ok := snap.Secrets[k]; !ok {
				if snap.Secrets == nil {
					snap.Secrets = map[string]string{}
				}
				snap.Secrets[k] = v
			}
		}
		res.Put = release.ChangedKeys(st.Settings, snap.Settings)
		st.Settings, st.Secrets = snap.Settings, snap.Secrets
		if err := e.save(); err != nil {
			return e.goBack(ctx, res, err)
		}
	}
	err := e.UI.Step(ctx, "Switching to "+name(to), func(ctx context.Context, s *ui.Step) error {
		if err := e.up().GoBack(ctx, s, to); err != nil {
			return err
		}
		s.Done("Switched to %s.", name(to))
		return nil
	})
	if err != nil {
		return e.goBack(ctx, res, err)
	}
	release.KillPoint(killSwitched)
	st.Release = to
	if archive == "" {
		if err := e.mark(release.PhaseRecreated); err != nil {
			st.Release = u.From
			return e.goBack(ctx, res, err)
		}
		return e.verify(ctx, res)
	}
	if err := e.mark(release.PhaseRestoring); err != nil {
		st.Release = u.From
		return e.goBack(ctx, res, err)
	}
	return e.restore(ctx, res)
}

func (e *Engine) RestoreHere(ctx context.Context, archive string) (Result, error) {
	cur := e.Inst.State.Release
	if err := Begin(e.Inst, cur, cur, release.PhaseResolved); err != nil {
		return Result{}, err
	}
	u := e.Inst.State.Upgrade
	u.Backup = archive
	if err := e.mark(release.PhaseRestoring); err != nil {
		e.clear()
		return Result{}, err
	}
	return e.restore(ctx, Result{From: cur, To: cur, Backup: archive})
}

func (e *Engine) restore(ctx context.Context, res Result) (Result, error) {
	u := e.Inst.State.Upgrade
	err := e.UI.Step(ctx, "Restoring the backup", func(ctx context.Context, s *ui.Step) error {
		if err := e.RT.Backups().Restore(ctx, s, u.Backup); err != nil {
			return err
		}
		if e.AfterRestore != nil {
			if err := e.AfterRestore(ctx); err != nil {
				return err
			}
		}
		s.Done("Restored the backup.")
		return nil
	})
	var begun *runtime.RestoreBegunError
	if errors.As(err, &begun) {
		return res, runtime.HalfRestored(ctx, err, release.UnfinishedFix(u))
	}
	if err != nil && u.From == u.To {
		var ee *ui.ExitError
		if errors.As(err, &ee) {
			return res, &ui.ExitError{Code: ee.Code, Msg: ee.Msg, Fix: release.UnfinishedFix(u), Err: err, LogPath: ee.LogPath, Tail: ee.Tail}
		}
		return res, ui.Wrap(err, "The backup didn't restore.", release.UnfinishedFix(u))
	}
	if err != nil {
		return e.goBack(ctx, res, errors.Join(ErrRestore, err))
	}
	release.KillPoint(killRestored)
	if err := e.mark(release.PhaseRecreated); err != nil {
		return e.goBack(ctx, res, err)
	}
	return e.verify(ctx, res)
}

func (e *Engine) Settle() (string, bool, error) {
	s, ok := e.up().(settler)
	if !ok {
		return "", false, nil
	}
	target := e.Inst.State.Release
	if u := e.Inst.State.Upgrade; u != nil && u.Phase == release.PhaseGoingBack && !e.NoWayBack(u) {
		target = u.From
	}
	moved, err := s.Settle(target)
	return target, moved, err
}

func (e *Engine) NoWayBack(u *instance.Upgrade) bool {
	return u.Backup == "" && u.From != u.To && e.DataNewer(u.From)
}

func (e *Engine) DataNewer(version string) bool {
	s, ok := e.up().(stamper)
	return ok && s.DataNewer(version)
}

func (e *Engine) CheckBack(ctx context.Context, to string) error {
	if c, ok := e.up().(backChecker); ok {
		return c.CheckBack(ctx, to)
	}
	return nil
}
