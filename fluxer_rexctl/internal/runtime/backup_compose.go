// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"cmp"
	"context"
	"errors"
	"slices"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type StartAgainError struct {
	Path string
	Err  error
}

func (e *StartAgainError) Error() string {
	return "runtime: saved " + e.Path + ", then Fluxer didn't start: " + e.Err.Error()
}

func (e *StartAgainError) Unwrap() error { return e.Err }

const copyRate = 100_000_000

func pauseFor(bytes uint64) time.Duration {
	return 30*time.Second + time.Duration(bytes/copyRate)*time.Second
}

func scopeOf(o BackupOptions) string {
	if o.WithoutUploads {
		return backup.ScopeWithoutUploads
	}
	return backup.ScopeFull
}

func BackupStageTitle(stage string) string {
	switch stage {
	case backup.StageFiles:
		return "Copying your settings"
	case backup.StageDatabase:
		return "Saving the database"
	case backup.StageStopping:
		return "Pausing Fluxer to copy uploads"
	case backup.StageVolumes:
		return "Copying uploads and search data while Fluxer is paused"
	case backup.StageStarting:
		return "Starting Fluxer again"
	case backup.StageCompressing:
		return "Compressing the backup while Fluxer runs"
	}
	return ""
}

func backupStageDone(stage string) string {
	switch stage {
	case backup.StageFiles:
		return "Copied your settings"
	case backup.StageDatabase:
		return "Saved the database"
	case backup.StageStopping:
		return "Paused Fluxer"
	case backup.StageVolumes:
		return "Copied uploads and search data"
	case backup.StageStarting:
		return "Started Fluxer again"
	case backup.StageCompressing:
		return "Compressed the backup"
	}
	return ""
}

type stageList struct {
	s   *ui.Step
	cur string
}

func (l *stageList) to(stage string) {
	t := BackupStageTitle(stage)
	if l.s == nil || t == "" || stage == l.cur {
		return
	}
	l.finish()
	l.cur = stage
	l.s.Title(t)
}

func (l *stageList) finish() {
	if l.s != nil && l.cur != "" {
		l.s.Item(l.cur, backupStageDone(l.cur), ui.StateOK, "")
	}
	l.cur = ""
}

type composeBackups struct{ c *compose }

func (b composeBackups) Plan(ctx context.Context, o BackupOptions) (BackupPlan, error) {
	p, err := b.c.project(ctx)
	if err != nil {
		return BackupPlan{}, err
	}
	sz, err := backup.Measure(ctx, b.c.inst, p, scopeOf(o))
	if err != nil {
		return BackupPlan{}, b.c.engineError(err, "Couldn't measure your instance's data.")
	}
	b.c.planned = &sz
	plan := BackupPlan{NeedBytes: int64(sz.Need), FreeBytes: -1}
	if free, err := backup.FreeSpace(cmp.Or(o.Dir, b.c.inst.BackupDir())); err == nil {
		plan.FreeBytes = int64(free)
	}
	if sz.Volumes == 0 {
		return plan, nil
	}
	containers, err := p.PS(ctx)
	if err != nil {
		return plan, nil
	}
	for _, c := range containers {
		if part := PartOf(c.Service); c.State == "running" && !slices.Contains(plan.PauseParts, part) {
			plan.PauseParts = append(plan.PauseParts, part)
		}
	}
	if len(plan.PauseParts) > 0 {
		plan.PauseFor = pauseFor(sz.Volumes)
	}
	return plan, nil
}

func stoppedServices(ctx context.Context, p *engine.Project) []string {
	cs, err := p.PS(ctx)
	if err != nil {
		return nil
	}
	running := false
	var stopped []string
	for _, c := range cs {
		switch {
		case c.State == "running":
			running = true
		case !stack.OneShot[c.Service] && !slices.Contains(stopped, c.Service):
			stopped = append(stopped, c.Service)
		}
	}
	if !running {
		return nil
	}
	return stopped
}

func (b composeBackups) Create(ctx context.Context, s *ui.Step, o BackupOptions) (string, error) {
	p, err := b.c.project(ctx)
	if err != nil {
		return "", err
	}
	down := stoppedServices(ctx, p)
	paused := false
	stages := &stageList{s: s}
	opts := backup.CreateOptions{
		Reason: cmp.Or(o.Reason, "manual"),
		Scope:  scopeOf(o),
		Sizes:  b.c.planned,
		Progress: func(stage string) {
			paused = paused || stage == backup.StageStarting
			stages.to(stage)
		},
		OnEvent: func(ev engine.Event) {
			if s != nil && ev.Total > 0 {
				s.Progress(ev.Current, ev.Total)
			}
		},
	}
	b.c.planned = nil
	info, err := backup.Create(ctx, b.c.inst, p, opts)
	if err != nil {
		return "", b.c.createError(err)
	}
	stages.finish()
	if !paused {
		return info.Path, nil
	}
	if err := p.WaitReady(ctx, engine.DefaultReadyTimeout, ShowReadiness(s, "Starting Fluxer again"), down...); err != nil {
		return info.Path, &StartAgainError{Path: info.Path, Err: err}
	}
	if len(down) > 0 && s != nil {
		s.Warn(leftStopped(partsOf(down)), "Run rexctl start to start them.")
	}
	return info.Path, nil
}

func (c *compose) createError(err error) error {
	var disk *backup.DiskError
	var missing *backup.MissingDataError
	var dump *backup.DumpError
	var stopped *backup.InterruptedError
	var compress *backup.CompressError
	var unknown *backup.SpaceUnknownError
	switch {
	case errors.Is(err, backup.ErrNeverStarted), errors.As(err, &disk), errors.As(err, &missing), errors.As(err, &dump),
		errors.As(err, &stopped), errors.As(err, &compress), errors.As(err, &unknown):
		return err
	}
	return c.engineError(err, "Couldn't back up your instance.")
}

func leftStopped(parts []string) string {
	if len(parts) == 1 {
		return "Fluxer's " + parts[0] + " part wasn't running before the backup, so rexctl left it stopped."
	}
	return "Fluxer's " + ui.JoinAnd(parts) + " parts weren't running before the backup, so rexctl left them stopped."
}

func (composeBackups) Verify(ctx context.Context, archive string) (*backup.Manifest, error) {
	return backup.Verify(archive)
}

func (b composeBackups) Restore(ctx context.Context, s *ui.Step, archive string) error {
	e, err := b.c.engine(ctx)
	if err != nil {
		return err
	}
	_, err = backup.Restore(ctx, archive, b.c.inst.Dir, e, backup.RestoreOptions{
		Progress: func(stage string) {
			if t := RestoreStageTitle(stage); t != "" && s != nil {
				s.Title(t)
			}
		},
	})
	return b.c.engineError(err, "Couldn't restore the backup.")
}

func RestoreStageTitle(stage string) string {
	switch stage {
	case backup.StageVerifying:
		return "Checking the backup"
	case backup.StageClearing:
		return "Removing the current instance"
	case backup.StageFiles:
		return "Restoring your settings"
	case backup.StagePulling:
		return "Downloading Fluxer"
	case backup.StageDatabase:
		return "Restoring the database"
	case backup.StageVolumes:
		return "Restoring uploads and search data"
	case backup.StageStarting:
		return "Starting Fluxer"
	}
	return ""
}
