// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"cmp"
	"context"
	"errors"
	"io"
	"os"
	"strings"
	"sync"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type nativeBackups struct{ r *nativeRT }

var measured = struct {
	sync.Mutex
	need map[string]uint64
}{need: map[string]uint64{}}

func (b nativeBackups) measure() (uint64, error) {
	root := b.r.n.Root()
	size, err := backup.DirSize(root)
	if err != nil {
		return 0, ui.Wrap(err, "Couldn't measure how much data your instance keeps in "+root+".", "Run the command again as root.")
	}
	need := size * 110 / 100
	measured.Lock()
	measured.need[root] = need
	measured.Unlock()
	return need, nil
}

func (b nativeBackups) need() (uint64, error) {
	root := b.r.n.Root()
	measured.Lock()
	need, ok := measured.need[root]
	delete(measured.need, root)
	measured.Unlock()
	if ok {
		return need, nil
	}
	return b.measure()
}

func (b nativeBackups) Plan(ctx context.Context, o BackupOptions) (BackupPlan, error) {
	if o.WithoutUploads {
		return BackupPlan{}, withoutUploadsOnNative()
	}
	need, err := b.measure()
	if err != nil {
		return BackupPlan{}, err
	}
	plan := BackupPlan{NeedBytes: int64(need), FreeBytes: -1}
	if free, err := backup.FreeSpace(cmp.Or(o.Dir, b.r.inst.BackupDir())); err == nil {
		plan.FreeBytes = int64(free)
	}
	if st, err := b.r.Status(ctx); err == nil && st.Running {
		for _, p := range st.Parts {
			if p.State != PartStopped && p.State != PartAbsent {
				plan.PauseParts = append(plan.PauseParts, p.Name)
			}
		}
		plan.PauseFor = pauseFor(need)
	}
	return plan, nil
}

func withoutUploadsOnNative() error {
	return unsupported("A native instance backs up its uploads every time.", "Run rexctl backup without leaving out uploads.")
}

func (b nativeBackups) Create(ctx context.Context, s *ui.Step, o BackupOptions) (string, error) {
	if o.WithoutUploads {
		return "", withoutUploadsOnNative()
	}
	if err := b.r.root(); err != nil {
		return "", err
	}
	set, err := deploy.ReleaseSet(b.r.inst)
	if err != nil {
		return "", err
	}
	dir := cmp.Or(o.Dir, b.r.inst.BackupDir())
	need, err := b.need()
	if err != nil {
		return "", err
	}
	if err := backup.NeedSpace(b.r.n.Root(), need); err != nil {
		return "", err
	}
	if !sameDisk(dir, b.r.n.Root()) {
		if err := backup.NeedSpace(dir, need); err != nil {
			return "", err
		}
	}
	st, _ := b.r.Status(ctx)
	if s != nil && st.Running {
		s.Title("Pausing Fluxer to copy its data")
	}
	path, err := b.r.n.Backup(ctx, set, dir, cmp.Or(o.Reason, "manual"), stepLog{s})
	if err != nil {
		return "", NativeError(err, "Couldn't back up your instance.")
	}
	if s != nil {
		s.Item("copy", "Copied the database, uploads and search data", ui.StateOK, "")
	}
	if !st.Running {
		return path, nil
	}
	if err := b.r.n.WaitReady(ctx, engine.DefaultReadyTimeout, ShowReadiness(s, "Starting Fluxer again")); err != nil {
		return path, &StartAgainError{Path: path, Err: err}
	}
	return path, nil
}

func sameDisk(a, b string) bool {
	x, errX := host.DiskID(a)
	y, errY := host.DiskID(b)
	return errX == nil && errY == nil && x == y
}

func (nativeBackups) Verify(ctx context.Context, archive string) (*backup.Manifest, error) {
	return backup.Verify(archive)
}

type RestoreBegunError struct{ Err error }

func (e *RestoreBegunError) Error() string { return e.Err.Error() }

func (e *RestoreBegunError) Unwrap() error { return e.Err }

func (b nativeBackups) Restore(ctx context.Context, s *ui.Step, archive string) error {
	if err := b.r.root(); err != nil {
		return err
	}
	set, err := deploy.ReleaseSet(b.r.inst)
	if err != nil {
		return err
	}
	release, err := holdStopped(b.r.n)
	if err != nil {
		return err
	}
	w := &restoreWatch{out: stepLog{s}}
	if err := b.r.n.Restore(ctx, set, archive, w); err != nil {
		err = NativeError(err, "Couldn't restore the backup.")
		if !w.touched() {
			return errors.Join(err, release())
		}
		return &RestoreBegunError{Err: err}
	}
	return release()
}

func holdStopped(n *native.Stack) (func() error, error) {
	if _, err := os.Lstat(n.StoppedMarker()); err == nil {
		return func() error { return nil }, nil
	}
	if err := setStopped(n, true); err != nil {
		return nil, err
	}
	return func() error { return setStopped(n, false) }, nil
}

func HalfRestored(ctx context.Context, err error, fix string) error {
	const left = " Fluxer's data may be only partly restored, so rexctl left Fluxer stopped."
	msg, code := "Couldn't restore the backup."+left, ui.ExitFailed
	var ee *ui.ExitError
	switch {
	case ctx.Err() != nil, errors.Is(err, context.Canceled), errors.As(err, &ee) && ee.Code == ui.ExitInterrupted:
		msg, code = "Stopped the restore before it finished."+left, ui.ExitInterrupted
	case errors.As(err, &ee) && ee.Msg != "":
		msg = ee.Msg + left
	}
	return &ui.ExitError{Code: code, Msg: msg, Fix: fix, Err: err}
}

type restoreWatch struct {
	out     io.Writer
	mu      sync.Mutex
	partial string
	begun   bool
}

const restoreStagePrefix = "restore: "

func (w *restoreWatch) Write(p []byte) (int, error) {
	w.mu.Lock()
	lines := strings.Split(w.partial+string(p), "\n")
	w.partial = lines[len(lines)-1]
	for _, line := range lines[:len(lines)-1] {
		i := strings.LastIndex(line, restoreStagePrefix)
		if i < 0 {
			continue
		}
		stage := strings.TrimSpace(line[i+len(restoreStagePrefix):])
		if stage != backup.StageVerifying && !strings.Contains(stage, " ") {
			w.begun = true
		}
	}
	w.mu.Unlock()
	return w.out.Write(p)
}

func (w *restoreWatch) touched() bool {
	w.mu.Lock()
	defer w.mu.Unlock()
	return w.begun
}
