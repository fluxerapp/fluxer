// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"bytes"
	"cmp"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	containerIncludeDir = ContainerData + "/backups/.instance-files"
	fluxerdNoSpace      = 4
)

type containerBackups struct{ r *containerRT }

func (b containerBackups) volumeSize(ctx context.Context, e containerEngine) int64 {
	var out bytes.Buffer
	if err := e.ExecSingle(ctx, b.r.name(), engine.ExecOptions{Stdout: &out}, "du", "-sk", ContainerData); err != nil {
		return -1
	}
	kb, err := strconv.ParseInt(strings.Fields(out.String() + " x")[0], 10, 64)
	if err != nil {
		return -1
	}
	return kb * 1024
}

func (b containerBackups) Plan(ctx context.Context, o BackupOptions) (BackupPlan, error) {
	if o.WithoutUploads {
		return BackupPlan{}, withoutUploadsInContainer()
	}
	plan := BackupPlan{NeedBytes: -1, FreeBytes: -1}
	if free, err := backup.FreeSpace(cmp.Or(o.Dir, b.r.inst.BackupDir())); err == nil {
		plan.FreeBytes = int64(free)
	}
	e, err := b.r.engine(ctx)
	if err != nil {
		return plan, err
	}
	if size := b.volumeSize(ctx, e); size > 0 {
		plan.NeedBytes = size * 110 / 100
	}
	if st, err := b.r.Status(ctx); err == nil && st.Running {
		for _, p := range st.Parts {
			if p.State != PartStopped && p.State != PartAbsent {
				plan.PauseParts = append(plan.PauseParts, p.Name)
			}
		}
		if plan.NeedBytes > 0 {
			plan.PauseFor = pauseFor(uint64(plan.NeedBytes))
		}
	}
	return plan, nil
}

func withoutUploadsInContainer() error {
	return unsupported("This instance backs up its uploads every time.", "Run rexctl backup without leaving out uploads.")
}

func instanceFiles(inst *instance.Instance) ([]engine.File, error) {
	var out []engine.File
	for _, name := range native.InstanceFiles {
		root := inst.Path(filepath.FromSlash(name))
		err := filepath.WalkDir(root, func(p string, d fs.DirEntry, err error) error {
			if errors.Is(err, fs.ErrNotExist) && p == root {
				return nil
			}
			if err != nil {
				return err
			}
			rel, err := filepath.Rel(inst.Dir, p)
			if err != nil {
				return err
			}
			rel = filepath.ToSlash(rel)
			if d.IsDir() {
				out = append(out, engine.File{Name: rel + "/", Mode: 0o700})
				return nil
			}
			if !d.Type().IsRegular() {
				return nil
			}
			data, err := os.ReadFile(p)
			if err != nil {
				return err
			}
			out = append(out, engine.File{Name: rel, Data: data, Mode: 0o600})
			return nil
		})
		if err != nil {
			return nil, err
		}
	}
	return withParents(out), nil
}

func withParents(files []engine.File) []engine.File {
	seen := map[string]bool{}
	var out []engine.File
	add := func(dir string) {
		var chain []string
		for d := dir; d != "." && d != "/" && d != ""; d = path.Dir(d) {
			chain = append([]string{d}, chain...)
		}
		for _, d := range chain {
			if !seen[d] {
				seen[d] = true
				out = append(out, engine.File{Name: d + "/", Mode: 0o700})
			}
		}
	}
	for _, f := range files {
		if strings.HasSuffix(f.Name, "/") {
			add(strings.TrimSuffix(f.Name, "/"))
			continue
		}
		add(path.Dir(f.Name))
		out = append(out, f)
	}
	return out
}

func archiveName(domain string, at time.Time, n int) string {
	var b strings.Builder
	for _, r := range strings.ToLower(domain) {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9', r == '.', r == '-':
			b.WriteRune(r)
		default:
			b.WriteRune('-')
		}
	}
	d := cmp.Or(strings.Trim(b.String(), ".-"), "instance")
	stamp := at.UTC().Format("20060102-150405")
	if n > 0 {
		return fmt.Sprintf("fluxer-%s-%s-%d.tar.zst", d, stamp, n)
	}
	return fmt.Sprintf("fluxer-%s-%s.tar.zst", d, stamp)
}

func freeArchivePath(dir, domain string) (string, error) {
	now := time.Now()
	for n := 0; n < 100; n++ {
		p := filepath.Join(dir, archiveName(domain, now, n))
		if _, err := os.Lstat(p); errors.Is(err, fs.ErrNotExist) {
			if _, err := os.Lstat(p + ".partial"); errors.Is(err, fs.ErrNotExist) {
				return p, nil
			}
		}
	}
	return "", errors.New("runtime: no free backup file name")
}

func (b containerBackups) Create(ctx context.Context, s *ui.Step, o BackupOptions) (string, error) {
	if o.WithoutUploads {
		return "", withoutUploadsInContainer()
	}
	r := b.r
	e, err := r.engine(ctx)
	if err != nil {
		return "", err
	}
	c, ok, err := e.Inspect(ctx, r.name())
	if err != nil {
		return "", ContainerError(err, "Couldn't read the state of Fluxer's container.")
	}
	if !ok {
		return "", ui.Refused("Fluxer's container isn't there, so there is nothing to back up.", "Run rexctl start to create it, then try again.")
	}
	dir := cmp.Or(o.Dir, r.inst.BackupDir())
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", ui.Wrap(err, "Couldn't create "+dir+".", "Check that you can write there, then try again.")
	}
	files, err := instanceFiles(r.inst)
	if err != nil {
		return "", ui.Wrap(err, "Couldn't read rexctl's own files in "+r.inst.Dir+".", "Check that you can read them, then try again.")
	}
	_ = e.ExecSingle(ctx, r.name(), engine.ExecOptions{}, "rm", "-rf", containerIncludeDir)
	staged := append([]engine.File{{Name: "backups/", Mode: 0o700}, {Name: "backups/.instance-files/", Mode: 0o700}}, prefixed("backups/.instance-files/", files)...)
	if err := e.WriteSingleFiles(ctx, r.name(), ContainerData, ContainerUID, ContainerUID, staged...); err != nil {
		return "", ContainerError(err, "Couldn't put rexctl's own files in the backup.")
	}
	dest, err := freeArchivePath(dir, r.inst.Domain())
	if err != nil {
		return "", err
	}
	partial := dest + ".partial"
	out, err := os.OpenFile(partial, os.O_CREATE|os.O_WRONLY|os.O_EXCL, 0o600)
	if err != nil {
		return "", ui.Wrap(err, "Couldn't write the backup to "+dir+".", "Check that you can write there, then try again.")
	}
	defer os.Remove(partial)
	args := []string{"backup", "--config", containerEnvFile, "--out", "-", "--reason", cmp.Or(o.Reason, "manual"), "--include", containerIncludeDir}
	if s != nil && c.Running() {
		s.Title("Pausing Fluxer to copy its data")
	}
	log := stepLog{s}
	if c.Running() {
		err = e.ExecSingle(ctx, r.name(), engine.ExecOptions{Stdout: out, Stderr: log}, append([]string{"fluxerd"}, args...)...)
	} else {
		spec := r.oneOff(r.inst.State.Release, args)
		err = e.RunOnce(ctx, spec, nil, out, log)
	}
	if cerr := out.Close(); err == nil {
		err = cerr
	}
	_ = e.ExecSingle(context.WithoutCancel(ctx), r.name(), engine.ExecOptions{}, "rm", "-rf", containerIncludeDir)
	if err != nil {
		var exit *engine.ExitError
		if errors.As(err, &exit) && exit.Code == fluxerdNoSpace {
			return "", ui.Wrap(err, "There isn't enough free space in Fluxer's volume to make the backup.", "Free up space on the disk that holds "+VolumeName(r.inst)+", then try again.")
		}
		return "", ContainerError(err, "Couldn't back up your instance.")
	}
	if _, err := backup.Verify(partial); err != nil {
		return "", ui.Wrap(err, "The backup didn't come out complete.", "Run rexctl backup --verbose to see why.")
	}
	if err := os.Rename(partial, dest); err != nil {
		return "", err
	}
	if s != nil {
		s.Item("copy", "Copied the database, uploads and search data", ui.StateOK, "")
	}
	if !c.Running() {
		return dest, nil
	}
	if err := r.n.WaitReady(ctx, engine.DefaultReadyTimeout, ShowReadiness(s, "Starting Fluxer again")); err != nil {
		return dest, &StartAgainError{Path: dest, Err: err}
	}
	return dest, nil
}

func prefixed(prefix string, files []engine.File) []engine.File {
	out := make([]engine.File, len(files))
	for i, f := range files {
		f.Name = prefix + f.Name
		out[i] = f
	}
	return out
}

func (r *containerRT) oneOff(version string, args []string) engine.SingleSpec {
	return engine.SingleSpec{
		Name:         r.name() + "-" + strconv.FormatInt(time.Now().UnixNano(), 36),
		Image:        ImageRef(version),
		Cmd:          args,
		Env:          []string{"FLUXER_BACKUP_ON_UPGRADE=false"},
		Labels:       map[string]string{LabelManagedBy: ManagedByRexctl},
		Volume:       VolumeName(r.inst),
		VolumeTarget: ContainerData,
	}
}

func (containerBackups) Verify(ctx context.Context, archive string) (*backup.Manifest, error) {
	return backup.Verify(archive)
}

func (b containerBackups) Restore(ctx context.Context, s *ui.Step, archive string) error {
	r := b.r
	e, err := r.engine(ctx)
	if err != nil {
		return err
	}
	f, err := os.Open(archive)
	if err != nil {
		return ui.Wrap(err, "Couldn't open "+archive+".", "Check the path and that you can read it.")
	}
	defer f.Close()
	if err := e.StopSingle(ctx, r.name(), ContainerStop); err != nil {
		return ContainerError(err, "Couldn't stop Fluxer before the restore.")
	}
	if err := keepDown(context.WithoutCancel(ctx), e, r.name(), restartPolicy(r.inst)); err != nil {
		return ContainerError(err, "Couldn't keep Fluxer stopped during the restore.")
	}
	w := &restoreWatch{out: stepLog{s}}
	spec := r.oneOff(r.inst.State.Release, []string{"restore", "--config", containerEnvFile, "-"})
	if err := e.RunOnce(ctx, spec, f, w, w); err != nil {
		err = ContainerError(err, "Couldn't restore the backup.")
		if !w.touched() {
			return err
		}
		r.inst.State.Stopped = true
		return &RestoreBegunError{Err: errors.Join(err, saveState(r.inst))}
	}
	return nil
}

func RestoredSecrets(ctx context.Context, r Runtime) (map[string]string, error) {
	c, ok := r.(*containerRT)
	if !ok {
		return nil, ErrUnsupported
	}
	return c.ReadSecrets(ctx)
}
