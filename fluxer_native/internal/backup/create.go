// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/lock"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const (
	natsVolume   = "nats-data"
	stopTimeout  = 2 * time.Minute
	startTimeout = 15 * time.Minute
)

type Target struct {
	Layout   spec.Layout
	Env      spec.Env
	PGBinDir string
}

type Supervisor interface {
	PS(ctx context.Context) ([]spec.PsEntry, error)
	Service(ctx context.Context, name, action string) error
	JetStream(ctx context.Context, action string) error
}

type CreateOptions struct {
	Dir      string
	EnvFile  string
	Include  string
	Project  string
	Release  string
	Reason   string
	Progress func(stage string)
}

type volumeDirEntry struct {
	name string
	dir  func(spec.Layout) string
	skip []string
}

var volumeDirs = []volumeDirEntry{
	{spec.SeaweedFSVolume, spec.Layout.SeaweedFSDir, nil},
	{spec.ObjectsVolume, spec.Layout.ObjectsDir, []string{spec.ObjectsStagingDir}},
	{spec.ValkeyVolume, spec.Layout.ValkeyDir, nil},
	{natsVolume, spec.Layout.NATSDir, nil},
	{"meilisearch-data", spec.Layout.MeilisearchDir, nil},
}

func uploadsVolume(name string) bool {
	return name == spec.SeaweedFSVolume || name == spec.ObjectsVolume
}

func volumeDir(l spec.Layout, name string) (string, bool) {
	for _, v := range volumeDirs {
		if v.name == name {
			return v.dir(l), true
		}
	}
	return "", false
}

func (t Target) externalDatabase() bool { return t.Env["FLUXER_POSTGRES_URL"] != "" }
func (t Target) filesystem() bool {
	return strings.TrimSpace(t.Env[spec.StorageBackendEnv]) == string(spec.StorageFilesystem)
}
func (t Target) externalUploads() bool { return !t.filesystem() && t.Env[spec.S3EndpointEnv] != "" }
func (t Target) bundledKV() bool {
	return t.Env[spec.KVURLEnv] == "" && strings.TrimSpace(t.Env[spec.KVModeEnv]) != string(spec.KVPostgres)
}

func (t Target) volumesToCopy() []string {
	var out []string
	for _, v := range volumeDirs {
		switch v.name {
		case spec.SeaweedFSVolume:
			if t.filesystem() || t.externalUploads() {
				continue
			}
		case spec.ObjectsVolume:
			if !t.filesystem() {
				continue
			}
		case spec.ValkeyVolume:
			if !t.bundledKV() {
				continue
			}
		}
		if fi, err := os.Stat(v.dir(t.Layout)); err == nil && fi.IsDir() {
			out = append(out, v.name)
		}
	}
	return out
}

func Create(ctx context.Context, t Target, sup Supervisor, opts CreateOptions) (path string, err error) {
	progress := opts.Progress
	if progress == nil {
		progress = func(string) {}
	}
	dir := cmp.Or(opts.Dir, t.Layout.BackupsDir())
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	os.Chmod(dir, 0o700)
	removePartials(dir)

	now := time.Now().UTC().Truncate(time.Second)
	final, err := newPath(dir, t.Env["FLUXER_DOMAIN"], now)
	if err != nil {
		return "", err
	}
	m := &Manifest{
		Project:   cmp.Or(opts.Project, "fluxer"),
		Domain:    t.Env["FLUXER_DOMAIN"],
		Release:   opts.Release,
		Reason:    opts.Reason,
		CreatedAt: now,
	}
	if t.externalDatabase() {
		m.Skipped = append(m.Skipped, SkippedDatabase)
	}
	if t.externalUploads() {
		m.Skipped = append(m.Skipped, SkippedUploads)
	}

	var ps []spec.PsEntry
	if sup == nil {
		held, err := lockData(t.Layout)
		if err != nil {
			return "", err
		}
		defer held.Release()
	} else if ps, err = sup.PS(ctx); err != nil {
		return "", err
	}
	if sup != nil && !t.externalDatabase() && !running(ps, spec.Postgres) {
		return "", errors.New("backup: postgres is not running")
	}

	w, err := createArchive(final + PartialSuffix)
	if err != nil {
		return "", err
	}
	ok := false
	defer func() {
		if !ok {
			w.abort()
		}
	}()

	progress(StageFiles)
	for _, f := range []struct{ src, name string }{{opts.EnvFile, EnvFileName}, {t.Layout.SecretsFile(), SecretsFileName}, {t.Layout.ModesFile(), ModesFileName}} {
		if f.src == "" {
			continue
		}
		if err := w.addFile(f.src, f.name); err != nil && !errors.Is(err, os.ErrNotExist) {
			return "", err
		}
	}
	if opts.Include != "" {
		reserved := []string{strings.TrimPrefix(EnvFileName, FilesPrefix), strings.TrimPrefix(SecretsFileName, FilesPrefix), strings.TrimPrefix(ModesFileName, FilesPrefix)}
		if err := w.addTree(opts.Include, strings.TrimSuffix(FilesPrefix, "/"), reserved); err != nil {
			return "", err
		}
	}

	if sup != nil {
		progress(StageStopping)
		var stopped []string
		stopped, err = stopChildren(ctx, sup, ps)
		defer func() {
			progress(StageStarting)
			if serr := startChildren(context.WithoutCancel(ctx), sup, stopped); serr != nil && err == nil {
				err = fmt.Errorf("backup: %s was written but the services did not start again: %w", path, serr)
			}
		}()
		if err != nil {
			return "", err
		}
	}

	if !t.externalDatabase() {
		progress(StageDatabase)
		sock := t.Layout.PGSocketDir()
		if sup == nil {
			pg, err := t.startPostgres(ctx, t.Layout.Tmp("backup"))
			if err != nil {
				return "", err
			}
			defer pg.stop()
			sock = pg.sock
		}
		if err := t.addDump(ctx, w, sock, final+".dump"+PartialSuffix); err != nil {
			return "", err
		}
		m.Database = DumpName
	}

	volumes := t.volumesToCopy()
	if sup != nil && slices.Contains(volumes, natsVolume) {
		if err := sup.JetStream(ctx, spec.ActionStop); err != nil {
			return "", fmt.Errorf("backup: pausing JetStream: %w", err)
		}
		defer func() {
			if serr := sup.JetStream(context.WithoutCancel(ctx), spec.ActionStart); serr != nil && err == nil {
				err = fmt.Errorf("backup: JetStream did not start again: %w", serr)
			}
		}()
	}

	progress(StageVolumes)
	m.Volumes = []string{}
	for _, name := range volumes {
		i := slices.IndexFunc(volumeDirs, func(v volumeDirEntry) bool { return v.name == name })
		if err := w.addTree(volumeDirs[i].dir(t.Layout), VolumesPrefix+name, volumeDirs[i].skip); err != nil {
			return "", err
		}
		m.Volumes = append(m.Volumes, name)
	}

	if err := w.finish(m); err != nil {
		return "", err
	}
	if err := os.Rename(final+PartialSuffix, final); err != nil {
		return "", err
	}
	ok = true
	syncDir(dir)
	return final, nil
}

func syncDir(dir string) {
	d, err := os.Open(dir)
	if err != nil {
		return
	}
	d.Sync()
	d.Close()
}

func lockData(l spec.Layout) (*lock.Lock, error) {
	if err := os.MkdirAll(l.Data, 0o700); err != nil {
		return nil, err
	}
	return lock.Acquire(l.LockFile())
}

func active(s spec.State) bool {
	return s != spec.StateStopped && s != spec.StateExited
}

func running(ps []spec.PsEntry, name string) bool {
	for _, e := range ps {
		if e.Name == name {
			return e.State == spec.StateRunning
		}
	}
	return false
}

func stopRank(name string) int {
	switch name {
	case spec.Gateway:
		return 0
	case spec.API:
		return 1
	case spec.Meilisearch, spec.SeaweedFSFiler:
		return 3
	case spec.SeaweedFS:
		return 4
	case spec.Valkey:
		return 5
	}
	return 2
}

func stopChildren(ctx context.Context, sup Supervisor, ps []spec.PsEntry) ([]string, error) {
	var names []string
	for _, e := range ps {
		if e.Oneshot || e.Name == spec.Postgres || !active(e.State) {
			continue
		}
		names = append(names, e.Name)
	}
	slices.SortStableFunc(names, func(a, b string) int { return cmp.Compare(stopRank(a), stopRank(b)) })
	for _, name := range names {
		if err := sup.Service(ctx, name, spec.ActionStop); err != nil {
			return names, fmt.Errorf("backup: stopping %s: %w", name, err)
		}
	}
	return names, waitChildren(ctx, sup, names, stopTimeout, func(e spec.PsEntry) bool { return !active(e.State) })
}

func startChildren(ctx context.Context, sup Supervisor, names []string) error {
	var errs []error
	for _, name := range slices.Backward(names) {
		if err := sup.Service(ctx, name, spec.ActionStart); err != nil {
			errs = append(errs, fmt.Errorf("starting %s: %w", name, err))
		}
	}
	if err := errors.Join(errs...); err != nil {
		return err
	}
	return waitChildren(ctx, sup, names, startTimeout, func(e spec.PsEntry) bool {
		return e.State == spec.StateRunning && (e.Health == spec.HealthHealthy || e.Health == spec.HealthNone)
	})
}

func waitChildren(ctx context.Context, sup Supervisor, names []string, timeout time.Duration, done func(spec.PsEntry) bool) error {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	for {
		ps, err := sup.PS(ctx)
		if err != nil {
			return err
		}
		var pending []string
		for _, name := range names {
			if i := slices.IndexFunc(ps, func(e spec.PsEntry) bool { return e.Name == name }); i >= 0 && !done(ps[i]) {
				pending = append(pending, name)
			}
		}
		if len(pending) == 0 {
			return nil
		}
		if err := sleep(ctx); err != nil {
			return fmt.Errorf("backup: waiting for %v: %w", pending, err)
		}
	}
}

func sleep(ctx context.Context) error {
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-time.After(250 * time.Millisecond):
		return nil
	}
}
