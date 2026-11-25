// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/lock"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const (
	lockName     = ".backup.lock"
	natsVolume   = "nats-data"
	edgeCAVolume = "edge-ca"
	stopTimeout  = 2 * time.Minute
	startTimeout = 15 * time.Minute
)

type Target struct {
	Layout    spec.Layout
	Env       spec.Env
	PGBinDir  string
	PGMajor   int
	PGDataDir string
	BinDir    string
	SetModes  map[string]string
}

type Supervisor interface {
	PS(ctx context.Context) ([]spec.PsEntry, error)
	Service(ctx context.Context, name, action string) error
	JetStream(ctx context.Context, action string) error
	Maintenance(ctx context.Context, on bool) error
}

type SpaceError struct {
	Dir  string `json:"dir"`
	Need uint64 `json:"need"`
	Free uint64 `json:"free"`
}

func (e *SpaceError) Error() string {
	return fmt.Sprintf("backup: %s has %d bytes free and the backup needs about %d", e.Dir, e.Free, e.Need)
}

const spaceHeadroomPct = 110

func (t Target) Need(volumes []string) uint64 {
	var db uint64
	if !t.externalDatabase() {
		db = treeSize(t.Layout.PostgresDir())
	}
	var vol uint64
	for _, name := range volumes {
		if dir, ok := volumeDir(t.Layout, name); ok {
			vol += treeSize(dir)
		}
	}
	return (2*db + vol) * spaceHeadroomPct / 100
}

func treeSize(dir string) uint64 {
	var n uint64
	filepath.WalkDir(dir, func(_ string, d fs.DirEntry, err error) error {
		if err != nil || !d.Type().IsRegular() {
			return nil
		}
		if fi, err := d.Info(); err == nil {
			n += uint64(fi.Size())
		}
		return nil
	})
	return n
}

type CreateOptions struct {
	Dir      string
	Out      io.Writer
	Scope    string
	Locked   bool
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
	{edgeCAVolume, edgeCADir, nil},
}

func edgeCADir(l spec.Layout) string {
	return filepath.Join(l.EdgeDir(), "caddy", "pki", "authorities", "local")
}

func internalLeavesDir(l spec.Layout) string {
	return filepath.Join(l.EdgeDir(), "caddy", "certificates", "local")
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
	busy, err := lockBackups(t.Layout)
	if err != nil {
		return "", err
	}
	defer busy.Release()
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
		Scope:     opts.Scope,
		CreatedAt: now,
	}
	databaseOnly := opts.Scope == ScopeDatabase
	if t.externalDatabase() {
		m.Skipped = append(m.Skipped, SkippedDatabase)
	}
	if t.externalUploads() {
		m.Skipped = append(m.Skipped, SkippedUploads)
	}

	var ps []spec.PsEntry
	switch {
	case sup != nil:
		if ps, err = sup.PS(ctx); err != nil {
			return "", err
		}
	case !opts.Locked:
		held, err := lockData(t.Layout)
		if err != nil {
			return "", err
		}
		defer held.Release()
	}
	if sup != nil && !t.externalDatabase() && !running(ps, spec.Postgres) {
		return "", errors.New("backup: postgres is not running")
	}
	var volumes []string
	if !databaseOnly {
		volumes = t.volumesToCopy()
	}
	need := t.Need(volumes)
	if opts.Out != nil {
		need = t.Need(nil) / 2
	}
	if free, err := freeBytes(dir); err == nil && need > free {
		return "", &SpaceError{Dir: dir, Need: need, Free: free}
	}

	var w *archiveWriter
	if opts.Out != nil {
		w, err = streamArchive(opts.Out)
	} else {
		w, err = createArchive(final + PartialSuffix)
	}
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
	envFile := opts.EnvFile
	if databaseOnly {
		envFile = ""
	}
	for _, f := range []struct{ src, name string }{{envFile, EnvFileName}, {t.Layout.SecretsFile(), SecretsFileName}, {t.Layout.ModesFile(), ModesFileName}} {
		if f.src == "" {
			continue
		}
		if err := w.addFile(f.src, f.name); err != nil && !errors.Is(err, os.ErrNotExist) {
			return "", err
		}
	}
	if opts.Include != "" && !databaseOnly {
		reserved := []string{strings.TrimPrefix(EnvFileName, FilesPrefix), strings.TrimPrefix(SecretsFileName, FilesPrefix), strings.TrimPrefix(ModesFileName, FilesPrefix)}
		if err := w.addTree(opts.Include, strings.TrimSuffix(FilesPrefix, "/"), reserved); err != nil {
			return "", err
		}
	}

	if !t.externalDatabase() {
		progress(StageDatabase)
		sock := t.Layout.PGSocketDir()
		if sup == nil {
			pg, err := t.StartPostgres(ctx, t.Layout.Tmp("backup"))
			if err != nil {
				return "", err
			}
			defer pg.Stop()
			sock = pg.Sock
		}
		if err := t.addDump(ctx, w, sock, final+".dump"+PartialSuffix); err != nil {
			return "", err
		}
		m.Database = DumpName
	}

	if sup != nil && len(volumes) > 0 {
		progress(StageStopping)
		if err := sup.Maintenance(ctx, true); err != nil {
			return "", fmt.Errorf("backup: showing the maintenance page: %w", err)
		}
		defer func() {
			if serr := sup.Maintenance(context.WithoutCancel(ctx), false); serr != nil && err == nil {
				err = fmt.Errorf("backup: the maintenance page did not go away: %w", serr)
			}
		}()
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
	if opts.Out != nil {
		ok = true
		return "", nil
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

func lockBackups(l spec.Layout) (*lock.Lock, error) {
	if err := os.MkdirAll(l.Data, 0o700); err != nil {
		return nil, err
	}
	held, err := lock.Acquire(filepath.Join(l.Data, lockName))
	if errors.Is(err, lock.ErrHeld) {
		return nil, errors.New("backup: another backup or restore of this instance is running. Try again when it has finished")
	}
	return held, err
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
