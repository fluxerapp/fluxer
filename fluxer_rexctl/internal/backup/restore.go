// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"archive/tar"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const postgresReadyTimeout = 3 * time.Minute

var errStop = errors.New("backup: stop")

type RestoreOptions struct {
	Progress func(stage string)
	OnEvent  func(engine.Event)
}

type ProjectInUseError struct {
	Project string
	Dir     string
}

func (e *ProjectInUseError) Error() string {
	return fmt.Sprintf("backup: project %s is in use by %s", e.Project, e.Dir)
}

type RestoreError struct {
	Part   string
	Output string
	Err    error
}

func (e *RestoreError) Error() string {
	return fmt.Sprintf("backup: restoring %s: %v", e.Part, e.Err)
}

func (e *RestoreError) Unwrap() error { return e.Err }

type PullError struct {
	Err error
}

func (e *PullError) Error() string {
	return fmt.Sprintf("backup: downloading images: %v", e.Err)
}

func (e *PullError) Unwrap() error { return e.Err }

func Restore(ctx context.Context, path, dir string, e *engine.Engine, opts RestoreOptions) (*instance.Instance, error) {
	progress := opts.Progress
	if progress == nil {
		progress = func(string) {}
	}
	progress(StageVerifying)
	m, err := Verify(path)
	if err != nil {
		return nil, err
	}
	dir, err = filepath.Abs(dir)
	if err != nil {
		return nil, err
	}
	existing, err := instance.Find(dir)
	if err != nil && !errors.Is(err, instance.ErrNotFound) {
		return nil, err
	}
	if err := checkProjectFree(ctx, e, m.Project, dir); err != nil {
		return nil, err
	}

	progress(StagePulling)
	if err := pullFromBackup(ctx, path, e, opts.OnEvent); err != nil {
		return nil, err
	}

	progress(StageClearing)
	var stale []string
	if existing != nil {
		if existing.State != nil {
			stale = slices.Collect(maps.Values(existing.State.Images.Refs))
		}
		stale = append(stale, release.KeptImages(existing)...)
		if err := e.Named(existing.Project(), existing.Dir).Down(ctx, true); err != nil {
			return nil, err
		}
		if err := clearInstanceFiles(dir); err != nil {
			return nil, err
		}
	}

	progress(StageFiles)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, err
	}
	if err := extractFiles(path, dir); err != nil {
		return nil, err
	}
	inst, err := instance.Find(dir)
	if err != nil {
		return nil, err
	}
	if err := adoptRestored(inst, e); err != nil {
		return nil, err
	}
	for _, d := range []string{inst.StateDir(), inst.BackupDir()} {
		if err := os.MkdirAll(d, 0o700); err != nil {
			return nil, err
		}
		if err := envfile.RestrictToOwner(d); err != nil {
			return nil, err
		}
	}

	p, err := deploy.Load(ctx, inst, e)
	if err != nil {
		return nil, &RestoreError{Part: "containers", Err: err}
	}
	if err := p.Down(ctx, true); err != nil {
		return nil, err
	}
	if err := p.Create(ctx); err != nil {
		return nil, &RestoreError{Part: "containers", Err: err}
	}

	vols, err := e.Volumes(ctx, p.Name)
	if err != nil {
		return nil, err
	}
	image, err := helperImage(ctx, inst, p, p.Model)
	if err != nil {
		return nil, err
	}
	if m.Database != "" {
		progress(StageDatabase)
		if v, ok := dataVolume(p.Model, p.Name, vols, "postgres-data"); ok && v.bind != "" {
			if err := helper(ctx, p, image, []engine.Mount{v.mount("/data", false)}, nil, io.Discard, "find", "/data", "-mindepth", "1", "-delete"); err != nil {
				return nil, &RestoreError{Part: "database", Err: err}
			}
		}
		if err := startPostgres(ctx, p); err != nil {
			return nil, err
		}
	}
	if err := restoreData(ctx, path, p, image, m, vols, "", progress); err != nil {
		return nil, err
	}

	progress(StageStarting)
	if _, err := deploy.Apply(ctx, inst, e, deploy.ApplyOptions{}); err != nil {
		return nil, err
	}
	removeStaleImages(ctx, p, stale)
	return inst, nil
}

func adoptRestored(inst *instance.Instance, e *engine.Engine) error {
	st := inst.State
	if st == nil {
		return fmt.Errorf("%w: no state in the backup", ErrDamaged)
	}
	st.Engine = instance.EngineRef{Kind: string(e.Kind), Host: e.Host, ID: e.ID}
	st.Stopped = false
	st.Installing = false
	st.Ejected = nil
	st.Upgrade = nil
	st.Pending = nil
	if prev := st.PreviousRelease; prev != "" {
		if _, err := os.Stat(inst.ReleaseDir(prev)); err != nil {
			st.PreviousRelease = ""
		}
	}
	rendered, err := renderRestored(inst)
	if err != nil {
		return err
	}
	st.Rendered = rendered
	return inst.Save()
}

func renderRestored(inst *instance.Instance) (map[string]string, error) {
	st := inst.State
	if st.Release != stack.LegacyRelease {
		if set, err := release.LoadSet(inst, st.Release); err == nil {
			r, err := inst.Render(set, stack.OverlaysFor(st.Settings), st.Images.Refs)
			if err != nil {
				return nil, err
			}
			return r.Hashes, nil
		}
	}
	out := map[string]string{}
	for name := range st.Rendered {
		data, err := os.ReadFile(inst.Generated(name))
		if err != nil {
			continue
		}
		out[name] = stack.Hash(data)
	}
	return out, nil
}

func removeStaleImages(ctx context.Context, p *engine.Project, stale []string) {
	current := p.Images()
	var drop []string
	for _, r := range stale {
		if !slices.Contains(current, r) && !slices.Contains(drop, r) {
			drop = append(drop, r)
		}
	}
	if len(drop) > 0 {
		_ = p.Engine.RemoveImages(ctx, drop)
	}
}

func pullFromBackup(ctx context.Context, path string, e *engine.Engine, onEvent func(engine.Event)) error {
	staging, err := os.MkdirTemp("", "rexctl-restore-")
	if err != nil {
		return err
	}
	defer os.RemoveAll(staging)
	if err := extractFiles(path, staging); err != nil {
		return err
	}
	inst, err := instance.Find(staging)
	if err != nil {
		return err
	}
	p, err := deploy.Load(ctx, inst, e)
	if err != nil {
		return &PullError{Err: err}
	}
	if err := p.Pull(ctx, onEvent); err != nil {
		return &PullError{Err: err}
	}
	return nil
}

func checkProjectFree(ctx context.Context, e *engine.Engine, project, dir string) error {
	projects, err := e.Projects(ctx)
	if err != nil {
		return err
	}
	for _, pr := range projects {
		if pr.Name != project || pr.ConfigFiles == "" {
			continue
		}
		first, _, _ := strings.Cut(pr.ConfigFiles, ",")
		other := stack.ProjectDir(strings.TrimSpace(first))
		if !samePath(other, dir) {
			return &ProjectInUseError{Project: project, Dir: other}
		}
	}
	return nil
}

func samePath(a, b string) bool {
	ra, err1 := filepath.EvalSymlinks(a)
	rb, err2 := filepath.EvalSymlinks(b)
	if err1 == nil && err2 == nil {
		return ra == rb
	}
	return filepath.Clean(a) == filepath.Clean(b)
}

func clearInstanceFiles(dir string) error {
	names, err := instanceFiles(dir)
	if err != nil {
		return err
	}
	for _, name := range names {
		if err := os.Remove(filepath.Join(dir, name)); err != nil && !errors.Is(err, os.ErrNotExist) {
			return err
		}
	}
	for _, p := range []string{
		filepath.Join(dir, instance.CertsDir),
		filepath.Join(dir, instance.CaddyDropInDir),
		stack.GeneratedPath(dir),
		filepath.Join(dir, instance.StateDirName, instance.ReleasesDir),
		filepath.Join(dir, instance.StateDirName, instance.StateFileName),
	} {
		if err := os.RemoveAll(p); err != nil {
			return err
		}
	}
	return nil
}

func extractFiles(path, dir string) error {
	err := walkArchive(path, func(hdr *tar.Header, r io.Reader) error {
		if !strings.HasPrefix(hdr.Name, FilesPrefix) {
			return errStop
		}
		rel := strings.TrimSuffix(strings.TrimPrefix(hdr.Name, FilesPrefix), "/")
		if rel == "" {
			return nil
		}
		if !filepath.IsLocal(filepath.FromSlash(rel)) {
			return fmt.Errorf("%w: unsafe path %q", ErrDamaged, hdr.Name)
		}
		target := filepath.Join(dir, filepath.FromSlash(rel))
		switch hdr.Typeflag {
		case tar.TypeDir:
			return os.MkdirAll(target, dirMode(hdr.Mode))
		case tar.TypeReg:
			if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
				return err
			}
			return writeFile(target, r, os.FileMode(hdr.Mode).Perm(), rel == instance.EnvFileName)
		}
		return nil
	})
	if errors.Is(err, errStop) {
		return nil
	}
	return err
}

func dirMode(mode int64) os.FileMode {
	m := os.FileMode(mode).Perm()
	if m == 0 {
		return 0o755
	}
	return m | 0o700
}

func writeFile(target string, r io.Reader, mode os.FileMode, private bool) error {
	if private {
		data, err := io.ReadAll(r)
		if err != nil {
			return err
		}
		return envfile.WriteFileAtomic(target, data, true)
	}
	if mode == 0 {
		mode = 0o644
	}
	tmp, err := os.CreateTemp(filepath.Dir(target), "."+filepath.Base(target)+".*")
	if err != nil {
		return err
	}
	name := tmp.Name()
	defer os.Remove(name)
	if _, err := io.Copy(tmp, r); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	if err := os.Chmod(name, mode); err != nil {
		return err
	}
	return os.Rename(name, target)
}

func startPostgres(ctx context.Context, p *engine.Project) error {
	if err := p.Start(ctx, postgresService); err != nil {
		return err
	}
	ctx, cancel := context.WithTimeout(ctx, postgresReadyTimeout)
	defer cancel()
	var stderr bytes.Buffer
	err := p.Exec(ctx, postgresService, engine.ExecOptions{Stderr: &stderr, KillOnCancel: true}, "sh", "-c",
		`until pg_isready -q -h 127.0.0.1 -U "$POSTGRES_USER" -d "$POSTGRES_DB"; do sleep 1; done`)
	if err != nil {
		return &RestoreError{Part: "database", Output: strings.TrimSpace(stderr.String()), Err: err}
	}
	return nil
}

type volumeSink struct {
	name string
	pw   *io.PipeWriter
	tw   *tar.Writer
	done chan error
	out  *bytes.Buffer
}

func RestoreVolume(ctx context.Context, path string, inst *instance.Instance, p *engine.Project, short string) error {
	m, err := ReadManifest(path)
	if err != nil {
		return err
	}
	if !slices.Contains(m.Volumes, short) {
		return fmt.Errorf("backup: %s isn't in the backup", short)
	}
	vols, err := p.Engine.Volumes(ctx, p.Name)
	if err != nil {
		return err
	}
	image, err := helperImage(ctx, inst, p, p.Model)
	if err != nil {
		return err
	}
	return restoreData(ctx, path, p, image, m, vols, short, func(string) {})
}

func ClearVolume(ctx context.Context, inst *instance.Instance, p *engine.Project, short string) error {
	vols, err := p.Engine.Volumes(ctx, p.Name)
	if err != nil {
		return err
	}
	v, ok := dataVolume(p.Model, p.Name, vols, short)
	if !ok {
		return nil
	}
	image, err := helperImage(ctx, inst, p, p.Model)
	if err != nil {
		return err
	}
	return helper(ctx, p, image, []engine.Mount{v.mount("/data", false)}, nil, io.Discard, "find", "/data", "-mindepth", "1", "-delete")
}

func restoreData(ctx context.Context, path string, p *engine.Project, image string, m *Manifest, vols []engine.Volume, only string, progress func(string)) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	var sink *volumeSink
	closeSink := func(failed error) error {
		if sink == nil {
			return failed
		}
		s := sink
		sink = nil
		if failed != nil {
			s.pw.CloseWithError(failed)
			<-s.done
			return failed
		}
		werr := s.tw.Close()
		s.pw.CloseWithError(werr)
		rerr := <-s.done
		if werr != nil {
			return werr
		}
		if rerr != nil {
			return &RestoreError{Part: s.name, Output: strings.TrimSpace(s.out.String()), Err: rerr}
		}
		return nil
	}
	openSink := func(short string) error {
		v, ok := dataVolume(p.Model, p.Name, vols, short)
		if !ok {
			return &RestoreError{Part: short, Err: errors.New("the volume wasn't created")}
		}
		pr, pw := io.Pipe()
		s := &volumeSink{name: short, pw: pw, tw: tar.NewWriter(pw), done: make(chan error, 1), out: &bytes.Buffer{}}
		go func() {
			err := p.Helper(ctx, engine.HelperOptions{
				Image:      image,
				Entrypoint: []string{"sh", "-c"},
				Command:    []string{"find /data -mindepth 1 -delete && exec tar -xf - -C /data"},
				Mounts:     []engine.Mount{v.mount("/data", false)},
				Stdin:      pr,
				Stderr:     s.out,
			})
			if err == nil {
				io.Copy(io.Discard, pr)
			}
			pr.CloseWithError(err)
			s.done <- err
		}()
		sink = s
		return nil
	}
	volumesStarted := false
	err := walkArchive(path, func(hdr *tar.Header, r io.Reader) error {
		if hdr.Name == DumpName {
			if err := closeSink(nil); err != nil {
				return err
			}
			if only != "" {
				return nil
			}
			return restoreDump(ctx, p, r)
		}
		short, inner, ok := volumeOf(hdr.Name)
		if !ok || only != "" && short != only {
			return closeSink(nil)
		}
		if !slices.Contains(m.Volumes, short) {
			return fmt.Errorf("%w: %s isn't in the manifest", ErrDamaged, short)
		}
		if !volumesStarted {
			volumesStarted = true
			progress(StageVolumes)
		}
		if sink == nil || sink.name != short {
			if err := closeSink(nil); err != nil {
				return err
			}
			if err := openSink(short); err != nil {
				return err
			}
		}
		out := *hdr
		out.Name = "./" + inner
		if inner == "." {
			out.Name = "./"
		}
		if hdr.Typeflag == tar.TypeDir && !strings.HasSuffix(out.Name, "/") {
			out.Name += "/"
		}
		if hdr.Typeflag == tar.TypeLink {
			_, target, ok := volumeOf(hdr.Linkname)
			if !ok {
				return fmt.Errorf("%w: unsafe link %q", ErrDamaged, hdr.Linkname)
			}
			out.Linkname = "./" + target
		}
		out.PAXRecords = nil
		out.Format = tar.FormatPAX
		if err := sink.tw.WriteHeader(&out); err != nil {
			return err
		}
		if hdr.Typeflag == tar.TypeReg {
			if _, err := io.Copy(sink.tw, r); err != nil {
				return err
			}
		}
		return nil
	})
	if err != nil {
		return closeSink(err)
	}
	return closeSink(nil)
}

func restoreDump(ctx context.Context, p *engine.Project, r io.Reader) error {
	var stderr bytes.Buffer
	err := p.Exec(ctx, postgresService, engine.ExecOptions{Stdin: r, Stderr: &stderr, KillOnCancel: true}, "sh", "-c",
		`exec pg_restore -U "$POSTGRES_USER" -d "$POSTGRES_DB" --exit-on-error --single-transaction`)
	if err != nil {
		return &RestoreError{Part: "database", Output: strings.TrimSpace(stderr.String()), Err: err}
	}
	return nil
}
