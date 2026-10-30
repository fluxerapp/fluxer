// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"archive/tar"
	"bytes"
	"context"
	"encoding/json"
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

var ErrUploadsNotHere = errors.New("backup: the uploads this backup belongs with aren't on this machine")

type UploadsNotHereError struct {
	Why string
}

func (e *UploadsNotHereError) Error() string { return ErrUploadsNotHere.Error() + ": " + e.Why }

func (e *UploadsNotHereError) Unwrap() error { return ErrUploadsNotHere }

type RestoreOptions struct {
	Progress func(stage string)
	OnEvent  func(engine.Event)
	Adopted  func(*instance.Instance)
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
	if err := CheckUploadsInPlace(ctx, dir, existing, e, m); err != nil {
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
	stale, err := clearForRestore(ctx, dir, existing, e, m)
	if err != nil {
		return nil, err
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
	if err := adoptRestored(inst, e, opts.Adopted); err != nil {
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
	if err := down(ctx, p, m); err != nil {
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
	if err := os.Remove(restoreMarkerPath(dir)); err != nil && !errors.Is(err, os.ErrNotExist) {
		return nil, err
	}
	removeStaleImages(ctx, p, stale)
	return inst, nil
}

func clearForRestore(ctx context.Context, dir string, existing *instance.Instance, e *engine.Engine, m *Manifest) ([]string, error) {
	if existing == nil {
		return nil, nil
	}
	var stale []string
	if existing.State != nil {
		stale = slices.Collect(maps.Values(existing.State.Images.Refs))
	}
	stale = append(stale, release.KeptImages(existing)...)
	if m.WithoutUploads() {
		if err := writeRestoreMarker(dir, m, e); err != nil {
			return nil, err
		}
	}
	if err := down(ctx, e.Named(existing.Project(), existing.Dir), m); err != nil {
		return nil, err
	}
	if err := clearInstanceFiles(dir); err != nil {
		return nil, err
	}
	return stale, nil
}

var downProject = func(ctx context.Context, p *engine.Project, keep []string) error {
	if len(keep) > 0 {
		return p.DownKeeping(ctx, keep...)
	}
	return p.Down(ctx, true)
}

func down(ctx context.Context, p *engine.Project, m *Manifest) error {
	return downProject(ctx, p, keptVolumes(m))
}

const restoreMarkerName = "restoring-without-uploads.json"

type restoreMarker struct {
	Project string             `json:"project"`
	Domain  string             `json:"domain"`
	Engine  instance.EngineRef `json:"engine"`
	Uploads UploadsRef         `json:"uploads"`
}

func restoreMarkerPath(dir string) string {
	return filepath.Join(dir, instance.StateDirName, restoreMarkerName)
}

func writeRestoreMarker(dir string, m *Manifest, e *engine.Engine) error {
	data, err := json.Marshal(restoreMarker{Project: m.Project, Domain: m.Domain, Engine: instance.EngineRef{Kind: string(e.Kind), Host: e.Host, ID: e.ID}, Uploads: *m.Uploads})
	if err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(restoreMarkerPath(dir)), 0o700); err != nil {
		return err
	}
	return envfile.WriteFileAtomic(restoreMarkerPath(dir), data, true)
}

func ResumesRestore(dir string, m *Manifest) bool {
	mk := readRestoreMarker(dir)
	return mk != nil && m.Uploads != nil && mk.Project == m.Project && mk.Domain == m.Domain && mk.Uploads == *m.Uploads
}

func readRestoreMarker(dir string) *restoreMarker {
	if dir == "" {
		return nil
	}
	data, err := os.ReadFile(restoreMarkerPath(dir))
	if err != nil {
		return nil
	}
	var mk restoreMarker
	if json.Unmarshal(data, &mk) != nil {
		return nil
	}
	return &mk
}

func keptVolumes(m *Manifest) []string {
	if m.WithoutUploads() {
		return []string{UploadsVolume}
	}
	return nil
}

func CheckUploadsInPlace(ctx context.Context, dir string, existing *instance.Instance, e *engine.Engine, m *Manifest) error {
	if existing != nil {
		if err := os.Remove(restoreMarkerPath(dir)); err != nil && !errors.Is(err, os.ErrNotExist) {
			return err
		}
	}
	if !m.WithoutUploads() {
		return nil
	}
	var vols []engine.Volume
	if m.Uploads != nil && m.Uploads.Bind == "" && e != nil && (existing != nil || ResumesRestore(dir, m)) {
		var err error
		if vols, err = e.Volumes(ctx, m.Project); err != nil {
			return err
		}
	}
	return uploadsInPlace(dir, existing, e, vols, m)
}

func uploadsInPlace(dir string, existing *instance.Instance, e *engine.Engine, vols []engine.Volume, m *Manifest) error {
	refuse := func(why string) error { return &UploadsNotHereError{Why: why} }
	if existing == nil && ResumesRestore(dir, m) {
		mk := readRestoreMarker(dir)
		switch {
		case e == nil:
			return refuse("no engine to check the uploads on")
		case movesEngine(mk.Engine, e):
			return refuse("the restore moves the instance to another engine")
		}
		return uploadsIdentity(m.Uploads, vols)
	}
	switch {
	case existing == nil || existing.State == nil:
		return refuse("there is no instance here")
	case m.Project != existing.Project() || m.Domain != existing.Domain():
		return refuse(fmt.Sprintf("the backup is for %s and the instance here is %s", m.Domain, existing.Domain()))
	case e == nil:
		return refuse("no engine to check the uploads on")
	case movesEngine(existing.State.Engine, e):
		return refuse("the restore moves the instance to another engine")
	case m.Uploads == nil:
		return refuse("the backup doesn't record which uploads it belongs with")
	case m.Uploads.Definition == "" || m.Uploads.Definition != uploadsDefinition(existing):
		return refuse("the uploads here were written by another version of the uploads service")
	}
	return uploadsIdentity(m.Uploads, vols)
}

func uploadsIdentity(u *UploadsRef, vols []engine.Volume) error {
	refuse := func(why string) error { return &UploadsNotHereError{Why: why} }
	if u.Bind != "" {
		ino, ok := fileInode(u.Bind)
		if !ok {
			return refuse(u.Bind + " is missing")
		}
		id, err := bindID(u.Bind, false)
		if err != nil {
			return refuse("couldn't read " + filepath.Join(u.Bind, UploadsIDFile))
		}
		if u.ID == "" || id != u.ID || ino != u.Inode {
			return refuse(u.Bind + " was made again since the backup")
		}
		return nil
	}
	for _, v := range vols {
		if v.Name != u.Volume {
			continue
		}
		if v.CreatedAt != u.CreatedAt {
			return refuse("volume " + u.Volume + " was made again since the backup")
		}
		return nil
	}
	return refuse("volume " + u.Volume + " is missing")
}

func movesEngine(was instance.EngineRef, e *engine.Engine) bool {
	if was.Kind != "" && was.Kind != string(e.Kind) {
		return true
	}
	return was.ID != "" && e.ID != "" && was.ID != e.ID
}

func adoptRestored(inst *instance.Instance, e *engine.Engine, adopted func(*instance.Instance)) error {
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
	if st.PreviousBackup != "" {
		if _, err := os.Stat(st.PreviousBackup); err != nil || st.PreviousRelease == "" {
			st.PreviousBackup = ""
		}
	}
	if adopted != nil {
		adopted(inst)
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
		filepath.Join(dir, instance.CADir),
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
	uploadsBind := ""
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
		if short == UploadsVolume && v.bind != "" {
			uploadsBind = v.bind
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
		err = closeSink(err)
	} else {
		err = closeSink(nil)
	}
	if uploadsBind != "" {
		if ferr := forgetUploadsID(uploadsBind); err == nil {
			err = ferr
		}
	}
	return err
}

func forgetUploadsID(bind string) error {
	if err := os.Remove(filepath.Join(bind, UploadsIDFile)); err != nil && !errors.Is(err, os.ErrNotExist) {
		return &RestoreError{Part: UploadsVolume, Err: err}
	}
	return nil
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
