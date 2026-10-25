// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"archive/tar"
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"maps"
	"net/url"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/compose-spec/compose-go/v2/types"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	postgresService = "postgres"
	maxLinkDepth    = 8
	diskHeadroomPct = 110
)

var ErrNeverStarted = errors.New("backup: Fluxer has never run and has no data yet")

type CreateOptions struct {
	Reason   string
	Progress func(stage string)
	OnEvent  func(engine.Event)
}

type MissingDataError struct {
	Volume string
}

func (e *MissingDataError) Error() string {
	return fmt.Sprintf("backup: no database data found in volume %s or a bind mount", e.Volume)
}

type DiskError struct {
	Dir       string
	Need      uint64
	Available uint64
}

func (e *DiskError) Error() string {
	return fmt.Sprintf("backup: %s needs %d bytes and has %d", e.Dir, e.Need, e.Available)
}

type DumpError struct {
	Output string
	Err    error
}

func (e *DumpError) Error() string {
	if e.Err != nil {
		return "backup: pg_dump failed: " + e.Err.Error()
	}
	return "backup: pg_dump output has no PGDMP header"
}

func (e *DumpError) Unwrap() error { return e.Err }

func Create(ctx context.Context, inst *instance.Instance, p *engine.Project, opts CreateOptions) (*Info, error) {
	progress := opts.Progress
	if progress == nil {
		progress = func(string) {}
	}
	dir := inst.BackupDir()
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return nil, err
	}
	os.Chmod(dir, 0o700)
	removePartials(dir)

	now := time.Now().UTC().Truncate(time.Second)
	final, err := newPath(dir, inst.Domain(), now)
	if err != nil {
		return nil, err
	}
	partial := final + PartialSuffix

	m := &Manifest{
		Project:   inst.Project(),
		Domain:    inst.Domain(),
		CreatedAt: now,
		Reason:    opts.Reason,
		Volumes:   []string{},
	}
	if inst.State != nil {
		m.Release = inst.State.Release
	}
	values := instanceValues(inst)
	model := p.Model
	if model == nil {
		if loaded, err := deploy.Load(ctx, inst, p.Engine); err == nil {
			model = loaded.Model
		}
	}
	bundledDB := !externalPostgres(values)
	if !bundledDB {
		m.Skipped = append(m.Skipped, SkippedDatabase)
	}
	volumes, err := volumesToCopy(ctx, inst, values, p, model, m)
	if err != nil {
		return nil, err
	}
	if bundledDB && len(volumes) == 0 {
		have, err := p.Engine.Volumes(ctx, p.Name)
		if err != nil {
			return nil, err
		}
		if _, ok := dataVolume(model, p.Name, have, "postgres-data"); !ok {
			if ps, err := p.PS(ctx); err == nil && len(ps) > 0 {
				return nil, &MissingDataError{Volume: p.Name + "_postgres-data"}
			}
			return nil, ErrNeverStarted
		}
	}
	running, err := runningServices(ctx, p)
	if err != nil {
		return nil, err
	}
	image, err := helperImage(ctx, inst, p)
	if err != nil {
		return nil, err
	}
	if err := checkDisk(ctx, p, image, dir, volumes); err != nil {
		return nil, err
	}

	w, err := createArchive(partial)
	if err != nil {
		return nil, err
	}
	ok := false
	defer func() {
		if !ok {
			w.abort()
		}
	}()

	progress(StageFiles)
	if err := addInstanceFiles(w, inst); err != nil {
		return nil, err
	}

	if bundledDB {
		progress(StageDatabase)
		if !slices.Contains(running, postgresService) {
			if len(volumes) == 0 {
				defer p.Stop(context.WithoutCancel(ctx), postgresService)
			}
			if err := startPostgres(ctx, p); err != nil {
				return nil, err
			}
		}
		if err := addDump(ctx, w, p, final+".dump"+PartialSuffix); err != nil {
			return nil, err
		}
		m.Database = DumpName
	}

	if len(volumes) > 0 {
		if err := copyVolumes(ctx, w, p, image, volumes, running, progress, opts.OnEvent); err != nil {
			return nil, err
		}
		for _, v := range volumes {
			m.Volumes = append(m.Volumes, v.short)
		}
	}

	if err := w.finish(m); err != nil {
		return nil, err
	}
	if err := os.Rename(partial, final); err != nil {
		return nil, err
	}
	ok = true
	syncDir(dir)
	fi, err := os.Stat(final)
	if err != nil {
		return nil, err
	}
	return &Info{File: filepath.Base(final), Path: final, CreatedAt: now, Release: m.Release, SizeBytes: fi.Size()}, nil
}

func syncDir(dir string) {
	d, err := os.Open(dir)
	if err != nil {
		return
	}
	d.Sync()
	d.Close()
}

func instanceValues(inst *instance.Instance) map[string]string {
	if inst.State == nil {
		return map[string]string{}
	}
	values, err := inst.Environment()
	if err != nil {
		return map[string]string{}
	}
	return values
}

func externalPostgres(values map[string]string) bool {
	if h := values["FLUXER_POSTGRES_HOST"]; h != "" && h != postgresService {
		return true
	}
	if raw := values["FLUXER_POSTGRES_URL"]; raw != "" {
		u, err := url.Parse(raw)
		return err != nil || u.Hostname() != postgresService
	}
	return false
}

func externalUploads(inst *instance.Instance, values map[string]string) bool {
	if inst.State != nil && inst.State.Settings[settings.StorageEndpoint] != "" {
		return true
	}
	if raw := values["FLUXER_S3_ENDPOINT"]; raw != "" {
		u, err := url.Parse(raw)
		return err != nil || u.Hostname() != "seaweedfs"
	}
	return false
}

type volumeRef struct {
	short string
	full  string
	bind  string
}

func (v volumeRef) mount(target string, readOnly bool) engine.Mount {
	return engine.Mount{Volume: v.full, Bind: v.bind, Target: target, ReadOnly: readOnly}
}

func dataVolume(model *types.Project, project string, have []engine.Volume, short string) (volumeRef, bool) {
	if full, ok := findVolume(have, project, short); ok {
		return volumeRef{short: short, full: full}, true
	}
	dm, ok := stack.DataMounts[short]
	if !ok || model == nil {
		return volumeRef{}, false
	}
	svc, ok := model.Services[dm.Service]
	if !ok {
		return volumeRef{}, false
	}
	for _, v := range svc.Volumes {
		if v.Type == types.VolumeTypeBind && v.Target == dm.Target && v.Source != "" {
			return volumeRef{short: short, bind: v.Source}, true
		}
	}
	return volumeRef{}, false
}

func volumesToCopy(ctx context.Context, inst *instance.Instance, values map[string]string, p *engine.Project, model *types.Project, m *Manifest) ([]volumeRef, error) {
	skipUploads := externalUploads(inst, values)
	if skipUploads {
		m.Skipped = append(m.Skipped, SkippedUploads)
	}
	have, err := p.Engine.Volumes(ctx, p.Name)
	if err != nil {
		return nil, err
	}
	var out []volumeRef
	for _, short := range stack.BackupVolumes {
		if skipUploads && short == "seaweedfs-data" {
			continue
		}
		if v, ok := dataVolume(model, p.Name, have, short); ok {
			out = append(out, v)
		}
	}
	return out, nil
}

func findVolume(vols []engine.Volume, project, short string) (string, bool) {
	for _, v := range vols {
		if v.Labels["com.docker.compose.volume"] == short && (v.Project == "" || v.Project == project) {
			return v.Name, true
		}
	}
	for _, v := range vols {
		if v.Name == project+"_"+short {
			return v.Name, true
		}
	}
	return "", false
}

func helperImage(ctx context.Context, inst *instance.Instance, p *engine.Project) (string, error) {
	if p.Model != nil {
		if img := p.Image(postgresService); img != "" {
			return img, nil
		}
	}
	if cs, err := p.PS(ctx); err == nil {
		for _, c := range cs {
			if c.Service == postgresService && c.Image != "" {
				return c.Image, nil
			}
		}
	}
	if inst.State != nil {
		if img := inst.State.Images.Refs[postgresService]; img != "" {
			return img, nil
		}
	}
	return "", errors.New("backup: no postgres image to run the copy with")
}

func helper(ctx context.Context, p *engine.Project, image string, mounts []engine.Mount, stdin io.Reader, stdout io.Writer, entrypoint string, args ...string) error {
	var stderr bytes.Buffer
	err := p.Helper(ctx, engine.HelperOptions{
		Image:      image,
		Entrypoint: []string{entrypoint},
		Command:    args,
		Mounts:     mounts,
		Stdin:      stdin,
		Stdout:     stdout,
		Stderr:     &stderr,
	})
	if err != nil {
		return &HelperError{Output: strings.TrimSpace(stderr.String()), Err: err}
	}
	return nil
}

type HelperError struct {
	Output string
	Err    error
}

func (e *HelperError) Error() string {
	if e.Output == "" {
		return e.Err.Error()
	}
	return e.Err.Error() + ": " + e.Output
}

func (e *HelperError) Unwrap() error { return e.Err }

func checkDisk(ctx context.Context, p *engine.Project, image, dir string, volumes []volumeRef) error {
	free, err := host.FreeDisk(dir)
	if err != nil || len(volumes) == 0 {
		return nil
	}
	var mounts []engine.Mount
	var paths []string
	for i, v := range volumes {
		mp := "/v/" + strconv.Itoa(i)
		mounts = append(mounts, v.mount(mp, true))
		paths = append(paths, mp)
	}
	var out bytes.Buffer
	if err := helper(ctx, p, image, mounts, nil, &out, "du", append([]string{"-sk"}, paths...)...); err != nil {
		return fmt.Errorf("backup: measuring volumes: %w", err)
	}
	var kb uint64
	sc := bufio.NewScanner(&out)
	for sc.Scan() {
		fields := strings.Fields(sc.Text())
		if len(fields) == 0 {
			continue
		}
		n, err := strconv.ParseUint(fields[0], 10, 64)
		if err == nil {
			kb += n
		}
	}
	need := kb * 1024 * diskHeadroomPct / 100
	if free < need {
		return &DiskError{Dir: dir, Need: need, Available: free}
	}
	return nil
}

func instanceFiles(dir string) ([]string, error) {
	ents, err := os.ReadDir(dir)
	if err != nil {
		return nil, err
	}
	var out []string
	for _, e := range ents {
		name := e.Name()
		if e.IsDir() || name == instance.EnvFileName || strings.HasPrefix(name, "recovery-kit-") || strings.HasSuffix(name, PartialSuffix) {
			continue
		}
		fi, err := os.Stat(filepath.Join(dir, name))
		if err != nil || !fi.Mode().IsRegular() {
			continue
		}
		out = append(out, name)
	}
	return out, nil
}

func addInstanceFiles(w *archiveWriter, inst *instance.Instance) error {
	dir := inst.Dir
	names, err := instanceFiles(dir)
	if err != nil {
		return err
	}
	for _, name := range names {
		if err := addFile(w, filepath.Join(dir, name), FilesPrefix+name, 0o644); err != nil {
			return err
		}
	}
	state := filepath.Join(dir, instance.StateDirName, instance.StateFileName)
	if _, err := os.Stat(state); err == nil {
		if err := addFile(w, state, FilesPrefix+instance.StateDirName+"/"+instance.StateFileName, 0o600); err != nil {
			return err
		}
	}
	for _, sub := range []string{instance.CertsDir, instance.CaddyDropInDir} {
		if err := addTree(w, filepath.Join(dir, sub), FilesPrefix+sub, 0); err != nil {
			return err
		}
	}
	if inst.State != nil && inst.State.Release != "" {
		rel := release.Dir(inst, inst.State.Release)
		name, err := filepath.Rel(dir, rel)
		if err != nil || !filepath.IsLocal(name) {
			return nil
		}
		return addTree(w, rel, FilesPrefix+filepath.ToSlash(name), 0)
	}
	return nil
}

func addFile(w *archiveWriter, src, name string, mode int64) error {
	f, err := os.Open(src)
	if err != nil {
		return err
	}
	defer f.Close()
	fi, err := f.Stat()
	if err != nil {
		return err
	}
	if mode == 0 {
		mode = int64(fi.Mode().Perm())
	}
	return w.add(&tar.Header{Name: name, Mode: mode, Size: fi.Size(), ModTime: fi.ModTime(), Typeflag: tar.TypeReg}, f)
}

func addTree(w *archiveWriter, src, name string, depth int) error {
	fi, err := os.Stat(src)
	if errors.Is(err, os.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	if fi.Mode().IsRegular() {
		return addFile(w, src, name, 0)
	}
	if !fi.IsDir() || depth > maxLinkDepth {
		return nil
	}
	if err := w.add(&tar.Header{Name: name + "/", Mode: int64(fi.Mode().Perm()), ModTime: fi.ModTime(), Typeflag: tar.TypeDir}, nil); err != nil {
		return err
	}
	ents, err := os.ReadDir(src)
	if err != nil {
		return err
	}
	for _, e := range ents {
		if err := addTree(w, filepath.Join(src, e.Name()), name+"/"+e.Name(), depth+1); err != nil {
			return err
		}
	}
	return nil
}

func runningServices(ctx context.Context, p *engine.Project) ([]string, error) {
	ps, err := p.PS(ctx)
	if err != nil {
		return nil, err
	}
	seen := map[string]bool{}
	for _, c := range ps {
		if c.State == "running" && c.Service != "" {
			seen[c.Service] = true
		}
	}
	return slices.Sorted(maps.Keys(seen)), nil
}

func addDump(ctx context.Context, w *archiveWriter, p *engine.Project, spool string) error {
	f, err := os.OpenFile(spool, os.O_CREATE|os.O_TRUNC|os.O_RDWR, 0o600)
	if err != nil {
		return err
	}
	defer os.Remove(spool)
	defer f.Close()
	var stderr bytes.Buffer
	err = p.Exec(ctx, postgresService, engine.ExecOptions{Stdout: f, Stderr: &stderr, KillOnCancel: true},
		"sh", "-c", `exec pg_dump -U "$POSTGRES_USER" -d "$POSTGRES_DB" --format=custom`)
	if err != nil {
		return &DumpError{Output: strings.TrimSpace(stderr.String()), Err: err}
	}
	head := make([]byte, 5)
	if _, err := f.ReadAt(head, 0); err != nil || string(head) != "PGDMP" {
		return &DumpError{Output: strings.TrimSpace(stderr.String())}
	}
	fi, err := f.Stat()
	if err != nil {
		return err
	}
	if _, err := f.Seek(0, io.SeekStart); err != nil {
		return err
	}
	return w.add(&tar.Header{Name: DumpName, Mode: 0o600, Size: fi.Size(), ModTime: time.Now(), Typeflag: tar.TypeReg}, f)
}

type InterruptedError struct {
	Restarted bool
	Err       error
}

func (e *InterruptedError) Error() string { return "backup: interrupted" }

func (e *InterruptedError) Unwrap() error { return e.Err }

func restartServices(ctx context.Context, p *engine.Project, running []string, onEvent func(engine.Event)) error {
	err := p.StartWatched(ctx, onEvent, running...)
	if err == nil {
		return nil
	}
	for _, svc := range running {
		if serr := p.Start(ctx, svc); serr != nil {
			return err
		}
	}
	return nil
}

func copyVolumes(ctx context.Context, w *archiveWriter, p *engine.Project, image string, volumes []volumeRef, running []string, progress func(string), onEvent func(engine.Event)) (err error) {
	progress(StageStopping)
	defer func() {
		if len(running) == 0 {
			return
		}
		progress(StageStarting)
		if serr := restartServices(context.WithoutCancel(ctx), p, running, onEvent); serr != nil {
			if err == nil || ctx.Err() != nil {
				err = serr
			}
			return
		}
		if ctx.Err() != nil {
			err = &InterruptedError{Restarted: true, Err: ctx.Err()}
		}
	}()
	if err := p.Stop(context.WithoutCancel(ctx)); err != nil {
		return err
	}
	if err := ctx.Err(); err != nil {
		return err
	}
	progress(StageVolumes)
	for _, v := range volumes {
		if err := copyVolume(ctx, w, p, image, v); err != nil {
			return err
		}
	}
	return nil
}

const volumeTarScript = `b=$(find /data -type f -exec stat -c '%i %s' {} + | awk '!seen[$1]++ { t += $2 } END { print t + 0 }') || exit 1
tar -cf - -C /data . || exit 1
echo "rexctl-bytes $b" >&2`

func copyVolume(ctx context.Context, w *archiveWriter, p *engine.Project, image string, v volumeRef) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	pr, pw := io.Pipe()
	var stderr bytes.Buffer
	done := make(chan error, 1)
	go func() {
		err := p.Helper(ctx, engine.HelperOptions{
			Image:      image,
			Entrypoint: []string{"sh", "-c"},
			Command:    []string{volumeTarScript},
			Mounts:     []engine.Mount{v.mount("/data", true)},
			Stdout:     pw,
			Stderr:     &stderr,
		})
		pw.CloseWithError(err)
		done <- err
	}()
	copied, werr := relayVolume(w, v.short, tar.NewReader(pr))
	if werr != nil {
		cancel()
		pr.CloseWithError(werr)
	} else {
		io.Copy(io.Discard, pr)
	}
	rerr := <-done
	var stalled *engine.Error
	if errors.Is(rerr, engine.ErrHelperStalled) && errors.As(rerr, &stalled) {
		return fmt.Errorf("backup: copying %s: %w", v.short, stalled.Err)
	}
	if werr != nil {
		return werr
	}
	out := strings.TrimSpace(stderr.String())
	if rerr != nil {
		return fmt.Errorf("backup: copying %s: %w: %s", v.short, rerr, out)
	}
	want, ok := measuredBytes(out)
	if !ok {
		return fmt.Errorf("backup: copying %s: the copy didn't report its size: %s", v.short, out)
	}
	if copied != want {
		return fmt.Errorf("backup: copying %s: copied %d bytes of %d", v.short, copied, want)
	}
	return nil
}

func measuredBytes(stderr string) (int64, bool) {
	lines := strings.Split(stderr, "\n")
	for i := len(lines) - 1; i >= 0; i-- {
		if rest, ok := strings.CutPrefix(strings.TrimSpace(lines[i]), "rexctl-bytes "); ok {
			n, err := strconv.ParseInt(strings.TrimSpace(rest), 10, 64)
			return n, err == nil
		}
	}
	return 0, false
}

func relayVolume(w *archiveWriter, short string, tr *tar.Reader) (int64, error) {
	var copied int64
	for {
		hdr, err := tr.Next()
		if errors.Is(err, io.EOF) {
			return copied, nil
		}
		if err != nil {
			return copied, err
		}
		inner := strings.TrimPrefix(strings.TrimPrefix(hdr.Name, "./"), "/")
		inner = strings.TrimSuffix(inner, "/")
		name := VolumesPrefix + short
		if inner != "" && inner != "." {
			name += "/" + inner
		}
		out := &tar.Header{
			Name:     name,
			Typeflag: hdr.Typeflag,
			Mode:     hdr.Mode,
			Uid:      hdr.Uid,
			Gid:      hdr.Gid,
			Uname:    hdr.Uname,
			Gname:    hdr.Gname,
			ModTime:  hdr.ModTime,
			Devmajor: hdr.Devmajor,
			Devminor: hdr.Devminor,
		}
		switch hdr.Typeflag {
		case tar.TypeReg:
			out.Size = hdr.Size
			copied += hdr.Size
		case tar.TypeDir:
			out.Name += "/"
		case tar.TypeSymlink:
			out.Linkname = hdr.Linkname
		case tar.TypeLink:
			target := strings.TrimPrefix(strings.TrimPrefix(hdr.Linkname, "./"), "/")
			out.Linkname = VolumesPrefix + short + "/" + target
		case tar.TypeChar, tar.TypeBlock, tar.TypeFifo:
		default:
			continue
		}
		if err := w.add(out, tr); err != nil {
			return copied, err
		}
	}
}
