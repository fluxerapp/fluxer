// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"archive/tar"
	"bufio"
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"maps"
	"net"
	"os"
	"path"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	ImageRepo            = "ghcr.io/fluxerapp/fluxer"
	imageRepoEnv         = "REXCTL_IMAGE_REPO"
	ContainerData        = native.ContainerData
	containerEnvFile     = ContainerData + "/fluxer.env"
	containerSecrets     = "secrets.env"
	containerStampRel    = "VERSION"
	ContainerStop        = 130 * time.Second
	ContainerUID         = 65532
	restartUnlessStopped = "unless-stopped"
	restartAlways        = "always"
	restartNever         = "no"
	LabelManagedBy       = "app.fluxer.managed-by"
	LabelVersion         = "app.fluxer.version"
	labelSpec            = "app.fluxer.spec"
	ManagedByRexctl      = "rexctl"
	lowPortsSysctl       = "net.ipv4.ip_unprivileged_port_start"
	containerHTTP        = 80
	containerHTTPS       = 443
	containerProxy       = 8080
	previousSuffix       = "-previous"
	volumeSuffix         = "-data"
	configDirName        = "config"
	proxyContainerTCP    = "tcp"
)

func imageRepo() string {
	if v := strings.TrimSpace(os.Getenv(imageRepoEnv)); v != "" {
		return v
	}
	return ImageRepo
}

func ImageRef(version string) string { return imageRepo() + ":" + version }

var imageDigest string

var digestPattern = regexp.MustCompile(`^sha256:[0-9a-f]{64}$`)

var ErrImageDigest = errors.New("runtime: the downloaded image doesn't match the digest built into this rexctl")

func pinnedDigest(version string) string {
	if version != stack.EmbeddedRelease() || imageRepo() != ImageRepo || !digestPattern.MatchString(imageDigest) {
		return ""
	}
	return imageDigest
}

func ContainerName(inst *instance.Instance) string { return inst.Project() }

func PreviousName(inst *instance.Instance) string { return inst.Project() + previousSuffix }

func VolumeName(inst *instance.Instance) string { return inst.Project() + volumeSuffix }

func composeVolumeLabels(project, volume string) map[string]string {
	return map[string]string{"com.docker.compose.project": project, "com.docker.compose.volume": volume}
}

func ConfigDir(inst *instance.Instance) string { return inst.Path(configDirName) }

func ContainerLabel(kind engine.Kind) string {
	if kind == "" {
		kind = engine.Docker
	}
	return kind.Name() + ", one container"
}

func settingPort(env map[string]string, key string, def int) int {
	if n, err := strconv.Atoi(strings.TrimSpace(env[key])); err == nil && n > 0 && n < 65536 {
		return n
	}
	return def
}

func ContainerPorts(inst *instance.Instance, env map[string]string) []engine.PortMap {
	st := inst.State
	var out []engine.PortMap
	mode := st.Settings[settings.EdgeMode]
	if mode == "" || mode == settings.EdgeBundled {
		http := settingPort(env, "FLUXER_HTTP_PORT", containerHTTP)
		https := settingPort(env, "FLUXER_HTTPS_PORT", containerHTTPS)
		out = append(out,
			engine.PortMap{Host: http, Container: http, Proto: "tcp"},
			engine.PortMap{Host: https, Container: https, Proto: "tcp"},
			engine.PortMap{Host: https, Container: https, Proto: "udp"})
	} else {
		bind, err := native.ProxyListen(st.Settings[settings.EdgeBind])
		if err != nil {
			bind = settings.DefaultBind
		}
		host, port, _ := net.SplitHostPort(bind)
		p, _ := strconv.Atoi(port)
		out = append(out, engine.PortMap{HostIP: host, Host: p, Container: containerProxy, Proto: proxyContainerTCP})
	}
	if env[stack.VoiceBundledKey] != "false" && env["FLUXER_LIVEKIT_ENABLED"] != "false" {
		tcp := settingPort(env, stack.VoiceTCPPortKey, 7881)
		udp := settingPort(env, stack.VoiceUDPPortKey, 7882)
		out = append(out, engine.PortMap{Host: tcp, Container: tcp, Proto: "tcp"}, engine.PortMap{Host: udp, Container: udp, Proto: "udp"})
	}
	return out
}

func restartPolicy(inst *instance.Instance) string {
	if inst.State.Engine.Kind == string(engine.Podman) {
		return restartAlways
	}
	return restartUnlessStopped
}

func ContainerSpec(inst *instance.Instance, version string, env map[string]string) engine.SingleSpec {
	s := engine.SingleSpec{
		Name:         ContainerName(inst),
		Image:        ImageRef(version),
		Cmd:          []string{"run", "--config", containerEnvFile},
		Env:          []string{"FLUXER_BACKUP_ON_UPGRADE=false"},
		Labels:       map[string]string{LabelManagedBy: ManagedByRexctl, LabelVersion: version},
		Ports:        ContainerPorts(inst, env),
		Volume:       VolumeName(inst),
		VolumeTarget: ContainerData,
		StopTimeout:  int(ContainerStop / time.Second),
		Restart:      restartPolicy(inst),
		Sysctls:      map[string]string{lowPortsSysctl: "0"},
		ExtraHosts:   slices.Clone(inst.State.ExtraHosts),
	}
	if inst.State.BootService == instance.BootServiceOff {
		s.Restart = restartNever
	}
	s.Labels[labelSpec] = specHash(s)
	return s
}

func specHash(s engine.SingleSpec) string {
	h := sha256.New()
	fmt.Fprintf(h, "%s\x00%q\x00%q\x00%s\x00%s\x00%d\x00", s.Image, s.Cmd, s.Env, s.Volume, s.VolumeTarget, s.StopTimeout)
	for _, p := range s.Ports {
		fmt.Fprintf(h, "%s\x00", p)
	}
	for _, k := range slices.Sorted(maps.Keys(s.Sysctls)) {
		fmt.Fprintf(h, "%s=%s\x00", k, s.Sysctls[k])
	}
	if s.User != "" || s.HostNetwork || len(s.Binds)+len(s.CapDrop)+len(s.CapAdd)+len(s.SecurityOpt) > 0 {
		fmt.Fprintf(h, "%s\x00%t\x00%q\x00%q\x00%q\x00%q\x00", s.User, s.HostNetwork, s.Binds, s.CapDrop, s.CapAdd, s.SecurityOpt)
	}
	if len(s.ExtraHosts) > 0 {
		fmt.Fprintf(h, "%q\x00", s.ExtraHosts)
	}
	return hex.EncodeToString(h.Sum(nil))[:16]
}

type containerEngine interface {
	Inspect(ctx context.Context, name string) (engine.Single, bool, error)
	CreateSingle(ctx context.Context, s engine.SingleSpec) error
	StartSingle(ctx context.Context, name string) error
	StopSingle(ctx context.Context, name string, timeout time.Duration) error
	RemoveSingle(ctx context.Context, name string) error
	RenameSingle(ctx context.Context, from, to string) error
	SetRestart(ctx context.Context, name, policy string) error
	PullImage(ctx context.Context, ref string, onEvent func(engine.Event)) error
	LocalImage(ctx context.Context, ref string) (string, bool, error)
	RepoDigestOf(ctx context.Context, ref string) string
	RepoDigestsOf(ctx context.Context, ref string) []string
	RemoveImages(ctx context.Context, refs []string) error
	DialSingle(ctx context.Context, name string, command ...string) (net.Conn, error)
	ExecSingle(ctx context.Context, name string, o engine.ExecOptions, command ...string) error
	RunOnce(ctx context.Context, s engine.SingleSpec, stdin io.Reader, stdout, stderr io.Writer) error
	ReadSingleFile(ctx context.Context, name, path string) ([]byte, error)
	WriteSingleFiles(ctx context.Context, name, dir string, uid, gid int, files ...engine.File) error
	WriteSingleTar(ctx context.Context, name, dir string, content io.Reader) error
	SingleFileSize(ctx context.Context, name, path string) (int64, error)
	SingleLogs(ctx context.Context, name string, o engine.LogsOptions, w io.Writer) error
	VolumeExists(ctx context.Context, name string) (bool, error)
	CopyOut(ctx context.Context, name, path string, fn func(name string, size int64, r io.Reader) error) error
	CreateVolume(ctx context.Context, name string, labels map[string]string) error
	ImageTags(ctx context.Context, repo string) ([]string, error)
	RemoveVolume(ctx context.Context, name string) error
}

type containerRT struct {
	inst    *instance.Instance
	u       *ui.UI
	connect Connect
	eng     containerEngine
	kind    engine.Kind
	n       *native.Stack
}

func newContainer(inst *instance.Instance, u *ui.UI, connect Connect) *containerRT {
	r := &containerRT{inst: inst, u: u, connect: connect, kind: engine.Kind(inst.State.Engine.Kind)}
	r.n = native.New(inst)
	r.n.Dial = r.dial
	r.n.Host = r.hostState
	return r
}

func (r *containerRT) engine(ctx context.Context) (containerEngine, error) {
	if r.eng != nil {
		return r.eng, nil
	}
	connect := r.connect
	if connect == nil {
		connect = func(ctx context.Context) (*engine.Engine, error) {
			pin := r.inst.State.Engine
			return engine.Connect(ctx, engine.ConnectOptions{Host: pin.Host, ID: pin.ID, Prefer: engine.Kind(pin.Kind), AllowOld: true})
		}
	}
	e, err := connect(ctx)
	if err != nil {
		return nil, EngineError(engine.Kind(r.inst.State.Engine.Kind), err)
	}
	r.eng, r.kind = e, e.Kind
	return e, nil
}

func (r *containerRT) Kind() Kind { return Container }

func (r *containerRT) name() string {
	if r.inst.State.Ejected != nil {
		return r.inst.Project() + "-" + composeService + "-1"
	}
	return ContainerName(r.inst)
}

func (r *containerRT) dial(ctx context.Context, target string) (net.Conn, error) {
	e, err := r.engine(ctx)
	if err != nil {
		return nil, err
	}
	return e.DialSingle(ctx, r.name(), "fluxerd", "dial", target)
}

func (r *containerRT) hostState(ctx context.Context) (string, int) {
	e, err := r.engine(ctx)
	if err != nil {
		return "", 0
	}
	c, ok, err := e.Inspect(ctx, r.name())
	if err != nil || !ok {
		return "inactive", 0
	}
	return unitState(c), c.RestartCount
}

func unitState(c engine.Single) string {
	switch c.State {
	case "running":
		return "active"
	case "restarting":
		return "activating"
	case "dead":
		return "failed"
	case "exited":
		if c.ExitCode != 0 && c.Restart == restartNever {
			return "failed"
		}
	}
	return "inactive"
}

func (r *containerRT) Info(ctx context.Context) Info {
	kind := r.kind
	if kind == "" {
		kind = engine.Kind(r.inst.State.Engine.Kind)
	}
	return Info{Kind: Container, Label: ContainerLabel(kind), Unit: r.name(), DataDir: r.dataRoot(), EdgeBind: r.n.EdgeBind()}
}

func (r *containerRT) dataRoot() string {
	if e, ok := r.eng.(*engine.Engine); ok && e != nil {
		return engine.Stores(r.inst.Dir, e, e.Kind).Data
	}
	return ""
}

func (r *containerRT) Status(ctx context.Context) (Status, error) {
	st := Status{Release: r.inst.State.Release, RunningRelease: r.inst.State.Release}
	if r.inst.Busy() {
		st.Busy = "Another rexctl command is changing this instance."
	}
	e, err := r.engine(ctx)
	if err != nil {
		return st, err
	}
	c, ok, err := e.Inspect(ctx, r.name())
	if err != nil {
		return st, ContainerError(err, "Couldn't read the state of Fluxer's container.")
	}
	if !ok {
		return st, nil
	}
	if v := c.Labels[LabelVersion]; v != "" {
		st.RunningRelease = v
	} else if _, tag, ok := strings.Cut(strings.TrimPrefix(c.Image, imageRepo()), ":"); ok && release.Valid(tag) {
		st.RunningRelease = tag
	}
	ns := r.n.Status(ctx)
	st.Running = c.Running()
	st.Restarts = ns.Restarts
	st.StartedAt = c.StartedAt
	st.Parts = withEmbedded(foldParts(ns.Containers), ns.Containers, st.StartedAt, r.inst.State.Settings)
	return st, nil
}

func ContainerError(err error, msg string) error {
	var ee *ui.ExitError
	var engErr *engine.Error
	switch {
	case err == nil:
		return nil
	case errors.As(err, &ee), errors.Is(err, context.Canceled):
		return err
	case errors.As(err, &engErr) && engErr.Class == engine.ClassUnreachable:
		return EngineError("", err)
	}
	if errors.Is(err, ErrImageDigest) {
		return ui.Wrap(err, msg+" The image it got isn't the one built into this release of rexctl, so rexctl didn't use it.", "Check where this machine gets images from ghcr.io, such as a mirror or a proxy, remove that image, then try again.")
	}
	var space *native.SpaceError
	if errors.As(err, &space) {
		return ui.Wrap(err, "There isn't enough free space in "+space.Dir+" to "+space.What+".", "Free up "+ui.Bytes(space.Short())+" on that disk, then try again.")
	}
	return ui.Wrap(err, msg, "Run the command again with --verbose to see why.")
}

type containerApplied struct {
	Created   bool
	Started   bool
	Restarted bool
	Reloaded  []string
}

type applyMode int

const (
	applyChanges applyMode = iota
	applyRestart
	applyCreate
)

func (r *containerRT) files(ctx context.Context, e containerEngine, set *stack.Set) (envData, secretsData []byte, err error) {
	old := map[string]string{}
	data, err := e.ReadSingleFile(ctx, r.name(), ContainerData+"/"+containerSecrets)
	switch {
	case errors.Is(err, engine.ErrNoFile):
	case err != nil:
		return nil, nil, err
	default:
		f, err := envfile.Parse(containerSecrets, data)
		if err != nil {
			return nil, nil, err
		}
		old = f.Map()
	}
	return r.n.Files(set, old)
}

func (r *containerRT) environment(set *stack.Set) (map[string]string, error) {
	env, _, err := r.n.Environment(set)
	return env, err
}

func (r *containerRT) spec(set *stack.Set, version string) (engine.SingleSpec, error) {
	env, err := r.environment(set)
	if err != nil {
		return engine.SingleSpec{}, err
	}
	return ContainerSpec(r.inst, version, env), nil
}

func (r *containerRT) writeHostCopy(envData []byte) error {
	dir := ConfigDir(r.inst)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return err
	}
	_, err := writeChanged(dir+"/fluxer.env", envData)
	return err
}

func writeChanged(path string, data []byte) (bool, error) {
	if old, err := os.ReadFile(path); err == nil && bytes.Equal(old, data) {
		return false, nil
	}
	return true, envfile.WriteFileAtomic(path, data, true)
}

func (r *containerRT) push(ctx context.Context, e containerEngine, set *stack.Set) (bool, error) {
	envData, secretsData, err := r.files(ctx, e, set)
	if err != nil {
		return false, err
	}
	if err := r.writeHostCopy(envData); err != nil {
		return false, err
	}
	files := append([]engine.File{
		{Name: "fluxer.env", Data: envData, Mode: 0o600},
		{Name: containerSecrets, Data: secretsData, Mode: 0o600},
	}, hostFiles(r.inst)...)
	changed := false
	for _, f := range files {
		if f.Data == nil {
			continue
		}
		have, err := e.ReadSingleFile(ctx, r.name(), ContainerData+"/"+f.Name)
		switch {
		case errors.Is(err, engine.ErrNoFile):
		case err != nil:
			return false, err
		case bytes.Equal(have, f.Data):
			continue
		}
		if err := e.WriteSingleFiles(ctx, r.name(), ContainerData, ContainerUID, ContainerUID, files...); err != nil {
			return false, err
		}
		changed = true
		break
	}
	pushed, err := r.pushGeoIP(ctx, e, set)
	return changed || pushed, err
}

func (r *containerRT) pushGeoIP(ctx context.Context, e containerEngine, set *stack.Set) (bool, error) {
	env, err := r.environment(set)
	if err != nil {
		return false, err
	}
	dir := native.ContainerFiles + "/" + instance.GeoIPDir
	name, ok := strings.CutPrefix(strings.TrimSpace(env[settings.GeoIPVar]), dir+"/")
	if !ok || name == "" || strings.Contains(name, "/") {
		if _, err := e.SingleFileSize(ctx, r.name(), dir); errors.Is(err, engine.ErrNoFile) {
			return false, nil
		}
		_ = e.ExecSingle(ctx, r.name(), engine.ExecOptions{}, "rm", "-rf", dir)
		return false, nil
	}
	src := r.inst.Path(instance.GeoIPDir, name)
	fi, err := os.Stat(src)
	if err != nil {
		return false, nil
	}
	size, err := e.SingleFileSize(ctx, r.name(), dir+"/"+name)
	switch {
	case errors.Is(err, engine.ErrNoFile):
	case err != nil:
		return false, err
	case size == fi.Size():
		return false, nil
	}
	if err := r.streamFile(ctx, e, src, path.Join(strings.TrimPrefix(dir, ContainerData+"/"), name), fi.Size()); err != nil {
		return false, err
	}
	_ = e.ExecSingle(ctx, r.name(), engine.ExecOptions{}, "find", dir, "-maxdepth", "1", "-type", "f", "!", "-name", name, "-delete")
	return true, nil
}

func (r *containerRT) streamFile(ctx context.Context, e containerEngine, src, name string, size int64) error {
	f, err := os.Open(src)
	if err != nil {
		return err
	}
	defer f.Close()
	pr, pw := io.Pipe()
	go func() {
		tw := tar.NewWriter(pw)
		now := time.Now()
		var chain []string
		for d := path.Dir(name); d != "." && d != "/"; d = path.Dir(d) {
			chain = append([]string{d}, chain...)
		}
		for _, d := range chain {
			if err := tw.WriteHeader(&tar.Header{Name: d + "/", Mode: 0o700, Uid: ContainerUID, Gid: ContainerUID, ModTime: now, Typeflag: tar.TypeDir}); err != nil {
				pw.CloseWithError(err)
				return
			}
		}
		if err := tw.WriteHeader(&tar.Header{Name: name, Mode: 0o600, Size: size, Uid: ContainerUID, Gid: ContainerUID, ModTime: now, Typeflag: tar.TypeReg}); err != nil {
			pw.CloseWithError(err)
			return
		}
		if _, err := io.CopyN(tw, f, size); err != nil {
			pw.CloseWithError(err)
			return
		}
		pw.CloseWithError(tw.Close())
	}()
	err = e.WriteSingleTar(ctx, r.name(), ContainerData, pr)
	pr.CloseWithError(err)
	return err
}

func hostFiles(inst *instance.Instance) []engine.File {
	rel := strings.TrimPrefix(native.ContainerFiles, ContainerData+"/")
	var out []engine.File
	for _, dir := range []string{instance.CertsDir, instance.CADir} {
		entries, err := os.ReadDir(inst.Path(dir))
		if err != nil {
			continue
		}
		var files []engine.File
		for _, ent := range entries {
			if !ent.Type().IsRegular() {
				continue
			}
			data, err := os.ReadFile(inst.Path(dir, ent.Name()))
			if err != nil {
				continue
			}
			files = append(files, engine.File{Name: rel + "/" + dir + "/" + ent.Name(), Data: data, Mode: 0o600})
		}
		if len(files) > 0 {
			out = append(out, engine.File{Name: rel + "/" + dir + "/", Mode: 0o700})
			out = append(out, files...)
		}
	}
	if len(out) > 0 {
		out = append([]engine.File{{Name: rel + "/", Mode: 0o700}}, out...)
	}
	return out
}

func (r *containerRT) create(ctx context.Context, e containerEngine, spec engine.SingleSpec, set *stack.Set) error {
	if err := e.CreateVolume(ctx, spec.Volume, composeVolumeLabels(r.inst.Project(), spec.Volume)); err != nil {
		return err
	}
	if err := e.CreateSingle(ctx, spec); err != nil {
		return err
	}
	if _, err := r.push(ctx, e, set); err != nil {
		_ = e.RemoveSingle(context.WithoutCancel(ctx), spec.Name)
		return err
	}
	return nil
}

func (r *containerRT) replace(ctx context.Context, e containerEngine, spec engine.SingleSpec, set *stack.Set) error {
	if err := e.StopSingle(ctx, spec.Name, ContainerStop); err != nil {
		return err
	}
	if err := e.RemoveSingle(ctx, spec.Name); err != nil {
		return err
	}
	return r.create(ctx, e, spec, set)
}

func (r *containerRT) apply(ctx context.Context, mode applyMode) (containerApplied, error) {
	var a containerApplied
	e, err := r.engine(ctx)
	if err != nil {
		return a, err
	}
	set, err := deploy.ReleaseSet(r.inst)
	if err != nil {
		return a, err
	}
	r.n.PinEdgeBind()
	spec, err := r.spec(set, r.inst.State.Release)
	if err != nil {
		return a, ContainerError(err, "rexctl can't work out Fluxer's settings.")
	}
	c, ok, err := e.Inspect(ctx, spec.Name)
	if err != nil {
		return a, err
	}
	switch {
	case !ok:
		if err := r.create(ctx, e, spec, set); err != nil {
			return a, err
		}
		a.Created = true
	case c.Labels[labelSpec] != spec.Labels[labelSpec]:
		if err := r.replace(ctx, e, spec, set); err != nil {
			return a, err
		}
		a.Created = true
	default:
		if c.Restart != spec.Restart {
			if err := e.SetRestart(ctx, spec.Name, spec.Restart); err != nil {
				return a, err
			}
		}
		changed, err := r.push(ctx, e, set)
		if err != nil {
			return a, err
		}
		running := c.Running()
		switch {
		case !running:
		case mode == applyRestart:
			if err := e.StopSingle(ctx, spec.Name, ContainerStop); err != nil {
				return a, err
			}
			a.Restarted = true
		case changed:
			reloaded, err := r.n.Reload(ctx)
			if err == nil {
				a.Reloaded = reloaded
				return a, r.save()
			}
			if err := e.StopSingle(ctx, spec.Name, ContainerStop); err != nil {
				return a, err
			}
			a.Restarted = true
		default:
			return a, r.save()
		}
	}
	if mode == applyCreate {
		if err := keepDown(ctx, e, spec.Name, spec.Restart); err != nil {
			return a, err
		}
		return a, r.save()
	}
	if err := e.StartSingle(ctx, spec.Name); err != nil {
		return a, err
	}
	if !a.Restarted {
		a.Started = true
	}
	return a, r.save()
}

func (r *containerRT) save() error {
	r.inst.State.Stopped = false
	return saveState(r.inst)
}

func (r *containerRT) Prepare(ctx context.Context) error {
	_, err := r.apply(ctx, applyCreate)
	return ContainerError(err, "Couldn't set up Fluxer's container.")
}

func (r *containerRT) wait(ctx context.Context, s *ui.Step, title string) error {
	err := r.n.WaitReady(ctx, startTimeout(), ShowReadiness(s, title))
	if err == nil || errors.Is(err, context.Canceled) {
		return err
	}
	fix := "Run rexctl logs to see why."
	var nr *engine.NotReadyError
	if errors.As(err, &nr) {
		msg := "Fluxer didn't become ready in time."
		switch {
		case nr.Fatal || slices.Contains(nr.Crashing, native.Supervisor):
			msg = "Fluxer's container keeps stopping."
		case len(nr.Crashing) == 1:
			msg = PartOf(nr.Crashing[0]) + " keeps stopping."
		case len(nr.Crashing) > 1:
			msg = ui.JoinAnd(partsOf(nr.Crashing)) + " keep stopping."
		}
		return ui.Wrap(err, msg, fix)
	}
	return ui.Wrap(err, "Fluxer didn't start.", fix)
}

func startTimeout() time.Duration { return engine.DefaultReadyTimeout }

func (r *containerRT) Start(ctx context.Context, s *ui.Step, o StartOptions) error {
	if _, err := r.apply(ctx, applyChanges); err != nil {
		return ContainerError(err, "Couldn't start Fluxer's container.")
	}
	if err := r.n.StartStopped(ctx); err != nil && !o.NoWait {
		r.u.Detail("%v", err)
	}
	if o.NoWait {
		return nil
	}
	return r.wait(ctx, s, "Starting Fluxer")
}

func (r *containerRT) Stop(ctx context.Context, s *ui.Step) error {
	e, err := r.engine(ctx)
	if err != nil {
		return err
	}
	r.inst.State.Stopped = true
	if err := saveState(r.inst); err != nil {
		return err
	}
	if err := e.StopSingle(context.WithoutCancel(ctx), r.name(), ContainerStop); err != nil {
		return ContainerError(err, "Couldn't stop Fluxer.")
	}
	if err := keepDown(context.WithoutCancel(ctx), e, r.name(), restartPolicy(r.inst)); err != nil {
		return ContainerError(err, "Couldn't keep Fluxer stopped after a restart of this machine.")
	}
	return nil
}

func keepDown(ctx context.Context, e containerEngine, name, policy string) error {
	if policy != restartAlways {
		return nil
	}
	return e.SetRestart(ctx, name, restartNever)
}

func (r *containerRT) Restart(ctx context.Context, s *ui.Step, parts []string) error {
	if len(parts) > 0 {
		children, err := childrenOf(r.inst, parts)
		if err != nil {
			return err
		}
		if entries, err := r.n.Entries(ctx); err == nil {
			children = slices.DeleteFunc(children, func(c string) bool {
				return !slices.ContainsFunc(entries, func(e native.PsEntry) bool { return e.Name == c })
			})
		}
		if len(children) == 0 {
			return unsupported("This instance runs without "+ui.JoinAnd(parts)+".", "Run rexctl status to see what runs.")
		}
		if err := r.n.Restart(ctx, children...); err != nil {
			return ContainerError(err, "Couldn't restart "+ui.JoinAnd(parts)+".")
		}
		return r.wait(ctx, s, "Restarting Fluxer")
	}
	if _, err := r.apply(ctx, applyRestart); err != nil {
		return ContainerError(err, "Couldn't restart Fluxer's container.")
	}
	return r.wait(ctx, s, "Restarting Fluxer")
}

func (r *containerRT) Down(ctx context.Context, s *ui.Step) error { return r.Stop(ctx, s) }

func (r *containerRT) Logs(ctx context.Context, w io.Writer, o LogOptions) error {
	lp, err := planLogs(r.inst, o.Parts)
	if err != nil {
		return err
	}
	e, err := r.engine(ctx)
	if err != nil {
		return err
	}
	tail, keep := o.Tail, 0
	if len(o.Parts) > 0 && o.Tail > 0 {
		tail, keep = 0, o.Tail
		if o.Follow {
			tail, keep = o.Tail*partTailFactor, 0
		}
	}
	pr, pw := io.Pipe()
	done := make(chan error, 1)
	go func() {
		err := containerLines(pr, w, lp, keep)
		pr.CloseWithError(err)
		done <- err
	}()
	err = e.SingleLogs(ctx, r.name(), engine.LogsOptions{Follow: o.Follow, Since: o.Since, Tail: tail}, pw)
	pw.Close()
	if lerr := <-done; err == nil {
		err = lerr
	}
	if err != nil && ctx.Err() == nil {
		return ContainerError(err, "Couldn't read the logs.")
	}
	return nil
}

func splitLabel(line string) (string, string) {
	if rest, ok := strings.CutPrefix(line, "["); ok {
		if label, msg, ok := strings.Cut(rest, "] "); ok && !strings.ContainsAny(label, " []") {
			return label, msg
		}
	}
	return native.Supervisor, line
}

const partTailFactor = 20

func containerLines(r io.Reader, w io.Writer, lp logPlan, keep int) error {
	var last []string
	sc := bufio.NewScanner(r)
	sc.Buffer(make([]byte, 64*1024), 4*1024*1024)
	for sc.Scan() {
		label, msg := splitLabel(sc.Text())
		service := journalService("fluxer-"+label, msg)
		if label == native.Supervisor {
			service = journalService(native.Supervisor, msg)
		}
		if len(lp.services) > 0 && !slices.Contains(lp.services, service) {
			continue
		}
		out, ok := lp.line(service, msg)
		if !ok {
			continue
		}
		if keep > 0 {
			last = append(last, out)
			if len(last) > keep {
				last = last[1:]
			}
			continue
		}
		if _, err := io.WriteString(w, out+"\n"); err != nil {
			return err
		}
	}
	for _, out := range last {
		if _, err := io.WriteString(w, out+"\n"); err != nil {
			return err
		}
	}
	return sc.Err()
}

func (r *containerRT) plan(o UninstallOptions) Removal {
	var rm Removal
	rm.Goes = append(rm.Goes, ui.KV{Key: "Container", Value: r.name()})
	if o.KeepData {
		rm.Stays = append(rm.Stays, ui.KV{Key: "Data", Value: "volume " + VolumeName(r.inst)}, ui.KV{Key: "Settings", Value: r.inst.Dir})
		rm.Kept = "Your data is still in the volume " + VolumeName(r.inst) + " and your settings are still in " + r.inst.Dir + "."
	} else {
		rm.Goes = append(rm.Goes, ui.KV{Key: "Data", Value: "volume " + VolumeName(r.inst)})
		rm.Goes, rm.Stays = folderRows(r.inst, o, rm.Goes, rm.Stays)
	}
	if !o.KeepData {
		rm.Goes = append(rm.Goes, ui.KV{Key: "Images", Value: imageRepo() + ", unless another container uses them"})
	}
	rm.Goes, rm.Stays = backupRows(r.inst, o, rm.Goes, rm.Stays)
	rm.Goes = append(rm.Goes, firewallRows(r.inst)...)
	rm.Stays = append(rm.Stays, proxyRows()...)
	rm.Stays = append(rm.Stays, r.machineRows()...)
	return rm
}

func (r *containerRT) machineRows() []ui.KV {
	var out []ui.KV
	if _, err := os.Stat(host.LowPortsSysctl.File); err == nil {
		out = append(out, ui.KV{Key: "Low ports", Value: host.LowPortsSysctl.File + ", which other containers may need"})
	}
	if r.kind == engine.Podman && r.rootless() {
		out = append(out, ui.KV{Key: "Podman", Value: "podman.socket, podman-restart.service and lingering for this account, which other containers may need"})
	}
	return out
}

func (r *containerRT) Uninstall(ctx context.Context, s *ui.Step, o UninstallOptions) error {
	e, err := r.engine(ctx)
	if err != nil {
		return err
	}
	ctx = context.WithoutCancel(ctx)
	for _, name := range []string{r.name(), PreviousName(r.inst)} {
		if err := e.StopSingle(ctx, name, ContainerStop); err != nil {
			return &ui.ExitError{Code: ui.ExitFailed, Msg: "Couldn't stop Fluxer, so rexctl removed nothing.", Fix: "Run rexctl stop, then run rexctl uninstall again.", Err: err}
		}
		if err := e.RemoveSingle(ctx, name); err != nil {
			return ContainerError(err, "Couldn't remove Fluxer's container.")
		}
	}
	var left []string
	if !o.KeepData {
		if err := e.RemoveVolume(ctx, VolumeName(r.inst)); err != nil {
			r.u.Detail("%v", err)
			left = append(left, "Your data is still in the volume "+VolumeName(r.inst)+".")
		}
		if tags, err := e.ImageTags(ctx, imageRepo()); err == nil {
			for _, tag := range tags {
				if err := e.RemoveImages(ctx, []string{ImageRef(tag)}); err != nil && !engine.InUse(err) {
					r.u.Detail("%v", err)
				}
			}
		}
	}
	stillThere(s, append(left, closeFirewall(ctx, r.inst)...))
	return nil
}

func (r *containerRT) Boot() Boot { return containerBoot{r} }

type containerBoot struct{ r *containerRT }

func (r *containerRT) rootless() bool {
	if e, ok := r.eng.(*engine.Engine); ok {
		return e.Rootless
	}
	return r.inst.State.Engine.Rootless()
}

func (b containerBoot) podman() bool { return b.r.kind == engine.Podman }

func (b containerBoot) State(ctx context.Context) (BootState, error) {
	e, err := b.r.engine(ctx)
	if err != nil {
		return BootState{}, err
	}
	c, ok, err := e.Inspect(ctx, b.r.name())
	if err != nil {
		return BootState{}, err
	}
	on := ok && c.Restart == restartPolicy(b.r.inst)
	detail := "Fluxer keeps running until you stop it or this machine restarts."
	if b.podman() {
		on = on && host.PodmanRestartOn(ctx, b.r.rootless())
	}
	return BootState{Enabled: on, Unit: "the container " + b.r.name(), Detail: detail}, nil
}

func (b containerBoot) set(ctx context.Context, on bool) error {
	e, err := b.r.engine(ctx)
	if err != nil {
		return err
	}
	b.r.inst.State.BootService = instance.BootServiceOff
	policy := restartNever
	if on {
		b.r.inst.State.BootService, policy = instance.BootServiceOn, restartPolicy(b.r.inst)
	}
	if err := saveState(b.r.inst); err != nil {
		return err
	}
	if on && b.podman() && !host.PodmanRestartOn(ctx, b.r.rootless()) {
		if err := host.PodmanRestartStep(b.r.rootless()).Run(ctx); err != nil {
			return err
		}
	}
	if _, ok, err := e.Inspect(ctx, b.r.name()); err != nil || !ok {
		return err
	}
	return e.SetRestart(ctx, b.r.name(), policy)
}

func (b containerBoot) steps(ctx context.Context, enable bool) ([]host.Step, error) {
	st, err := b.State(ctx)
	if err != nil || st.Enabled == enable {
		return nil, err
	}
	name, tool := b.r.name(), "docker"
	if b.podman() {
		tool = "podman"
	}
	if !enable {
		return []host.Step{{Do: "Stops starting the container " + name + " with the machine. Fluxer keeps running.", Command: tool + " update --restart " + restartNever + " " + name, Run: func(ctx context.Context) error { return b.set(ctx, false) }}}, nil
	}
	steps := []host.Step{{Do: "Starts the container " + name + " with the machine from now on.", Command: tool + " update --restart " + restartPolicy(b.r.inst) + " " + name, Run: func(ctx context.Context) error { return b.set(ctx, true) }}}
	if b.podman() && !host.PodmanRestartOn(ctx, b.r.rootless()) {
		pr := host.PodmanRestartStep(b.r.rootless())
		pr.Run = nil
		steps = append(steps, pr)
	}
	return steps, nil
}

func (b containerBoot) Enable(ctx context.Context, s *ui.Step) error {
	return ContainerError(b.set(ctx, true), "Couldn't make Fluxer start with this machine.")
}

func (b containerBoot) Disable(ctx context.Context, s *ui.Step) error {
	return ContainerError(b.set(ctx, false), "Couldn't stop Fluxer starting with this machine.")
}

func (r *containerRT) Admin(ctx context.Context) (adminapi.Transport, error) {
	if _, err := r.engine(ctx); err != nil {
		return nil, err
	}
	t := adminapi.NewHost("", native.APIAddr, deploy.ClientIPHeader(r.inst))
	t.Dial = func(ctx context.Context, target string) (net.Conn, error) {
		return r.dial(ctx, target)
	}
	return t, nil
}

func (r *containerRT) Settings() Settings { return containerSettings{r} }
func (r *containerRT) Backups() Backups   { return containerBackups{r} }
func (r *containerRT) Upgrades() Upgrades { return containerUpgrades{r} }

func FluxerdStack(r Runtime) *native.Stack {
	switch r := r.(type) {
	case *containerRT:
		return r.n
	case *nativeRT:
		return r.n
	}
	return nil
}

func (r *containerRT) ReadSecrets(ctx context.Context) (map[string]string, error) {
	e, err := r.engine(ctx)
	if err != nil {
		return nil, err
	}
	data, err := e.ReadSingleFile(ctx, r.name(), ContainerData+"/"+containerSecrets)
	if err != nil {
		return nil, err
	}
	f, err := envfile.Parse(containerSecrets, data)
	if err != nil {
		return nil, err
	}
	return f.Map(), nil
}
