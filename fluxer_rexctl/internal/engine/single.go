// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"archive/tar"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"net/netip"
	"strconv"
	"strings"
	"sync"
	"time"

	cerrdefs "github.com/containerd/errdefs"
	"github.com/moby/moby/api/pkg/stdcopy"
	"github.com/moby/moby/api/types/container"
	"github.com/moby/moby/api/types/mount"
	"github.com/moby/moby/api/types/network"
	"github.com/moby/moby/client"
)

type PortMap struct {
	HostIP    string
	Host      int
	Container int
	Proto     string
}

func (p PortMap) String() string {
	s := strconv.Itoa(p.Host) + ":" + strconv.Itoa(p.Container)
	if p.HostIP != "" {
		s = p.HostIP + ":" + s
	}
	if p.Proto != "" && p.Proto != "tcp" {
		s += "/" + p.Proto
	}
	return s
}

type SingleSpec struct {
	Name         string
	Image        string
	Entrypoint   []string
	Cmd          []string
	Env          []string
	Labels       map[string]string
	Ports        []PortMap
	Volume       string
	VolumeTarget string
	StopTimeout  int
	Restart      string
	Sysctls      map[string]string
	Stdin        bool
	User         string
	Binds        []string
	HostNetwork  bool
	CapDrop      []string
	CapAdd       []string
	SecurityOpt  []string
	ExtraHosts   []string
}

type Single struct {
	ID           string
	Name         string
	Image        string
	ImageID      string
	State        string
	Health       string
	ExitCode     int
	RestartCount int
	StartedAt    time.Time
	Labels       map[string]string
	Restart      string
	Ports        []PortMap
}

func (s Single) Running() bool { return s.State == "running" || s.State == "restarting" }

type File struct {
	Name string
	Data []byte
	Mode int64
}

func (e *Engine) single(op string, err error) error {
	if err == nil {
		return nil
	}
	err = daemonText(err)
	return &Error{Op: op, Class: ClassOf(err), Err: err}
}

func (e *Engine) Inspect(ctx context.Context, name string) (Single, bool, error) {
	c, err := e.api()
	if err != nil {
		return Single{}, false, err
	}
	res, err := c.ContainerInspect(ctx, name, client.ContainerInspectOptions{})
	if cerrdefs.IsNotFound(err) {
		return Single{}, false, nil
	}
	if err != nil {
		return Single{}, false, e.single("inspect", err)
	}
	r := res.Container
	out := Single{ID: r.ID, Name: strings.TrimPrefix(r.Name, "/"), ImageID: r.Image, RestartCount: r.RestartCount}
	if r.Config != nil {
		out.Image, out.Labels = r.Config.Image, r.Config.Labels
	}
	if r.HostConfig != nil {
		out.Restart = string(r.HostConfig.RestartPolicy.Name)
		for port, binds := range r.HostConfig.PortBindings {
			for _, b := range binds {
				host, _ := strconv.Atoi(b.HostPort)
				ip := ""
				if b.HostIP.IsValid() && !b.HostIP.IsUnspecified() {
					ip = b.HostIP.String()
				}
				out.Ports = append(out.Ports, PortMap{HostIP: ip, Host: host, Container: int(port.Num()), Proto: string(port.Proto())})
			}
		}
	}
	if st := r.State; st != nil {
		out.State, out.ExitCode = string(st.Status), st.ExitCode
		if st.Health != nil {
			out.Health = string(st.Health.Status)
		}
		if t, err := time.Parse(time.RFC3339Nano, st.StartedAt); err == nil && t.Year() > 1 {
			out.StartedAt = t
		}
	}
	return out, true, nil
}

func createOptions(s SingleSpec) (client.ContainerCreateOptions, error) {
	cfg := &container.Config{Image: s.Image, Entrypoint: s.Entrypoint, Cmd: s.Cmd, Env: s.Env, Labels: s.Labels, User: s.User}
	if s.StopTimeout > 0 {
		cfg.StopTimeout = &s.StopTimeout
	}
	if s.Stdin {
		cfg.AttachStdin, cfg.OpenStdin, cfg.StdinOnce = true, true, true
		cfg.AttachStdout, cfg.AttachStderr = true, true
	}
	hc := &container.HostConfig{Sysctls: s.Sysctls, Binds: s.Binds, CapDrop: s.CapDrop, CapAdd: s.CapAdd, SecurityOpt: s.SecurityOpt, ExtraHosts: s.ExtraHosts}
	if s.HostNetwork {
		hc.NetworkMode = "host"
	}
	if s.Restart != "" {
		hc.RestartPolicy = container.RestartPolicy{Name: container.RestartPolicyMode(s.Restart)}
	}
	if s.Volume != "" {
		hc.Mounts = []mount.Mount{{Type: mount.TypeVolume, Source: s.Volume, Target: s.VolumeTarget}}
	}
	if len(s.Ports) > 0 {
		cfg.ExposedPorts = network.PortSet{}
		hc.PortBindings = network.PortMap{}
		for _, p := range s.Ports {
			proto := p.Proto
			if proto == "" {
				proto = "tcp"
			}
			port, err := network.ParsePort(strconv.Itoa(p.Container) + "/" + proto)
			if err != nil {
				return client.ContainerCreateOptions{}, err
			}
			b := network.PortBinding{HostPort: strconv.Itoa(p.Host)}
			if p.HostIP != "" {
				addr, err := netip.ParseAddr(p.HostIP)
				if err != nil {
					return client.ContainerCreateOptions{}, err
				}
				b.HostIP = addr
			}
			cfg.ExposedPorts[port] = struct{}{}
			hc.PortBindings[port] = append(hc.PortBindings[port], b)
		}
	}
	return client.ContainerCreateOptions{Name: s.Name, Config: cfg, HostConfig: hc}, nil
}

func (e *Engine) CreateSingle(ctx context.Context, s SingleSpec) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	opts, err := createOptions(s)
	if err != nil {
		return &Error{Op: "create", Class: ClassInvalidConfig, Err: err}
	}
	_, err = c.ContainerCreate(ctx, opts)
	return e.single("create", err)
}

func (e *Engine) StartSingle(ctx context.Context, name string) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	_, err = c.ContainerStart(ctx, name, client.ContainerStartOptions{})
	return e.single("start", err)
}

func (e *Engine) StopSingle(ctx context.Context, name string, timeout time.Duration) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	secs := int(timeout / time.Second)
	_, err = c.ContainerStop(ctx, name, client.ContainerStopOptions{Timeout: &secs})
	if cerrdefs.IsNotFound(err) {
		return nil
	}
	return e.single("stop", err)
}

func (e *Engine) RemoveSingle(ctx context.Context, name string) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	_, err = c.ContainerRemove(ctx, name, client.ContainerRemoveOptions{RemoveVolumes: true})
	if cerrdefs.IsNotFound(err) {
		return nil
	}
	return e.single("remove", err)
}

func (e *Engine) RenameSingle(ctx context.Context, from, to string) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	_, err = c.ContainerRename(ctx, from, client.ContainerRenameOptions{NewName: to})
	return e.single("rename", err)
}

func (e *Engine) SetRestart(ctx context.Context, name, policy string) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	_, err = c.ContainerUpdate(ctx, name, client.ContainerUpdateOptions{RestartPolicy: &container.RestartPolicy{Name: container.RestartPolicyMode(policy)}})
	return e.single("update", err)
}

func (e *Engine) PullImage(ctx context.Context, ref string, onEvent func(Event)) error {
	c, err := e.conn()
	if err != nil {
		return err
	}
	if onEvent == nil {
		onEvent = func(Event) {}
	}
	return e.single("pull", pullOne(ctx, c, ref, ref, onEvent))
}

func (e *Engine) RepoDigestOf(ctx context.Context, ref string) string {
	c, err := e.api()
	if err != nil {
		return ""
	}
	res, err := c.ImageInspect(ctx, ref)
	if err != nil {
		return ""
	}
	repo, _, _ := strings.Cut(ref, ":")
	for _, d := range res.RepoDigests {
		if strings.HasPrefix(d, repo+"@") {
			return d
		}
	}
	return ""
}

func (e *Engine) RepoDigestsOf(ctx context.Context, ref string) []string {
	c, err := e.api()
	if err != nil {
		return nil
	}
	res, err := c.ImageInspect(ctx, ref)
	if err != nil {
		return nil
	}
	repo, _, _ := strings.Cut(ref, ":")
	var out []string
	for _, d := range res.RepoDigests {
		if strings.HasPrefix(d, repo+"@") {
			out = append(out, d)
		}
	}
	return out
}

func (e *Engine) VolumeExists(ctx context.Context, name string) (bool, error) {
	c, err := e.api()
	if err != nil {
		return false, err
	}
	_, err = c.VolumeInspect(ctx, name, client.VolumeInspectOptions{})
	if cerrdefs.IsNotFound(err) {
		return false, nil
	}
	return err == nil, e.single("volume", err)
}

func (e *Engine) CreateVolume(ctx context.Context, name string, labels map[string]string) error {
	if ok, err := e.VolumeExists(ctx, name); err != nil || ok {
		return err
	}
	c, err := e.api()
	if err != nil {
		return err
	}
	_, err = c.VolumeCreate(ctx, client.VolumeCreateOptions{Name: name, Labels: labels})
	return e.single("volume", err)
}

func (e *Engine) ImageTags(ctx context.Context, repo string) ([]string, error) {
	c, err := e.api()
	if err != nil {
		return nil, err
	}
	res, err := c.ImageList(ctx, client.ImageListOptions{Filters: make(client.Filters).Add("reference", repo)})
	if err != nil {
		return nil, e.single("images", err)
	}
	var out []string
	for _, img := range res.Items {
		for _, ref := range img.RepoTags {
			i := strings.LastIndex(ref, ":")
			if i > strings.LastIndex(ref, "/") && sameRepo(ref[:i], repo) {
				out = append(out, ref[i+1:])
			}
		}
	}
	return out, nil
}

func sameRepo(a, b string) bool {
	norm := func(s string) string {
		for _, p := range []string{"localhost/", "docker.io/library/", "docker.io/"} {
			s = strings.TrimPrefix(s, p)
		}
		return s
	}
	return norm(a) == norm(b)
}

func (e *Engine) ExecSingle(ctx context.Context, name string, o ExecOptions, command ...string) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	return execIn(ctx, c, name, o, command)
}

type execConn struct {
	hijack client.HijackedResponse
	out    *io.PipeReader
	once   sync.Once
}

func (x *execConn) Read(p []byte) (int, error)  { return x.out.Read(p) }
func (x *execConn) Write(p []byte) (int, error) { return x.hijack.Conn.Write(p) }

func (x *execConn) Close() error {
	x.once.Do(func() {
		x.hijack.Close()
		x.out.Close()
	})
	return nil
}

func (x *execConn) LocalAddr() net.Addr                { return x.hijack.Conn.LocalAddr() }
func (x *execConn) RemoteAddr() net.Addr               { return x.hijack.Conn.RemoteAddr() }
func (x *execConn) SetDeadline(t time.Time) error      { return x.hijack.Conn.SetDeadline(t) }
func (x *execConn) SetReadDeadline(t time.Time) error  { return x.hijack.Conn.SetReadDeadline(t) }
func (x *execConn) SetWriteDeadline(t time.Time) error { return x.hijack.Conn.SetWriteDeadline(t) }

func (e *Engine) DialSingle(ctx context.Context, name string, command ...string) (net.Conn, error) {
	c, err := e.api()
	if err != nil {
		return nil, err
	}
	created, err := c.ExecCreate(ctx, name, client.ExecCreateOptions{Cmd: command, AttachStdin: true, AttachStdout: true, AttachStderr: true})
	if err != nil {
		return nil, e.single("exec", err)
	}
	att, err := c.ExecAttach(ctx, created.ID, client.ExecAttachOptions{})
	if err != nil {
		return nil, e.single("exec", err)
	}
	return Demux(att.HijackedResponse), nil
}

func Demux(h client.HijackedResponse) net.Conn {
	pr, pw := io.Pipe()
	stderr := &tailBuffer{}
	go func() {
		_, err := stdcopy.StdCopy(pw, stderr, h.Reader)
		if err == nil && strings.TrimSpace(stderr.String()) != "" {
			err = errors.New(lastLine(stderr.String()))
		}
		pw.CloseWithError(err)
	}()
	return &execConn{hijack: h, out: pr}
}

func (e *Engine) RunOnce(ctx context.Context, s SingleSpec, stdin io.Reader, stdout, stderr io.Writer) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	s.Stdin = stdin != nil
	opts, err := createOptions(s)
	if err != nil {
		return &Error{Op: "run", Class: ClassInvalidConfig, Err: err}
	}
	opts.Config.AttachStdout, opts.Config.AttachStderr = true, true
	created, err := c.ContainerCreate(context.WithoutCancel(ctx), opts)
	if err != nil {
		return e.single("run", err)
	}
	defer func() {
		_, _ = c.ContainerRemove(context.WithoutCancel(ctx), created.ID, client.ContainerRemoveOptions{Force: true, RemoveVolumes: true})
	}()
	att, err := c.ContainerAttach(ctx, created.ID, client.ContainerAttachOptions{Stream: true, Stdin: stdin != nil, Stdout: true, Stderr: true})
	if err != nil {
		return e.single("run", err)
	}
	defer att.Close()
	waited := c.ContainerWait(context.WithoutCancel(ctx), created.ID, client.ContainerWaitOptions{Condition: container.WaitConditionNextExit})
	if _, err := c.ContainerStart(ctx, created.ID, client.ContainerStartOptions{}); err != nil {
		return e.single("run", err)
	}
	tail := &tailBuffer{}
	done := stream(att.HijackedResponse, stdin, orDiscard(stdout), io.MultiWriter(tail, orDiscard(stderr)))
	select {
	case <-ctx.Done():
		stop := 30
		_, _ = c.ContainerStop(context.WithoutCancel(ctx), created.ID, client.ContainerStopOptions{Timeout: &stop})
		<-done
		return ctx.Err()
	case err := <-done:
		if err != nil {
			return e.single("run", err)
		}
	}
	select {
	case res := <-waited.Result:
		if res.StatusCode != 0 {
			return &ExitError{Code: int(res.StatusCode), Stderr: tail.String()}
		}
		return nil
	case err := <-waited.Error:
		return e.single("run", err)
	}
}

func (e *Engine) ReadSingleFile(ctx context.Context, name, path string) ([]byte, error) {
	c, err := e.api()
	if err != nil {
		return nil, err
	}
	res, err := c.CopyFromContainer(ctx, name, client.CopyFromContainerOptions{SourcePath: path})
	if cerrdefs.IsNotFound(err) {
		return nil, fmt.Errorf("%w: %s", ErrNoFile, path)
	}
	if err != nil {
		return nil, e.single("copy", err)
	}
	defer res.Content.Close()
	tr := tar.NewReader(res.Content)
	for {
		hdr, err := tr.Next()
		if err != nil {
			return nil, fmt.Errorf("%w: %s", ErrNoFile, path)
		}
		if hdr.Typeflag == tar.TypeReg {
			return io.ReadAll(io.LimitReader(tr, 16<<20))
		}
	}
}

var ErrNoFile = errors.New("engine: no such file in the container")

func (e *Engine) CopyOut(ctx context.Context, name, path string, fn func(name string, size int64, r io.Reader) error) error {
	content, err := e.ReadSingleTar(ctx, name, path)
	if err != nil {
		return err
	}
	defer content.Close()
	tr := tar.NewReader(content)
	for {
		hdr, err := tr.Next()
		if errors.Is(err, io.EOF) {
			return nil
		}
		if err != nil {
			return e.single("copy", err)
		}
		if hdr.Typeflag != tar.TypeReg {
			continue
		}
		if err := fn(hdr.Name, hdr.Size, tr); err != nil {
			return err
		}
	}
}

func (e *Engine) ReadSingleTar(ctx context.Context, name, path string) (io.ReadCloser, error) {
	c, err := e.api()
	if err != nil {
		return nil, err
	}
	res, err := c.CopyFromContainer(ctx, name, client.CopyFromContainerOptions{SourcePath: path})
	if cerrdefs.IsNotFound(err) {
		return nil, fmt.Errorf("%w: %s", ErrNoFile, path)
	}
	if err != nil {
		return nil, e.single("copy", err)
	}
	return res.Content, nil
}

func (e *Engine) SingleFileSize(ctx context.Context, name, path string) (int64, error) {
	c, err := e.api()
	if err != nil {
		return 0, err
	}
	res, err := c.ContainerStatPath(ctx, name, client.ContainerStatPathOptions{Path: path})
	if cerrdefs.IsNotFound(err) {
		return 0, fmt.Errorf("%w: %s", ErrNoFile, path)
	}
	if err != nil {
		return 0, e.single("stat", err)
	}
	return res.Stat.Size, nil
}

func (e *Engine) WriteSingleTar(ctx context.Context, name, dir string, content io.Reader) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	_, err = c.CopyToContainer(ctx, name, client.CopyToContainerOptions{DestinationPath: dir, Content: content})
	return e.single("copy", err)
}

func (e *Engine) WriteSingleFiles(ctx context.Context, name, dir string, uid, gid int, files ...File) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	var b bytes.Buffer
	tw := tar.NewWriter(&b)
	now := time.Now()
	for _, f := range files {
		hdr := &tar.Header{Name: f.Name, Mode: f.Mode, Size: int64(len(f.Data)), Uid: uid, Gid: gid, ModTime: now, Typeflag: tar.TypeReg}
		if strings.HasSuffix(f.Name, "/") {
			hdr.Typeflag, hdr.Size = tar.TypeDir, 0
		}
		if err := tw.WriteHeader(hdr); err != nil {
			return err
		}
		if _, err := tw.Write(f.Data); err != nil {
			return err
		}
	}
	if err := tw.Close(); err != nil {
		return err
	}
	_, err = c.CopyToContainer(ctx, name, client.CopyToContainerOptions{DestinationPath: dir, Content: &b})
	return e.single("copy", err)
}

func (e *Engine) SingleLogs(ctx context.Context, name string, o LogsOptions, w io.Writer) error {
	c, err := e.api()
	if err != nil {
		return err
	}
	opts := client.ContainerLogsOptions{ShowStdout: true, ShowStderr: true, Follow: o.Follow, Since: o.Since}
	if o.Tail > 0 {
		opts.Tail = strconv.Itoa(o.Tail)
	}
	rc, err := c.ContainerLogs(ctx, name, opts)
	if err != nil {
		return e.single("logs", err)
	}
	defer rc.Close()
	_, err = stdcopy.StdCopy(w, w, rc)
	if ctx.Err() != nil {
		return nil
	}
	return err
}
