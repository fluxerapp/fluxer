// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	cerrdefs "github.com/containerd/errdefs"
	"github.com/docker/compose/v5/pkg/api"
	"github.com/moby/moby/api/pkg/stdcopy"
	"github.com/moby/moby/api/types/container"
	"github.com/moby/moby/api/types/mount"
	"github.com/moby/moby/client"
)

const killGrace = 10 * time.Second

type ExitError struct {
	Code   int
	Stderr string
}

func (e *ExitError) Error() string {
	if line := lastLine(e.Stderr); line != "" {
		return fmt.Sprintf("exit status %d: %s", e.Code, line)
	}
	return fmt.Sprintf("exit status %d", e.Code)
}

func lastLine(s string) string {
	lines := strings.Split(strings.TrimSpace(s), "\n")
	return strings.TrimSpace(lines[len(lines)-1])
}

const stderrTail = 64 << 10

type tailBuffer struct{ buf []byte }

func (t *tailBuffer) Write(p []byte) (int, error) {
	t.buf = append(t.buf, p...)
	if len(t.buf) > stderrTail {
		t.buf = t.buf[len(t.buf)-stderrTail:]
	}
	return len(p), nil
}

func (t *tailBuffer) String() string { return string(t.buf) }

func orDiscard(w io.Writer) io.Writer {
	if w == nil {
		return io.Discard
	}
	return w
}

func (p *Project) target(ctx context.Context, c client.APIClient, service string) (string, error) {
	f := projectFilter(p.Name).
		Add("label", api.ServiceLabel+"="+service).
		Add("label", api.OneoffLabel+"=False").
		Add("label", api.ContainerNumberLabel+"=1").
		Add("status", "running")
	res, err := c.ContainerList(ctx, client.ContainerListOptions{Filters: f})
	if err != nil {
		return "", &Error{Op: "exec", Class: ClassOf(err), Err: err}
	}
	want := p.Name + "-" + service + "-1"
	for _, s := range res.Items {
		for _, n := range s.Names {
			if strings.TrimPrefix(n, "/") == want {
				return s.ID, nil
			}
		}
	}
	return "", &Error{Op: "exec", Class: ClassOther, Err: fmt.Errorf("%s is not running", service)}
}

func (p *Project) Exec(ctx context.Context, service string, o ExecOptions, command ...string) error {
	c, err := p.Engine.api()
	if err != nil {
		return err
	}
	id, err := p.target(ctx, c, service)
	if err != nil {
		return err
	}
	cmd := command
	pidFile := ""
	if o.KillOnCancel {
		pidFile = "/tmp/rexctl-" + randomID() + ".pid"
		cmd = append([]string{"sh", "-c", `echo $$ > ` + pidFile + ` && exec "$@"`, "--"}, command...)
	}
	created, err := c.ExecCreate(ctx, id, client.ExecCreateOptions{
		User:         o.User,
		Env:          o.Env,
		Cmd:          cmd,
		AttachStdin:  o.Stdin != nil,
		AttachStdout: true,
		AttachStderr: true,
	})
	if err != nil {
		return &Error{Op: "exec", Class: ClassOf(err), Err: err}
	}
	att, err := c.ExecAttach(ctx, created.ID, client.ExecAttachOptions{})
	if err != nil {
		return &Error{Op: "exec", Class: ClassOf(err), Err: err}
	}
	defer att.Close()
	tail := &tailBuffer{}
	done := stream(att.HijackedResponse, o.Stdin, orDiscard(o.Stdout), io.MultiWriter(tail, orDiscard(o.Stderr)))
	select {
	case err = <-done:
	case <-ctx.Done():
		if pidFile != "" {
			killExec(context.WithoutCancel(ctx), c, id, pidFile)
		}
		att.Close()
		<-done
		return ctx.Err()
	}
	if err != nil {
		return &Error{Op: "exec", Class: ClassOf(err), Err: err}
	}
	insp, err := inspectExec(ctx, c, created.ID)
	if err != nil {
		return &Error{Op: "exec", Class: ClassOf(err), Err: err}
	}
	if pidFile != "" {
		cleanup(context.WithoutCancel(ctx), c, id, pidFile)
	}
	if insp.ExitCode != 0 {
		return &ExitError{Code: insp.ExitCode, Stderr: tail.String()}
	}
	return nil
}

func inspectExec(ctx context.Context, c client.APIClient, id string) (client.ExecInspectResult, error) {
	for i := 0; ; i++ {
		insp, err := c.ExecInspect(ctx, id, client.ExecInspectOptions{})
		if err != nil || !insp.Running || i >= 50 {
			return insp, err
		}
		select {
		case <-ctx.Done():
			return insp, ctx.Err()
		case <-time.After(100 * time.Millisecond):
		}
	}
}

func stream(h client.HijackedResponse, stdin io.Reader, stdout, stderr io.Writer) <-chan error {
	done := make(chan error, 1)
	if stdin != nil {
		go func() {
			_, _ = io.Copy(h.Conn, stdin)
			_ = h.CloseWrite()
		}()
	}
	go func() {
		_, err := stdcopy.StdCopy(stdout, stderr, h.Reader)
		done <- err
	}()
	return done
}

func randomID() string {
	b := make([]byte, 8)
	_, _ = rand.Read(b)
	return hex.EncodeToString(b)
}

func killExec(ctx context.Context, c client.APIClient, id, pidFile string) {
	script := fmt.Sprintf(`pid=$(cat %[1]s 2>/dev/null) || exit 0; kill -TERM "$pid" 2>/dev/null || { rm -f %[1]s; exit 0; }; i=0; while [ $i -lt %[2]d ]; do kill -0 "$pid" 2>/dev/null || { rm -f %[1]s; exit 0; }; sleep 1; i=$((i+1)); done; kill -KILL "$pid" 2>/dev/null; rm -f %[1]s`, pidFile, int(killGrace/time.Second))
	runQuiet(ctx, c, id, script)
}

func cleanup(ctx context.Context, c client.APIClient, id, pidFile string) {
	runQuiet(ctx, c, id, "rm -f "+pidFile)
}

func runQuiet(ctx context.Context, c client.APIClient, id, script string) {
	ctx, cancel := context.WithTimeout(ctx, killGrace+20*time.Second)
	defer cancel()
	created, err := c.ExecCreate(ctx, id, client.ExecCreateOptions{Cmd: []string{"sh", "-c", script}, AttachStdout: true, AttachStderr: true})
	if err != nil {
		detailf("engine: %v", err)
		return
	}
	att, err := c.ExecAttach(ctx, created.ID, client.ExecAttachOptions{})
	if err != nil {
		detailf("engine: %v", err)
		return
	}
	defer att.Close()
	_, _ = stdcopy.StdCopy(io.Discard, io.Discard, att.Reader)
}

func (p *Project) Helper(ctx context.Context, o HelperOptions) error {
	c, err := p.Engine.api()
	if err != nil {
		return err
	}
	image := o.Image
	if image == "" {
		image = p.Image("postgres")
	}
	if image == "" {
		return &Error{Op: "helper", Class: ClassInvalidConfig, Err: errors.New("no image for the helper container")}
	}
	mounts := make([]mount.Mount, 0, len(o.Mounts))
	for _, m := range o.Mounts {
		if m.Bind != "" {
			mounts = append(mounts, mount.Mount{Type: mount.TypeBind, Source: m.Bind, Target: m.Target, ReadOnly: m.ReadOnly})
			continue
		}
		mounts = append(mounts, mount.Mount{Type: mount.TypeVolume, Source: p.volumeName(ctx, c, m.Volume), Target: m.Target, ReadOnly: m.ReadOnly})
	}
	for attempt := 1; ; attempt++ {
		out := &deliveredWriter{}
		err := p.runHelper(ctx, c, image, mounts, o, out)
		if attempt == 1 && out.n.Load() == 0 && o.Stdin == nil && errors.Is(err, ErrHelperStalled) {
			detailf("engine: helper stalled before it sent any output, starting it again")
			continue
		}
		if attempt > 1 && errors.Is(err, ErrHelperStalled) {
			return &Error{Op: "helper", Class: ClassOther, Err: fmt.Errorf("%w, also after rexctl started a fresh one", ErrHelperStalled)}
		}
		return err
	}
}

type deliveredWriter struct {
	mu     sync.Mutex
	w      io.Writer
	n      atomic.Int64
	closed bool
}

func (d *deliveredWriter) Write(b []byte) (int, error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.closed {
		return 0, ErrHelperStalled
	}
	n, err := d.w.Write(b)
	d.n.Add(int64(n))
	return n, err
}

func (d *deliveredWriter) close() {
	d.mu.Lock()
	d.closed = true
	d.mu.Unlock()
}

func (p *Project) runHelper(ctx context.Context, c client.APIClient, image string, mounts []mount.Mount, o HelperOptions, out *deliveredWriter) error {
	if err := p.removeHelpers(ctx, c); err != nil {
		return err
	}
	created, err := c.ContainerCreate(context.WithoutCancel(ctx), client.ContainerCreateOptions{
		Config: &container.Config{
			Image:        image,
			Entrypoint:   append([]string{"sh", "-c", heartbeatScript, "rexctl"}, o.Entrypoint...),
			Cmd:          o.Command,
			AttachStdin:  o.Stdin != nil,
			AttachStdout: true,
			AttachStderr: true,
			OpenStdin:    o.Stdin != nil,
			StdinOnce:    o.Stdin != nil,
			Labels: map[string]string{
				api.ProjectLabel: p.Name,
				api.ServiceLabel: "rexctl-helper",
				api.OneoffLabel:  "True",
			},
		},
		HostConfig: &container.HostConfig{Mounts: mounts, NetworkMode: "none", LogConfig: container.LogConfig{Type: "none"}},
		Name:       p.Name + "-rexctl-helper-" + randomID(),
	})
	if err != nil {
		return &Error{Op: "helper", Class: ClassOf(err), Images: []string{image}, Err: err}
	}
	id := created.ID
	defer func() {
		rctx, cancel := context.WithTimeout(context.WithoutCancel(ctx), 30*time.Second)
		defer cancel()
		if _, err := c.ContainerRemove(rctx, id, client.ContainerRemoveOptions{Force: true, RemoveVolumes: true}); err != nil {
			detailf("engine: removing helper %s: %v", id, err)
		}
	}()
	if err := ctx.Err(); err != nil {
		return err
	}
	ctx, stop := context.WithCancelCause(ctx)
	defer stop(nil)
	att, err := c.ContainerAttach(ctx, id, client.ContainerAttachOptions{Stream: true, Stdin: o.Stdin != nil, Stdout: true, Stderr: true})
	if err != nil {
		return &Error{Op: "helper", Class: ClassOf(err), Err: err}
	}
	defer att.Close()
	if _, err := c.ContainerStart(ctx, id, client.ContainerStartOptions{}); err != nil {
		return &Error{Op: "helper", Class: ClassOf(err), Err: err}
	}
	tail := &tailBuffer{}
	errOut := &deliveredWriter{w: io.MultiWriter(tail, orDiscard(o.Stderr))}
	out.w = orDiscard(o.Stdout)
	now := time.Now()
	act := &activity{start: now, last: now}
	go act.watch(ctx, func(cause error) {
		out.close()
		errOut.close()
		stop(cause)
	})
	wait := c.ContainerWait(ctx, id, client.ContainerWaitOptions{Condition: container.WaitConditionNotRunning})
	stderr := &heartbeatFilter{w: errOut}
	defer func() { out.n.Add(errOut.n.Load()) }()
	done := stream(att.HijackedResponse, o.Stdin, act.writer(out), act.writer(stderr))
	exited := pollExit(ctx, c, id)
	var code int64
	select {
	case code = <-exited:
	case res := <-wait.Result:
		if res.Error != nil && res.Error.Message != "" {
			return &Error{Op: "helper", Class: ClassOther, Err: errors.New(res.Error.Message)}
		}
		code = res.StatusCode
	case err := <-wait.Error:
		if ctx.Err() != nil {
			return p.stopHelper(ctx, c, id)
		}
		return &Error{Op: "helper", Class: ClassOf(err), Err: err}
	case <-ctx.Done():
		return p.stopHelper(ctx, c, id)
	}
	drainStream(done, act, stderr)
	if code != 0 {
		return &ExitError{Code: int(code), Stderr: tail.String()}
	}
	return nil
}

func drainStream(done <-chan error, act *activity, stderr *heartbeatFilter) {
	t := time.NewTicker(time.Second)
	defer t.Stop()
	for {
		select {
		case <-done:
			stderr.flush()
			return
		case <-t.C:
			if act.stalled(5 * time.Second) {
				return
			}
		}
	}
}

func pollExit(ctx context.Context, c client.APIClient, id string) <-chan int64 {
	out := make(chan int64, 1)
	go func() {
		t := time.NewTicker(10 * time.Second)
		defer t.Stop()
		for {
			select {
			case <-ctx.Done():
				return
			case <-t.C:
			}
			in, err := c.ContainerInspect(ctx, id, client.ContainerInspectOptions{})
			if err != nil || in.Container.State == nil {
				continue
			}
			if st := in.Container.State; st.Status == container.StateExited || st.Status == container.StateDead {
				detailf("engine: helper %s exited but the engine never reported it, using its exit code", id)
				out <- int64(st.ExitCode)
				return
			}
		}
	}()
	return out
}

func (p *Project) removeHelpers(ctx context.Context, c client.APIClient) error {
	f := projectFilter(p.Name).Add("label", api.ServiceLabel+"=rexctl-helper")
	res, err := c.ContainerList(ctx, client.ContainerListOptions{All: true, Filters: f})
	if err != nil {
		return &Error{Op: "helper", Class: ClassOf(err), Err: err}
	}
	for _, ct := range res.Items {
		if _, err := c.ContainerRemove(ctx, ct.ID, client.ContainerRemoveOptions{Force: true, RemoveVolumes: true}); err != nil && !cerrdefs.IsNotFound(err) {
			return &Error{Op: "helper", Class: ClassOf(err), Err: err}
		}
	}
	return nil
}

func (p *Project) stopHelper(ctx context.Context, c client.APIClient, id string) error {
	kctx, cancel := context.WithTimeout(context.WithoutCancel(ctx), 30*time.Second)
	defer cancel()
	if _, err := c.ContainerKill(kctx, id, client.ContainerKillOptions{}); err != nil {
		detailf("engine: killing helper %s: %v", id, err)
	}
	if cause := context.Cause(ctx); errors.Is(cause, ErrHelperStalled) {
		return &Error{Op: "helper", Class: ClassOther, Err: cause}
	}
	return ctx.Err()
}

func (p *Project) volumeName(ctx context.Context, c client.APIClient, name string) string {
	if p.Model != nil {
		if v, ok := p.Model.Volumes[name]; ok && v.Name != "" {
			return v.Name
		}
		return name
	}
	res, err := c.VolumeList(ctx, client.VolumeListOptions{Filters: projectFilter(p.Name).Add("label", api.VolumeLabel+"="+name)})
	if err == nil && len(res.Items) == 1 {
		return res.Items[0].Name
	}
	return name
}
