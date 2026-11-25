// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

type nativeVoice struct{ r *nativeRT }

func (v nativeVoice) Local(ctx context.Context) ([]instance.VoiceServer, error) {
	return localVoice(v.r.inst), nil
}

func (nativeVoice) Images() bool { return false }

func nativePortFree(number int, proto string) bool {
	used, _, err := host.PortInUse(proto, number)
	return err == nil && !used
}

func (v nativeVoice) Prepare(srv *instance.VoiceServer, pending []instance.VoiceServer) error {
	var taken []int
	for _, other := range append(localVoice(v.r.inst), pending...) {
		if other.HTTPPort > 0 {
			taken = append(taken, other.HTTPPort)
		}
	}
	port := voice.PickNativeHTTPPort(taken, nativePortFree)
	if port == 0 {
		return ui.Refused("No free local port is left for another voice server.", "Remove a voice server you no longer use, then try again.")
	}
	srv.HTTPPort, srv.HTTPBind = port, voice.NativeLoopback
	srv.Project, srv.Network = voice.NativeChild(srv.ID), ""
	srv.Webhook = "http://" + v.r.n.APIAddr() + "/webhooks/livekit"
	srv.Edge = voice.EdgeInstance
	srv.Image, srv.Pinned, srv.BindIP = "", false, ""
	return nil
}

func (nativeVoice) InUse(ctx context.Context) ([]string, []host.Port) { return nil, nil }

func (nativeVoice) IPv6Blocked(ctx context.Context, bindIP string) (string, string, error) {
	return "", "", nil
}

func (nativeVoice) Internal(srv *instance.VoiceServer) string {
	return "http://" + voice.NativeAddr(srv.HTTPPort)
}

func (v nativeVoice) envFile(srv *instance.VoiceServer) string {
	return filepath.Join(voice.ServerDir(v.r.inst.Dir, srv.ID), voice.NativeEnvFile)
}

func (v nativeVoice) dropIn(id string) string {
	return filepath.Join(v.r.inst.Path(instance.CaddyDropInDir), voice.DropInName(id))
}

func (v nativeVoice) Stale(srv *instance.VoiceServer, image string) bool {
	have, err := os.ReadFile(v.envFile(srv))
	return err != nil || !bytes.Equal(have, deploy.VoiceEnv(v.r.inst, srv).NativeFile())
}

func serviceGroup() (int, bool) {
	u, err := user.Lookup(native.ServiceUser)
	if err != nil {
		return 0, false
	}
	gid, err := strconv.Atoi(u.Gid)
	return gid, err == nil
}

func (v nativeVoice) Write(srv *instance.VoiceServer) error {
	if err := v.writeFiles(srv); err != nil {
		var ee *ui.ExitError
		if errors.As(err, &ee) {
			return err
		}
		return ui.Wrap(err, "Couldn't write the voice server's files.", "Check that you can write to "+v.r.inst.Dir+".")
	}
	return nil
}

func (v nativeVoice) writeFiles(srv *instance.VoiceServer) error {
	dir := voice.ServerDir(v.r.inst.Dir, srv.ID)
	if err := os.MkdirAll(dir, 0o750); err != nil {
		return err
	}
	path := v.envFile(srv)
	if err := envfile.WriteFileAtomic(path, deploy.VoiceEnv(v.r.inst, srv).NativeFile(), true); err != nil {
		return err
	}
	if os.Geteuid() != 0 || v.r.ctr != nil {
		return nil
	}
	gid, ok := serviceGroup()
	if !ok {
		return ui.Failed("The account "+native.ServiceUser+" doesn't exist on this machine.", "Run rexctl doctor to see what else is missing.")
	}
	for _, p := range []string{filepath.Dir(dir), dir, path} {
		if err := os.Chown(p, 0, gid); err != nil {
			return err
		}
	}
	if err := os.Chmod(filepath.Dir(dir), 0o750); err != nil {
		return err
	}
	if err := os.Chmod(dir, 0o750); err != nil {
		return err
	}
	return os.Chmod(path, 0o640)
}

func ReloadEdge(ctx context.Context, inst *instance.Instance) error {
	return reloadNativeUnit(ctx, native.New(inst).Unit())
}

func reloadNativeUnit(ctx context.Context, unit string) error {
	out, err := exec.CommandContext(ctx, "systemctl", "try-reload-or-restart", unit).CombinedOutput()
	if err != nil && len(bytes.TrimSpace(out)) > 0 {
		return errors.New(string(bytes.TrimSpace(out)))
	}
	return err
}

const (
	voiceChildAppear = 10 * time.Second
	voiceChildPoll   = time.Second
)

func (v nativeVoice) reload(ctx context.Context) error {
	if v.r.ctr != nil {
		if _, err := v.r.n.Reload(context.WithoutCancel(ctx)); err != nil {
			return ui.Wrap(err, "Couldn't reload the voice container with the voice servers.", "Run rexctl restart, then rexctl voice status.")
		}
		return nil
	}
	if err := reloadNativeUnit(context.WithoutCancel(ctx), v.r.n.Unit()); err != nil {
		return ui.Wrap(err, "Couldn't reload Fluxer with the voice servers.", "Run rexctl restart, then rexctl voice status.")
	}
	return nil
}

func (v nativeVoice) entry(ctx context.Context, name string) (native.PsEntry, bool) {
	entries, err := v.r.n.Entries(ctx)
	if err != nil {
		return native.PsEntry{}, false
	}
	for _, e := range entries {
		if e.Name == name {
			return e, true
		}
	}
	return native.PsEntry{}, false
}

func (v nativeVoice) appeared(ctx context.Context, name string, after time.Time, within time.Duration) bool {
	deadline := time.Now().Add(within)
	for {
		if e, ok := v.entry(ctx, name); ok && (after.IsZero() || e.StartedAt.After(after)) {
			return true
		}
		if time.Now().After(deadline) {
			return false
		}
		select {
		case <-ctx.Done():
			return false
		case <-time.After(voiceChildPoll):
		}
	}
}

func (v nativeVoice) healthy(ctx context.Context, s *ui.Step, srv *instance.VoiceServer) error {
	ctx, cancel := context.WithTimeout(ctx, deploy.VoiceStartTimeout)
	defer cancel()
	for {
		e, ok := v.entry(ctx, srv.Project)
		if ok {
			c := native.Container(e)
			if c.State == "running" && (c.Health == "" || c.Health == "healthy") {
				return nil
			}
			if s != nil {
				s.Title(fmt.Sprintf("Waiting for voice server %s, %s", srv.ID, strings.TrimSuffix(c.State+", "+c.Health, ", ")))
			}
		}
		select {
		case <-ctx.Done():
			if errors.Is(context.Cause(ctx), context.Canceled) {
				return ctx.Err()
			}
			return voiceStartFailed(srv, fmt.Errorf("it wasn't healthy after %s", deploy.VoiceStartTimeout))
		case <-time.After(voiceChildPoll):
		}
	}
}

func (v nativeVoice) restartChild(ctx context.Context, s *ui.Step, srv *instance.VoiceServer) error {
	if err := v.r.n.Service(context.WithoutCancel(ctx), srv.Project, native.ActionRestart); err != nil {
		return voiceStartFailed(srv, err)
	}
	return v.healthy(ctx, s, srv)
}

func (v nativeVoice) startChild(ctx context.Context, s *ui.Step, srv *instance.VoiceServer) error {
	if err := v.reload(ctx); err != nil {
		return err
	}
	if !v.appeared(ctx, srv.Project, time.Time{}, voiceChildAppear) {
		return ui.Failed(fmt.Sprintf("Fluxer didn't pick up the voice server %s.", srv.ID), "Run rexctl restart when no calls are running, then rexctl voice status "+srv.ID+". A restart ends every call on this machine.")
	}
	return v.healthy(ctx, s, srv)
}

func (v nativeVoice) CanChange() error { return v.r.root() }

func (v nativeVoice) Start(ctx context.Context, s *ui.Step, srv *instance.VoiceServer) error {
	if err := v.r.root(); err != nil {
		return err
	}
	before, running := v.entry(ctx, srv.Project)
	old, _ := os.ReadFile(v.envFile(srv))
	if err := v.Write(srv); err != nil {
		return err
	}
	if !running {
		return v.startChild(ctx, s, srv)
	}
	if now, err := os.ReadFile(v.envFile(srv)); err == nil && bytes.Equal(old, now) {
		return v.restartChild(ctx, s, srv)
	}
	if err := v.reload(ctx); err != nil {
		return err
	}
	if !v.appeared(ctx, srv.Project, before.StartedAt, voiceChildAppear) {
		return ui.Failed(fmt.Sprintf("Fluxer didn't pick up the change to %s.", srv.ID), "Run rexctl restart when no calls are running, then rexctl voice status "+srv.ID+". A restart ends every call on this machine.")
	}
	return v.healthy(ctx, s, srv)
}

func (v nativeVoice) Add(ctx context.Context, s *ui.Step, srv instance.VoiceServer) error {
	if err := v.r.root(); err != nil {
		return err
	}
	saved, err := voiceServer(v.r.inst, srv.ID)
	if err != nil {
		return err
	}
	if err := v.Write(saved); err != nil {
		return err
	}
	if err := envfile.WriteFileAtomic(v.dropIn(saved.ID), voice.NativeDropIn(saved.ID, saved.HTTPPort), false); err != nil {
		return ui.Wrap(err, "Couldn't write the edge route.", "Check that you can write to "+v.r.inst.Dir+".")
	}
	return v.startChild(ctx, s, saved)
}

func (v nativeVoice) Remove(ctx context.Context, s *ui.Step, id string) error {
	if err := v.r.root(); err != nil {
		return err
	}
	srv, err := voiceServer(v.r.inst, id)
	if err != nil {
		return err
	}
	if _, running := v.entry(ctx, srv.Project); running {
		if err := v.r.n.Service(context.WithoutCancel(ctx), srv.Project, native.ActionStop); err != nil {
			return NativeError(err, "Couldn't stop the voice server "+id+".")
		}
	}
	if err := os.Remove(v.dropIn(id)); err != nil && !errors.Is(err, os.ErrNotExist) {
		return ui.Wrap(err, "Couldn't remove the edge route of "+id+".", "Check that you can write to "+v.r.inst.Dir+", then run the same command again.")
	}
	if err := os.RemoveAll(voice.ServerDir(v.r.inst.Dir, id)); err != nil {
		return ui.Wrap(err, "Couldn't delete the files of "+id+".", "Check that you can write to "+v.r.inst.Dir+", then run the same command again.")
	}
	if err := v.reload(ctx); err != nil {
		s.Warn(fmt.Sprintf("The edge keeps the route of %s until it reloads (%v).", id, err), "Run rexctl restart to drop it.")
	}
	return nil
}

func (nativeVoice) StartAll(ctx context.Context, s *ui.Step) error { return nil }

func (nativeVoice) StopAll(ctx context.Context, s *ui.Step) error { return nil }

func (v nativeVoice) Restart(ctx context.Context, srv *instance.VoiceServer) error {
	if err := v.r.root(); err != nil {
		return err
	}
	if _, running := v.entry(ctx, srv.Project); !running {
		return v.Start(ctx, nil, srv)
	}
	return v.restartChild(ctx, nil, srv)
}

func (v nativeVoice) State(ctx context.Context, srv *instance.VoiceServer) string {
	entries, err := v.r.n.Entries(ctx)
	if err != nil {
		return "unknown"
	}
	for _, e := range entries {
		if e.Name != srv.Project {
			continue
		}
		if e.Health != "" {
			return e.State + ", " + e.Health
		}
		return e.State
	}
	return "not running"
}

func (v nativeVoice) Logs(ctx context.Context, w io.Writer, srv *instance.VoiceServer, o LogOptions) error {
	opts := engine.LogsOptions{Follow: o.Follow, Since: o.Since, Tail: o.Tail, Stdout: w}
	if err := v.r.n.Logs(ctx, opts, srv.Project); err != nil && ctx.Err() == nil {
		return NativeError(err, "Couldn't read the logs of "+srv.ID+".")
	}
	return nil
}
