// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

type composeVoice struct{ c *compose }

func (v composeVoice) Local(ctx context.Context) ([]instance.VoiceServer, error) {
	return localVoice(v.c.inst), nil
}

func (composeVoice) Images() bool { return true }

func (v composeVoice) Write(srv *instance.VoiceServer) error {
	return writeComposeVoice(v.c.inst, srv)
}

func (v composeVoice) Prepare(srv *instance.VoiceServer, pending []instance.VoiceServer) error {
	inst := v.c.inst
	srv.Project = voice.ProjectName(inst.Project(), srv.ID)
	srv.Network = inst.Project() + "_fluxer"
	srv.Webhook = "http://api:8080/webhooks/livekit"
	srv.Edge = voice.EdgeInstance
	return nil
}

func (v composeVoice) InUse(ctx context.Context) ([]string, []host.Port) {
	e, err := v.c.engine(ctx)
	if err != nil {
		return nil, nil
	}
	var ids []string
	prefix := voice.ProjectName(v.c.inst.Project(), "")
	if projects, err := e.Projects(ctx); err == nil {
		for _, p := range projects {
			if id, ok := strings.CutPrefix(p.Name, prefix); ok && id != "" {
				ids = append(ids, id)
			}
		}
	}
	var ports []host.Port
	if owners, err := e.PublishedHostPorts(ctx); err == nil {
		for _, o := range owners {
			ports = append(ports, host.Port{Number: o.Number, Proto: o.Proto})
		}
	}
	return ids, ports
}

func (v composeVoice) IPv6Blocked(ctx context.Context, bindIP string) (string, string, error) {
	e, err := v.c.engine(ctx)
	if err != nil {
		return "", "", err
	}
	on, _ := strconv.ParseBool(strings.TrimSpace(v.c.inst.State.Settings[settings.IPv6]))
	_, has6, err := e.NetworkIPv6(ctx, v.c.inst.Project()+"_fluxer")
	if err != nil {
		return "", "", v.c.engineError(err, "rexctl can't read Fluxer's network.")
	}
	switch {
	case !e.IPv6Voice():
		return deploy.IPv6EngineText(e), fmt.Sprintf("Upgrade Docker to %s or later.", engine.MinDockerIPv6), nil
	case !on || !has6:
		return "network.ipv6 is off", "Run rexctl config set network.ipv6 true first.", nil
	case bindIP != "":
		return "--ip binds the ports to one IPv4 address", "Leave out --ip.", nil
	}
	return "", "", nil
}

func (composeVoice) Internal(srv *instance.VoiceServer) string { return voice.InternalURL(srv.ID) }

func shippedComposeVoice(inst *instance.Instance) map[string][]byte {
	if inst.State.IsVoice() {
		return nil
	}
	set, err := inst.Set()
	if err != nil {
		return nil
	}
	out := map[string][]byte{}
	for _, name := range []string{voice.ComposeFile, voice.HTTPComposeFile} {
		data, err := voice.ShippedFile(set, name)
		if err != nil {
			return nil
		}
		out[name] = data
	}
	return out
}

func writeComposeVoice(inst *instance.Instance, srv *instance.VoiceServer) error {
	dir := voice.ServerDir(inst.Dir, srv.ID)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return err
	}
	if err := envfile.RestrictToOwner(dir); err != nil {
		return err
	}
	for name, data := range shippedComposeVoice(inst) {
		if err := os.WriteFile(filepath.Join(dir, name), data, 0o644); err != nil {
			return err
		}
	}
	return envfile.WriteFileAtomic(filepath.Join(dir, voice.EnvFile), deploy.VoiceEnv(inst, srv).File(), true)
}

func (v composeVoice) Stale(srv *instance.VoiceServer, image string) bool {
	inst := v.c.inst
	if srv.Image != image {
		return true
	}
	dir := voice.ServerDir(inst.Dir, srv.ID)
	for name, want := range shippedComposeVoice(inst) {
		have, err := os.ReadFile(filepath.Join(dir, name))
		if err != nil || !bytes.Equal(have, want) {
			return true
		}
	}
	have, err := os.ReadFile(filepath.Join(dir, voice.EnvFile))
	return err != nil || !bytes.Equal(have, deploy.VoiceEnv(inst, srv).File())
}

func (v composeVoice) Start(ctx context.Context, s *ui.Step, srv *instance.VoiceServer) error {
	inst := v.c.inst
	if err := writeComposeVoice(inst, srv); err != nil {
		return ui.Wrap(err, "Couldn't write the voice server's files.", "Check that you can write to "+inst.Dir+".")
	}
	e, err := v.c.engine(ctx)
	if err != nil {
		return err
	}
	if srv.Network == voice.MachineNetwork {
		if err := e.EnsureNetwork(ctx, voice.MachineNetwork, voice.EdgeProject, "voice", srv.NodeIPv6 != ""); err != nil {
			return v.c.engineError(err, "Couldn't create the voice network.")
		}
	}
	p, err := e.Load(ctx, deploy.VoiceSpec(inst, srv))
	if err != nil {
		return v.c.engineError(err, "Couldn't read the voice server's Compose files.")
	}
	if err := p.Pull(ctx, pullLog(s)); err != nil {
		return v.c.engineError(err, "Couldn't download LiveKit.")
	}
	if err := p.Up(context.WithoutCancel(ctx), engine.UpOptions{RemoveOrphans: true, Wait: true, WaitTimeout: deploy.VoiceStartTimeout}); err != nil {
		return voiceStartFailed(srv, err)
	}
	return nil
}

func pullLog(s *ui.Step) func(engine.Event) {
	return func(ev engine.Event) {
		if s != nil && ev.Total > 0 {
			s.Progress(ev.Current, ev.Total)
		}
	}
}

func (composeVoice) CanChange() error { return nil }

func (v composeVoice) Add(ctx context.Context, s *ui.Step, srv instance.VoiceServer) error {
	saved, err := voiceServer(v.c.inst, srv.ID)
	if err != nil {
		return err
	}
	if err := v.Start(ctx, s, saved); err != nil {
		return err
	}
	if saved.Edge != voice.EdgeInstance {
		return nil
	}
	dropIn := filepath.Join(v.c.inst.Path(instance.CaddyDropInDir), voice.DropInName(saved.ID))
	if err := os.WriteFile(dropIn, voice.DropIn(saved.ID), 0o644); err != nil {
		return ui.Wrap(err, "Couldn't write the edge route.", "Check that you can write to "+v.c.inst.Dir+".")
	}
	return v.reloadEdge(ctx)
}

func (v composeVoice) reloadEdge(ctx context.Context) error {
	p, err := v.c.project(ctx)
	if err != nil {
		return err
	}
	err = p.Exec(ctx, "edge", engine.ExecOptions{}, "caddy", "reload", "--config", "/etc/caddy/Caddyfile", "--adapter", "caddyfile")
	return v.c.engineError(err, "Couldn't reload the edge with the voice routes.")
}

func (v composeVoice) Remove(ctx context.Context, s *ui.Step, id string) error {
	inst := v.c.inst
	srv, err := voiceServer(inst, id)
	if err != nil {
		return err
	}
	e, err := v.c.engine(ctx)
	if err != nil {
		return err
	}
	if err := e.Named(srv.Project, "").DownWithin(context.WithoutCancel(ctx), true, deploy.VoiceStopTimeout); err != nil {
		return v.c.engineError(err, fmt.Sprintf("Couldn't remove the voice server %s.", id))
	}
	_ = os.RemoveAll(voice.ServerDir(inst.Dir, id))
	if srv.Edge != voice.EdgeInstance || inst.State.IsVoice() {
		return nil
	}
	if err := os.Remove(filepath.Join(inst.Path(instance.CaddyDropInDir), voice.DropInName(id))); err != nil && !errors.Is(err, os.ErrNotExist) {
		return ui.Wrap(err, "Couldn't remove the edge route of "+id+".", "Check that you can write to "+inst.Dir+", then run the same command again.")
	}
	if err := v.reloadEdge(ctx); err != nil {
		s.Warn(fmt.Sprintf("The edge keeps the route of %s until it reloads (%v).", id, err), "Run rexctl restart to drop it.")
	}
	return nil
}

func (v composeVoice) StartAll(ctx context.Context, s *ui.Step) error {
	attached := deploy.VoiceOnNetwork(v.c.inst)
	if len(attached) == 0 {
		return nil
	}
	e, err := v.c.engine(ctx)
	if err != nil {
		return err
	}
	var failed []string
	for _, a := range attached {
		srv := a.Server
		p, err := e.Load(ctx, deploy.VoiceSpec(a.Owner, &srv))
		if err == nil {
			err = p.Up(context.WithoutCancel(ctx), engine.UpOptions{Wait: true, WaitTimeout: deploy.VoiceStartTimeout})
		}
		if err != nil {
			failed = append(failed, srv.ID)
			if s != nil {
				s.Log(fmt.Sprintf("%s: %v", srv.ID, err))
			}
		}
	}
	return voiceAllFailed(failed, "start")
}

func voiceAllFailed(failed []string, verb string) error {
	if len(failed) == 0 {
		return nil
	}
	fix := "Run rexctl voice status to see why."
	if len(failed) == 1 {
		fix = fmt.Sprintf("Run rexctl voice status %s to see why.", failed[0])
	}
	return ui.Failed(fmt.Sprintf("Couldn't %s the voice server %s.", verb, ui.JoinAnd(failed)), fix)
}

func (v composeVoice) StopAll(ctx context.Context, s *ui.Step) error {
	if len(v.c.inst.State.Voice) == 0 {
		return nil
	}
	e, err := v.c.engine(ctx)
	if err != nil {
		return err
	}
	var failed []string
	for i := range v.c.inst.State.Voice {
		srv := &v.c.inst.State.Voice[i]
		if err := deploy.VoiceProject(e, v.c.inst, srv).StopWithin(context.WithoutCancel(ctx), deploy.VoiceStopTimeout); err != nil {
			failed = append(failed, srv.ID)
		}
	}
	return voiceAllFailed(failed, "stop")
}

func (v composeVoice) Restart(ctx context.Context, srv *instance.VoiceServer) error {
	e, err := v.c.engine(ctx)
	if err != nil {
		return err
	}
	if err := deploy.VoiceProject(e, v.c.inst, srv).Restart(context.WithoutCancel(ctx), "livekit"); err != nil {
		return voiceStartFailed(srv, err)
	}
	return nil
}

func (v composeVoice) State(ctx context.Context, srv *instance.VoiceServer) string {
	e, err := v.c.engine(ctx)
	if err != nil {
		return "unknown"
	}
	cs, err := deploy.VoiceProject(e, v.c.inst, srv).PS(ctx)
	if err != nil || len(cs) == 0 {
		return "not running"
	}
	state := cs[0].State
	if cs[0].Health != "" {
		state += ", " + cs[0].Health
	}
	return state
}

func (v composeVoice) Logs(ctx context.Context, w io.Writer, srv *instance.VoiceServer, o LogOptions) error {
	e, err := v.c.engine(ctx)
	if err != nil {
		return err
	}
	opts := engine.LogsOptions{Follow: o.Follow, Since: o.Since, Tail: o.Tail, Stdout: w}
	if err := deploy.VoiceProject(e, v.c.inst, srv).Logs(ctx, opts, "livekit"); err != nil && ctx.Err() == nil {
		return v.c.engineError(err, "Couldn't read the logs of "+srv.ID+".")
	}
	return nil
}
