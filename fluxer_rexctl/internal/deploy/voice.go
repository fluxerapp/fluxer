// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func VoiceBundled(inst *instance.Instance) bool {
	return inst.State != nil && !inst.State.IsVoice() && stack.VoiceBundled(inst.State.Settings[settings.VoiceBundled])
}

func DefaultVoiceRegion(inst *instance.Instance) string {
	var region struct {
		ID string `json:"id"`
	}
	raw := strings.TrimSpace(inst.Setting("FLUXER_LIVEKIT_DEFAULT_REGION"))
	if raw != "" && json.Unmarshal([]byte(raw), &region) == nil && strings.TrimSpace(region.ID) != "" {
		return strings.TrimSpace(region.ID)
	}
	return "default"
}

func BundledVoiceServerID(inst *instance.Instance) string {
	if !VoiceBundled(inst) {
		return ""
	}
	return DefaultVoiceRegion(inst) + "-server-1"
}

const (
	VoiceStartTimeout = 3 * time.Minute
	VoiceStopTimeout  = 30 * time.Second
)

func VoiceEnv(inst *instance.Instance, srv *instance.VoiceServer) voice.Env {
	env := voice.Env{
		ID:         srv.ID,
		Key:        srv.Key,
		Secret:     srv.Secret,
		WebhookURL: srv.Webhook,
		Network:    srv.Network,
		TCPPort:    srv.TCPPort,
		UDPPort:    srv.UDPPort,
		HTTPPort:   srv.HTTPPort,
		NodeIP:     srv.NodeIP,
		BindIP:     srv.BindIP,
		Image:      srv.Image,
		Memory:     srv.Memory,
	}
	if !inst.State.IsVoice() {
		env.StunPrimary = inst.Setting("FLUXER_LIVEKIT_STUN_PRIMARY")
		env.StunSecondary = inst.Setting("FLUXER_LIVEKIT_STUN_SECONDARY")
		env.LogLevel = inst.Setting("FLUXER_LIVEKIT_LOG_LEVEL")
	}
	return env
}

func VoiceSpec(inst *instance.Instance, srv *instance.VoiceServer) engine.Spec {
	files := []string{voice.ComposeFile}
	if srv.HTTPPort > 0 {
		files = append(files, voice.HTTPComposeFile)
	}
	return engine.Spec{
		Name:  srv.Project,
		Dir:   voice.ServerDir(inst.Dir, srv.ID),
		Files: files,
		Env:   VoiceEnv(inst, srv).Values(),
	}
}

func StaleVoiceEndpoints(inst *instance.Instance) []string {
	base := voice.WSOrigin(inst.Origin())
	var out []string
	for _, srv := range inst.State.Voice {
		if base != "" && srv.Edge == voice.EdgeInstance && srv.Endpoint != base+voice.Path(srv.ID) {
			out = append(out, srv.ID)
		}
	}
	return out
}

func MoveVoiceEndpoints(ctx context.Context, api *adminapi.Client, inst *instance.Instance) ([]string, error) {
	stale := StaleVoiceEndpoints(inst)
	base := voice.WSOrigin(inst.Origin())
	var moved []string
	var errs []error
	for i := range inst.State.Voice {
		srv := &inst.State.Voice[i]
		if !slices.Contains(stale, srv.ID) {
			continue
		}
		endpoint := base + voice.Path(srv.ID)
		if _, err := api.UpdateVoiceServer(ctx, srv.Region, srv.ID, adminapi.UpdateVoiceServerRequestBody{Endpoint: &endpoint}); err != nil {
			errs = append(errs, fmt.Errorf("%s: %w", srv.ID, err))
			continue
		}
		srv.Endpoint = endpoint
		moved = append(moved, srv.ID)
	}
	if len(moved) > 0 {
		if err := save(inst); err != nil {
			errs = append(errs, err)
		}
	}
	return moved, errors.Join(errs...)
}

type AttachedVoice struct {
	Owner  *instance.Instance
	Server instance.VoiceServer
}

var (
	downInstance = func(ctx context.Context, e *engine.Engine, inst *instance.Instance) error {
		return e.Named(inst.Project(), inst.Dir).Down(context.WithoutCancel(ctx), false)
	}
	stopVoice = func(ctx context.Context, e *engine.Engine, v AttachedVoice) error {
		return e.Named(v.Server.Project, "").DownWithin(ctx, false, VoiceStopTimeout)
	}
	startVoice = func(ctx context.Context, e *engine.Engine, v AttachedVoice) error {
		p, err := e.Load(ctx, VoiceSpec(v.Owner, &v.Server))
		if err != nil {
			return err
		}
		return p.Up(ctx, engine.UpOptions{RemoveOrphans: true, Wait: true, WaitTimeout: VoiceStartTimeout})
	}
)

func VoiceOnNetwork(inst *instance.Instance) []AttachedVoice {
	network := inst.Project() + "_fluxer"
	var out []AttachedVoice
	add := func(owner *instance.Instance) {
		for _, srv := range owner.State.Voice {
			if srv.Network == network {
				out = append(out, AttachedVoice{Owner: owner, Server: srv})
			}
		}
	}
	add(inst)
	for _, dir := range instance.ReadRecord(instance.OwnScope()).Instances {
		if dir == inst.Dir {
			continue
		}
		if other, err := instance.Find(dir); err == nil && other.State.IsVoice() {
			add(other)
		}
	}
	return out
}

func detachVoice(ctx context.Context, e *engine.Engine, inst *instance.Instance) ([]AttachedVoice, error) {
	servers := VoiceOnNetwork(inst)
	bg := context.WithoutCancel(ctx)
	for i, v := range servers {
		if err := stopVoice(bg, e, v); err != nil {
			attachVoice(ctx, e, servers[:i])
			return nil, fmt.Errorf("deploy: stopping voice server %s: %w", v.Server.ID, err)
		}
	}
	return servers, nil
}

func attachVoice(ctx context.Context, e *engine.Engine, servers []AttachedVoice) []string {
	bg := context.WithoutCancel(ctx)
	var failed []string
	for _, v := range servers {
		if err := startVoice(bg, e, v); err != nil {
			failed = append(failed, v.Server.ID)
		}
	}
	return failed
}
