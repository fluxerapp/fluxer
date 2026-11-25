// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func writeVoiceSysctl(ctx context.Context, app *App) {
	if runtime.GOOS != "linux" {
		return
	}
	if euid() != 0 {
		if voice.ReadSysctl("net.core.rmem_max") != "16777216" || voice.ReadSysctl("net.core.wmem_max") != "16777216" {
			showVoiceSysctlCommands(app)
		}
		return
	}
	if data, err := os.ReadFile(voiceSysctlFile); err != nil || string(data) != voiceSysctl {
		if err := os.WriteFile(voiceSysctlFile, []byte(voiceSysctl), 0o644); err != nil {
			app.UI.Detail("writing %s: %v", voiceSysctlFile, err)
			return
		}
	}
	if voice.ReadSysctl("net.core.rmem_max") == "16777216" && voice.ReadSysctl("net.core.wmem_max") == "16777216" {
		return
	}
	if err := runQuietCommand(ctx, "sysctl", "-p", voiceSysctlFile); err != nil {
		app.UI.Detail("loading %s: %v", voiceSysctlFile, err)
	}
}

func showVoiceSysctlCommands(app *App) {
	app.UI.Info("LiveKit needs larger UDP buffers for many calls, and only root can raise them. Run these commands:")
	app.UI.Info("  printf '%s' | sudo tee %s", strings.ReplaceAll(voiceSysctl, "\n", `\n`), voiceSysctlFile)
	app.UI.Info("  sudo sysctl -p %s", voiceSysctlFile)
}

func ensureVoiceEdge(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, s *ui.Step) error {
	dir := inst.Path(voice.EdgeDir)
	if err := os.MkdirAll(filepath.Join(dir, voice.SitesDir), 0o755); err != nil {
		return err
	}
	if err := writeVoiceSites(inst); err != nil {
		return err
	}
	p, err := e.Load(ctx, voiceEdgeSpec(inst))
	if err != nil {
		return rt.EngineError("", err)
	}
	if err := p.Pull(ctx, app.progress(s, "Starting")); err != nil {
		return rt.EngineError("", err)
	}
	if err := p.Up(context.WithoutCancel(ctx), engine.UpOptions{Wait: true, WaitTimeout: deploy.VoiceStartTimeout}); err != nil {
		return ui.Wrap(rt.EngineError("", err), "The voice edge didn't start.", "Run rexctl voice status to see why.")
	}
	return nil
}

func voiceEdgeSpec(inst *instance.Instance) engine.Spec {
	env := map[string]string{"FLUXER_VOICE_NETWORK": voice.MachineNetwork}
	if inst.State.VoiceEdge != nil && inst.State.VoiceEdge.Email != "" {
		env["FLUXER_VOICE_EDGE_EMAIL"] = inst.State.VoiceEdge.Email
	}
	if inst.State.VoiceEdge != nil && inst.State.VoiceEdge.IPv6 {
		env["FLUXER_VOICE_NETWORK_IPV6"] = "true"
	}
	for _, s := range inst.State.Voice {
		if u, err := url.Parse(s.Endpoint); err == nil && s.Edge == voice.EdgeOwn && voice.IsIPv4(u.Hostname()) {
			env["FLUXER_VOICE_EDGE_IP"] = u.Hostname()
		}
	}
	return engine.Spec{Name: voice.EdgeProject, Dir: inst.Path(voice.EdgeDir), Files: []string{voice.EdgeComposeFile}, Env: env}
}

func writeVoiceSites(inst *instance.Instance) error {
	dir := inst.Path(voice.EdgeDir, voice.SitesDir)
	byAddress := map[string][]string{}
	for _, s := range inst.State.Voice {
		if s.Edge != voice.EdgeOwn {
			continue
		}
		u, err := url.Parse(s.Endpoint)
		if err != nil {
			continue
		}
		byAddress[u.Hostname()] = append(byAddress[u.Hostname()], s.ID)
	}
	entries, _ := os.ReadDir(dir)
	for _, ent := range entries {
		keep := false
		for address := range byAddress {
			if ent.Name() == voice.SiteFileName(address) {
				keep = true
			}
		}
		if !keep {
			_ = os.Remove(filepath.Join(dir, ent.Name()))
		}
	}
	for address, ids := range byAddress {
		ip := strings.Trim(address, "0123456789.") == ""
		if err := os.WriteFile(filepath.Join(dir, voice.SiteFileName(address)), voice.SiteFile(address, ids, ip), 0o644); err != nil {
			return err
		}
	}
	return nil
}

func routeVoiceServer(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer) error {
	if fluxerdVoice(inst) && srv.Edge == voice.EdgeOwn {
		return nil
	}
	switch srv.Edge {
	case voice.EdgeOwn:
		if err := writeVoiceSites(inst); err != nil {
			return err
		}
		return reloadCaddy(ctx, e.Named(voice.EdgeProject, inst.Path(voice.EdgeDir)), "edge", "/etc/caddy/Caddyfile")
	case voice.EdgeInstance:
		target := inst
		if inst.State.IsVoice() && inst.State.VoiceEdge != nil && inst.State.VoiceEdge.Instance != "" {
			other, err := instance.Find(inst.State.VoiceEdge.Instance)
			if err != nil {
				return err
			}
			target = other
		}
		path := filepath.Join(target.Path(instance.CaddyDropInDir), voice.DropInName(srv.ID))
		if err := os.WriteFile(path, voice.DropIn(srv.ID), 0o644); err != nil {
			return err
		}
		return reloadCaddy(ctx, e.Named(target.Project(), target.Dir), "edge", "/etc/caddy/Caddyfile")
	}
	app.UI.Info("Point your proxy at %s for %s, with WebSocket upgrades and a read timeout of at least 120 seconds.", voice.HTTPAddress(srv.HTTPBind, srv.HTTPPort), srv.Endpoint)
	if !voice.HTTPBindPrivate(srv.HTTPBind) {
		app.UI.Warn(fmt.Sprintf("%s has no TLS of its own.", voice.HTTPAddress(srv.HTTPBind, srv.HTTPPort)), "Let only your proxy's network connect to it in your firewall.")
	}
	return nil
}

func unrouteVoiceServer(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer) {
	if fluxerdVoice(inst) {
		return
	}
	switch srv.Edge {
	case voice.EdgeOwn:
		if err := writeVoiceSites(inst); err == nil {
			_ = reloadCaddy(ctx, e.Named(voice.EdgeProject, inst.Path(voice.EdgeDir)), "edge", "/etc/caddy/Caddyfile")
		}
	case voice.EdgeInstance:
		target := inst
		if inst.State.IsVoice() && inst.State.VoiceEdge != nil && inst.State.VoiceEdge.Instance != "" {
			other, err := instance.Find(inst.State.VoiceEdge.Instance)
			if err != nil {
				app.UI.Detail("%v", err)
				return
			}
			target = other
		}
		_ = os.Remove(filepath.Join(target.Path(instance.CaddyDropInDir), voice.DropInName(srv.ID)))
		if err := reloadCaddy(ctx, e.Named(target.Project(), target.Dir), "edge", "/etc/caddy/Caddyfile"); err != nil {
			app.UI.Detail("reloading the edge: %v", err)
		}
	}
}
