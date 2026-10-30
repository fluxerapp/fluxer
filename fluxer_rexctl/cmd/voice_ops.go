// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
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
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func voiceSet(inst *instance.Instance) *stack.Set {
	if inst != nil && inst.State != nil && !inst.State.IsVoice() {
		if set, err := inst.Set(); err == nil {
			if _, err := voice.ShippedFile(set, voice.ComposeFile); err == nil {
				return set
			}
		}
	}
	return stack.Embedded()
}

func writeVoiceServer(inst *instance.Instance, srv *instance.VoiceServer) error {
	dir := voice.ServerDir(inst.Dir, srv.ID)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return err
	}
	if err := envfile.RestrictToOwner(dir); err != nil {
		return err
	}
	set := voiceSet(inst)
	for _, name := range []string{voice.ComposeFile, voice.HTTPComposeFile} {
		data, err := voice.ShippedFile(set, name)
		if err != nil {
			return err
		}
		if err := os.WriteFile(filepath.Join(dir, name), data, 0o644); err != nil {
			return err
		}
	}
	return envfile.WriteFileAtomic(filepath.Join(dir, voice.EnvFile), deploy.VoiceEnv(inst, srv).File(), true)
}

func startVoiceServer(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer, s *ui.Step) error {
	if srv.Network == voice.MachineNetwork {
		if err := e.EnsureNetwork(ctx, voice.MachineNetwork, voice.EdgeProject, "voice", srv.NodeIPv6 != ""); err != nil {
			return engineFailure(err)
		}
	}
	p, err := e.Load(ctx, deploy.VoiceSpec(inst, srv))
	if err != nil {
		return engineFailure(err)
	}
	if err := p.Pull(ctx, app.progress(s, "Starting")); err != nil {
		return engineFailure(err)
	}
	if err := p.Up(context.WithoutCancel(ctx), engine.UpOptions{RemoveOrphans: true, Wait: true, WaitTimeout: deploy.VoiceStartTimeout}); err != nil {
		return ui.Wrap(engineFailure(err), fmt.Sprintf("The voice server %s didn't start.", srv.ID), fmt.Sprintf("Run rexctl voice status %s to see why.", srv.ID))
	}
	return nil
}

func removeVoiceServerProject(ctx context.Context, e *engine.Engine, srv *instance.VoiceServer) error {
	return engineFailure(e.Named(srv.Project, "").DownWithin(context.WithoutCancel(ctx), true, deploy.VoiceStopTimeout))
}

func voiceProject(e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer) *engine.Project {
	return e.Named(srv.Project, voice.ServerDir(inst.Dir, srv.ID))
}

func ownLiveKitImage() string {
	if images, err := stack.EmbeddedImages(); err == nil {
		if ref, err := images.Image("livekit"); err == nil && ref != "" {
			return ref
		}
	}
	return voice.DefaultImage
}

func instanceLiveKitImage(inst *instance.Instance) string {
	name := settings.ImageVars["livekit"]
	for _, ref := range []string{inst.State.Settings[settings.ImagesPrefix+"livekit"], inst.State.Settings[name], inst.State.Images.Refs["livekit"], inst.Setting(name)} {
		if ref = strings.TrimSpace(ref); ref != "" && !strings.Contains(ref, "$") {
			return ref
		}
	}
	return ownLiveKitImage()
}

func voicePortStrings(srv *instance.VoiceServer) []host.Port {
	return []host.Port{{Number: srv.TCPPort, Proto: "tcp"}, {Number: srv.UDPPort, Proto: "udp"}}
}

func portStrings(ports []host.Port) []string {
	out := make([]string, len(ports))
	for i, p := range ports {
		out[i] = p.String()
	}
	return out
}

func parsePorts(specs []string) []host.Port {
	var out []host.Port
	for _, s := range specs {
		num, proto, ok := strings.Cut(s, "/")
		n, err := strconv.Atoi(num)
		if ok && err == nil {
			out = append(out, host.Port{Number: n, Proto: proto})
		}
	}
	return out
}

func takenVoicePorts(ctx context.Context, e *engine.Engine, inst *instance.Instance) map[host.Port]bool {
	taken := map[host.Port]bool{}
	if e != nil {
		if owners, err := e.PublishedHostPorts(ctx); err == nil {
			for _, o := range owners {
				taken[host.Port{Number: o.Number, Proto: o.Proto}] = true
			}
		}
	}
	dirs := instance.ReadRecord(instance.OwnScope()).Instances
	if inst != nil && !slices.Contains(dirs, inst.Dir) {
		dirs = append(dirs, inst.Dir)
	}
	for _, dir := range dirs {
		other := inst
		if inst == nil || dir != inst.Dir {
			found, err := instance.Find(dir)
			if err != nil {
				continue
			}
			other = found
		}
		for _, srv := range other.State.Voice {
			for _, p := range srv.Ports() {
				taken[host.Port{Number: p.Number, Proto: p.Proto}] = true
			}
		}
		if deploy.VoiceBundled(other) {
			tcp, udp := 7881, 7882
			if n, err := strconv.Atoi(other.State.Settings[settings.VoiceTCPPort]); err == nil {
				tcp = n
			}
			if n, err := strconv.Atoi(other.State.Settings[settings.VoiceUDPPort]); err == nil {
				udp = n
			}
			taken[host.Port{Number: tcp, Proto: "tcp"}] = true
			taken[host.Port{Number: udp, Proto: "udp"}] = true
		}
	}
	return taken
}

func voiceIDsInUse(ctx context.Context, e *engine.Engine, inst *instance.Instance, projectPrefix string) []string {
	ids := []string{voice.EdgeID}
	dirs := instance.ReadRecord(instance.OwnScope()).Instances
	if inst != nil && !slices.Contains(dirs, inst.Dir) {
		dirs = append(dirs, inst.Dir)
	}
	for _, dir := range dirs {
		other := inst
		if inst == nil || dir != inst.Dir {
			found, err := instance.Find(dir)
			if err != nil {
				continue
			}
			other = found
		}
		for _, srv := range other.State.Voice {
			ids = append(ids, srv.ID)
		}
		if other.State.IsVoice() {
			continue
		}
		entries, _ := os.ReadDir(other.Path(instance.CaddyDropInDir))
		for _, ent := range entries {
			name, ok := strings.CutPrefix(ent.Name(), voice.DropInPrefix)
			if id, caddy := strings.CutSuffix(name, ".caddy"); ok && caddy {
				ids = append(ids, id)
			}
		}
	}
	if e != nil {
		if projects, err := e.Projects(ctx); err == nil {
			for _, p := range projects {
				if id, ok := strings.CutPrefix(p.Name, projectPrefix); ok && id != "" {
					ids = append(ids, id)
				}
			}
		}
	}
	return ids
}

func freeOnHost(number int, proto string) bool {
	used, _, err := host.PortInUse(proto, number)
	return err == nil && !used
}

func openVoiceFirewall(ctx context.Context, app *App, ports []host.Port, noFirewall bool) []string {
	if noFirewall {
		return nil
	}
	fw, err := host.DetectFirewall(ctx)
	if err != nil || fw == host.FirewallNone {
		return nil
	}
	closed, err := host.ClosedPorts(ctx, fw, ports)
	if err != nil || len(closed) == 0 {
		return nil
	}
	if err := host.OpenPorts(ctx, fw, closed); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't open %s in %s.", strings.Join(portStrings(closed), ", "), fw), "Open them yourself before people connect.")
		return nil
	}
	app.UI.OK("Opened %s in %s.", strings.Join(portStrings(closed), ", "), fw)
	return portStrings(closed)
}

func closeVoiceFirewall(ctx context.Context, app *App, opened []string) {
	if len(opened) == 0 {
		return
	}
	fw, err := host.DetectFirewall(ctx)
	if err != nil || fw == host.FirewallNone {
		return
	}
	if err := host.ClosePorts(ctx, fw, parsePorts(opened)); err != nil {
		app.UI.Detail("closing %s: %v", strings.Join(opened, ", "), err)
	}
}

func reloadCaddy(ctx context.Context, p *engine.Project, service, caddyfile string) error {
	return p.Exec(ctx, service, engine.ExecOptions{}, "caddy", "reload", "--config", caddyfile, "--adapter", "caddyfile")
}

func moveVoiceEndpoints(ctx context.Context, app *App, inst *instance.Instance) {
	if len(deploy.StaleVoiceEndpoints(inst)) == 0 {
		return
	}
	origin := voice.WSOrigin(inst.Origin())
	p, err := app.Named(ctx, inst)
	if err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't move the voice servers to %s (%v).", origin, err), "Run rexctl doctor --fix to try again.")
		return
	}
	moved, err := deploy.MoveVoiceEndpoints(ctx, app.adminClient(inst, p), inst)
	if len(moved) > 0 {
		app.UI.OK("Moved %s to %s.", ui.JoinAnd(moved), origin)
	}
	if err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't move every voice server to %s (%v).", origin, adminapi.Explain(err)), "Run rexctl doctor --fix to try again.")
	}
}

func followVoicePublicIP(inst *instance.Instance) []*instance.VoiceServer {
	ip := strings.TrimSpace(inst.State.Settings[settings.VoicePublicIP])
	var out []*instance.VoiceServer
	for i := range inst.State.Voice {
		srv := &inst.State.Voice[i]
		if srv.Joined || srv.IPPinned || srv.NodeIP == ip || ip == "" && srv.NodeIPv6 != "" {
			continue
		}
		srv.NodeIP = ip
		out = append(out, srv)
	}
	return out
}

var machineIPv4 = host.PublicIPv4

func followMachineIPv4(ctx context.Context, app *App, inst *instance.Instance) {
	if inst.State.IsVoice() || strings.TrimSpace(inst.State.Settings[settings.VoicePublicIP]) != "" {
		return
	}
	var due []*instance.VoiceServer
	for i := range inst.State.Voice {
		srv := &inst.State.Voice[i]
		if !srv.Joined && !srv.IPPinned && srv.BindIP == "" && srv.NodeIP != "" && srv.NodeIPv6 != "" {
			due = append(due, srv)
		}
	}
	if len(due) == 0 {
		return
	}
	pctx, cancel := context.WithTimeout(ctx, 10*time.Second)
	ip, err := machineIPv4(pctx)
	cancel()
	if err != nil || !voice.IsIPv4(ip) {
		return
	}
	var moved []*instance.VoiceServer
	for _, srv := range due {
		if srv.NodeIP != ip {
			srv.NodeIP = ip
			moved = append(moved, srv)
		}
	}
	if len(moved) == 0 {
		return
	}
	if err := saveVoiceState(inst); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't give the voice servers this machine's new IPv4 address %s (%v).", ip, err), "Run rexctl start again.")
		return
	}
	for _, srv := range moved {
		app.UI.Info("Voice server %s now gives clients this machine's IPv4 address %s.", srv.ID, ip)
	}
}

func moveVoicePublicIP(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, restart bool) {
	moved := followVoicePublicIP(inst)
	if len(moved) == 0 {
		return
	}
	if err := saveVoiceState(inst); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't give the voice servers the new public address (%v).", err), "Run rexctl config set voice.public_ip again.")
		return
	}
	var api *adminapi.Client
	if restart {
		api = app.adminClient(inst, e.Named(inst.Project(), inst.Dir))
	}
	for _, srv := range moved {
		var err error
		if restart {
			err = upgradeVoiceServer(ctx, app, e, api, inst, srv, srv.Image, defaultDrainTimeout)
		} else {
			err = writeVoiceServer(inst, srv)
		}
		if err != nil {
			app.UI.Warn(fmt.Sprintf("Voice server %s still gives clients its old address (%v).", srv.ID, err), "Run rexctl voice status "+srv.ID+" to see why.")
		}
	}
}

func voiceProbeTarget(inst *instance.Instance, srv *instance.VoiceServer) (string, string) {
	switch {
	case srv.Edge == voice.EdgeNone:
		return voice.LocalBase(srv.HTTPBind, srv.HTTPPort), ""
	case inst.State.IsVoice():
		return voice.HTTPBase(srv.Endpoint), "127.0.0.1:443"
	}
	return voice.InstanceEdge(srv.Endpoint, inst.State.Settings[settings.EdgeMode], inst.Setting("FLUXER_HTTPS_PORT"), inst.Setting("FLUXER_EDGE_BIND"))
}

func waitForEndpoint(ctx context.Context, base, edgeAddr string, insecure bool, limit time.Duration) error {
	deadline := time.Now().Add(limit)
	var last error
	for {
		if last = voice.ProbeThroughEdge(ctx, base, edgeAddr, insecure); last == nil {
			return nil
		}
		if time.Now().After(deadline) {
			return last
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(2 * time.Second):
		}
	}
}

type transportResult struct {
	Path voice.MediaPath
	Err  error
}

func runTransportTests(ctx context.Context, endpoint string, paths []voice.MediaPath, token func() (string, error), insecure bool) []transportResult {
	var out []transportResult
	for _, p := range paths {
		tok, err := token()
		if err == nil {
			err = voice.TestTransport(ctx, endpoint, tok, p, insecure, 20*time.Second)
		}
		out = append(out, transportResult{Path: p, Err: err})
	}
	return out
}

func serverPaths(srv *instance.VoiceServer) []voice.MediaPath {
	family := voice.FamilyOf(srv.NodeIP, srv.NodeIPv6)
	if family == voice.FamilyNone {
		family = voice.FamilyIPv4
	}
	return voice.MediaPaths(family, family)
}

func reportTransports(app *App, results []transportResult, local bool) bool {
	ok := true
	for _, r := range results {
		name := r.Path.Name()
		if r.Err == nil {
			app.UI.OK("%s connects.", name)
			continue
		}
		if errors.Is(r.Err, voice.ErrNoTurn) {
			app.UI.Info("rexctl didn't test %s. The server gives clients no TURN server over UDP or TCP.", name)
			continue
		}
		ok = false
		app.UI.Warn(fmt.Sprintf("%s doesn't connect (%v).", name, r.Err), "")
	}
	if local && !ok {
		app.UI.Info("Run rexctl doctor to check the ports, the firewall on this machine and the address the server gives clients.")
	}
	if local {
		app.UI.Info("This test ran on the machine that runs the server. It can't see a firewall at your provider or your router.")
	}
	return ok
}

func runQuietCommand(ctx context.Context, name string, args ...string) error {
	return exec.CommandContext(ctx, name, args...).Run()
}

func restoreBundledVoiceServer(ctx context.Context, app *App, inst *instance.Instance) {
	id := deploy.BundledVoiceServerID(inst)
	if id == "" {
		return
	}
	region := deploy.DefaultVoiceRegion(inst)
	fix := fmt.Sprintf("Add %s to the region %s in the admin panel, with the key and secret in LIVEKIT_API_KEY and LIVEKIT_API_SECRET.", id, region)
	p, err := app.Named(ctx, inst)
	if err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't add the bundled voice server %s (%v).", id, err), fix)
		return
	}
	api := app.adminClient(inst, p)
	regions, err := api.VoiceRegions(ctx)
	if err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't add the bundled voice server %s (%v).", id, adminapi.Explain(err)), fix)
		return
	}
	for _, r := range regions {
		for _, s := range r.Servers {
			if s.ServerId == id {
				return
			}
		}
	}
	key, secret := inst.Setting("LIVEKIT_API_KEY"), inst.Setting("LIVEKIT_API_SECRET")
	if key == "" || secret == "" {
		app.UI.Warn(fmt.Sprintf("Couldn't add the bundled voice server %s. LIVEKIT_API_KEY or LIVEKIT_API_SECRET is empty.", id), fix)
		return
	}
	body := adminapi.CreateVoiceServerRequestBody{ServerId: id, Endpoint: voice.WSOrigin(inst.Origin()) + "/livekit", ApiKey: key, ApiSecret: secret}
	if _, err := api.CreateVoiceServer(ctx, region, body); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't add the bundled voice server %s (%v).", id, adminapi.Explain(err)), fix)
		return
	}
	app.UI.OK("Added the bundled voice server %s to the region %s.", id, region)
}

func saveVoiceState(inst *instance.Instance) error {
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save the voice server state.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	return nil
}

func voiceLogLines(ctx context.Context, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer, tail int) []string {
	var buf bytes.Buffer
	if err := voiceProject(e, inst, srv).Logs(ctx, engine.LogsOptions{Tail: tail, Stdout: &buf}, "livekit"); err != nil {
		return nil
	}
	return voice.LogLines(&buf, func(string) bool { return true })
}

func voiceWebhookLog(ctx context.Context, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer) (string, bool) {
	failed, sent := "", false
	for _, line := range voiceLogLines(ctx, e, inst, srv, 2000) {
		lower := strings.ToLower(line)
		switch {
		case strings.Contains(lower, "failed to send webhook") || strings.Contains(lower, "could not send webhook"):
			failed = strings.TrimSpace(line)
			if len(failed) > 160 {
				failed = failed[:160]
			}
		case strings.Contains(lower, "sent webhook") || strings.Contains(lower, "webhook sent"):
			failed, sent = "", true
		}
	}
	return failed, sent
}

func voiceInstances(inst *instance.Instance) string {
	var out []string
	for _, s := range inst.State.Voice {
		if s.Instance != "" && !slices.Contains(out, s.Instance) {
			out = append(out, s.Instance)
		}
	}
	if len(out) == 0 {
		return "another instance"
	}
	return ui.JoinAnd(out)
}
