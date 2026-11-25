// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func voiceRuntime(ctx context.Context, app *App, inst *instance.Instance) (rt.VoiceServers, error) {
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return nil, err
	}
	return rt.VoiceOf(r), nil
}

func engineVoice(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) (rt.VoiceServers, error) {
	r, err := rt.OpenWith(ctx, inst, app.UI, func(ctx context.Context) (*engine.Engine, error) {
		switch {
		case e != nil:
			return e, nil
		case inst.State.IsVoice() && inst.State.Runtime == instance.RuntimeContainer:
			return rt.VoiceEngine(ctx, inst)
		}
		return app.Engine(ctx, inst)
	})
	if err != nil {
		return nil, err
	}
	return rt.VoiceOf(r), nil
}

func writeVoiceServer(inst *instance.Instance, srv *instance.VoiceServer) error {
	r, err := rt.Open(context.Background(), inst, nil)
	if err != nil {
		return err
	}
	return rt.VoiceOf(r).Write(srv)
}

func startVoiceServer(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer, s *ui.Step) error {
	vs, err := engineVoice(ctx, app, inst, e)
	if err != nil {
		return err
	}
	return vs.Start(ctx, s, srv)
}

func withFix(err error, fix string) error {
	var ee *ui.ExitError
	if errors.As(err, &ee) {
		out := *ee
		out.Fix = fix
		return &out
	}
	return ui.Wrap(err, err.Error(), fix)
}

func removeVoiceServerProject(ctx context.Context, e *engine.Engine, srv *instance.VoiceServer) error {
	return rt.EngineError("", e.Named(srv.Project, "").DownWithin(context.WithoutCancel(ctx), true, deploy.VoiceStopTimeout))
}

func instanceLiveKitImage(inst *instance.Instance) string {
	name := settings.ImageVars["livekit"]
	for _, ref := range []string{inst.State.Settings[settings.ImagesPrefix+"livekit"], inst.State.Settings[name], inst.State.Images.Refs["livekit"], inst.Setting(name)} {
		if ref = strings.TrimSpace(ref); ref != "" && !strings.Contains(ref, "$") {
			return ref
		}
	}
	return voice.DefaultImage
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
		if b := deploy.BundledVoiceServer(other); b != nil {
			taken[host.Port{Number: b.TCPPort, Proto: "tcp"}] = true
			taken[host.Port{Number: b.UDPPort, Proto: "udp"}] = true
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
		app.UI.Warn(fmt.Sprintf("Couldn't close %s in the firewall.", portWords(parsePorts(opened))), "Close it yourself, since no voice server uses it now.")
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
	api, _, err := app.Admin(ctx)
	if err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't move the voice servers to %s (%v).", origin, err), "Run rexctl doctor --fix to try again.")
		return
	}
	moved, err := deploy.MoveVoiceEndpoints(ctx, api, inst)
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

func moveVoicePublicIP(ctx context.Context, app *App, inst *instance.Instance, restart bool) {
	moved := followVoicePublicIP(inst)
	if len(moved) == 0 {
		return
	}
	if err := saveVoiceState(inst); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't give the voice servers the new public address (%v).", err), "Run rexctl config set voice.public_ip again.")
		return
	}
	vs, err := voiceRuntime(ctx, app, inst)
	if err != nil {
		app.UI.Warn(fmt.Sprintf("Voice servers still give clients their old address (%v).", err), "Run rexctl voice status to see why.")
		return
	}
	var api *adminapi.Client
	if restart {
		if api, _, err = app.Admin(ctx); err != nil {
			app.UI.Detail("connecting to the admin API: %v", err)
		}
	}
	for _, srv := range moved {
		var err error
		if restart {
			err = upgradeVoiceServer(ctx, app, vs, api, inst, srv, srv.Image, defaultDrainTimeout)
		} else {
			err = vs.Write(srv)
		}
		if err != nil {
			app.UI.Warn(fmt.Sprintf("Voice server %s still gives clients its old address (%v).", srv.ID, err), "Run rexctl voice status "+srv.ID+" to see why.")
		}
	}
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
	if err := registerBundledVoiceServer(ctx, app, inst, false); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't add the bundled voice server %s (%v).", id, err), fmt.Sprintf("Run rexctl voice resync %s.", id))
	}
}

func registerBundledVoiceServer(ctx context.Context, app *App, inst *instance.Instance, resync bool) error {
	id := deploy.BundledVoiceServerID(inst)
	region := deploy.DefaultVoiceRegion(inst)
	api, _, err := app.Admin(ctx)
	if err != nil {
		return err
	}
	srv := deploy.BundledVoiceServer(inst)
	if srv.Key == "" || srv.Secret == "" {
		return ui.Failed(fmt.Sprintf("The bundled voice server %s has no key yet.", id), "Run rexctl restart, then try again.")
	}
	regions, err := api.VoiceRegions(ctx)
	if err != nil {
		return adminapi.Explain(err)
	}
	for _, r := range regions {
		for _, s := range r.Servers {
			if s.ServerId != id {
				continue
			}
			if !resync {
				return nil
			}
			if _, err := api.UpdateVoiceServer(ctx, r.Id, id, adminapi.UpdateVoiceServerRequestBody{ApiKey: &srv.Key, ApiSecret: &srv.Secret, Endpoint: &srv.Endpoint}); err != nil {
				return adminapi.Explain(err)
			}
			app.UI.OK("Your instance has the key, secret and endpoint of the bundled voice server %s again.", id)
			return nil
		}
	}
	body := adminapi.CreateVoiceServerRequestBody{ServerId: id, Endpoint: srv.Endpoint, ApiKey: srv.Key, ApiSecret: srv.Secret}
	if _, err := api.CreateVoiceServer(ctx, region, body); err != nil {
		return adminapi.Explain(err)
	}
	app.UI.OK("Added the bundled voice server %s to the region %s.", id, region)
	return nil
}

func saveVoiceState(inst *instance.Instance) error {
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save the voice server state.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	return nil
}

func voiceLogLines(ctx context.Context, vs rt.VoiceServers, srv *instance.VoiceServer, tail int) []string {
	var buf bytes.Buffer
	if err := vs.Logs(ctx, &buf, srv, rt.LogOptions{Tail: tail}); err != nil {
		return nil
	}
	return voice.LogLines(&buf, func(string) bool { return true })
}

func voiceWebhookLog(lines []string) (string, bool) {
	failed, sent := "", false
	for _, line := range lines {
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
