// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"bytes"
	"context"
	"fmt"
	"net/http"
	"net/url"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func init() {
	Register(Check{ID: "voice-servers", Phases: Installed | Voice, Run: runVoiceServers})
	Register(Check{ID: "voice-endpoints", Phases: Installed, Run: runVoiceEndpoints, Fix: fixVoiceEndpoints})
	Register(Check{ID: "voice-ports", Phases: Installed | Voice, Run: runVoicePorts})
	Register(Check{ID: "voice-node-ip", Phases: Installed | Voice, Run: runVoiceNodeIP})
	Register(Check{ID: "voice-tls", Phases: Voice, Run: runVoiceTLS})
	Register(Check{ID: "voice-registration", Phases: Voice, Run: runVoiceRegistration})
	Register(Check{ID: "voice-webhook", Phases: Installed | Voice, Run: runVoiceWebhook})
	Register(Check{ID: "voice-keys", Phases: Installed, Run: runVoiceKeys})
	Register(Check{ID: "voice-health", Phases: Installed, Run: runVoiceHealth})
}

var voiceHTTP = &http.Client{Timeout: 15 * time.Second}

func (e *Env) voiceServers() []instance.VoiceServer {
	if e.Instance == nil || e.Instance.State == nil {
		return nil
	}
	return e.Instance.State.Voice
}

func (e *Env) voiceProbeTarget(s instance.VoiceServer) (string, string) {
	switch {
	case s.Edge == voice.EdgeNone:
		return voice.LocalBase(s.HTTPPort), ""
	case e.Instance.State.IsVoice():
		return voice.HTTPBase(s.Endpoint), "127.0.0.1:443"
	}
	return voice.InstanceEdge(s.Endpoint, e.setting(settings.EdgeMode), e.value("FLUXER_HTTPS_PORT"), e.value("FLUXER_EDGE_BIND"))
}

func (e *Env) voiceProject(s instance.VoiceServer) *engine.Project {
	return e.Engine.Named(s.Project, voice.ServerDir(e.Instance.Dir, s.ID))
}

func (e *Env) voiceLogs(ctx context.Context, s instance.VoiceServer, tail int) string {
	var buf bytes.Buffer
	if err := e.voiceProject(s).Logs(ctx, engine.LogsOptions{Tail: tail, Stdout: &buf}, "livekit"); err != nil {
		return ""
	}
	return buf.String()
}

func runVoiceServers(ctx context.Context, e *Env) Result {
	servers := e.voiceServers()
	if len(servers) == 0 || e.Engine == nil {
		return skip("")
	}
	var bad []string
	for _, s := range servers {
		cs, err := e.voiceProject(s).PS(ctx)
		healthy := err == nil && len(cs) > 0 && cs[0].State == "running" && (cs[0].Health == "" || cs[0].Health == "healthy")
		if !healthy {
			bad = append(bad, s.ID+" isn't running")
			continue
		}
		base, dial := e.voiceProbeTarget(s)
		if err := voice.ProbeThroughEdge(ctx, base, dial, true); err != nil {
			bad = append(bad, s.ID+" doesn't answer through the edge")
		}
	}
	if len(bad) > 0 {
		r := fail(strings.Join(bad, ", ")+".", "Run rexctl voice status to see why, and rexctl voice remove <id> for a server you no longer need.")
		r.Details = bad
		return r
	}
	if len(servers) == 1 {
		return pass("The voice server runs and answers.")
	}
	return pass(fmt.Sprintf("All %d voice servers run and answer.", len(servers)))
}

func runVoiceEndpoints(_ context.Context, e *Env) Result {
	if e.Instance == nil || e.Instance.State == nil || e.Instance.State.IsVoice() {
		return skip("")
	}
	if !slices.ContainsFunc(e.Instance.State.Voice, func(s instance.VoiceServer) bool { return s.Edge == voice.EdgeInstance }) {
		return skip("")
	}
	stale := deploy.StaleVoiceEndpoints(e.Instance)
	if len(stale) == 0 {
		return pass("Every voice server uses your instance's address.")
	}
	origin := voice.WSOrigin(e.Instance.Origin())
	var details []string
	for _, id := range stale {
		if s := e.Instance.State.VoiceServer(id); s != nil {
			details = append(details, fmt.Sprintf("%s uses %s", id, s.Endpoint))
		}
	}
	r := fail(fmt.Sprintf("%s still %s the old address of your instance.", ui.JoinAnd(stale), pick(len(stale), "uses", "use")), fmt.Sprintf("Run rexctl doctor --fix to move %s to %s.", pick(len(stale), "it", "them"), origin))
	r.Details = details
	r.Fixable = true
	return r
}

func fixVoiceEndpoints(ctx context.Context, e *Env) error {
	if e.Admin == nil {
		return ErrNoFix
	}
	_, err := deploy.MoveVoiceEndpoints(ctx, e.Admin, e.Instance)
	return err
}

func runVoicePorts(ctx context.Context, e *Env) Result {
	servers := e.voiceServers()
	if len(servers) == 0 || e.Engine == nil {
		return skip("")
	}
	owners, err := e.Engine.PublishedHostPorts(ctx)
	if err != nil {
		return skip("")
	}
	var problems []string
	var ports []host.Port
	for _, s := range servers {
		for _, p := range []host.Port{{Number: s.TCPPort, Proto: "tcp"}, {Number: s.UDPPort, Proto: "udp"}} {
			ports = append(ports, p)
			mine, other := false, ""
			for _, o := range owners {
				if o.Number == p.Number && o.Proto == p.Proto {
					if o.Project == s.Project {
						mine = true
					} else {
						other = o.Project
					}
				}
			}
			switch {
			case other != "":
				problems = append(problems, fmt.Sprintf("%s of %s is also published by %s", p, s.ID, other))
			case !mine:
				problems = append(problems, fmt.Sprintf("%s of %s isn't published", p, s.ID))
			}
		}
	}
	if fw, err := host.DetectFirewall(ctx); err == nil && fw != host.FirewallNone {
		if closed, err := host.ClosedPorts(ctx, fw, ports); err == nil {
			for _, p := range closed {
				problems = append(problems, fmt.Sprintf("%s is closed in %s", p, fw))
			}
		}
	}
	if len(problems) > 0 {
		r := fail(strings.Join(problems, ", ")+".", "Open the ports in the firewall on this machine and at your provider, and run rexctl voice status.")
		r.Details = problems
		return r
	}
	return pass("Every voice port is published. Check that your provider's firewall lets them in too.")
}

func runVoiceNodeIP(ctx context.Context, e *Env) Result {
	servers := e.voiceServers()
	if len(servers) == 0 || e.Engine == nil {
		return skip("")
	}
	public, perr := e.publicAddress(ctx)
	var problems []string
	for _, s := range servers {
		if s.NodeIP != "" {
			if perr == nil && s.NodeIP != public && s.BindIP == "" {
				problems = append(problems, fmt.Sprintf("%s gives clients %s and this machine's public address is %s", s.ID, s.NodeIP, public))
			}
			continue
		}
		if strings.Contains(strings.ToLower(e.voiceLogs(ctx, s, 300)), "could not resolve external ip") {
			problems = append(problems, s.ID+" can't find its public address")
		}
	}
	if len(problems) > 0 {
		fix := "Run rexctl config set voice.public_ip with the right address. A server added with --ip keeps its address. Remove it and add it again with the right --ip."
		if e.Instance.State.IsVoice() {
			fix = "Run rexctl voice remove <id>, then rexctl voice join with a new token and --ip set to the right address."
		}
		return warn(strings.Join(problems, ", ")+".", fix)
	}
	return pass("Voice servers give clients the right address.")
}

func runVoiceTLS(ctx context.Context, e *Env) Result {
	var problems []string
	checked := 0
	for _, s := range e.voiceServers() {
		if s.Edge != voice.EdgeOwn {
			continue
		}
		checked++
		exp, err := voice.CertificateExpiry(ctx, s.Endpoint, "127.0.0.1:443")
		switch {
		case err != nil:
			problems = append(problems, fmt.Sprintf("%s has no valid certificate (%v)", s.Endpoint, err))
		case time.Until(exp) < 48*time.Hour:
			problems = append(problems, fmt.Sprintf("the certificate of %s expires %s", s.Endpoint, exp.Local().Format("2 January 15:04")))
		}
	}
	if checked == 0 {
		return skip("")
	}
	if len(problems) > 0 {
		return fail(strings.Join(problems, ", ")+".", "Check that ports 80 and 443 are open to the internet. docker logs fluxer-voice-edge-edge-1 shows the ACME error.")
	}
	return pass("Voice certificates are valid for more than 2 days.")
}

func runVoiceRegistration(ctx context.Context, e *Env) Result {
	servers := e.voiceServers()
	if len(servers) == 0 {
		return skip("")
	}
	client := &voice.Client{HTTP: voiceHTTP}
	var problems []string
	fix := "Run rexctl voice status, and rexctl voice remove <id> for a server the instance no longer knows."
	for _, s := range servers {
		node, err := client.GetNode(ctx, s.NodeURL, s.Key, s.Secret)
		switch voice.CodeOf(err) {
		case "":
		case voice.CodeNodeUnknown:
			problems = append(problems, fmt.Sprintf("%s was deleted on %s", s.ID, s.Instance))
			fix = "Run rexctl voice remove " + s.ID + ", then join again with a new token from rexctl voice token."
			continue
		case voice.CodeTokenExpired:
			return fail(voice.ClockProblem(err, s.Instance, time.Now())+".", voice.ClockFix)
		case voice.CodeSignatureInvalid:
			problems = append(problems, fmt.Sprintf("%s has another secret on %s", s.ID, s.Instance))
			fix = "Put the server's secret back in the admin panel. It's FLUXER_VOICE_API_SECRET in " + filepath.Join(voice.ServerDir(e.Instance.Dir, s.ID), voice.EnvFile) + ". Or run rexctl voice remove " + s.ID + " and join again with a new token."
			continue
		}
		if err != nil {
			problems = append(problems, fmt.Sprintf("%s can't connect to %s (%v)", s.ID, s.Instance, err))
			continue
		}
		if node.Endpoint != s.Endpoint || node.RegionID != s.Region {
			problems = append(problems, fmt.Sprintf("%s is registered with %s in %s, and this machine serves %s in %s", s.ID, node.Endpoint, node.RegionID, s.Endpoint, s.Region))
		}
	}
	if len(problems) > 0 {
		return fail(strings.Join(problems, ", ")+".", fix)
	}
	return pass("Every voice server is registered with its instance.")
}

func runVoiceWebhook(ctx context.Context, e *Env) Result {
	servers := e.voiceServers()
	if len(servers) == 0 || e.Engine == nil {
		return skip("")
	}
	client := &voice.Client{HTTP: voiceHTTP}
	var problems []string
	rejected := false
	for _, s := range servers {
		logs := strings.ToLower(e.voiceLogs(ctx, s, 2000))
		if i := strings.LastIndex(logs, "failed to send webhook"); i >= 0 && i > strings.LastIndex(logs, "sent webhook") {
			problems = append(problems, fmt.Sprintf("%s can't send webhooks to %s", s.ID, s.Webhook))
			continue
		}
		problem, fault := webhookProblem(ctx, client, s, e.webhookTarget(s))
		switch fault {
		case webhookClock:
			return fail(problem+".", voice.ClockFix)
		case webhookSecret:
			rejected = true
		}
		if problem != "" {
			problems = append(problems, problem)
		}
	}
	if len(problems) > 0 {
		fix := "Check that the webhook URL answers, and that no firewall or bot protection in front of the instance blocks it."
		if rejected {
			fix = "Put the server's own secret back in the admin panel. It's FLUXER_VOICE_API_SECRET in voice/<id>/.env in " + e.Instance.Dir + "."
		}
		return fail(strings.Join(problems, ", ")+".", fix)
	}
	return pass("Your instance accepts webhooks from every voice server.")
}

func (e *Env) webhookTarget(s instance.VoiceServer) string {
	u, err := url.Parse(s.Webhook)
	if err != nil || u.Hostname() != "api" || e.Instance == nil {
		return s.Webhook
	}
	return strings.TrimRight(e.Instance.Origin(), "/") + "/api" + u.Path
}

type webhookFault int

const (
	webhookOK webhookFault = iota
	webhookDown
	webhookSecret
	webhookClock
)

func webhookProblem(ctx context.Context, client *voice.Client, s instance.VoiceServer, target string) (string, webhookFault) {
	now := time.Now()
	status, date, err := client.TestWebhook(ctx, target, s.Key, s.Secret, now)
	host := target
	if u, perr := url.Parse(target); perr == nil {
		host = u.Host
	}
	rejected := status == http.StatusBadRequest || status == http.StatusUnauthorized
	switch {
	case err != nil:
		return fmt.Sprintf("%s can't connect to %s (%v)", s.ID, target, err), webhookDown
	case rejected && voice.ClockOff(date, host, now) != "":
		return voice.ClockOff(date, host, now), webhookClock
	case rejected:
		return fmt.Sprintf("%s rejects webhooks signed with the secret of %s", target, s.ID), webhookSecret
	case status < 200 || status > 299:
		return fmt.Sprintf("%s answers webhooks from %s with HTTP %d", target, s.ID, status), webhookDown
	}
	return "", webhookOK
}

func runVoiceKeys(ctx context.Context, e *Env) Result {
	if e.Admin == nil || e.Instance == nil || e.Instance.State.IsVoice() {
		return skip("")
	}
	regions, err := e.Admin.VoiceRegions(ctx)
	if err != nil {
		return skip("")
	}
	var shared []string
	for _, r := range regions {
		for _, s := range r.Servers {
			if s.DuplicateApiKey {
				shared = append(shared, s.ServerId)
			}
		}
	}
	if len(shared) > 0 {
		slices.Sort(shared)
		return warn(fmt.Sprintf("Voice servers %s share an API key.", ui.JoinAnd(shared)),
			"Give each server its own key. Remove the newer server and add it again with rexctl voice add or rexctl voice join.")
	}
	return pass("Every voice server has its own API key.")
}

func runVoiceHealth(ctx context.Context, e *Env) Result {
	if e.Admin == nil || e.Instance == nil || e.Instance.State.IsVoice() {
		return skip("")
	}
	regions, err := e.Admin.VoiceRegions(ctx)
	if err != nil {
		return skip("")
	}
	var down, stuck []string
	for _, r := range regions {
		for _, s := range r.Servers {
			if !s.IsActive {
				continue
			}
			if s.Health.Status == "down" {
				down = append(down, s.ServerId+" at "+s.Endpoint)
			}
			if s.Draining && s.UpdatedAt != nil {
				if t, err := time.Parse(time.RFC3339, *s.UpdatedAt); err == nil && time.Since(t) > time.Hour {
					stuck = append(stuck, s.ServerId)
				}
			}
		}
	}
	switch {
	case len(down) > 0:
		return fail(fmt.Sprintf("%s %s down.", ui.JoinAnd(down), pick(len(down), "is", "are")), "Run rexctl voice status on the machine that runs each one.")
	case len(stuck) > 0:
		return warn(fmt.Sprintf("%s %s been draining for more than an hour.", ui.JoinAnd(stuck), pick(len(stuck), "has", "have")), "Clear Draining in the admin panel, or remove the servers.")
	}
	return pass("Every active voice server is up.")
}

func pick(n int, one, many string) string {
	if n == 1 {
		return one
	}
	return many
}
