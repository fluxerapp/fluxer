// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"sync"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

var startUpgradedVoiceServer = startVoiceServer

func upgradeVoiceServer(ctx context.Context, app *App, e *engine.Engine, api *adminapi.Client, inst *instance.Instance, srv *instance.VoiceServer, image string, drain time.Duration) error {
	active, draining, err := voiceServerFlags(ctx, api, inst, srv)
	if err != nil {
		app.UI.Detail("reading the state of %s: %v", srv.ID, err)
	}
	if err == nil && !draining {
		srv.Drained = false
	}
	drains := err == nil && draining && srv.Drained
	if err == nil && upgradeDrains(active, draining) {
		drains = startUpgradeDrain(ctx, app, api, inst, srv)
	}
	if drains {
		defer endUpgradeDrain(ctx, app, api, inst, srv)
	}
	title, done := "Upgrading voice server "+srv.ID, fmt.Sprintf("Upgraded voice server %s to %s.", srv.ID, image)
	if srv.Image == image {
		title, done = "Restarting voice server "+srv.ID, fmt.Sprintf("Restarted voice server %s.", srv.ID)
	}
	next := *srv
	next.Image = image
	if err := writeVoiceServer(inst, &next); err != nil {
		return err
	}
	grace := drain
	if grace <= 0 {
		grace = defaultDrainTimeout
	}
	err = app.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
		s.Info("LiveKit waits until the calls on %s end, for up to %s.", srv.ID, grace)
		if err := startUpgradedVoiceServer(ctx, app, e, inst, &next, s); err != nil {
			return err
		}
		s.Done("%s", done)
		return nil
	})
	if err != nil {
		return err
	}
	srv.Image = image
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	if srv.Edge != voice.EdgeNone {
		base, dial := voiceProbeTarget(inst, srv)
		_ = waitForEndpoint(ctx, base, dial, true, 2*time.Minute)
	}
	return nil
}

func startUpgradeDrain(ctx context.Context, app *App, api *adminapi.Client, inst *instance.Instance, srv *instance.VoiceServer) bool {
	srv.Drained = true
	if err := saveVoiceState(inst); err != nil {
		srv.Drained = false
		app.UI.Warn(fmt.Sprintf("Couldn't mark %s as draining (%v).", srv.ID, err), "")
		return false
	}
	if err := setVoiceDraining(ctx, api, inst, srv, true); err != nil {
		srv.Drained = false
		_ = saveVoiceState(inst)
		app.UI.Warn(fmt.Sprintf("Couldn't mark %s as draining (%v).", srv.ID, err), "")
		return false
	}
	return true
}

func endUpgradeDrain(ctx context.Context, app *App, api *adminapi.Client, inst *instance.Instance, srv *instance.VoiceServer) {
	if err := setVoiceDraining(context.WithoutCancel(ctx), api, inst, srv, false); err != nil {
		app.UI.Warn(fmt.Sprintf("%s is still draining (%v).", srv.ID, err), "Run rexctl upgrade again, or clear Draining for it in the admin panel.")
		return
	}
	srv.Drained = false
	if err := saveVoiceState(inst); err != nil {
		app.UI.Warn(fmt.Sprintf("Couldn't record that %s takes calls again (%v).", srv.ID, err), "")
	}
}

func voiceServerFlags(ctx context.Context, api *adminapi.Client, inst *instance.Instance, srv *instance.VoiceServer) (bool, bool, error) {
	if inst.State.IsVoice() {
		client := &voice.Client{HTTP: &http.Client{Timeout: 30 * time.Second}}
		node, err := client.GetNode(ctx, srv.NodeURL, srv.Key, srv.Secret)
		if err != nil {
			return false, false, err
		}
		return node.IsActive, node.Draining, nil
	}
	if api == nil {
		return false, false, errors.New("no admin API")
	}
	regions, err := api.VoiceRegions(ctx)
	if err != nil {
		return false, false, err
	}
	for _, r := range regions {
		for _, s := range r.Servers {
			if s.ServerId == srv.ID {
				return s.IsActive, s.Draining, nil
			}
		}
	}
	return false, false, fmt.Errorf("the instance has no voice server called %s", srv.ID)
}

func upgradeDrains(active, draining bool) bool {
	return active && !draining
}

func voiceTargetImage(srv *instance.VoiceServer, image string) string {
	if srv.Pinned {
		return srv.Image
	}
	return image
}

func voiceServerStale(inst *instance.Instance, srv *instance.VoiceServer, image string) bool {
	if srv.Image != image {
		return true
	}
	dir := voice.ServerDir(inst.Dir, srv.ID)
	set := voiceSet(inst)
	for _, name := range []string{voice.ComposeFile, voice.HTTPComposeFile} {
		want, err := voice.ShippedFile(set, name)
		if err != nil {
			return false
		}
		have, err := os.ReadFile(filepath.Join(dir, name))
		if err != nil || !bytes.Equal(have, want) {
			return true
		}
	}
	have, err := os.ReadFile(filepath.Join(dir, voice.EnvFile))
	return err != nil || !bytes.Equal(have, deploy.VoiceEnv(inst, srv).File())
}

func upgradeLocalVoiceServers(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) {
	if len(inst.State.Voice) > 0 {
		followMachineIPv4(ctx, app, inst)
		image := instanceLiveKitImage(inst)
		api := app.adminClient(inst, e.Named(inst.Project(), inst.Dir))
		for i := range inst.State.Voice {
			srv := &inst.State.Voice[i]
			target := voiceTargetImage(srv, image)
			if !voiceServerStale(inst, srv, target) {
				continue
			}
			if err := upgradeVoiceServer(ctx, app, e, api, inst, srv, target, defaultDrainTimeout); err != nil {
				app.UI.Warn(fmt.Sprintf("Couldn't upgrade voice server %s (%v).", srv.ID, err), "Run rexctl voice status "+srv.ID+" to see why.")
			}
		}
	}
	for _, other := range voiceMachinesUsing(inst) {
		err := app.Locked(ctx, other, func() error {
			refreshJoinedServers(ctx, app, other, true)
			for _, srv := range staleVoiceServers(other, ownLiveKitImage()) {
				if err := upgradeVoiceServer(ctx, app, e, nil, other, srv, voiceTargetImage(srv, ownLiveKitImage()), defaultDrainTimeout); err != nil {
					return err
				}
			}
			return nil
		})
		if err != nil {
			app.UI.Warn(fmt.Sprintf("Couldn't upgrade the voice servers in %s (%v).", other.Dir, err), "Run rexctl upgrade --dir "+other.Dir+".")
		}
	}
}

func staleVoiceServers(inst *instance.Instance, image string) []*instance.VoiceServer {
	var out []*instance.VoiceServer
	for i := range inst.State.Voice {
		srv := &inst.State.Voice[i]
		if voiceServerStale(inst, srv, voiceTargetImage(srv, image)) {
			out = append(out, srv)
		}
	}
	return out
}

func voiceMachinesUsing(inst *instance.Instance) []*instance.Instance {
	var out []*instance.Instance
	for _, dir := range instance.ReadRecord(instance.OwnScope()).Instances {
		other, err := instance.Find(dir)
		if err == nil && other.State.IsVoice() && other.State.VoiceEdge != nil && other.State.VoiceEdge.Instance == inst.Dir {
			out = append(out, other)
		}
	}
	return out
}

func instanceBuild(ctx context.Context, domain string) (string, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, "https://"+domain+"/.well-known/fluxer", nil)
	if err != nil {
		return "", err
	}
	resp, err := (&http.Client{Timeout: 20 * time.Second}).Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusNotModified {
		return "", fmt.Errorf("voice: %s answered %d", domain, resp.StatusCode)
	}
	return resp.Header.Get("X-Fluxer-Version"), nil
}

func ownAPIBuild() string {
	images, err := stack.EmbeddedImages()
	if err != nil {
		return ""
	}
	return images.Builds["api"]
}

func compareInstanceBuild(build, own string) int {
	if !release.Valid(build) || !release.Valid(own) {
		return 0
	}
	return release.Compare(build, own)
}

type voiceMachinePlan struct {
	domain  string
	image   string
	update  *release.Release
	older   bool
	pending []*instance.VoiceServer
}

func planVoiceMachineUpgrade(ctx context.Context, app *App, inst *instance.Instance, report bool) (*voiceMachinePlan, error) {
	p := &voiceMachinePlan{domain: inst.State.Voice[0].Instance, image: ownLiveKitImage()}
	build, err := instanceBuild(ctx, p.domain)
	if err != nil {
		return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't connect to %s. Nothing changed.", p.domain), "Run rexctl upgrade again once the instance answers.")
	}
	switch compareInstanceBuild(build, ownAPIBuild()) {
	case -1:
		p.older = true
		return p, nil
	case 1:
		rel, err := resolveRelease(ctx, "")
		if err != nil {
			return nil, err
		}
		if release.Compare(rel.Version, app.Version) > 0 {
			p.update = rel
			return p, nil
		}
	}
	refreshJoinedServers(ctx, app, inst, report)
	p.pending = staleVoiceServers(inst, p.image)
	return p, nil
}

func printVoiceMachinePlan(app *App, p *voiceMachinePlan) {
	switch {
	case p.update != nil:
		app.UI.Info("%s runs a newer release than this rexctl. rexctl would update itself to %s, then upgrade the voice servers once %s runs that release or a newer one.", p.domain, p.update.Version, p.domain)
	case p.older:
		app.UI.Info("%s runs an older release than this rexctl. Voice servers stay on their LiveKit release until the instance catches up.", p.domain)
	case len(p.pending) == 0:
		app.UI.OK("Every voice server on this machine is up to date.")
	default:
		for _, s := range p.pending {
			app.UI.Info("rexctl would drain %s and start it again with %s.", s.ID, voiceTargetImage(s, p.image))
		}
	}
}

func runVoiceMachineCheck(ctx context.Context, app *App, inst *instance.Instance) error {
	if len(inst.State.Voice) == 0 {
		if app.Flags.JSON {
			return app.UI.JSON(upgradeCheckJSON{Current: app.Version, Latest: app.Version})
		}
		app.UI.OK("This machine runs no voice servers.")
		return nil
	}
	p, err := planVoiceMachineUpgrade(ctx, app, inst, false)
	if err != nil {
		return err
	}
	if app.Flags.JSON {
		out := upgradeCheckJSON{Current: app.Version, Latest: app.Version, Available: len(p.pending) > 0}
		if p.update != nil {
			out.Latest, out.Available = p.update.Version, true
		}
		return app.UI.JSON(out)
	}
	printVoiceMachinePlan(app, p)
	return nil
}

func runVoiceMachineUpgrade(ctx context.Context, app *App, inst *instance.Instance, dryRun bool) error {
	if len(inst.State.Voice) == 0 {
		app.UI.OK("This machine runs no voice servers.")
		return nil
	}
	p, err := planVoiceMachineUpgrade(ctx, app, inst, !dryRun)
	if err != nil {
		return err
	}
	if dryRun || p.older {
		printVoiceMachinePlan(app, p)
		return nil
	}
	if p.update != nil {
		target := p.update.Version
		asset, err := p.update.AssetFor(runtime.GOOS, runtime.GOARCH)
		if err != nil {
			return ui.Failed(fmt.Sprintf("There's no rexctl %s for this machine.", target), "")
		}
		if err := app.UI.Step(ctx, "Updating rexctl", func(ctx context.Context, s *ui.Step) error {
			if err := selfUpdate(ctx, asset); err != nil {
				return err
			}
			s.Done("Updated rexctl to %s.", target)
			return nil
		}); err != nil {
			return ui.Wrap(err, "Couldn't update rexctl.", "Run rexctl upgrade again.")
		}
		if err := reexec(os.Args[1:]); err != nil {
			return ui.Wrap(err, "Updated rexctl but couldn't start it again.", "Run rexctl upgrade again.")
		}
		return nil
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		if inst.State.VoiceEdge != nil && inst.State.VoiceEdge.Instance == "" {
			if err := app.UI.Step(ctx, "Updating the voice edge", func(ctx context.Context, s *ui.Step) error {
				if err := ensureVoiceEdge(ctx, app, e, inst, s); err != nil {
					return err
				}
				if err := reloadCaddy(ctx, e.Named(voice.EdgeProject, inst.Path(voice.EdgeDir)), "edge", "/etc/caddy/Caddyfile"); err != nil {
					return engineFailure(err)
				}
				s.Done("Updated the voice edge.")
				return nil
			}); err != nil {
				return err
			}
		}
		for _, srv := range p.pending {
			if err := upgradeVoiceServer(ctx, app, e, nil, inst, srv, voiceTargetImage(srv, p.image), defaultDrainTimeout); err != nil {
				return err
			}
		}
		if len(p.pending) == 0 {
			app.UI.OK("Every voice server on this machine is up to date.")
		}
		return nil
	})
}

func refreshJoinedServers(ctx context.Context, app *App, inst *instance.Instance, report bool) {
	if len(inst.State.Voice) == 0 {
		return
	}
	client := &voice.Client{HTTP: &http.Client{Timeout: 30 * time.Second}}
	for i := range inst.State.Voice {
		srv := &inst.State.Voice[i]
		if srv.NodeURL == "" {
			continue
		}
		node, err := client.GetNode(ctx, srv.NodeURL, srv.Key, srv.Secret)
		if err != nil {
			app.UI.Detail("reading %s from %s: %v", srv.ID, srv.Instance, err)
			continue
		}
		if node.Turn != nil {
			srv.Turn = settings.TurnServersValue(*node.Turn)
		}
		if !report || !slices.Contains(node.Features, voice.FeatureServerAddresses) || stringOf(node.PublicIPv4) == srv.NodeIP && stringOf(node.PublicIPv6) == srv.NodeIPv6 {
			continue
		}
		v4, v6 := srv.NodeIP, srv.NodeIPv6
		if _, err := client.UpdateNode(ctx, srv.NodeURL, srv.Key, srv.Secret, voice.NodeUpdate{PublicIPv4: &v4, PublicIPv6: &v6}); err != nil {
			app.UI.Detail("reporting the addresses of %s to %s: %v", srv.ID, srv.Instance, err)
		}
	}
}

func runVoiceMachineUninstall(ctx context.Context, app *App, inst *instance.Instance) error {
	_, servers, _ := localVoiceServers(inst, nil)
	if len(servers) == 0 {
		e, err := app.Engine(ctx, inst)
		if err != nil {
			return err
		}
		return app.Locked(ctx, inst, func() error { return removeVoiceMachine(ctx, app, e, inst) })
	}
	return removeVoiceServers(ctx, app, inst, servers, defaultDrainTimeout, true)
}

func removeLocalVoiceServersForUninstall(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) {
	eachVoiceServer(app, inst, "removing", func(srv *instance.VoiceServer) error {
		return removeVoiceServerProject(ctx, e, srv)
	})
	for _, srv := range inst.State.Voice {
		closeVoiceFirewall(ctx, app, srv.Opened)
	}
	for _, other := range voiceMachinesUsing(inst) {
		for _, s := range other.State.Voice {
			app.UI.Warn(fmt.Sprintf("The voice server %s on this machine used this instance's edge and stops working.", s.ID), "Run rexctl voice remove "+s.ID+voiceDirFlag(other.Dir)+".")
		}
	}
}

func eachVoiceServer(app *App, inst *instance.Instance, verb string, fn func(srv *instance.VoiceServer) error) {
	errs := make([]error, len(inst.State.Voice))
	var wg sync.WaitGroup
	for i := range inst.State.Voice {
		wg.Go(func() { errs[i] = fn(&inst.State.Voice[i]) })
	}
	wg.Wait()
	for i, err := range errs {
		if err != nil {
			app.UI.Detail("%s voice server %s: %v", verb, inst.State.Voice[i].ID, err)
		}
	}
}

func voiceDirFlag(dir string) string {
	if l, err := instance.Resolve(""); err == nil && l.Dir == dir {
		return ""
	}
	return " --dir " + dir
}

func startLocalVoiceServers(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) {
	followMachineIPv4(ctx, app, inst)
	for _, v := range deploy.VoiceOnNetwork(inst) {
		srv := &v.Server
		p, err := e.Load(ctx, deploy.VoiceSpec(v.Owner, srv))
		if err == nil {
			err = p.Up(context.WithoutCancel(ctx), engine.UpOptions{Wait: true, WaitTimeout: deploy.VoiceStartTimeout})
		}
		if err != nil {
			app.UI.Warn(fmt.Sprintf("Voice server %s didn't start (%v).", srv.ID, engineFailure(err)), "Run rexctl voice status "+srv.ID+" to see why.")
		}
	}
}

func stopLocalVoiceServers(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) {
	eachVoiceServer(app, inst, "stopping", func(srv *instance.VoiceServer) error {
		return voiceProject(e, inst, srv).StopWithin(context.WithoutCancel(ctx), deploy.VoiceStopTimeout)
	})
}
