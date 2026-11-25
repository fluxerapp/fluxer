// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func voiceMigrateTarget(to string) (string, error) {
	switch to {
	case runtimeSystemd:
		return instance.RuntimeNative, nil
	case runtimeDocker, runtimePodman:
		return instance.RuntimeContainer, nil
	}
	return "", ui.Usage("Pass --to as systemd, docker or podman.")
}

func checkVoiceMigrate(inst *instance.Instance, kind, to string) error {
	if inst.State.Runtime == kind && (kind != instance.RuntimeContainer || inst.State.Engine.Kind == to) {
		return ui.Refused(fmt.Sprintf("The voice servers on this machine already run on %s.", voiceRuntimeLabel(inst)), "")
	}
	if len(inst.State.Voice) == 0 {
		return ui.Refused("This machine runs no voice servers, so there's nothing to move.", "Run rexctl voice join to add one.")
	}
	if euid() != 0 && (kind == instance.RuntimeNative || inst.State.Runtime == instance.RuntimeNative) {
		return ui.Refused("Voice servers under systemd are managed as root.", "Run the same command again with sudo.")
	}
	if kind == instance.RuntimeNative && (runtime.GOOS != "linux" || !host.BootUnitSupported()) {
		return ui.Refused("A voice server under systemd runs only on Linux with systemd.", "Pass --to docker to move them into one container.")
	}
	address := ""
	for _, s := range inst.State.Voice {
		switch s.Edge {
		case voice.EdgeInstance:
			return ui.Refused(fmt.Sprintf("The voice server %s uses the edge of the Fluxer instance on this machine.", s.ID), "Move that instance first, or join from a machine of its own.")
		case voice.EdgeOwn:
			if address != "" && address != s.Hostname {
				return ui.Refused(fmt.Sprintf("The voice servers on this machine use both %s and %s, and the voice service serves one address.", address, s.Hostname), "Remove the servers on one of them with rexctl voice remove first.")
			}
			address = s.Hostname
		}
	}
	return nil
}

func movedVoiceState(inst *instance.Instance, kind string, e *engine.Engine) error {
	inst.State.Runtime = kind
	if kind == instance.RuntimeContainer && e != nil {
		inst.State.Engine = instance.EngineRef{Kind: string(e.Kind), Host: e.Host, ID: e.ID}
	}
	var taken []int
	for i := range inst.State.Voice {
		s := &inst.State.Voice[i]
		s.Project, s.Image, s.Pinned, s.Network, s.BindIP = voice.NativeChild(s.ID), "", false, "", ""
		if s.Edge == voice.EdgeNone {
			taken = append(taken, s.HTTPPort)
			continue
		}
		s.HTTPBind = ""
		s.HTTPPort = voice.PickNativeHTTPPort(taken, freeOnHost)
		if s.HTTPPort == 0 {
			return ui.Refused("No free local port is left for the voice servers.", "Free a port from 17900 to 18079, then run the same command again.")
		}
		taken = append(taken, s.HTTPPort)
	}
	return nil
}

func copyState(s *instance.State) (*instance.State, error) {
	data, err := json.Marshal(s)
	if err != nil {
		return nil, err
	}
	out := &instance.State{}
	return out, json.Unmarshal(data, out)
}

func stopOldVoice(ctx context.Context, old *instance.Instance, e *engine.Engine) error {
	if fluxerdVoice(old) {
		return removeVoiceHost(ctx, old, false)
	}
	for i := range old.State.Voice {
		if err := rt.EngineError("", e.Named(old.State.Voice[i].Project, "").DownWithin(context.WithoutCancel(ctx), false, deploy.VoiceStopTimeout)); err != nil {
			return err
		}
	}
	if edge := old.State.VoiceEdge; edge != nil && edge.Instance == "" && ownComposeEdge(old) {
		return rt.EngineError("", e.Named(voice.EdgeProject, old.Path(voice.EdgeDir)).Down(context.WithoutCancel(ctx), false))
	}
	return nil
}

func ownComposeEdge(inst *instance.Instance) bool {
	for _, s := range inst.State.Voice {
		if s.Edge == voice.EdgeOwn {
			return true
		}
	}
	return false
}

func startOldVoice(ctx context.Context, app *App, old *instance.Instance, e *engine.Engine) error {
	if fluxerdVoice(old) {
		return app.UI.Step(ctx, "Starting the voice service again", func(ctx context.Context, s *ui.Step) error {
			if err := ensureVoiceHost(ctx, app, old, nil, s); err != nil {
				return err
			}
			s.Done("Started the voice service on %s again.", voiceRuntimeLabel(old))
			return nil
		})
	}
	if ownComposeEdge(old) {
		if err := app.UI.Step(ctx, "Starting the old edge again", func(ctx context.Context, s *ui.Step) error {
			return ensureVoiceEdge(ctx, app, e, old, s)
		}); err != nil {
			return err
		}
	}
	for i := range old.State.Voice {
		srv := &old.State.Voice[i]
		p, err := e.Load(ctx, deploy.VoiceSpec(old, srv))
		if err != nil {
			return rt.EngineError("", err)
		}
		if err := p.Up(context.WithoutCancel(ctx), engine.UpOptions{RemoveOrphans: true, Wait: true, WaitTimeout: deploy.VoiceStartTimeout}); err != nil {
			return rt.EngineError("", err)
		}
	}
	return nil
}

func cleanOldVoice(ctx context.Context, app *App, old, inst *instance.Instance, e *engine.Engine) {
	if fluxerdVoice(old) {
		if old.State.Runtime == instance.RuntimeNative {
			removeNativeVoiceFiles(old)
		} else if err := removeVoiceHost(ctx, old, true); err != nil {
			app.UI.Detail("removing the old voice container: %v", err)
		}
		return
	}
	for i := range old.State.Voice {
		srv := &old.State.Voice[i]
		_ = e.Named(srv.Project, "").DownWithin(context.WithoutCancel(ctx), true, deploy.VoiceStopTimeout)
		dir := voice.ServerDir(inst.Dir, srv.ID)
		for _, name := range []string{voice.ComposeFile, voice.HTTPComposeFile, voice.EnvFile} {
			_ = os.Remove(filepath.Join(dir, name))
		}
	}
	if ownComposeEdge(old) {
		_ = e.Named(voice.EdgeProject, old.Path(voice.EdgeDir)).Down(context.WithoutCancel(ctx), true)
		_ = os.RemoveAll(old.Path(voice.EdgeDir))
	}
}

func runVoiceMigrate(ctx context.Context, app *App, inst *instance.Instance, to string, dryRun bool) error {
	kind, err := voiceMigrateTarget(to)
	if err != nil {
		return err
	}
	if err := checkVoiceMigrate(inst, kind, to); err != nil {
		return err
	}
	domain := inst.State.Voice[0].Instance
	build, err := instanceBuild(ctx, domain)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("rexctl can't connect to %s. Nothing changed.", domain), "Run the same command again once the instance is back up.")
	}
	old := &instance.Instance{Dir: inst.Dir}
	if old.State, err = copyState(inst.State); err != nil {
		return err
	}
	var oldEngine, newEngine *engine.Engine
	if oldEngine, err = voiceMachineEngine(ctx, app, old); err != nil {
		return err
	}
	next := &instance.Instance{Dir: inst.Dir}
	if next.State, err = copyState(inst.State); err != nil {
		return err
	}
	next.State.Runtime = kind
	if kind == instance.RuntimeContainer {
		next.State.Engine = instance.EngineRef{}
		if to == runtimePodman {
			next.State.Engine.Kind = string(engine.Podman)
		} else {
			next.State.Engine.Kind = string(engine.Docker)
		}
		if newEngine, err = rt.VoiceEngine(ctx, next); err != nil {
			return err
		}
		if err := podmanRefusal(newEngine); err != nil {
			return err
		}
	}
	if err := movedVoiceState(next, kind, newEngine); err != nil {
		return err
	}
	rel, err := voiceHostRelease(ctx, next, build)
	if err != nil {
		return err
	}
	ids := make([]string, len(inst.State.Voice))
	for i, s := range inst.State.Voice {
		ids[i] = s.ID
	}
	app.UI.Card(ui.Card{
		Title: "Move the voice servers on this machine",
		Rows: []ui.KV{
			{Key: "Servers", Value: ui.JoinAnd(ids)},
			{Key: "From", Value: voiceRuntimeLabel(old)},
			{Key: "To", Value: voiceRuntimeLabel(next)},
			{Key: "Release", Value: "Fluxer " + rel.Version},
		},
		Body: []string{
			"rexctl drains each server first, so new calls go to other servers while it moves. Calls still running on this machine end when it moves.",
			"The endpoints, keys and ports stay the same, so " + domain + " needs no change.",
		},
	})
	if dryRun {
		return nil
	}
	if err := confirmVoice(app, "Move them?"); err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error { return moveVoiceMachine(ctx, app, inst, old, next, oldEngine, newEngine, rel) })
}

func moveVoiceMachine(ctx context.Context, app *App, inst, old, next *instance.Instance, oldEngine, newEngine *engine.Engine, rel *release.Release) error {
	var drained []*instance.VoiceServer
	for i := range inst.State.Voice {
		srv := &inst.State.Voice[i]
		active, draining, err := voiceServerFlags(ctx, nil, inst, srv)
		if err == nil && active && !draining && startUpgradeDrain(ctx, app, nil, inst, srv) {
			drained = append(drained, srv)
		}
	}
	undrain := func(target *instance.Instance) {
		for _, d := range drained {
			if srv := target.State.VoiceServer(d.ID); srv != nil {
				srv.Drained = true
				endUpgradeDrain(ctx, app, nil, target, srv)
			}
		}
	}
	for _, srv := range drained {
		waitForDrain(ctx, app, inst, srv, defaultDrainTimeout)
	}
	certs := exportVoiceCerts(ctx, old, oldEngine)
	if err := app.UI.Step(ctx, "Stopping the voice servers on "+voiceRuntimeLabel(old), func(ctx context.Context, s *ui.Step) error {
		if err := stopOldVoice(ctx, old, oldEngine); err != nil {
			return err
		}
		s.Done("Stopped the voice servers on %s.", voiceRuntimeLabel(old))
		return nil
	}); err != nil {
		undrain(inst)
		return err
	}
	inst.State = next.State
	start := func(ctx context.Context, s *ui.Step) error {
		if err := saveVoiceState(inst); err != nil {
			return err
		}
		if err := ensureVoiceHostWith(ctx, app, inst, rel, certs, s); err != nil {
			return err
		}
		vs, err := engineVoice(ctx, app, inst, newEngine)
		if err != nil {
			return err
		}
		for i := range inst.State.Voice {
			if err := vs.Start(ctx, s, &inst.State.Voice[i]); err != nil {
				return err
			}
		}
		for i := range inst.State.Voice {
			srv := &inst.State.Voice[i]
			if srv.Edge == voice.EdgeNone {
				continue
			}
			base, dial := deploy.VoiceProbeTarget(inst, srv)
			if err := waitForEndpoint(ctx, base, dial, false, 3*time.Minute); err != nil {
				return ui.Wrap(err, fmt.Sprintf("%s doesn't respond on %s.", srv.Endpoint, voiceRuntimeLabel(inst)), "Run rexctl logs edge to see why.")
			}
		}
		s.Done("The voice servers run on %s.", voiceRuntimeLabel(inst))
		return nil
	}
	if err := app.UI.Step(ctx, "Starting the voice servers on "+voiceRuntimeLabel(next), start); err != nil {
		app.UI.Warn(fmt.Sprintf("The voice servers didn't start on %s, so rexctl goes back.", voiceRuntimeLabel(next)), "")
		goneBack := goBackVoice(ctx, app, inst, old, oldEngine)
		undrain(inst)
		if goneBack != nil {
			return ui.Wrap(errors.Join(err, goneBack), "The move failed and rexctl couldn't go back.", "Run rexctl voice status, then rexctl doctor.")
		}
		return withFix(err, "rexctl went back, so the voice servers run as before. Run rexctl doctor to see what blocked the move.")
	}
	cleanOldVoice(ctx, app, old, inst, oldEngine)
	undrain(inst)
	waitInstanceSees(ctx, app, inst, 2*time.Minute)
	app.UI.OK("Moved %s to %s.", plural(len(inst.State.Voice), "voice server", "voice servers"), voiceRuntimeLabel(inst))
	return nil
}

func goBackVoice(ctx context.Context, app *App, inst, old *instance.Instance, oldEngine *engine.Engine) error {
	if err := removeVoiceHost(ctx, inst, true); err != nil {
		app.UI.Detail("removing the new voice service: %v", err)
	}
	inst.State = old.State
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	vs, err := engineVoice(ctx, app, inst, oldEngine)
	if err != nil {
		return err
	}
	for i := range inst.State.Voice {
		if err := vs.Write(&inst.State.Voice[i]); err != nil {
			return err
		}
	}
	return startOldVoice(ctx, app, inst, oldEngine)
}
