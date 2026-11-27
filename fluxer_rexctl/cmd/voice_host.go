// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"net/url"
	"os"
	"runtime"

	"io"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

const voiceHostWait = 2 * time.Minute

func fluxerdVoice(inst *instance.Instance) bool {
	return inst.State.IsVoice() && (inst.State.Runtime == instance.RuntimeNative || inst.State.Runtime == instance.RuntimeContainer)
}

func voiceRuntimeLabel(inst *instance.Instance) string {
	switch inst.State.Runtime {
	case instance.RuntimeNative:
		return "Linux, systemd"
	case instance.RuntimeContainer:
		return engine.Kind(inst.State.Engine.Kind).Name() + ", one container"
	}
	return engine.Kind(inst.State.Engine.Kind).Name()
}

func detectHost(ctx context.Context) *host.Info {
	h, _ := host.Detect(ctx, "/")
	return h
}

func pickVoiceRuntime(ctx context.Context, flag string) (string, error) {
	switch flag {
	case runtimeSystemd:
		if runtime.GOOS != "linux" || !host.BootUnitSupported() {
			return "", ui.Refused("A voice server under systemd runs only on Linux with systemd.", "Leave out --runtime systemd to run it in a container.")
		}
		if euid() != 0 {
			return "", ui.Refused("A voice server under systemd is managed as root.", "Run the same command again with sudo.")
		}
		return instance.RuntimeNative, nil
	case runtimeDocker, runtimePodman:
		return instance.RuntimeContainer, nil
	case "":
		if euid() == 0 && host.BootUnitSupported() && nativeOS(detectHost(ctx)) {
			return instance.RuntimeNative, nil
		}
		return instance.RuntimeContainer, nil
	}
	return "", ui.Usage("Pass --runtime as systemd, docker or podman.")
}

func voiceHostEnv(inst *instance.Instance) map[string]string {
	env := map[string]string{"FLUXER_VOICE_ONLY": "true"}
	address := ""
	for _, s := range inst.State.Voice {
		if s.Edge != voice.EdgeOwn {
			continue
		}
		if u, err := url.Parse(s.Endpoint); err == nil && u.Hostname() != "" {
			address = u.Hostname()
		}
	}
	if address == "" {
		env["FLUXER_EDGE_MODE"] = "none"
	} else {
		env["FLUXER_DOMAIN"] = address
	}
	if e := inst.State.VoiceEdge; e != nil && e.Email != "" {
		env["FLUXER_EDGE_GLOBAL_OPTIONS"] = "email " + e.Email
	}
	return env
}

func voiceHostAddressClash(inst *instance.Instance, endpoint string) error {
	u, err := url.Parse(endpoint)
	if err != nil {
		return nil
	}
	for _, s := range inst.State.Voice {
		o, err := url.Parse(s.Endpoint)
		if err == nil && s.Edge == voice.EdgeOwn && o.Hostname() != u.Hostname() {
			return ui.Refused(fmt.Sprintf("The voice servers on this machine use %s, and one machine serves one address.", o.Hostname()), "Pass --hostname "+o.Hostname()+", or join from another machine.")
		}
	}
	return nil
}

func voiceHostRelease(ctx context.Context, inst *instance.Instance, instanceRelease string) (*release.Release, error) {
	version := ""
	if release.Valid(instanceRelease) {
		version = instanceRelease
	}
	if inst.State.Runtime == instance.RuntimeNative {
		if version != "" {
			if _, err := release.Resolve(ctx, version); errors.Is(err, release.ErrUnknownVersion) {
				version = ""
			}
		}
		return nativeRelease(ctx, version, "Run rexctl voice join --runtime docker to run the voice server in a container.")
	}
	if version != "" {
		return &release.Release{Version: version}, nil
	}
	return resolveRelease(ctx, "")
}

func ensureVoiceHost(ctx context.Context, app *App, inst *instance.Instance, rel *release.Release, s *ui.Step) error {
	return ensureVoiceHostWith(ctx, app, inst, rel, nil, s)
}

func ensureVoiceHostWith(ctx context.Context, app *App, inst *instance.Instance, rel *release.Release, certs []certFile, s *ui.Step) error {
	if inst.State.Runtime == instance.RuntimeNative {
		if err := importNativeVoiceCerts(ctx, inst, certs); err != nil {
			app.UI.Detail("moving the certificates: %v", err)
		}
		return ensureNativeVoiceHost(ctx, inst, rel, s)
	}
	return ensureContainerVoiceHost(ctx, app, inst, rel, certs, s)
}

func ensureNativeVoiceHost(ctx context.Context, inst *instance.Instance, rel *release.Release, s *ui.Step) error {
	n := native.New(inst)
	before, release := n.Running(), inst.State.Release
	if rel != nil && before != rel.Version {
		if !n.Has(rel.Version) {
			s.Title("Downloading Fluxer " + rel.Version)
			if _, err := n.Fetch(ctx, rel, s.Progress); err != nil {
				return rt.NativeError(err, "Couldn't download Fluxer.")
			}
		}
		if err := n.Flip(rel.Version); err != nil {
			return rt.NativeError(err, "Couldn't unpack Fluxer.")
		}
	}
	err := startNativeVoiceHost(ctx, inst, n, s)
	if err == nil {
		if rel != nil {
			inst.State.Release = rel.Version
		}
		return saveVoiceState(inst)
	}
	if before == "" || before == n.Running() {
		return err
	}
	s.Title("Going back to Fluxer " + before)
	back := n.Flip(before)
	if back == nil {
		back = startNativeVoiceHost(context.WithoutCancel(ctx), inst, n, s)
	}
	if back != nil {
		return errors.Join(err, back)
	}
	inst.State.Release = release
	if back := saveVoiceState(inst); back != nil {
		return errors.Join(err, back)
	}
	return &rt.WentBack{Err: err}
}

func startNativeVoiceHost(ctx context.Context, inst *instance.Instance, n *native.Stack, s *ui.Step) error {
	if err := os.Chmod(inst.Dir, 0o755); err != nil {
		return ui.Wrap(err, "Couldn't open "+inst.Dir+" to Fluxer's service account.", "Check that root owns it.")
	}
	if err := os.MkdirAll(inst.Path("voice"), 0o750); err != nil {
		return err
	}
	s.Title("Starting the voice service")
	a, err := n.ApplyVoice(ctx, voiceHostEnv(inst), s.Warn)
	if err != nil {
		return rt.NativeError(err, "Couldn't start the voice service.")
	}
	if !a.Started && !a.Restarted && n.ActiveState(ctx) == "active" && !n.RunsCurrent(ctx) {
		if err := n.Restart(ctx); err != nil {
			return rt.NativeError(err, "Couldn't restart the voice service.")
		}
	}
	return waitVoiceHost(ctx, n, n.RunsCurrent)
}

func waitVoiceHost(ctx context.Context, n *native.Stack, current func(context.Context) bool) error {
	ctx, cancel := context.WithTimeout(ctx, voiceHostWait)
	defer cancel()
	for {
		if _, err := n.Entries(ctx); err == nil && (current == nil || current(ctx)) {
			return nil
		}
		select {
		case <-ctx.Done():
			return ui.Failed("The voice service didn't start.", "Run rexctl logs edge to see why.")
		case <-time.After(time.Second):
		}
	}
}

func voiceConnect(e *engine.Engine) rt.Connect {
	return func(context.Context) (*engine.Engine, error) { return e, nil }
}

func ensureContainerVoiceHost(ctx context.Context, app *App, inst *instance.Instance, rel *release.Release, certs []certFile, s *ui.Step) error {
	e, err := rt.VoiceEngine(ctx, inst)
	if err != nil {
		return err
	}
	version := inst.State.Release
	if rel != nil {
		version = rel.Version
	}
	if version == "" {
		return ui.Failed("rexctl doesn't know which Fluxer release this voice machine runs.", "Run rexctl upgrade.")
	}
	image := rt.ImageRef(version)
	if _, found, err := e.LocalImage(ctx, image); err != nil || !found {
		s.Title("Downloading " + image)
		if err := e.PullImage(ctx, image, app.progress(s, "Downloading")); err != nil {
			return ui.Wrap(rt.EngineError(e.Kind, err), "Couldn't download "+image+".", "Check this machine's internet connection, then run the same command again.")
		}
	}
	if err := os.MkdirAll(inst.Path("voice"), 0o750); err != nil {
		return err
	}
	s.Title("Starting the voice container")
	n := rt.VoiceStack(inst, voiceConnect(e))
	err = rt.EnsureVoiceContainer(ctx, e, rt.VoiceContainerSpec(inst, version, voiceHostEnv(inst)), containerCertSeed(e, certs), func(ctx context.Context) error {
		return waitVoiceHost(ctx, n, nil)
	})
	if err != nil {
		return err
	}
	inst.State.Release = version
	return saveVoiceState(inst)
}

func removeVoiceHost(ctx context.Context, inst *instance.Instance, data bool) error {
	if inst.State.Runtime == instance.RuntimeNative {
		n := native.New(inst)
		if err := n.Halt(context.WithoutCancel(ctx)); err != nil {
			return rt.NativeError(err, "Couldn't stop the voice service.")
		}
		u := n.BootUnit()
		steps, err := u.RemoveSteps(ctx)
		if err != nil {
			return rt.NativeError(err, "Couldn't remove the voice service.")
		}
		if err := host.RunSteps(ctx, steps); err != nil {
			return rt.NativeError(err, "Couldn't remove the voice service.")
		}
		if data {
			removeNativeVoiceFiles(inst)
		}
		return nil
	}
	e, err := rt.VoiceEngine(ctx, inst)
	if err != nil {
		return err
	}
	return rt.RemoveVoiceContainer(ctx, e, data)
}

func removeNativeVoiceFiles(inst *instance.Instance) {
	for _, dir := range []string{"releases", "current", "previous", "data", "caddy.d", "certs"} {
		_ = os.RemoveAll(inst.Path(dir))
	}
}

func voiceHostLogs(ctx context.Context, inst *instance.Instance, w io.Writer, o rt.LogOptions) error {
	opts := engine.LogsOptions{Follow: o.Follow, Since: o.Since, Tail: o.Tail, Stdout: w}
	if inst.State.Runtime == instance.RuntimeNative {
		err := native.Journal(ctx, native.LogsArgs(native.New(inst).Unit(), opts, "edge"), opts.Stdout)
		if err != nil && ctx.Err() == nil {
			return ui.Wrap(err, "Couldn't read the logs of the voice service.", "Run journalctl -u "+native.New(inst).Unit()+".")
		}
		return nil
	}
	e, err := rt.VoiceEngine(ctx, inst)
	if err != nil {
		return err
	}
	if err := e.SingleLogs(ctx, rt.VoiceContainer, opts, w); err != nil && !errors.Is(err, context.Canceled) {
		return ui.Wrap(rt.EngineError(e.Kind, err), "Couldn't read the logs of the voice container.", "Run rexctl voice status to check the voice servers.")
	}
	return nil
}

func chooseJoinRuntime(ctx context.Context, inst *instance.Instance, fresh bool, f *voiceFlags) error {
	if fresh {
		kind, err := pickVoiceRuntime(ctx, f.Runtime)
		if err != nil {
			return err
		}
		inst.State.Runtime = kind
		switch f.Runtime {
		case runtimeDocker:
			inst.State.Engine.Kind = string(engine.Docker)
		case runtimePodman:
			inst.State.Engine.Kind = string(engine.Podman)
		}
		return nil
	}
	if !fluxerdVoice(inst) {
		return ui.Refused("The voice servers on this machine use the older layout with Docker Compose, and rexctl no longer adds servers to it.", "Move them first with rexctl migrate --to systemd or rexctl migrate --to docker.")
	}
	if f.Runtime == "" {
		return nil
	}
	want := instance.RuntimeContainer
	if f.Runtime == runtimeSystemd {
		want = instance.RuntimeNative
	}
	if want != inst.State.Runtime {
		return ui.Refused(fmt.Sprintf("The voice servers on this machine run on %s.", voiceRuntimeLabel(inst)), "Leave out --runtime, or move them first with rexctl migrate --to "+f.Runtime+".")
	}
	return nil
}

func joinEngine(ctx context.Context, inst *instance.Instance) (*engine.Engine, error) {
	if inst.State.Runtime == instance.RuntimeNative {
		return nil, nil
	}
	return rt.VoiceEngine(ctx, inst)
}

func voiceMachineEngine(ctx context.Context, app *App, inst *instance.Instance) (*engine.Engine, error) {
	switch inst.State.Runtime {
	case instance.RuntimeNative:
		return nil, nil
	case instance.RuntimeContainer:
		return rt.VoiceEngine(ctx, inst)
	}
	return app.Engine(ctx, inst)
}

func voiceServerUp(ctx context.Context, vs rt.VoiceServers, srv *instance.VoiceServer) bool {
	state := vs.State(ctx, srv)
	return state == "running" || state == "running, healthy"
}

func upgradeFailedFix(drained []*instance.VoiceServer, down map[string]bool) string {
	var ids []string
	for _, srv := range drained {
		if down[srv.ID] {
			ids = append(ids, srv.ID)
		}
	}
	if len(ids) == 0 {
		return "Run rexctl logs edge to see why, then rexctl upgrade again."
	}
	return fmt.Sprintf("%s stay draining, so no new calls go to them. Run rexctl logs edge to see why, then rexctl voice undrain once rexctl voice status shows them running.", ui.JoinAnd(ids))
}

func upgradeVoiceHost(ctx context.Context, app *App, inst *instance.Instance, p *voiceMachinePlan) error {
	vs, err := voiceRuntime(ctx, app, inst)
	if err != nil {
		return err
	}
	if p.target == "" {
		for _, srv := range p.pending {
			if err := upgradeVoiceServer(ctx, app, vs, nil, inst, srv, srv.Image, defaultDrainTimeout); err != nil {
				return err
			}
		}
		if len(p.pending) == 0 {
			app.UI.OK("Every voice server on this machine is up to date.")
		}
		return nil
	}
	rel, err := voiceHostRelease(ctx, inst, p.target)
	if err != nil {
		return err
	}
	var drained []*instance.VoiceServer
	for i := range inst.State.Voice {
		srv := &inst.State.Voice[i]
		active, draining, err := voiceServerFlags(ctx, nil, inst, srv)
		if err == nil && active && !draining && startUpgradeDrain(ctx, app, nil, inst, srv) {
			drained = append(drained, srv)
		}
	}
	for _, srv := range drained {
		waitForDrain(ctx, app, inst, srv, defaultDrainTimeout)
	}
	down := map[string]bool{}
	err = app.UI.Step(ctx, "Upgrading the voice service to Fluxer "+rel.Version, func(ctx context.Context, s *ui.Step) error {
		err := ensureVoiceHost(ctx, app, inst, rel, s)
		var back *rt.WentBack
		wentBack := errors.As(err, &back)
		if err != nil && !wentBack {
			for _, srv := range drained {
				down[srv.ID] = !voiceServerUp(ctx, vs, srv)
			}
			return withFix(err, upgradeFailedFix(drained, down))
		}
		var failed error
		for i := range inst.State.Voice {
			srv := &inst.State.Voice[i]
			if err := vs.Start(ctx, s, srv); err != nil {
				down[srv.ID] = true
				failed = cmp.Or(failed, err)
			}
		}
		switch {
		case wentBack:
			return withFix(back.Err, "rexctl went back to Fluxer "+inst.State.Release+". "+upgradeFailedFix(drained, down))
		case failed != nil:
			return withFix(failed, upgradeFailedFix(drained, down))
		}
		s.Done("Upgraded the voice service to Fluxer %s.", rel.Version)
		return nil
	})
	for _, srv := range drained {
		if !down[srv.ID] {
			endUpgradeDrain(ctx, app, nil, inst, srv)
		}
	}
	if err != nil {
		return err
	}
	for i := range inst.State.Voice {
		srv := &inst.State.Voice[i]
		if srv.Edge != voice.EdgeNone {
			base, dial := deploy.VoiceProbeTarget(inst, srv)
			_ = waitForEndpoint(ctx, base, dial, true, 2*time.Minute)
		}
	}
	waitInstanceSees(ctx, app, inst, 2*time.Minute)
	return nil
}

func rerouteVoiceServer(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer) error {
	if !fluxerdVoice(inst) {
		return routeVoiceServer(ctx, app, e, inst, srv)
	}
	return app.UI.Step(ctx, "Moving the voice service to "+srv.Hostname, func(ctx context.Context, s *ui.Step) error {
		if err := ensureVoiceHost(ctx, app, inst, nil, s); err != nil {
			return err
		}
		s.Done("The voice service answers on %s.", srv.Hostname)
		return nil
	})
}
