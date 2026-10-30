// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func testVoiceApp() (*App, *bytes.Buffer) {
	out := &bytes.Buffer{}
	return &App{UI: ui.New(ui.Options{Stdin: strings.NewReader(""), Stdout: out, Stderr: out})}, out
}

func voiceMachine(t *testing.T, servers ...instance.VoiceServer) *instance.Instance {
	t.Helper()
	dir := resolved(t, t.TempDir())
	inst := &instance.Instance{Dir: dir, State: &instance.State{
		Schema: instance.StateSchema, Kind: instance.KindVoice, Project: voice.MachineProject,
		Settings: map[string]string{}, Secrets: map[string]string{}, Voice: servers,
	}}
	if err := os.MkdirAll(inst.Path(instance.StateDirName), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	if err := instance.Remember(instance.OwnScope(), dir, false); err != nil {
		t.Fatal(err)
	}
	return inst
}

func TestRemovingAVoiceMachineTurnsOffAutoUpgrade(t *testing.T) {
	clearRecords(t)
	inst := voiceMachine(t)
	var removed []string
	removeAutoUpgradeFor = func(_ context.Context, inst *instance.Instance, _ *engine.Engine) (bool, error) {
		removed = append(removed, inst.Dir)
		return true, nil
	}
	t.Cleanup(func() { removeAutoUpgradeFor = removeAutoUpgrade })
	app, _ := testVoiceApp()
	if err := removeVoiceMachine(context.Background(), app, nil, inst); err != nil {
		t.Fatal(err)
	}
	if len(removed) != 1 || removed[0] != inst.Dir {
		t.Fatalf("auto-upgrade removed for %v, want %s", removed, inst.Dir)
	}
	if _, err := os.Stat(inst.Dir); !os.IsNotExist(err) {
		t.Fatalf("%s is still there", inst.Dir)
	}
}

func TestAJoinedServerNextToFluxerIsFoundWithoutDir(t *testing.T) {
	clearRecords(t)
	fluxer, err := instance.Create(resolved(t, t.TempDir()), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	if err := fluxer.Save(); err != nil {
		t.Fatal(err)
	}
	if err := instance.Remember(instance.OwnScope(), fluxer.Dir, true); err != nil {
		t.Fatal(err)
	}
	machine := voiceMachine(t, instance.VoiceServer{ID: "fra-server-1", Edge: voice.EdgeInstance})
	machine.State.VoiceEdge = &instance.VoiceEdge{Instance: fluxer.Dir}
	if err := machine.Save(); err != nil {
		t.Fatal(err)
	}
	owner, srv := findVoiceServer(fluxer, "fra-server-1")
	if srv == nil || owner.Dir != machine.Dir {
		t.Fatalf("findVoiceServer found %v in %s", srv, owner.Dir)
	}
	if _, _, err := localVoiceServers(fluxer, []string{"fra-server-9"}); err == nil {
		t.Fatal("an unknown ID was found")
	}
	if got := voiceDirFlag(machine.Dir); got != " --dir "+machine.Dir {
		t.Fatalf("voiceDirFlag = %q", got)
	}
	if got := voiceDirFlag(fluxer.Dir); got != "" {
		t.Fatalf("voiceDirFlag for the default = %q", got)
	}
	using := voiceMachinesUsing(fluxer)
	if len(using) != 1 || using[0].Dir != machine.Dir {
		t.Fatalf("voiceMachinesUsing = %v", using)
	}
}

func TestVoiceServerStaleFollowsTheShippedFiles(t *testing.T) {
	srv := instance.VoiceServer{ID: "fra-server-1", Key: "fluxer-fra-server-1", Secret: strings.Repeat("a", 64), TCPPort: 7883, UDPPort: 7884, Image: "livekit/livekit-server:v1", Memory: "1g", Webhook: "https://chat.example.com/api/webhooks/livekit", Network: voice.MachineNetwork}
	inst := voiceMachine(t, srv)
	saved := &inst.State.Voice[0]
	if err := writeVoiceServer(inst, saved); err != nil {
		t.Fatal(err)
	}
	if voiceServerStale(inst, saved, saved.Image) {
		t.Fatal("a freshly written server is stale")
	}
	if !voiceServerStale(inst, saved, "livekit/livekit-server:v2") {
		t.Fatal("a new image isn't stale")
	}
	compose := filepath.Join(voice.ServerDir(inst.Dir, saved.ID), voice.ComposeFile)
	if err := os.WriteFile(compose, []byte("services: {}\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if !voiceServerStale(inst, saved, saved.Image) {
		t.Fatal("a changed compose file isn't stale")
	}
	saved.Pinned = true
	if got := voiceTargetImage(saved, "livekit/livekit-server:v2"); got != saved.Image {
		t.Fatalf("a pinned server moves to %s", got)
	}
}

func TestRestoreDropsServersMissingFromTheBackup(t *testing.T) {
	before := []instance.VoiceServer{{ID: "a", Project: "fluxer-voice-a"}, {ID: "b", Project: "fluxer-voice-b"}}
	after := []instance.VoiceServer{{ID: "a", Project: "fluxer-voice-a"}}
	got := droppedVoiceServers(before, after)
	if len(got) != 1 || got[0].ID != "b" {
		t.Fatalf("dropped %v", got)
	}
}

func TestBindAddressOnlyWhenTheMachineHasIt(t *testing.T) {
	local := []string{"10.0.0.5", "198.51.100.8"}
	if got := bindAddress("198.51.100.8", local); got != "198.51.100.8" {
		t.Errorf("a local address binds to %q", got)
	}
	if got := bindAddress("203.0.113.7", local); got != "" {
		t.Errorf("an address behind NAT binds to %q", got)
	}
}

func TestJoinFirewallText(t *testing.T) {
	if got := joinFirewallText(host.FirewallNone, 4, true); !strings.HasPrefix(got, "none found") {
		t.Errorf("no firewall: %s", got)
	}
	if got := joinFirewallText(host.Firewall("ufw"), 4, true); !strings.Contains(got, "left alone") {
		t.Errorf("--no-firewall: %s", got)
	}
	if got := joinFirewallText(host.Firewall("ufw"), 4, false); !strings.Contains(got, "will open 4 ports") {
		t.Errorf("ufw: %s", got)
	}
}

func TestTakenVoicePortsIncludeTheHTTPPort(t *testing.T) {
	clearRecords(t)
	inst := voiceMachine(t, instance.VoiceServer{ID: "b", TCPPort: 7883, UDPPort: 7884, HTTPPort: 7882, Edge: voice.EdgeNone})
	taken := takenVoicePorts(context.Background(), nil, inst)
	if !taken[host.Port{Number: 7882, Proto: "tcp"}] {
		t.Fatalf("the HTTP port of a stopped server is free: %v", taken)
	}
	if !instancePortsContain(instance.RecordedVoicePorts(), 7882, "tcp") {
		t.Fatal("the recorded voice ports miss the HTTP port")
	}
}

func instancePortsContain(ports []instance.VoicePort, number int, proto string) bool {
	for _, p := range ports {
		if p.Number == number && p.Proto == proto {
			return true
		}
	}
	return false
}

func TestDrainWaitDoesNotTreatAnUnreadableServerAsEmpty(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusBadGateway)
	}))
	defer srv.Close()
	port := srv.Listener.Addr().(*net.TCPAddr).Port
	inst := &instance.Instance{Dir: t.TempDir(), State: &instance.State{Kind: instance.KindVoice}}
	server := &instance.VoiceServer{ID: "fra-server-1", Edge: voice.EdgeNone, HTTPPort: port, Endpoint: "wss://voice.example.com/livekit-fra-server-1", Key: "k", Secret: "s"}
	app, out := testVoiceApp()
	start := time.Now()
	waitForDrain(context.Background(), app, inst, server, time.Second)
	if time.Since(start) < time.Second {
		t.Fatal("the wait ended before its limit")
	}
	if strings.Contains(out.String(), "No calls left") || !strings.Contains(out.String(), "couldn't read the calls") {
		t.Fatalf("output: %s", out.String())
	}
}

func TestPickHTTPPortSkipsTheServersOwnTCPPort(t *testing.T) {
	taken := map[host.Port]bool{{Number: 7880, Proto: "tcp"}: true}
	free := func(int, string) bool { return true }
	if got := pickHTTPPort(taken, 7881, free); got != 7882 {
		t.Fatalf("pickHTTPPort = %d, want 7882", got)
	}
}

func TestInstallAfterAJoinUsesTheStandardFolder(t *testing.T) {
	clearRecords(t)
	machine := voiceMachine(t, instance.VoiceServer{ID: "fra-server-1", Instance: "chat.example.com"})
	app, out := testVoiceApp()
	dir, err := installTarget(app)
	if err != nil {
		t.Fatal(err)
	}
	want, err := instance.DefaultDir()
	if err != nil {
		t.Fatal(err)
	}
	if dir != want {
		t.Fatalf("install goes to %s, want %s", dir, want)
	}
	if !strings.Contains(out.String(), machine.Dir) {
		t.Fatalf("output: %s", out.String())
	}

	app, _ = testVoiceApp()
	app.Flags.Dir = machine.Dir
	_, err = installTarget(app)
	var exit *ui.ExitError
	if !errors.As(err, &exit) || exit.Code != ui.ExitRefused || !strings.Contains(exit.Msg, "chat.example.com") {
		t.Fatalf("install with --dir of a voice machine: %v", err)
	}
}

func TestBootStartStartsVoiceServersWhenFluxerIsAlreadyUp(t *testing.T) {
	clearRecords(t)
	inst, err := instance.Create(resolved(t, t.TempDir()), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Voice = []instance.VoiceServer{{ID: "fra-server-1", Network: "fluxer_fluxer"}}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	app, _ := testVoiceApp()
	app.Flags.Dir = inst.Dir
	app.engine = &engine.Engine{Kind: engine.Podman}
	var started []string
	prevUp, prevStart := bootStackUp, startVoiceAtBoot
	t.Cleanup(func() { bootStackUp, startVoiceAtBoot = prevUp, prevStart })
	bootStackUp = func(context.Context, *App, *instance.Instance) bool { return true }
	startVoiceAtBoot = func(_ context.Context, _ *App, got *instance.Instance, _ *engine.Engine) {
		started = append(started, got.Dir)
	}
	if err := runBootStart(context.Background(), app); err != nil {
		t.Fatal(err)
	}
	if len(started) != 1 || started[0] != inst.Dir {
		t.Fatalf("voice servers started for %v", started)
	}

	started = nil
	inst.State.Stopped = true
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	if err := runBootStart(context.Background(), app); err != nil {
		t.Fatal(err)
	}
	if len(started) != 0 {
		t.Fatalf("a stopped instance started voice servers for %v", started)
	}
}

func TestVoiceCommandsRefuseAnEjectedInstance(t *testing.T) {
	clearRecords(t)
	inst, err := instance.Create(resolved(t, t.TempDir()), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Ejected = &instance.Ejected{At: time.Now()}
	inst.State.Voice = []instance.VoiceServer{{ID: "fra-server-1", Edge: voice.EdgeInstance}}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	before, err := os.ReadFile(inst.StatePath())
	if err != nil {
		t.Fatal(err)
	}
	sites, _ := os.ReadDir(inst.Path("caddy.d"))
	refused := func(name string, err error) {
		t.Helper()
		var exit *ui.ExitError
		if !errors.As(err, &exit) || exit.Code != ui.ExitRefused || !strings.Contains(exit.Fix, "rexctl takeover") {
			t.Fatalf("%s: %v", name, err)
		}
	}
	app, _ := testVoiceApp()
	app.Flags.Dir = inst.Dir
	refused("voice add", runVoiceAdd(context.Background(), app, nil, &voiceFlags{}))
	refused("voice remove", removeVoiceServers(context.Background(), app, inst, []*instance.VoiceServer{&inst.State.Voice[0]}, 0, false))
	after, err := os.ReadFile(inst.StatePath())
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(before, after) {
		t.Fatal("the state changed")
	}
	if now, _ := os.ReadDir(inst.Path("caddy.d")); len(now) != len(sites) {
		t.Fatalf("caddy.d went from %d to %d files", len(sites), len(now))
	}
}

func TestRemovingAJoinedServerThatNeverRegisteredSkipsTheInstance(t *testing.T) {
	clearRecords(t)
	inst := voiceMachine(t, instance.VoiceServer{ID: "fra-server-1", Instance: "chat.example.com", Edge: voice.EdgeOwn, Joined: true})
	app, out := testVoiceApp()
	if err := unregisterVoiceServer(context.Background(), app, nil, inst, &inst.State.Voice[0], time.Minute); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(out.String(), "fra-server-1 never joined chat.example.com") {
		t.Fatalf("output: %s", out.String())
	}
}

func TestVoiceMachineUpgradeRefusesPrefetchAndTo(t *testing.T) {
	clearRecords(t)
	inst := voiceMachine(t, instance.VoiceServer{ID: "fra-server-1", Edge: voice.EdgeInstance})
	for _, f := range []*upgradeFlags{{Prefetch: true}, {To: "2026.1029.100000"}} {
		app, out := testVoiceApp()
		app.Flags.Dir = inst.Dir
		err := runUpgrade(context.Background(), app, f)
		var ee *ui.ExitError
		if !errors.As(err, &ee) || ee.Code != ui.ExitUsage || !strings.Contains(ee.Msg, "voice machine") {
			t.Fatalf("flags %+v gave %v", f, err)
		}
		if strings.Contains(out.String(), "drain") {
			t.Fatalf("flags %+v drained: %q", f, out.String())
		}
	}
}

type drainNode struct {
	mu       sync.Mutex
	draining bool
	patches  []bool
}

func (n *drainNode) serve(t *testing.T) string {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		n.mu.Lock()
		defer n.mu.Unlock()
		if r.Method == http.MethodPatch {
			var update voice.NodeUpdate
			if err := json.NewDecoder(r.Body).Decode(&update); err == nil && update.Draining != nil {
				n.draining = *update.Draining
				n.patches = append(n.patches, *update.Draining)
			}
		}
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(map[string]any{"is_active": true, "draining": n.draining})
	}))
	t.Cleanup(srv.Close)
	return srv.URL
}

func stubUpgradedVoiceStart(t *testing.T, start func() error) {
	t.Helper()
	prev := startUpgradedVoiceServer
	t.Cleanup(func() { startUpgradedVoiceServer = prev })
	startUpgradedVoiceServer = func(context.Context, *App, *engine.Engine, *instance.Instance, *instance.VoiceServer, *ui.Step) error {
		return start()
	}
}

func TestFailedVoiceUpgradeClearsItsDrainAndStaysStale(t *testing.T) {
	clearRecords(t)
	node := &drainNode{}
	inst := voiceMachine(t, instance.VoiceServer{ID: "fra-server-1", NodeURL: node.serve(t), Key: "k", Secret: strings.Repeat("s", 64), Image: "livekit:old", Edge: voice.EdgeNone})
	stubUpgradedVoiceStart(t, func() error { return errors.New("pull failed") })
	app, _ := testVoiceApp()
	srv := &inst.State.Voice[0]
	if err := upgradeVoiceServer(context.Background(), app, nil, nil, inst, srv, "livekit:new", time.Second); err == nil {
		t.Fatal("the failed start gave no error")
	}
	if node.draining || len(node.patches) != 2 || !node.patches[0] || node.patches[1] {
		t.Fatalf("draining %v after patches %v", node.draining, node.patches)
	}
	saved, err := instance.Find(inst.Dir)
	if err != nil {
		t.Fatal(err)
	}
	if got := saved.State.Voice[0]; got.Image != "livekit:old" || got.Drained {
		t.Fatalf("saved server %+v", got)
	}
	if !voiceServerStale(saved, &saved.State.Voice[0], "livekit:new") {
		t.Fatal("the failed server reads as current")
	}
}

func TestVoiceUpgradeClearsADrainLeftByAnEarlierRun(t *testing.T) {
	clearRecords(t)
	node := &drainNode{draining: true}
	inst := voiceMachine(t, instance.VoiceServer{ID: "fra-server-1", NodeURL: node.serve(t), Key: "k", Secret: strings.Repeat("s", 64), Image: "livekit:old", Edge: voice.EdgeNone, Drained: true})
	stubUpgradedVoiceStart(t, func() error { return nil })
	app, _ := testVoiceApp()
	srv := &inst.State.Voice[0]
	if err := upgradeVoiceServer(context.Background(), app, nil, nil, inst, srv, "livekit:new", time.Second); err != nil {
		t.Fatal(err)
	}
	if node.draining || len(node.patches) != 1 || node.patches[0] {
		t.Fatalf("draining %v after patches %v", node.draining, node.patches)
	}
	if srv.Image != "livekit:new" || srv.Drained {
		t.Fatalf("server %+v", srv)
	}
}

func TestVoiceUpgradeLeavesAnAdminDrainAlone(t *testing.T) {
	clearRecords(t)
	node := &drainNode{draining: true}
	inst := voiceMachine(t, instance.VoiceServer{ID: "fra-server-1", NodeURL: node.serve(t), Key: "k", Secret: strings.Repeat("s", 64), Image: "livekit:old", Edge: voice.EdgeNone})
	stubUpgradedVoiceStart(t, func() error { return nil })
	app, _ := testVoiceApp()
	if err := upgradeVoiceServer(context.Background(), app, nil, nil, inst, &inst.State.Voice[0], "livekit:new", time.Second); err != nil {
		t.Fatal(err)
	}
	if !node.draining || len(node.patches) > 0 {
		t.Fatalf("draining %v after patches %v", node.draining, node.patches)
	}
}

func TestCancelledVoiceUpgradeStillClearsItsDrain(t *testing.T) {
	clearRecords(t)
	node := &drainNode{}
	inst := voiceMachine(t, instance.VoiceServer{ID: "fra-server-1", NodeURL: node.serve(t), Key: "k", Secret: strings.Repeat("s", 64), Image: "livekit:old", Edge: voice.EdgeNone})
	ctx, cancel := context.WithCancel(context.Background())
	stubUpgradedVoiceStart(t, func() error {
		cancel()
		return context.Canceled
	})
	app, _ := testVoiceApp()
	if err := upgradeVoiceServer(ctx, app, nil, nil, inst, &inst.State.Voice[0], "livekit:new", time.Second); err == nil {
		t.Fatal("the cancelled upgrade gave no error")
	}
	if node.draining {
		t.Fatalf("still draining after patches %v", node.patches)
	}
}
