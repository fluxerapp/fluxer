// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

type voiceEvents struct {
	got  []string
	fail map[string]bool
}

func recordVoice(t *testing.T) *voiceEvents {
	t.Helper()
	ev := &voiceEvents{fail: map[string]bool{}}
	oldDown, oldStop, oldStart := downInstance, stopVoice, startVoice
	t.Cleanup(func() { downInstance, stopVoice, startVoice = oldDown, oldStop, oldStart })
	downInstance = func(_ context.Context, _ *engine.Engine, inst *instance.Instance) error {
		ev.got = append(ev.got, "down "+inst.Project())
		return nil
	}
	stopVoice = func(_ context.Context, _ *engine.Engine, v AttachedVoice) error {
		ev.got = append(ev.got, "stop "+v.Server.ID)
		if ev.fail["stop "+v.Server.ID] {
			return errors.New("stuck")
		}
		return nil
	}
	startVoice = func(_ context.Context, _ *engine.Engine, v AttachedVoice) error {
		ev.got = append(ev.got, "start "+v.Server.ID)
		if ev.fail["start "+v.Server.ID] {
			return errors.New("no network")
		}
		return nil
	}
	return ev
}

func voiceHome(t *testing.T) string {
	t.Helper()
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(home, ".config"))
	return home
}

func fluxerWithVoice(t *testing.T, home string, servers ...instance.VoiceServer) *instance.Instance {
	t.Helper()
	inst, err := instance.Create(filepath.Join(home, "fluxer"), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Voice = servers
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	if err := instance.Remember(instance.OwnScope(), inst.Dir, true); err != nil {
		t.Fatal(err)
	}
	return inst
}

func joinedMachine(t *testing.T, home string, servers ...instance.VoiceServer) {
	t.Helper()
	inst := &instance.Instance{Dir: filepath.Join(home, "voice"), State: &instance.State{
		Schema: instance.StateSchema, Kind: instance.KindVoice, Project: voice.MachineProject,
		Settings: map[string]string{}, Secrets: map[string]string{}, Voice: servers,
	}}
	if err := os.MkdirAll(inst.Path(instance.StateDirName), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	if err := instance.Remember(instance.OwnScope(), inst.Dir, false); err != nil {
		t.Fatal(err)
	}
}

func TestIPv6ChangeTakesVoiceServersOffTheNetworkFirst(t *testing.T) {
	home := voiceHome(t)
	inst := fluxerWithVoice(t, home, instance.VoiceServer{ID: "fra-server-1", Project: "fluxer-voice-fra-server-1", Network: "fluxer_fluxer"})
	joinedMachine(t, home,
		instance.VoiceServer{ID: "fra-server-2", Project: "fluxer-voice-fra-server-2", Network: "fluxer_fluxer"},
		instance.VoiceServer{ID: "sgp-server-1", Project: "fluxer-voice-sgp-server-1", Network: voice.MachineNetwork},
	)
	inst.State.Stopped = true
	inst.State.Pending = &instance.Pending{Settings: map[string]string{settings.IPv6: "true"}, Keys: []string{settings.IPv6}}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	ev := recordVoice(t)
	if err := Resolve(context.Background(), inst, nil); err != nil {
		t.Fatal(err)
	}
	want := []string{"stop fra-server-1", "stop fra-server-2", "down fluxer"}
	if !slices.Equal(ev.got, want) {
		t.Fatalf("events = %v, want %v", ev.got, want)
	}
	if inst.State.Pending != nil {
		t.Fatal("the pending change is still there")
	}
}

func TestVoiceServersStartAgainAfterTheNetworkChange(t *testing.T) {
	home := voiceHome(t)
	inst := fluxerWithVoice(t, home,
		instance.VoiceServer{ID: "fra-server-1", Project: "fluxer-voice-fra-server-1", Network: "fluxer_fluxer"},
		instance.VoiceServer{ID: "fra-server-2", Project: "fluxer-voice-fra-server-2", Network: "fluxer_fluxer"},
	)
	ev := recordVoice(t)
	ev.fail["start fra-server-2"] = true
	detached, err := detachVoice(context.Background(), nil, inst)
	if err != nil {
		t.Fatal(err)
	}
	failed := attachVoice(context.Background(), nil, detached)
	want := []string{"stop fra-server-1", "stop fra-server-2", "start fra-server-1", "start fra-server-2"}
	if !slices.Equal(ev.got, want) {
		t.Fatalf("events = %v, want %v", ev.got, want)
	}
	if !slices.Equal(failed, []string{"fra-server-2"}) {
		t.Fatalf("failed = %v", failed)
	}
}

func TestAServerThatWontStopBringsTheOthersBack(t *testing.T) {
	home := voiceHome(t)
	inst := fluxerWithVoice(t, home,
		instance.VoiceServer{ID: "fra-server-1", Project: "fluxer-voice-fra-server-1", Network: "fluxer_fluxer"},
		instance.VoiceServer{ID: "fra-server-2", Project: "fluxer-voice-fra-server-2", Network: "fluxer_fluxer"},
	)
	ev := recordVoice(t)
	ev.fail["stop fra-server-2"] = true
	if _, err := detachVoice(context.Background(), nil, inst); err == nil {
		t.Fatal("detach succeeded with a stuck server")
	}
	want := []string{"stop fra-server-1", "stop fra-server-2", "start fra-server-1"}
	if !slices.Equal(ev.got, want) {
		t.Fatalf("events = %v, want %v", ev.got, want)
	}
}

type endpointAdmin struct {
	sent map[string]string
	fail string
}

func (f *endpointAdmin) Do(_ context.Context, req adminapi.Request) (*adminapi.Response, error) {
	if req.Method != http.MethodPatch {
		return &adminapi.Response{Status: 404, Body: json.RawMessage(`{}`)}, nil
	}
	if strings.HasSuffix(req.Path, "/"+f.fail) {
		return &adminapi.Response{Status: 500, Body: json.RawMessage(`{"code":"GENERAL_ERROR","message":"boom"}`)}, nil
	}
	var body struct {
		Endpoint string `json:"endpoint"`
	}
	if err := json.Unmarshal(req.Body, &body); err != nil {
		return nil, err
	}
	f.sent[req.Path] = body.Endpoint
	return &adminapi.Response{Status: 200, Body: json.RawMessage(`{"server":{}}`)}, nil
}

func TestANewDomainMovesVoiceAddEndpoints(t *testing.T) {
	dir := t.TempDir()
	inst := &instance.Instance{Dir: dir, State: &instance.State{
		Schema: instance.StateSchema, Project: "fluxer", Secrets: map[string]string{},
		Settings: map[string]string{"FLUXER_PUBLIC_ORIGIN": "https://new.example.com"},
		Voice: []instance.VoiceServer{
			{ID: "default-server-2", Region: "default", Endpoint: "wss://old.example.com/livekit-default-server-2", Edge: voice.EdgeInstance},
			{ID: "default-server-3", Region: "default", Endpoint: "wss://old.example.com/livekit-default-server-3", Edge: voice.EdgeInstance},
			{ID: "fra-server-1", Region: "fra", Endpoint: "wss://voice.example.net/livekit-fra-server-1", Edge: voice.EdgeOwn},
		},
	}}
	if err := os.MkdirAll(inst.Path(instance.StateDirName), 0o700); err != nil {
		t.Fatal(err)
	}
	if got := StaleVoiceEndpoints(inst); !slices.Equal(got, []string{"default-server-2", "default-server-3"}) {
		t.Fatalf("stale = %v", got)
	}
	fake := &endpointAdmin{sent: map[string]string{}, fail: "default-server-3"}
	moved, err := MoveVoiceEndpoints(context.Background(), adminapi.New(fake, ""), inst)
	if err == nil || !strings.Contains(err.Error(), "default-server-3") {
		t.Fatalf("err = %v", err)
	}
	if !slices.Equal(moved, []string{"default-server-2"}) {
		t.Fatalf("moved = %v", moved)
	}
	if got := fake.sent["/admin/voice/regions/default/servers/default-server-2"]; got != "wss://new.example.com/livekit-default-server-2" {
		t.Fatalf("sent %v", fake.sent)
	}
	saved, err := instance.Find(dir)
	if err != nil {
		t.Fatal(err)
	}
	if got := saved.State.VoiceServer("default-server-2").Endpoint; got != "wss://new.example.com/livekit-default-server-2" {
		t.Fatalf("saved endpoint = %s", got)
	}
	if got := saved.State.VoiceServer("default-server-3").Endpoint; got != "wss://old.example.com/livekit-default-server-3" {
		t.Fatalf("failed server changed to %s", got)
	}
	if got := StaleVoiceEndpoints(inst); !slices.Equal(got, []string{"default-server-3"}) {
		t.Fatalf("stale after the move = %v", got)
	}
}
