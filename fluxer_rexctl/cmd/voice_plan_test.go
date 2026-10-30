// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func fluxerWithVoice(t *testing.T, servers ...instance.VoiceServer) *instance.Instance {
	t.Helper()
	inst, err := instance.Create(resolved(t, t.TempDir()), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Settings["domain"] = "chat.example.com"
	inst.State.Voice = servers
	if err := os.MkdirAll(inst.Path(instance.CaddyDropInDir), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	if err := instance.Remember(instance.OwnScope(), inst.Dir, true); err != nil {
		t.Fatal(err)
	}
	return inst
}

func refusal(t *testing.T, err error) *ui.ExitError {
	t.Helper()
	var ee *ui.ExitError
	if !errors.As(err, &ee) {
		t.Fatalf("not a refusal: %v", err)
	}
	return ee
}

func TestVoiceIDsAreUniqueOnTheWholeMachine(t *testing.T) {
	clearRecords(t)
	fluxer := fluxerWithVoice(t, instance.VoiceServer{ID: "default-server-2", Edge: voice.EdgeInstance})
	if err := os.WriteFile(filepath.Join(fluxer.Path(instance.CaddyDropInDir), voice.DropInName("hand-made")), voice.DropIn("hand-made"), 0o644); err != nil {
		t.Fatal(err)
	}
	machine := voiceMachine(t, instance.VoiceServer{ID: "default-server-3", Edge: voice.EdgeInstance, Joined: true})
	machine.State.VoiceEdge = &instance.VoiceEdge{Instance: fluxer.Dir}
	if err := machine.Save(); err != nil {
		t.Fatal(err)
	}
	for _, inst := range []*instance.Instance{fluxer, machine} {
		taken := voiceIDsInUse(context.Background(), nil, inst, voice.JoinedProject(""))
		for _, id := range []string{"edge", "default-server-2", "default-server-3", "hand-made"} {
			if !slices.Contains(taken, id) {
				t.Errorf("from %s: %s is free in %v", inst.Dir, id, taken)
			}
		}
	}
}

func TestJoinSkipsAnIDThatAServerNextDoorUses(t *testing.T) {
	clearRecords(t)
	machine := voiceMachine(t)
	preview := &voice.Preview{ServerID: "default-server-2"}
	preview.Region.ID = "default"
	taken := []string{"edge", "default-server-1", "default-server-2"}
	id, err := chooseJoinID(preview, "", machine, taken)
	if err != nil || id != "default-server-3" {
		t.Fatalf("suggested ID: %s, %v", id, err)
	}
	if _, err := chooseJoinID(preview, "default-server-1", machine, taken); !strings.Contains(refusal(t, err).Fix, "--id") {
		t.Fatalf("taken --id: %v", err)
	}
	preview.ServerIDPinned = true
	if _, err := chooseJoinID(preview, "", machine, taken); !strings.Contains(refusal(t, err).Fix, "new token") {
		t.Fatalf("taken pinned ID: %v", err)
	}
	if _, err := chooseJoinID(preview, "other", machine, nil); !strings.Contains(refusal(t, err).Fix, "Leave out --id") {
		t.Fatalf("--id against a pinned ID: %v", err)
	}
	preview = &voice.Preview{ServerID: "edge"}
	preview.Region.ID = "edge"
	if id, err := chooseJoinID(preview, "", machine, []string{"edge"}); err != nil || id != "edge-server-1" {
		t.Fatalf("edge: %s, %v", id, err)
	}
}

func TestJoinRetryKeepsItsOwnID(t *testing.T) {
	clearRecords(t)
	machine := voiceMachine(t, instance.VoiceServer{ID: "fra-server-1"})
	preview := &voice.Preview{ServerID: "fra-server-1"}
	if id, err := chooseJoinID(preview, "", machine, []string{"fra-server-1"}); err != nil || id != "fra-server-1" {
		t.Fatalf("retry: %s, %v", id, err)
	}
	machine.State.Voice[0].NodeURL = "https://chat.example.com/api/voice/node"
	if _, err := chooseJoinID(preview, "", machine, nil); err == nil {
		t.Fatal("a joined server was joined again")
	}
}

func TestVoiceAddSkipsIDsOfJoinedServersNextDoor(t *testing.T) {
	clearRecords(t)
	fluxer := fluxerWithVoice(t)
	machine := voiceMachine(t, instance.VoiceServer{ID: "default-server-2", Edge: voice.EdgeInstance, Joined: true})
	machine.State.VoiceEdge = &instance.VoiceEdge{Instance: fluxer.Dir}
	if err := machine.Save(); err != nil {
		t.Fatal(err)
	}
	regions := []adminapi.VoiceRegion{{VoiceRegionAdminResponse: adminapi.VoiceRegionAdminResponse{Id: "default"}, Servers: []adminapi.VoiceServerAdminResponse{{ServerId: "default-server-1"}}}}
	app, _ := testVoiceApp()
	f := &voiceFlags{Count: 1, IP: "203.0.113.9", TCPPort: 7883, UDPPort: 7884}
	plans, err := planVoiceAdd(context.Background(), app, nil, fluxer, regions, "default", f)
	if err != nil || len(plans) != 1 || plans[0].srv.ID != "default-server-3" {
		t.Fatalf("plans %v, %v", plans, err)
	}
	f.ID = "default-server-2"
	if _, err := planVoiceAdd(context.Background(), app, nil, fluxer, regions, "default", f); !strings.Contains(refusal(t, err).Fix, "--id") {
		t.Fatalf("taken --id: %v", err)
	}
}

func TestEdgeIsNotAServerID(t *testing.T) {
	if err := checkServerID("edge"); err == nil {
		t.Fatal("edge is a server ID")
	}
	if err := checkServerID("fra-server-1"); err != nil {
		t.Fatal(err)
	}
	app, _ := testVoiceApp()
	c := newVoiceTokenCmd(app)
	c.SetArgs([]string{"--id", "edge"})
	c.SilenceUsage, c.SilenceErrors = true, true
	if err := c.Execute(); refusal(t, err).Code != ui.ExitUsage {
		t.Fatalf("token --id edge: %v", err)
	}
}

func nodeServer(t *testing.T, status int, node map[string]any) string {
	t.Helper()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(status)
		_ = json.NewEncoder(w).Encode(node)
	}))
	t.Cleanup(srv.Close)
	return srv.URL
}

func TestUpgradeReadsDrainingOnAVoiceMachine(t *testing.T) {
	url := nodeServer(t, http.StatusOK, map[string]any{"is_active": true, "draining": true})
	inst := &instance.Instance{Dir: t.TempDir(), State: &instance.State{Kind: instance.KindVoice}}
	srv := &instance.VoiceServer{ID: "fra-server-1", NodeURL: url, Key: "k", Secret: strings.Repeat("s", 64)}
	active, draining, err := voiceServerFlags(context.Background(), nil, inst, srv)
	if err != nil || !active || !draining {
		t.Fatalf("active %v, draining %v, %v", active, draining, err)
	}
	if upgradeDrains(active, draining) {
		t.Fatal("the upgrade clears a draining flag an admin set")
	}
	srv.NodeURL = nodeServer(t, http.StatusBadGateway, map[string]any{})
	if _, _, err := voiceServerFlags(context.Background(), nil, inst, srv); err == nil {
		t.Fatal("an unreadable node gave a state")
	}
	fluxer := &instance.Instance{Dir: t.TempDir(), State: &instance.State{}}
	if _, _, err := voiceServerFlags(context.Background(), nil, fluxer, srv); err == nil {
		t.Fatal("no admin API gave a state")
	}
}

func TestVoiceAddServersFollowThePublicIP(t *testing.T) {
	inst := &instance.Instance{Dir: t.TempDir(), State: &instance.State{Settings: map[string]string{settings.VoicePublicIP: "198.51.100.20"}, Voice: []instance.VoiceServer{
		{ID: "a", NodeIP: "198.51.100.10"},
		{ID: "b", NodeIP: "198.51.100.11", IPPinned: true},
		{ID: "c", NodeIP: "198.51.100.10", Joined: true},
		{ID: "d", NodeIP: "198.51.100.20"},
	}}}
	moved := followVoicePublicIP(inst)
	if len(moved) != 1 || moved[0].ID != "a" || inst.State.Voice[0].NodeIP != "198.51.100.20" {
		t.Fatalf("moved %v", moved)
	}
	if inst.State.Voice[1].NodeIP != "198.51.100.11" || inst.State.Voice[2].NodeIP != "198.51.100.10" {
		t.Fatalf("a pinned or joined server moved: %+v", inst.State.Voice)
	}
	delete(inst.State.Settings, settings.VoicePublicIP)
	moved = followVoicePublicIP(inst)
	if len(moved) != 2 || inst.State.Voice[0].NodeIP != "" || inst.State.Voice[3].NodeIP != "" {
		t.Fatalf("clearing the setting moved %v", moved)
	}
}

func TestPinnedHostnameIsRequired(t *testing.T) {
	old, oldWait := hostnameLookup, dnsWait
	t.Cleanup(func() { hostnameLookup, dnsWait = old, oldWait })
	dnsWait = 0
	hostnameLookup = func() voice.Resolver {
		return func(context.Context, string) (voice.DNSAnswer, error) {
			return voice.DNSAnswer{A: []string{"192.0.2.1"}}, nil
		}
	}
	pinned := "voice.example.com"
	plan := &joinPlan{preview: &voice.Preview{Hostname: &pinned}, reach: voice.Reachability{PublicIPv4: "203.0.113.7"}}
	app, out := testVoiceApp()
	if err := chooseAddress(context.Background(), app, plan, "fra-server-1", &voiceFlags{Hostname: "other.example.com"}); !strings.Contains(refusal(t, err).Msg, pinned) {
		t.Fatalf("another --hostname: %v", err)
	}
	err := chooseAddress(context.Background(), app, plan, "fra-server-1", &voiceFlags{})
	if ee := refusal(t, err); !strings.Contains(ee.Msg, pinned) || plan.address != "" {
		t.Fatalf("pinned name not on this machine: %v, address %q", err, plan.address)
	}
	if strings.Contains(out.String(), "rexctl voice hostname") {
		t.Fatalf("offered the IP address: %s", out.String())
	}
	plan.preview.Hostname = nil
	if err := chooseAddress(context.Background(), app, plan, "fra-server-1", &voiceFlags{Hostname: "other.example.com"}); err != nil || plan.address != "203.0.113.7" || !plan.ipCert {
		t.Fatalf("unpinned name: %v, address %q", err, plan.address)
	}
}

func TestEndpointRefusalShowsThePinnedHostname(t *testing.T) {
	err := joinError(&voice.APIError{Code: voice.CodeEndpointInvalid, Detail: "The endpoint must use the hostname voice.example.com"}, "chat.example.com", "fra-server-1")
	if ee := refusal(t, err); !strings.Contains(ee.Fix, "voice.example.com") || strings.Contains(ee.Fix, "IPv4") {
		t.Fatalf("refusal: %+v", ee)
	}
}

func TestOriginWarningSaysWhatToDo(t *testing.T) {
	if got := originFix(originUnlisted, "chat.example.com", "2026.1030.400"); got != "Upgrade Fluxer on chat.example.com to 2026.1030.400, then reload open pages." {
		t.Errorf("old release: %s", got)
	}
	if got := originFix(originPending, "chat.example.com", "2026.1030.400"); !strings.Contains(got, "rexctl voice status") {
		t.Errorf("pending: %s", got)
	}
	for _, c := range []struct {
		body   string
		listed bool
	}{
		{`{"endpoints":{"api_public":"https://chat.example.com/api"}}`, false},
		{`{"endpoints":{"api_public":"https://chat.example.com/api"},"voice_origins":[]}`, true},
	} {
		srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
			_, _ = w.Write([]byte(c.body))
		}))
		disc, err := (&voice.Client{}).Discover(context.Background(), srv.URL)
		srv.Close()
		if err != nil || disc.ListsOrigins != c.listed {
			t.Errorf("%s: %+v, %v", c.body, disc, err)
		}
	}
}

func TestVoiceAddListsTheProviderPorts(t *testing.T) {
	got := providerPortsText(&instance.VoiceServer{TCPPort: 7883, UDPPort: 7884})
	if !strings.Contains(got, "7883/tcp and 7884/udp") {
		t.Fatal(got)
	}
}

func TestATakenPinnedIDDoesNotAskForID(t *testing.T) {
	taken := &voice.APIError{Code: voice.CodeIDInUse}
	ee := refusal(t, pinnedJoinError(taken, "chat.example.com", "fra-server-1"))
	if !strings.Contains(ee.Msg, "fra-server-1") || strings.Contains(ee.Fix, "Pass --id") || !strings.Contains(ee.Fix, "rexctl voice token --id") {
		t.Fatalf("pinned: %+v", ee)
	}
	if ee := refusal(t, joinError(taken, "chat.example.com", "fra-server-1")); ee.Fix != "Pass --id with another ID." {
		t.Fatalf("unpinned: %+v", ee)
	}
	var exit *ui.ExitError
	if err := joinError(&voice.APIError{Code: "SOMETHING_ELSE", Message: "No"}, "chat.example.com", "fra-server-1"); !errors.As(err, &exit) || !strings.HasPrefix(exit.Msg, "chat.example.com refused the request (") {
		t.Fatalf("other refusal: %v", err)
	}
}

func TestJoinSummarySaysWhyItPickedTheRegion(t *testing.T) {
	km := 6200.0
	preview := func(source string) *voice.Preview {
		p := &voice.Preview{}
		p.Region.ID, p.Region.Name, p.Region.DistanceKm, p.Region.Source = "fra", "Frankfurt", &km, source
		return p
	}
	cases := map[string]string{
		"nearest":   "fra (Frankfurt), nearest existing region, 6200 km",
		"token":     "fra (Frankfurt), from the token, 6200 km away",
		"requested": "fra (Frankfurt), from --region, 6200 km away",
		"":          "fra (Frankfurt), 6200 km away",
	}
	for source, want := range cases {
		if got := joinRegionText(preview(source), ""); got != want {
			t.Errorf("%q: %s, want %s", source, got, want)
		}
	}
	if got := joinRegionText(preview(""), "fra"); got != "fra (Frankfurt), from --region, 6200 km away" {
		t.Errorf("an older API with --region: %s", got)
	}
}

func TestAFailedLocalTestSaysWhatToCheck(t *testing.T) {
	const hint = "Run rexctl doctor to check the ports"
	failed := []transportResult{{Transport: "udp", Err: errors.New("timeout")}}
	app, out := testVoiceApp()
	if reportTransports(app, failed, true) || strings.Count(out.String(), hint) != 1 {
		t.Fatalf("local failure: %s", out.String())
	}
	app, out = testVoiceApp()
	reportTransports(app, []transportResult{{Transport: "udp"}}, true)
	if strings.Contains(out.String(), hint) {
		t.Fatalf("a passing test asks for doctor: %s", out.String())
	}
	app, out = testVoiceApp()
	reportTransports(app, failed, false)
	if strings.Contains(out.String(), hint) {
		t.Fatalf("a remote test asks for doctor: %s", out.String())
	}
}
