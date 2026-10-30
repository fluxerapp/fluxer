// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func webhookReceiver(t *testing.T, secret string) *httptest.Server {
	t.Helper()
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		parts := strings.Split(r.Header.Get("Authorization"), ".")
		if len(parts) != 3 {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		mac := hmac.New(sha256.New, []byte(secret))
		mac.Write([]byte(parts[0] + "." + parts[1]))
		if base64.RawURLEncoding.EncodeToString(mac.Sum(nil)) != parts[2] {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		payload, _ := base64.RawURLEncoding.DecodeString(parts[1])
		var claims map[string]any
		_ = json.Unmarshal(payload, &claims)
		sum := sha256.Sum256(body)
		var event struct {
			Room struct{ Name string } `json:"room"`
		}
		_ = json.Unmarshal(body, &event)
		if claims["sha256"] != base64.StdEncoding.EncodeToString(sum[:]) || claims["iss"] != "fluxer-fra-server-1" || !strings.HasPrefix(event.Room.Name, voice.TestRoomPrefix) {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		w.WriteHeader(http.StatusOK)
	}))
}

func TestWebhookCheckSendsASignedTestEvent(t *testing.T) {
	srv := webhookReceiver(t, "right")
	defer srv.Close()
	client := &voice.Client{HTTP: srv.Client()}
	s := instance.VoiceServer{ID: "fra-server-1", Key: "fluxer-fra-server-1", Secret: "right"}
	if problem, _ := webhookProblem(context.Background(), client, s, srv.URL); problem != "" {
		t.Errorf("right secret: %s", problem)
	}
	s.Secret = "wrong"
	problem, fault := webhookProblem(context.Background(), client, s, srv.URL)
	if fault != webhookSecret || !strings.Contains(problem, "signed with the secret of fra-server-1") {
		t.Errorf("wrong secret: %q, %v", problem, fault)
	}
}

func TestWebhookTargetForServersOnTheFluxerMachine(t *testing.T) {
	inst := &instance.Instance{State: &instance.State{Settings: map[string]string{}}}
	inst.State.Settings["FLUXER_PUBLIC_ORIGIN"] = "https://chat.example.com"
	e := &Env{Instance: inst}
	if got := e.webhookTarget(instance.VoiceServer{Webhook: "http://api:8080/webhooks/livekit"}); got != "https://chat.example.com/api/webhooks/livekit" {
		t.Errorf("same machine: %s", got)
	}
	if got := e.webhookTarget(instance.VoiceServer{Webhook: "https://chat.example.com/api/webhooks/livekit"}); got != "https://chat.example.com/api/webhooks/livekit" {
		t.Errorf("joined: %s", got)
	}
}

func TestVoiceEndpointsCheckSeesAnOldDomain(t *testing.T) {
	inst := &instance.Instance{State: &instance.State{Settings: map[string]string{"FLUXER_PUBLIC_ORIGIN": "https://new.example.com"}}}
	inst.State.Voice = []instance.VoiceServer{{ID: "default-server-2", Endpoint: "wss://old.example.com/livekit-default-server-2", Edge: voice.EdgeInstance}}
	r := runVoiceEndpoints(context.Background(), &Env{Instance: inst})
	if r.Status != Fail || !r.Fixable || !strings.Contains(r.Message, "default-server-2") || !strings.Contains(r.Fix, "wss://new.example.com") {
		t.Fatalf("old domain: %+v", r)
	}
	inst.State.Voice[0].Endpoint = "wss://new.example.com/livekit-default-server-2"
	if r := runVoiceEndpoints(context.Background(), &Env{Instance: inst}); r.Status != Pass {
		t.Fatalf("new domain: %+v", r)
	}
}

func TestWebhookCheckReportsAClockThatIsOff(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Date", time.Now().Add(-3*time.Minute).UTC().Format(http.TimeFormat))
		w.WriteHeader(http.StatusUnauthorized)
	}))
	defer srv.Close()
	client := &voice.Client{HTTP: srv.Client()}
	s := instance.VoiceServer{ID: "fra-server-1", Key: "fluxer-fra-server-1", Secret: "right"}
	problem, fault := webhookProblem(context.Background(), client, s, srv.URL)
	if fault != webhookClock || !strings.Contains(problem, "about 3 minutes ahead") {
		t.Fatalf("clock ahead: %q, %v", problem, fault)
	}
}
