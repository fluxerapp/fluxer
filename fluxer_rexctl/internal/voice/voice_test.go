// SPDX-License-Identifier: AGPL-3.0-or-later

package voice

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"errors"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/compose-spec/compose-go/v2/template"
	"go.yaml.in/yaml/v4"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestPickPortsSkipsEveryTakenPort(t *testing.T) {
	taken := map[host.Port]bool{
		{Number: 7881, Proto: "tcp"}: true,
		{Number: 7884, Proto: "udp"}: true,
	}
	busy := func(n int, proto string) bool { return !(n == 7885 && proto == "tcp") }
	tcp, udp := PickPorts(taken, busy)
	if tcp != 7887 || udp != 7888 {
		t.Fatalf("got %d/%d, want 7887/7888", tcp, udp)
	}
}

func TestNextIDIsUniqueAcrossRegions(t *testing.T) {
	if got := NextID("fra", []string{"fra-server-1", "default-server-2", "fra-server-3"}); got != "fra-server-2" {
		t.Fatalf("got %s", got)
	}
}

func TestEnvSetsExternalIPOnlyWithoutNodeIP(t *testing.T) {
	e := Env{ID: "fra-server-1", Key: "fluxer-fra-server-1", Secret: "s", WebhookURL: "http://api:8080/webhooks/livekit", Network: "fluxer_fluxer", TCPPort: 7883, UDPPort: 7884, Memory: "2g"}
	v := e.Values()
	if v["FLUXER_VOICE_USE_EXTERNAL_IP"] != "true" || v["FLUXER_VOICE_GOMEMLIMIT"] != "1740MiB" || v["FLUXER_LIVEKIT_IMAGE"] != DefaultImage {
		t.Fatalf("unexpected values %v", v)
	}
	e.NodeIP = "198.51.100.8"
	if e.Values()["FLUXER_VOICE_USE_EXTERNAL_IP"] != "false" {
		t.Fatal("node IP must turn off external IP discovery")
	}
	if _, ok := e.Values()["FLUXER_VOICE_HTTP_PORT"]; ok {
		t.Fatal("no HTTP port without --no-edge")
	}
	if !strings.Contains(string(e.File()), "FLUXER_VOICE_API_SECRET=s\n") {
		t.Fatal("env file misses the secret")
	}
}

func TestShippedComposeRendersWithTheEnv(t *testing.T) {
	data, err := ShippedFile(stack.Embedded(), ComposeFile)
	if err != nil {
		t.Fatal(err)
	}
	e := Env{ID: "fra-server-1", Key: "k", Secret: "s", WebhookURL: "http://api:8080/webhooks/livekit", Network: "fluxer_fluxer", TCPPort: 7883, UDPPort: 7884, NodeIP: "198.51.100.8", BindIP: "198.51.100.8"}
	values := e.Values()
	out, err := template.Substitute(string(data), func(k string) (string, bool) {
		v, ok := values[k]
		return v, ok
	})
	if err != nil {
		t.Fatal(err)
	}
	var doc struct {
		Services map[string]struct {
			Ports    []string `yaml:"ports"`
			Networks map[string]struct {
				Aliases []string `yaml:"aliases"`
			} `yaml:"networks"`
			Environment map[string]string `yaml:"environment"`
		} `yaml:"services"`
		Networks map[string]struct {
			Name string `yaml:"name"`
		} `yaml:"networks"`
	}
	if err := yaml.Unmarshal([]byte(out), &doc); err != nil {
		t.Fatal(err)
	}
	svc := doc.Services["livekit"]
	if !slices.Equal(svc.Ports, []string{"198.51.100.8:7883:7883/tcp", "198.51.100.8:7884:7884/udp"}) {
		t.Fatalf("ports %v", svc.Ports)
	}
	if !slices.Equal(svc.Networks["voice"].Aliases, []string{"voice-fra-server-1"}) || doc.Networks["voice"].Name != "fluxer_fluxer" {
		t.Fatalf("network %+v %+v", svc.Networks, doc.Networks)
	}
	for _, want := range []string{"node_ip: \"198.51.100.8\"", "use_external_ip: false", "tcp_port: 7883", "- http://api:8080/webhooks/livekit"} {
		if !strings.Contains(svc.Environment["LIVEKIT_CONFIG"], want) {
			t.Fatalf("LIVEKIT_CONFIG misses %q:\n%s", want, svc.Environment["LIVEKIT_CONFIG"])
		}
	}
	if svc.Environment["LIVEKIT_KEYS"] != "k: s" {
		t.Fatalf("keys %q", svc.Environment["LIVEKIT_KEYS"])
	}
}

func TestEdgeFiles(t *testing.T) {
	if got := string(DropIn("fra-server-2")); got != "handle_path /livekit-fra-server-2/* {\n\timport upstream voice-fra-server-2:7880\n}\n" {
		t.Fatalf("drop-in %q", got)
	}
	site := string(SiteFile("203.0.113.7", []string{"b", "a"}, true))
	for _, want := range []string{"203.0.113.7 {", "profile shortlived", "handle_path /livekit-a/*", "import livekit voice-b:7880", "respond 404"} {
		if !strings.Contains(site, want) {
			t.Fatalf("site misses %q:\n%s", want, site)
		}
	}
	if strings.Index(site, "/livekit-a/") > strings.Index(site, "/livekit-b/") {
		t.Fatal("routes are not sorted")
	}
	if strings.Contains(string(SiteFile("voice.example.com", []string{"a"}, false)), "issuer") {
		t.Fatal("a hostname site must use Caddy's default issuers")
	}
	if Endpoint("203.0.113.7", "a") != "wss://203.0.113.7/livekit-a" {
		t.Fatal("endpoint")
	}
}

func TestSizing(t *testing.T) {
	if MachineMemory(1<<30) != "512m" || MachineMemory(4<<30) != "2048m" || MachineMemory(64<<30) != "8192m" {
		t.Fatal("machine memory must be half the host, between 512 MB and 8 GB")
	}
	if SoftLimit(4) != 300 || SoftLimit(0) != 75 {
		t.Fatal("soft limit")
	}
	if n, ok := ParseBytes("1536m"); !ok || n != 1536<<20 {
		t.Fatal("parse")
	}
}

func TestCheckHostname(t *testing.T) {
	cases := []struct {
		answer DNSAnswer
		want   HostnameCheck
	}{
		{DNSAnswer{}, HostnameMissing},
		{DNSAnswer{A: []string{"203.0.113.7"}}, HostnameOK},
		{DNSAnswer{A: []string{"203.0.113.9"}}, HostnameElsewhere},
		{DNSAnswer{A: []string{"104.16.1.1", "172.67.1.1"}}, HostnameProxied},
		{DNSAnswer{A: []string{"203.0.113.7"}, AAAA: []string{"2001:db8::9"}}, HostnameIPv6Elsewhere},
		{DNSAnswer{A: []string{"203.0.113.7"}, AAAA: []string{"2001:db8::7"}}, HostnameOK},
	}
	for _, c := range cases {
		if got := CheckHostname(c.answer, "203.0.113.7", []string{"2001:db8::7"}); got != c.want {
			t.Errorf("%+v: got %d, want %d", c.answer, got, c.want)
		}
	}
}

func TestReachability(t *testing.T) {
	if r := Assess("203.0.113.7", []string{"10.0.0.4"}, "10.0.0.4"); !r.BehindNAT || r.CGNAT {
		t.Fatalf("%+v", r)
	}
	if r := Assess("203.0.113.7", []string{"100.64.3.2"}, "100.64.3.2"); !r.CGNAT {
		t.Fatalf("%+v", r)
	}
	if r := Assess("203.0.113.7", []string{"203.0.113.7", "100.100.1.1"}, "203.0.113.7"); r.BehindNAT || r.CGNAT {
		t.Fatalf("tailscale on another interface is fine: %+v", r)
	}
}

func TestValidToken(t *testing.T) {
	good := "fvj_123_" + strings.Repeat("a", 43)
	if !ValidToken(good) || ValidToken("fvj_x_"+strings.Repeat("a", 43)) || ValidToken("flx_123") {
		t.Fatal("token format")
	}
}

func verifyNodeToken(t *testing.T, header, secret, method string, body []byte) map[string]any {
	t.Helper()
	token := strings.TrimPrefix(header, "Bearer ")
	parts := strings.Split(token, ".")
	mac := hmac.New(sha256.New, []byte(secret))
	mac.Write([]byte(parts[0] + "." + parts[1]))
	if base64.RawURLEncoding.EncodeToString(mac.Sum(nil)) != parts[2] {
		t.Fatal("bad signature")
	}
	payload, _ := base64.RawURLEncoding.DecodeString(parts[1])
	var claims map[string]any
	_ = json.Unmarshal(payload, &claims)
	digest := ""
	if len(body) > 0 {
		sum := sha256.Sum256(body)
		digest = base64.StdEncoding.EncodeToString(sum[:])
	}
	if claims["aud"] != NodeAudience || claims["method"] != method || claims["path"] != NodePath || claims["sha256"] != digest {
		t.Fatalf("claims %v", claims)
	}
	return claims
}

func TestNodeClientSignsEveryRequest(t *testing.T) {
	var seen []string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		claims := verifyNodeToken(t, r.Header.Get("Authorization"), "secret", r.Method, body)
		if claims["iss"] != "fluxer-a" {
			t.Fatalf("iss %v", claims["iss"])
		}
		seen = append(seen, r.Method+" "+string(body))
		if r.Method == http.MethodDelete {
			w.WriteHeader(http.StatusNoContent)
			return
		}
		_, _ = w.Write([]byte(`{"server_id":"a","region_id":"fra","endpoint":"wss://x/livekit-a","is_active":true,"draining":true,"health":{"status":"up"}}`))
	}))
	defer srv.Close()
	c := &Client{HTTP: srv.Client()}
	ctx := context.Background()
	if n, err := c.GetNode(ctx, srv.URL, "fluxer-a", "secret"); err != nil || n.Health.Status != "up" {
		t.Fatal(err)
	}
	draining := true
	if _, err := c.UpdateNode(ctx, srv.URL, "fluxer-a", "secret", NodeUpdate{Draining: &draining}); err != nil {
		t.Fatal(err)
	}
	if err := c.DeleteNode(ctx, srv.URL, "fluxer-a", "secret"); err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(seen, []string{"GET ", `PATCH {"draining":true}`, "DELETE "}) {
		t.Fatalf("requests %v", seen)
	}
}

func TestJoinClientErrors(t *testing.T) {
	for _, code := range []string{CodeTokenInvalid, CodeTokenBusy, CodeEndpointUnreachable, CodeKeyMismatch, CodeIDInUse, CodeKeyInUse} {
		srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.Header.Get("Authorization") != "VoiceJoin fvj_1_t" {
				t.Fatalf("auth %q", r.Header.Get("Authorization"))
			}
			w.WriteHeader(http.StatusConflict)
			_, _ = w.Write([]byte(`{"code":"` + code + `","message":"no","detail":"HTTP 502"}`))
		}))
		_, err := (&Client{HTTP: srv.Client()}).Join(context.Background(), srv.URL, "fvj_1_t", JoinRequest{})
		srv.Close()
		if CodeOf(err) != code || !strings.Contains(err.Error(), "HTTP 502") {
			t.Fatalf("%s: %v", code, err)
		}
	}
}

func TestDiscover(t *testing.T) {
	srv := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/.well-known/fluxer" {
			t.Fatalf("path %s", r.URL.Path)
		}
		_, _ = w.Write([]byte(`{"api_code_version":7,"endpoints":{"api_public":"https://chat.example.com/api/"},"features":{"voice_enabled":true},"voice_origins":["wss://v.example.com"]}`))
	}))
	defer srv.Close()
	d, err := (&Client{HTTP: srv.Client()}).Discover(context.Background(), srv.URL)
	if err != nil || d.APIPublic != "https://chat.example.com/api" || !d.VoiceEnabled || d.VoiceOrigins[0] != "wss://v.example.com" {
		t.Fatalf("%+v %v", d, err)
	}
}

func TestTestTokenIsHiddenAndShort(t *testing.T) {
	tok := TestToken("k", "s", "rexctl-test-abc", time.Unix(1000, 0))
	payload, _ := base64.RawURLEncoding.DecodeString(strings.Split(tok, ".")[1])
	var claims struct {
		Exp   int64          `json:"exp"`
		Video map[string]any `json:"video"`
	}
	_ = json.Unmarshal(payload, &claims)
	if claims.Exp != 1060 || claims.Video["hidden"] != true || claims.Video["room"] != "rexctl-test-abc" {
		t.Fatalf("%+v", claims)
	}
}

func TestClosedSignallingAfterTheAnswerMeansNoMediaPath(t *testing.T) {
	eof := io.EOF
	if got := signallingClosed(TransportUDP, true, eof).Error(); got != "voice: no UDP media path to the server" {
		t.Errorf("after the answer: %q", got)
	}
	if got := signallingClosed(TransportTCP, false, eof).Error(); !strings.Contains(got, "signalling closed") {
		t.Errorf("before the answer: %q", got)
	}
}

func TestRejectedTokenIsReportedAndNotPrinted(t *testing.T) {
	srv := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "invalid token", http.StatusUnauthorized)
	}))
	defer srv.Close()
	endpoint := "wss://" + strings.TrimPrefix(srv.URL, "https://") + "/livekit-x"
	err := TestTransport(context.Background(), endpoint, "secret-token-value", TransportUDP, true, 5*time.Second)
	if !errors.Is(err, ErrTokenRejected) {
		t.Fatalf("got %v", err)
	}
	if strings.Contains(err.Error(), "secret-token-value") {
		t.Errorf("the error shows the token: %v", err)
	}
}

func TestFailedDialHidesTheToken(t *testing.T) {
	srv := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "gone", http.StatusBadGateway)
	}))
	defer srv.Close()
	endpoint := "wss://" + strings.TrimPrefix(srv.URL, "https://") + "/livekit-x"
	err := TestTransport(context.Background(), endpoint, "secret-token-value", TransportTCP, true, 5*time.Second)
	if err == nil || errors.Is(err, ErrTokenRejected) || strings.Contains(err.Error(), "secret-token-value") {
		t.Errorf("got %v", err)
	}
}

func TestConnectSrcAllows(t *testing.T) {
	csp := []string{"default-src 'self'; connect-src 'self' wss://voice.example.com https://voice.example.com; img-src *"}
	if !ConnectSrcAllows(csp, "wss://voice.example.com") {
		t.Error("listed origin refused")
	}
	if ConnectSrcAllows(csp, "wss://other.example.com") || ConnectSrcAllows([]string{"img-src wss://voice.example.com"}, "wss://voice.example.com") {
		t.Error("unlisted origin allowed")
	}
}

func TestLocalProbesSpeakPlainHTTPWithoutThePublicPath(t *testing.T) {
	var paths []string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		paths = append(paths, r.URL.Path)
		switch r.URL.Path {
		case "/":
			_, _ = io.WriteString(w, "OK")
		case "/twirp/livekit.RoomService/ListRooms":
			_, _ = io.WriteString(w, `{"rooms":[{"name":"guild_1_channel_2","num_participants":3},{"name":"`+TestRoomPrefix+`x","num_participants":1}]}`)
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()
	port := srv.Listener.Addr().(*net.TCPAddr).Port
	base := LocalBase(port)
	if err := ProbeThroughEdge(context.Background(), base, "", true); err != nil {
		t.Fatalf("probe: %v", err)
	}
	load, err := ListRooms(context.Background(), base, "", "key", "secret", true)
	if err != nil {
		t.Fatalf("list rooms: %v", err)
	}
	if load != (RoomLoad{Rooms: 1, Participants: 3}) {
		t.Fatalf("load = %+v", load)
	}
	if !slices.Equal(paths, []string{"/", "/twirp/livekit.RoomService/ListRooms"}) {
		t.Fatalf("paths = %v", paths)
	}
}

func TestHTTPBaseKeepsThePathOfTheEndpoint(t *testing.T) {
	for in, want := range map[string]string{
		"wss://chat.example.com/livekit-fra-server-1/": "https://chat.example.com/livekit-fra-server-1",
		"ws://127.0.0.1:8080/livekit-x":                "http://127.0.0.1:8080/livekit-x",
	} {
		if got := HTTPBase(in); got != want {
			t.Fatalf("HTTPBase(%q) = %q, want %q", in, got, want)
		}
	}
}

func TestInstanceEdgeFollowsTheEdgeMode(t *testing.T) {
	const endpoint = "wss://chat.example.com/livekit-fra-server-1"
	cases := []struct {
		mode, port, bind, base, addr string
	}{
		{"", "", "", "https://chat.example.com/livekit-fra-server-1", "127.0.0.1:443"},
		{"", "8443", "", "https://chat.example.com/livekit-fra-server-1", "127.0.0.1:8443"},
		{"", "${FLUXER_HTTPS_PORT}", "", "https://chat.example.com/livekit-fra-server-1", "127.0.0.1:443"},
		{"proxy", "443", "", "http://chat.example.com/livekit-fra-server-1", "127.0.0.1:8080"},
		{"proxy", "443", "0.0.0.0:9000", "http://chat.example.com/livekit-fra-server-1", "127.0.0.1:9000"},
		{"proxy", "443", "[::]:9000", "http://chat.example.com/livekit-fra-server-1", "127.0.0.1:9000"},
		{"proxy", "443", "10.0.0.5:8080", "http://chat.example.com/livekit-fra-server-1", "10.0.0.5:8080"},
		{"tunnel", "443", "", "http://chat.example.com/livekit-fra-server-1", "127.0.0.1:80"},
		{"tunnel", "443", "127.0.0.1:8081", "http://chat.example.com/livekit-fra-server-1", "127.0.0.1:8081"},
	}
	for _, c := range cases {
		base, addr := InstanceEdge(endpoint, c.mode, c.port, c.bind)
		if base != c.base || addr != c.addr {
			t.Errorf("InstanceEdge(%q, %q, %q) = %s, %s, want %s, %s", c.mode, c.port, c.bind, base, addr, c.base, c.addr)
		}
	}
}

func TestProxyModeProbeSpeaksPlainHTTPWithTheDomain(t *testing.T) {
	var hosts, paths []string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		hosts, paths = append(hosts, r.Host), append(paths, r.URL.Path)
		_, _ = io.WriteString(w, "OK")
	}))
	defer srv.Close()
	base, addr := InstanceEdge("wss://chat.example.com/livekit-fra-server-1", "proxy", "443", srv.Listener.Addr().String())
	if err := ProbeThroughEdge(context.Background(), base, addr, false); err != nil {
		t.Fatalf("probe: %v", err)
	}
	if !slices.Equal(hosts, []string{"chat.example.com"}) || !slices.Equal(paths, []string{"/livekit-fra-server-1/"}) {
		t.Fatalf("hosts = %v, paths = %v", hosts, paths)
	}
}

func TestAClockProblemIsNotAWrongSecret(t *testing.T) {
	now := time.Date(2026, 10, 30, 12, 0, 0, 0, time.UTC)
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Date", now.Add(-3*time.Minute).Format(http.TimeFormat))
		w.WriteHeader(http.StatusUnauthorized)
		_, _ = io.WriteString(w, `{"code":"VOICE_NODE_TOKEN_EXPIRED","message":"expired"}`)
	}))
	defer srv.Close()
	client := &Client{HTTP: srv.Client()}
	_, err := client.GetNode(context.Background(), srv.URL, "fluxer-fra-server-1", "secret")
	if CodeOf(err) != CodeTokenExpired {
		t.Fatalf("code = %q", CodeOf(err))
	}
	if got := ClockProblem(err, "chat.example.com", now); got != "The clock on this machine is about 3 minutes ahead of chat.example.com" {
		t.Fatalf("ahead: %q", got)
	}
	if got := ClockProblem(err, "chat.example.com", now.Add(-6*time.Minute)); got != "The clock on this machine is about 3 minutes behind chat.example.com" {
		t.Fatalf("behind: %q", got)
	}
	if got := ClockProblem(&APIError{Code: CodeSignatureInvalid}, "chat.example.com", now); got != "" {
		t.Fatalf("wrong secret: %q", got)
	}
}

func TestJoinedServersUseTheMachineProjectPrefix(t *testing.T) {
	if got := JoinedProject("fra-server-1"); got != "fluxer-voice-fra-server-1" {
		t.Fatalf("JoinedProject = %s", got)
	}
	if JoinedProject(EdgeID) != EdgeProject {
		t.Fatal("the edge ID no longer matches the edge project")
	}
}
