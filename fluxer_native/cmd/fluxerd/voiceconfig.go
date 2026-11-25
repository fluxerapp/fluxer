// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"net"
	"net/http"
	"net/url"
	"os"
	"regexp"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

var voiceIDPattern = regexp.MustCompile(`^[a-z0-9][a-z0-9-]{0,62}$`)

const (
	voiceStunPrimary   = "stun.l.google.com:19302"
	voiceStunSecondary = "stun1.l.google.com:19302"
	voiceTurnEnv       = "FLUXER_LIVEKIT_TURN_SERVERS"
)

type voiceConfig struct {
	ID      string
	Key     string
	Secret  string
	Webhook string
	TCPPort int
	UDPPort int
	NodeIP  string
	Turn    string
}

func (v voiceConfig) YAML() string {
	var b strings.Builder
	fmt.Fprintf(&b, "# LiveKit config for the Fluxer voice server %s.\n", v.ID)
	b.WriteString("port: 7880\nlog_level: info\n")
	fmt.Fprintf(&b, "rtc:\n  tcp_port: %d\n  udp_port: %d\n", v.TCPPort, v.UDPPort)
	if v.NodeIP != "" {
		fmt.Fprintf(&b, "  use_external_ip: false\n  node_ip: %q\n", v.NodeIP)
	} else {
		b.WriteString("  use_external_ip: true\n")
	}
	fmt.Fprintf(&b, "  stun_servers:\n    - %s\n    - %s\n", voiceStunPrimary, voiceStunSecondary)
	if t := strings.TrimSpace(v.Turn); t != "" && t != "[]" {
		fmt.Fprintf(&b, "  turn_servers: %s\n", t)
	}
	b.WriteString("turn:\n  enabled: false\n")
	fmt.Fprintf(&b, "keys:\n  %s: %s\n", v.Key, v.Secret)
	fmt.Fprintf(&b, "webhook:\n  api_key: %s\n  urls:\n    - %s\n", v.Key, v.Webhook)
	return b.String()
}

type adminServer struct {
	ServerID string `json:"server_id"`
}

type adminRegion struct {
	ID        string        `json:"id"`
	IsDefault bool          `json:"is_default"`
	Servers   []adminServer `json:"servers"`
}

func pickVoiceRegion(regions []adminRegion, want string) (adminRegion, error) {
	if want != "" {
		for _, r := range regions {
			if r.ID == want {
				return r, nil
			}
		}
		return adminRegion{}, fmt.Errorf("there's no voice region called %s. Create it in the admin panel first", want)
	}
	for _, r := range regions {
		if r.IsDefault {
			return r, nil
		}
	}
	if len(regions) > 0 {
		return regions[0], nil
	}
	return adminRegion{}, errors.New("the instance has no voice regions. Create one in the admin panel first")
}

func registeredRegion(regions []adminRegion, id string, bundled bool) (adminRegion, bool, error) {
	for _, r := range regions {
		for _, sv := range r.Servers {
			if sv.ServerID != id {
				continue
			}
			if bundled && r.IsDefault && id == r.ID+"-server-1" {
				return adminRegion{}, false, fmt.Errorf("%s is the voice server inside Fluxer, so voice-config can't give it a new config", id)
			}
			return r, true, nil
		}
	}
	return adminRegion{}, false, nil
}

func voiceServerBody(v voiceConfig, endpoint string, o voiceOptions, update bool) map[string]any {
	body := map[string]any{"endpoint": endpoint, "api_key": v.Key, "api_secret": v.Secret}
	if !update {
		yes := true
		body["server_id"] = v.ID
		body["is_active"], body["draining"] = &yes, &yes
		body["notes"] = "Added with fluxerd voice-config on " + o.now.UTC().Format("2006-01-02")
	}
	if v.NodeIP != "" {
		body["public_ipv4"] = v.NodeIP
	}
	if o.coords {
		body["latitude"], body["longitude"] = o.lat, o.long
	}
	if o.softLimit > 0 {
		body["soft_connection_limit"] = o.softLimit
	}
	return body
}

type voiceOptions struct {
	coords    bool
	lat, long float64
	softLimit int
	now       time.Time
}

func nextVoiceID(region string, regions []adminRegion) string {
	var taken []string
	for _, r := range regions {
		for _, s := range r.Servers {
			taken = append(taken, s.ServerID)
		}
	}
	for n := 1; ; n++ {
		id := fmt.Sprintf("%s-server-%d", region, n)
		if !slices.Contains(taken, id) {
			return id
		}
	}
}

func checkVoiceEndpoint(endpoint string) error {
	u, err := url.Parse(endpoint)
	if err != nil || u.Scheme != "wss" || u.Hostname() == "" {
		return fmt.Errorf("--endpoint must be a wss:// URL, such as wss://voice.example.com, not %q", endpoint)
	}
	return nil
}

func cmdVoiceConfig(args []string) error {
	fs := flag.NewFlagSet("voice-config", flag.ContinueOnError)
	cf := addConfigFlags(fs)
	endpoint := fs.String("endpoint", "", "public wss:// URL of the LiveKit server, behind your proxy's certificate")
	id := fs.String("id", "", "server ID (default <region>-server-<n>), or a registered one for a new config with a new secret")
	region := fs.String("region", "", "voice region (default the instance's default region)")
	tcpPort := fs.Int("tcp-port", spec.PortLiveKitRTCTCP, "TCP port for media")
	udpPort := fs.Int("udp-port", spec.PortLiveKitRTCUDP, "UDP port for media")
	nodeIP := fs.String("ip", "", "public IPv4 address LiveKit gives to clients (default found with STUN)")
	lat := fs.Float64("lat", 0, "latitude of the server, for picking the nearest one")
	long := fs.Float64("long", 0, "longitude of the server, for picking the nearest one")
	softLimit := fs.Int("soft-limit", 0, "connections after which calls prefer other servers")
	if err := fs.Parse(args); err != nil {
		return exitError(2)
	}
	if err := checkVoiceEndpoint(*endpoint); err != nil {
		return err
	}
	if *id != "" && !voiceIDPattern.MatchString(*id) {
		return fmt.Errorf("%q isn't a server ID. Use lowercase letters, digits and dashes", *id)
	}
	for _, p := range []int{*tcpPort, *udpPort} {
		if p < 1 || p > 65535 {
			return fmt.Errorf("pass a port from 1 to 65535, not %d", p)
		}
	}
	if ip := net.ParseIP(*nodeIP); *nodeIP != "" && (ip == nil || ip.To4() == nil) {
		return fmt.Errorf("--ip must be an IPv4 address, not %q", *nodeIP)
	}
	if *softLimit < 0 {
		return fmt.Errorf("--soft-limit must be 0 or more, not %d", *softLimit)
	}
	coords := false
	fs.Visit(func(f *flag.Flag) { coords = coords || f.Name == "lat" || f.Name == "long" })
	cfg, err := cf.load()
	if err != nil {
		return err
	}
	if voiceOnly(cfg) {
		return errors.New("run voice-config on the Fluxer machine, not on a voice server")
	}
	ctx, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()
	op := operatorClient(cfg.Layout.OperatorSocket())
	op.place = cf.runtime
	webhook, err := publicWebhook(ctx)
	if err != nil {
		return err
	}
	var list struct {
		Regions []adminRegion `json:"regions"`
	}
	if err := op.call(ctx, http.MethodGet, "/admin/voice/regions?include_servers=true", nil, &list); err != nil {
		return err
	}
	r, update, err := registeredRegion(list.Regions, *id, cfg.Env["FLUXER_LIVEKIT_ENABLED"] != "false" && cfg.Env["FLUXER_LIVEKIT_BUNDLED"] != "false")
	if err != nil {
		return err
	}
	if update && *region != "" && *region != r.ID {
		return fmt.Errorf("%s is in region %s. Remove it in the admin panel to move it to %s", *id, r.ID, *region)
	}
	if !update {
		if r, err = pickVoiceRegion(list.Regions, *region); err != nil {
			return err
		}
	}
	serverID := *id
	if serverID == "" {
		serverID = nextVoiceID(r.ID, list.Regions)
	}
	secret := make([]byte, 32)
	if _, err := rand.Read(secret); err != nil {
		return err
	}
	v := voiceConfig{
		ID: serverID, Key: "fluxer-" + serverID, Secret: hex.EncodeToString(secret), Webhook: webhook,
		TCPPort: *tcpPort, UDPPort: *udpPort, NodeIP: *nodeIP, Turn: cfg.Env[voiceTurnEnv],
	}
	body := voiceServerBody(v, *endpoint, voiceOptions{coords: coords, lat: *lat, long: *long, softLimit: *softLimit, now: time.Now()}, update)
	servers := "/admin/voice/regions/" + url.PathEscape(r.ID) + "/servers"
	if update {
		if err := op.call(ctx, http.MethodPatch, servers+"/"+url.PathEscape(v.ID), body, nil); err != nil {
			return fmt.Errorf("updating %s: %w", v.ID, err)
		}
		fmt.Print(v.YAML())
		fmt.Fprintf(os.Stderr, "Gave %s in region %s a new secret. Restart LiveKit there with this config, because the old one no longer works.\n", v.ID, r.ID)
		return nil
	}
	if err := op.call(ctx, http.MethodPost, servers, body, nil); err != nil {
		return fmt.Errorf("registering %s: %w", v.ID, err)
	}
	fmt.Print(v.YAML())
	fmt.Fprintf(os.Stderr, "Registered %s in region %s at %s. It stays draining, so no call goes to it yet.\n", v.ID, r.ID, *endpoint)
	fmt.Fprintf(os.Stderr, "Run LiveKit with this config, put it behind a proxy with a certificate for %s, and open %d/tcp and %d/udp.\n", hostOf(*endpoint), v.TCPPort, v.UDPPort)
	fmt.Fprintf(os.Stderr, "Then turn off draining for %s in the admin panel, under Voice.\n", v.ID)
	if v.Turn != "" && v.Turn != "[]" {
		fmt.Fprintln(os.Stderr, "The config holds your TURN servers. When you change them, run voice-config again with --id "+v.ID+" for a new config.")
	}
	return nil
}

func hostOf(endpoint string) string {
	u, _ := url.Parse(endpoint)
	return u.Hostname()
}

func publicWebhook(ctx context.Context) (string, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, spec.URL(spec.PortAPI)+"/.well-known/fluxer", nil)
	if err != nil {
		return "", err
	}
	req.Header.Set("X-Forwarded-For", spec.Loopback)
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", fmt.Errorf("Fluxer's API isn't reachable on this machine (%w). Run voice-config where Fluxer runs", err)
	}
	defer resp.Body.Close()
	var doc struct {
		Endpoints struct {
			APIPublic string `json:"api_public"`
		} `json:"endpoints"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&doc); err != nil || doc.Endpoints.APIPublic == "" {
		return "", fmt.Errorf("Fluxer's discovery document has no public API address (HTTP %d)", resp.StatusCode)
	}
	return strings.TrimRight(doc.Endpoints.APIPublic, "/") + "/webhooks/livekit", nil
}
