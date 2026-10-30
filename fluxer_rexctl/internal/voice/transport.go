// SPDX-License-Identifier: AGPL-3.0-or-later

package voice

import (
	"context"
	"crypto/rand"
	"crypto/tls"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"net/url"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/pion/webrtc/v4"
	"golang.org/x/net/websocket"
	"google.golang.org/protobuf/encoding/protowire"
)

type Transport string

const (
	TransportUDP Transport = "udp"
	TransportTCP Transport = "tcp"
)

type MediaPath struct {
	Transport Transport
	Family    string
	Relay     bool
}

func (p MediaPath) Name() string {
	family := "IPv4"
	if p.Family == FamilyIPv6 {
		family = "IPv6"
	}
	if p.Relay {
		return "IPv4 through TURN"
	}
	return strings.ToUpper(string(p.Transport)) + " over " + family
}

func (p MediaPath) networkTypes() []webrtc.NetworkType {
	switch {
	case p.Relay:
		return []webrtc.NetworkType{webrtc.NetworkTypeUDP4, webrtc.NetworkTypeUDP6}
	case p.Transport == TransportTCP && p.Family == FamilyIPv6:
		return []webrtc.NetworkType{webrtc.NetworkTypeTCP6}
	case p.Transport == TransportTCP:
		return []webrtc.NetworkType{webrtc.NetworkTypeTCP4}
	case p.Family == FamilyIPv6:
		return []webrtc.NetworkType{webrtc.NetworkTypeUDP6}
	}
	return []webrtc.NetworkType{webrtc.NetworkTypeUDP4}
}

func MediaPaths(server, tester string) []MediaPath {
	var out []MediaPath
	for _, fam := range []string{FamilyIPv4, FamilyIPv6} {
		if hasFamily(server, fam) && hasFamily(tester, fam) {
			out = append(out, MediaPath{TransportUDP, fam, false}, MediaPath{TransportTCP, fam, false})
		}
	}
	return out
}

func hasFamily(set, fam string) bool { return set == fam || set == FamilyDual }

var (
	ErrTransportTimeout = errors.New("voice: no media path within the time limit")
	ErrTokenRejected    = errors.New("voice: the server rejected the access token")
	ErrNoTurn           = errors.New("voice: the instance gives clients no TURN server over UDP or TCP")
	ErrSignalling       = errors.New("voice: signalling connection failed")
	errNotIPv4          = errors.New("voice: not an IPv4 address")
)

func ipv4Only(network, _ string, _ syscall.RawConn) error {
	if network != "tcp4" {
		return errNotIPv4
	}
	return nil
}

const TestRoomPrefix = "rexctl-test-"

func TestRoom() string {
	b := make([]byte, 6)
	_, _ = rand.Read(b)
	return TestRoomPrefix + hex.EncodeToString(b)
}

func TestToken(key, secret, room string, now time.Time) string {
	b := make([]byte, 4)
	_, _ = rand.Read(b)
	return SignHS256(secret, map[string]any{
		"iss": key,
		"sub": "rexctl-test-" + hex.EncodeToString(b),
		"nbf": now.Add(-10 * time.Second).Unix(),
		"exp": now.Add(time.Minute).Unix(),
		"video": map[string]any{
			"room":              room,
			"roomJoin":          true,
			"hidden":            true,
			"canPublish":        true,
			"canSubscribe":      true,
			"canPublishData":    true,
			"canPublishSources": []string{"microphone"},
		},
	})
}

type signalMessage struct {
	Offer   *webrtc.SessionDescription `json:"offer,omitempty"`
	Answer  *webrtc.SessionDescription `json:"answer,omitempty"`
	Trickle *struct {
		CandidateInit string `json:"candidateInit"`
		Target        any    `json:"target"`
	} `json:"trickle,omitempty"`
	Leave *struct{} `json:"leave,omitempty"`
}

type sdpJSON struct {
	Type string `json:"type"`
	SDP  string `json:"sdp"`
}

func rtcURL(endpoint, token string) (string, string, error) {
	u, err := url.Parse(endpoint)
	if err != nil {
		return "", "", err
	}
	u.Path = strings.TrimRight(u.Path, "/") + "/rtc"
	q := url.Values{}
	q.Set("access_token", token)
	q.Set("auto_subscribe", "0")
	q.Set("protocol", "9")
	q.Set("sdk", "go")
	q.Set("version", "1.0.0")
	u.RawQuery = q.Encode()
	origin := "https://" + u.Host
	if u.Scheme == "ws" {
		origin = "http://" + u.Host
	}
	return u.String(), origin, nil
}

func protoBytes(b []byte, num protowire.Number) [][]byte {
	var out [][]byte
	for len(b) > 0 {
		n, typ, l := protowire.ConsumeTag(b)
		if l < 0 {
			return out
		}
		b = b[l:]
		if typ == protowire.BytesType {
			v, m := protowire.ConsumeBytes(b)
			if m < 0 {
				return out
			}
			if n == num {
				out = append(out, v)
			}
			b = b[m:]
			continue
		}
		m := protowire.ConsumeFieldValue(n, typ, b)
		if m < 0 {
			return out
		}
		b = b[m:]
	}
	return out
}

func protoString(b []byte, num protowire.Number) string {
	values := protoBytes(b, num)
	if len(values) == 0 {
		return ""
	}
	return string(values[len(values)-1])
}

func relayServers(ctx context.Context, raw []byte) []webrtc.ICEServer {
	var out []webrtc.ICEServer
	for _, join := range protoBytes(raw, 1) {
		for _, s := range protoBytes(join, 5) {
			var urls []string
			for _, u := range protoBytes(s, 1) {
				if v4 := ipv4TurnURL(ctx, string(u)); v4 != "" {
					urls = append(urls, v4)
				}
			}
			if len(urls) > 0 {
				out = append(out, webrtc.ICEServer{URLs: urls, Username: protoString(s, 2), Credential: protoString(s, 3)})
			}
		}
	}
	return out
}

func ipv4TurnURL(ctx context.Context, raw string) string {
	rest, ok := strings.CutPrefix(raw, "turn:")
	if !ok {
		return ""
	}
	hostPort, query, _ := strings.Cut(rest, "?")
	h, port, err := net.SplitHostPort(hostPort)
	if err != nil {
		h, port = hostPort, "3478"
	}
	if !IsIPv4(h) {
		addrs, err := net.DefaultResolver.LookupNetIP(ctx, "ip4", h)
		if err != nil || len(addrs) == 0 {
			return ""
		}
		h = addrs[0].Unmap().String()
	}
	out := "turn:" + net.JoinHostPort(h, port)
	if query != "" {
		out += "?" + query
	}
	return out
}

func TestTransport(ctx context.Context, endpoint, token string, path MediaPath, insecure bool, timeout time.Duration) error {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	target, origin, err := rtcURL(endpoint, token)
	if err != nil {
		return err
	}
	cfg, err := websocket.NewConfig(target, origin)
	if err != nil {
		return err
	}
	cfg.TlsConfig = &tls.Config{InsecureSkipVerify: insecure}
	if path.Relay {
		cfg.Dialer = &net.Dialer{Control: ipv4Only}
	}
	ws, err := cfg.DialContext(ctx)
	if err != nil {
		return dialFailed(ctx, target, insecure, err)
	}
	defer ws.Close()

	joined := make(chan []byte, 1)
	answers := make(chan webrtc.SessionDescription, 1)
	candidates := make(chan webrtc.ICECandidateInit, 32)
	readErr := make(chan error, 1)
	go func() {
		first := true
		for {
			var raw []byte
			if err := websocket.Message.Receive(ws, &raw); err != nil {
				readErr <- err
				return
			}
			if first {
				first = false
				joined <- raw
				continue
			}
			var msg signalMessage
			if json.Unmarshal(raw, &msg) != nil {
				continue
			}
			switch {
			case msg.Answer != nil:
				answers <- *msg.Answer
			case msg.Trickle != nil && publisherTarget(msg.Trickle.Target):
				var init webrtc.ICECandidateInit
				if json.Unmarshal([]byte(msg.Trickle.CandidateInit), &init) == nil {
					candidates <- init
				}
			case msg.Leave != nil:
				readErr <- errors.New("voice: the server ended the session")
				return
			}
		}
	}()

	var join []byte
	select {
	case join = <-joined:
	case err := <-readErr:
		return fmt.Errorf("voice: signalling closed before joining: %w", err)
	case <-ctx.Done():
		return fmt.Errorf("voice: no join response: %w", ctx.Err())
	}

	conf := webrtc.Configuration{}
	if path.Relay {
		conf.ICEServers = relayServers(ctx, join)
		if len(conf.ICEServers) == 0 {
			return ErrNoTurn
		}
		conf.ICETransportPolicy = webrtc.ICETransportPolicyRelay
	}
	se := webrtc.SettingEngine{}
	se.SetNetworkTypes(path.networkTypes())
	se.SetICEMulticastDNSMode(0)
	api := webrtc.NewAPI(webrtc.WithSettingEngine(se))
	pc, err := api.NewPeerConnection(conf)
	if err != nil {
		return err
	}
	defer pc.Close()

	var sendMu sync.Mutex
	send := func(v any) error {
		data, err := json.Marshal(v)
		if err != nil {
			return err
		}
		sendMu.Lock()
		defer sendMu.Unlock()
		return websocket.Message.Send(ws, string(data))
	}

	connected := make(chan struct{})
	failed := make(chan struct{})
	var once sync.Once
	pc.OnICEConnectionStateChange(func(s webrtc.ICEConnectionState) {
		switch s {
		case webrtc.ICEConnectionStateConnected, webrtc.ICEConnectionStateCompleted:
			once.Do(func() { close(connected) })
		case webrtc.ICEConnectionStateFailed:
			once.Do(func() { close(failed) })
		}
	})
	pc.OnICECandidate(func(c *webrtc.ICECandidate) {
		if c == nil {
			return
		}
		init, err := json.Marshal(c.ToJSON())
		if err != nil {
			return
		}
		_ = send(map[string]any{"trickle": map[string]any{"candidateInit": string(init), "target": "PUBLISHER"}})
	})
	if _, err := pc.CreateDataChannel("_reliable", nil); err != nil {
		return err
	}

	offer, err := pc.CreateOffer(nil)
	if err != nil {
		return err
	}
	if err := pc.SetLocalDescription(offer); err != nil {
		return err
	}
	if err := send(map[string]any{"offer": sdpJSON{Type: "offer", SDP: offer.SDP}}); err != nil {
		return err
	}
	defer func() { _ = send(map[string]any{"leave": map[string]any{}}) }()

	var pending []webrtc.ICECandidateInit
	haveAnswer := false
	for {
		select {
		case answer := <-answers:
			if err := pc.SetRemoteDescription(answer); err != nil {
				return err
			}
			haveAnswer = true
			for _, c := range pending {
				_ = pc.AddICECandidate(c)
			}
			pending = nil
		case c := <-candidates:
			if !haveAnswer {
				pending = append(pending, c)
				continue
			}
			_ = pc.AddICECandidate(c)
		case <-connected:
			return nil
		case <-failed:
			return noMediaPath(path)
		case err := <-readErr:
			return signallingClosed(path, haveAnswer, err)
		case <-ctx.Done():
			return ErrTransportTimeout
		}
	}
}

func dialFailed(ctx context.Context, target string, insecure bool, err error) error {
	var de *websocket.DialError
	if errors.As(err, &de) {
		err = de.Err
	}
	u, perr := url.Parse(target)
	if perr == nil {
		u.Scheme = strings.Replace(u.Scheme, "ws", "http", 1)
		u.Path += "/validate"
		client := &http.Client{Timeout: 10 * time.Second, Transport: &http.Transport{TLSClientConfig: &tls.Config{InsecureSkipVerify: insecure}}}
		req, rerr := http.NewRequestWithContext(ctx, http.MethodGet, u.String(), nil)
		if rerr == nil {
			if resp, derr := client.Do(req); derr == nil {
				resp.Body.Close()
				if resp.StatusCode == http.StatusUnauthorized {
					return ErrTokenRejected
				}
			}
		}
	}
	return fmt.Errorf("%w: %w", ErrSignalling, err)
}

func noMediaPath(path MediaPath) error {
	return fmt.Errorf("voice: no media path to the server with %s", path.Name())
}

func signallingClosed(path MediaPath, answered bool, err error) error {
	if answered {
		return noMediaPath(path)
	}
	return fmt.Errorf("voice: signalling closed: %w", err)
}

func publisherTarget(v any) bool {
	switch t := v.(type) {
	case nil:
		return true
	case string:
		return t == "PUBLISHER"
	case float64:
		return t == 0
	}
	return false
}
