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
	"net/http"
	"net/url"
	"strings"
	"sync"
	"time"

	"github.com/pion/webrtc/v4"
	"golang.org/x/net/websocket"
)

type Transport string

const (
	TransportUDP Transport = "udp"
	TransportTCP Transport = "tcp"
)

var (
	ErrTransportTimeout = errors.New("voice: no media path within the time limit")
	ErrTokenRejected    = errors.New("voice: the server rejected the access token")
)

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

func TestTransport(ctx context.Context, endpoint, token string, transport Transport, insecure bool, timeout time.Duration) error {
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
	ws, err := cfg.DialContext(ctx)
	if err != nil {
		return dialFailed(ctx, target, insecure, err)
	}
	defer ws.Close()

	se := webrtc.SettingEngine{}
	if transport == TransportTCP {
		se.SetNetworkTypes([]webrtc.NetworkType{webrtc.NetworkTypeTCP4})
	} else {
		se.SetNetworkTypes([]webrtc.NetworkType{webrtc.NetworkTypeUDP4})
	}
	se.SetICEMulticastDNSMode(0)
	api := webrtc.NewAPI(webrtc.WithSettingEngine(se))
	pc, err := api.NewPeerConnection(webrtc.Configuration{})
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

	joined := make(chan struct{})
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
				close(joined)
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

	select {
	case <-joined:
	case err := <-readErr:
		return fmt.Errorf("voice: signalling closed before joining: %w", err)
	case <-ctx.Done():
		return fmt.Errorf("voice: no join response: %w", ctx.Err())
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
			return noMediaPath(transport)
		case err := <-readErr:
			return signallingClosed(transport, haveAnswer, err)
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
	return fmt.Errorf("voice: signalling connection failed: %w", err)
}

func noMediaPath(transport Transport) error {
	return fmt.Errorf("voice: no %s media path to the server", strings.ToUpper(string(transport)))
}

func signallingClosed(transport Transport, answered bool, err error) error {
	if answered {
		return noMediaPath(transport)
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
