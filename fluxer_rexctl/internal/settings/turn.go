// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"math"
	"slices"
	"strings"
	"unicode"
)

const TurnServersVar = "FLUXER_LIVEKIT_TURN_SERVERS"

type TurnServer struct {
	Host       string `json:"host"`
	Port       int    `json:"port"`
	Protocol   string `json:"protocol"`
	Username   string `json:"username,omitempty"`
	Credential string `json:"credential,omitempty"`
	Secret     string `json:"secret,omitempty"`
	TTL        int    `json:"ttl,omitempty"`
}

var turnFields = []string{"host", "port", "protocol", "username", "credential", "secret", "ttl"}

var errTurnList = errors.New("Give the TURN servers as one JSON array of objects with host, port and protocol.")

func ParseTurnServers(raw string) ([]TurnServer, error) {
	raw = strings.TrimSpace(raw)
	if raw == "" {
		return nil, nil
	}
	var entries []json.RawMessage
	if err := json.Unmarshal([]byte(raw), &entries); err != nil || entries == nil {
		return nil, errTurnList
	}
	list := make([]TurnServer, 0, len(entries))
	for i, data := range entries {
		t, err := parseTurnServer(data, i+1)
		if err != nil {
			return nil, err
		}
		list = append(list, t)
	}
	return list, nil
}

func parseTurnServer(data json.RawMessage, n int) (TurnServer, error) {
	var fields map[string]json.RawMessage
	if err := json.Unmarshal(data, &fields); err != nil || fields == nil {
		return TurnServer{}, fmt.Errorf("Give TURN server %d as a JSON object.", n)
	}
	for key := range fields {
		if !slices.Contains(turnFields, key) {
			return TurnServer{}, fmt.Errorf("TURN server %d has the field %q, which Fluxer doesn't know. Use host, port, protocol, username, credential, secret and ttl.", n, key)
		}
	}
	var t TurnServer
	var ok, hasUser, hasCred, hasSecret, hasTTL bool
	if t.Host, ok = turnString(fields, "host"); !ok || strings.ContainsAny(t.Host, ":/[]") || strings.IndexFunc(t.Host, turnSpace) >= 0 {
		return TurnServer{}, fmt.Errorf("Give TURN server %d a host that is a hostname or an IPv4 address.", n)
	}
	if t.Port, ok = turnInt(fields, "port"); !ok || t.Port < 1 || t.Port > 65535 {
		return TurnServer{}, fmt.Errorf("Give TURN server %d a port from 1 to 65535.", n)
	}
	if t.Protocol, ok = turnString(fields, "protocol"); !ok || t.Protocol != "udp" && t.Protocol != "tcp" && t.Protocol != "tls" {
		return TurnServer{}, fmt.Errorf("Give TURN server %d the protocol udp, tcp or tls.", n)
	}
	for _, f := range []struct {
		key string
		dst *string
		has *bool
	}{{"username", &t.Username, &hasUser}, {"credential", &t.Credential, &hasCred}, {"secret", &t.Secret, &hasSecret}} {
		if _, *f.has = fields[f.key]; !*f.has {
			continue
		}
		if *f.dst, ok = turnString(fields, f.key); !ok {
			return TurnServer{}, fmt.Errorf("Give TURN server %d a %s that is text and not empty.", n, f.key)
		}
	}
	switch {
	case hasSecret && (hasUser || hasCred):
		return TurnServer{}, fmt.Errorf("Give TURN server %d a secret, or a username and credential, and not both.", n)
	case hasUser != hasCred:
		return TurnServer{}, fmt.Errorf("Give TURN server %d both a username and a credential.", n)
	}
	if _, hasTTL = fields["ttl"]; hasTTL {
		if t.TTL, ok = turnInt(fields, "ttl"); !ok || t.TTL < 1 || !hasSecret {
			return TurnServer{}, fmt.Errorf("Give TURN server %d a ttl only with a secret, as a whole number of seconds from 1.", n)
		}
	}
	return t, nil
}

func turnString(fields map[string]json.RawMessage, key string) (string, bool) {
	data, ok := fields[key]
	if !ok || bytes.Equal(bytes.TrimSpace(data), []byte("null")) {
		return "", false
	}
	var s string
	if json.Unmarshal(data, &s) != nil || s == "" {
		return "", false
	}
	return s, true
}

func turnInt(fields map[string]json.RawMessage, key string) (int, bool) {
	data, ok := fields[key]
	if !ok || bytes.Equal(bytes.TrimSpace(data), []byte("null")) {
		return 0, false
	}
	var f float64
	if json.Unmarshal(data, &f) != nil || f != math.Trunc(f) || f < math.MinInt32 || f > math.MaxInt32 {
		return 0, false
	}
	return int(f), true
}

func turnSpace(r rune) bool {
	return unicode.IsSpace(r) || r == 0xFEFF
}

func TurnServersValue(list []TurnServer) string {
	if len(list) == 0 {
		return ""
	}
	var b bytes.Buffer
	enc := json.NewEncoder(&b)
	enc.SetEscapeHTML(false)
	_ = enc.Encode(list)
	return strings.ReplaceAll(strings.TrimSpace(b.String()), "'", `'`)
}

func validTurnServers(v string) error {
	_, err := ParseTurnServers(v)
	return err
}

func NormalTurnServers(raw string) (string, error) {
	list, err := ParseTurnServers(raw)
	if err != nil {
		return "", err
	}
	return TurnServersValue(list), nil
}
