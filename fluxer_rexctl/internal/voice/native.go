// SPDX-License-Identifier: AGPL-3.0-or-later

package voice

import (
	"fmt"
	"net"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
)

const (
	NativeEnvFile       = "livekit.env"
	nativeChildPrefix   = "livekit-"
	NativeLoopback      = "127.0.0.1"
	firstNativeHTTPPort = 17900
	lastNativeHTTPPort  = 18079
	defaultStunPrimary  = "stun.l.google.com:19302"
	defaultStunBackup   = "stun1.l.google.com:19302"
)

func NativeChild(id string) string { return nativeChildPrefix + id }

func NativeAddr(port int) string { return net.JoinHostPort(NativeLoopback, strconv.Itoa(port)) }

func NativeDropIn(id string, port int) []byte {
	return fmt.Appendf(nil, "handle_path %s/* {\n\timport stream %s\n}\n", Path(id), NativeAddr(port))
}

func PickNativeHTTPPort(taken []int, free func(number int, proto string) bool) int {
	for p := firstNativeHTTPPort; p <= lastNativeHTTPPort; p++ {
		if !slices.Contains(taken, p) && (free == nil || free(p, "tcp")) {
			return p
		}
	}
	return 0
}

func (e Env) NativeValues() map[string]string {
	v := e.Values()
	bind := NativeLoopback
	if e.HTTPBind != "" {
		bind = e.HTTPBind
	}
	var b strings.Builder
	fmt.Fprintf(&b, "port: %d\nbind_addresses:\n  - %s\nlog_level: %s\n", e.HTTPPort, bind, orDefault(e.LogLevel, "info"))
	fmt.Fprintf(&b, "rtc:\n  tcp_port: %d\n  udp_port: %d\n  use_external_ip: %s\n  node_ip: %q\n", e.TCPPort, e.UDPPort, v["FLUXER_VOICE_USE_EXTERNAL_IP"], v["FLUXER_VOICE_NODE_IP"])
	fmt.Fprintf(&b, "  ips:\n    includes: %s\n", v["FLUXER_VOICE_ICE_IPS"])
	fmt.Fprintf(&b, "  stun_servers:\n    - %s\n    - %s\n", orDefault(e.StunPrimary, defaultStunPrimary), orDefault(e.StunSecondary, defaultStunBackup))
	fmt.Fprintf(&b, "  turn_servers: %s\nturn:\n  enabled: false\n", orDefault(e.TurnServers, "[]"))
	fmt.Fprintf(&b, "webhook:\n  api_key: %s\n  urls:\n    - %s\n", e.Key, e.WebhookURL)
	out := map[string]string{
		"FLUXER_VOICE_ID":        e.ID,
		"FLUXER_VOICE_HTTP_PORT": strconv.Itoa(e.HTTPPort),
		"GOMEMLIMIT":             v["FLUXER_VOICE_GOMEMLIMIT"],
		"LIVEKIT_KEYS":           e.Key + ": " + e.Secret,
		"LIVEKIT_CONFIG":         b.String(),
	}
	if bind != NativeLoopback {
		out["FLUXER_VOICE_HTTP_BIND"] = bind
	}
	return out
}

func (e Env) NativeFile() []byte {
	values := e.NativeValues()
	keys := make([]string, 0, len(values))
	for k := range values {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	var b strings.Builder
	for _, k := range keys {
		fmt.Fprintf(&b, "%s=%s\n", k, envfile.Quote(values[k]))
	}
	return []byte(b.String())
}
