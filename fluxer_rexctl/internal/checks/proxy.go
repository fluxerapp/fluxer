// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"bufio"
	"context"
	"crypto/tls"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/netip"
	"os"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "proxy", Phases: Installed, Run: runProxy})
}

func runProxy(ctx context.Context, e *Env) Result {
	m := e.mode()
	if e.Instance == nil || e.Domain == "" || m == modeBundled || privateDomain(e.Domain) || overlayDomain(e.Domain) || !e.edgeUp(ctx) {
		return skip("")
	}
	noun := "proxy"
	if m == modeTunnel {
		noun = "tunnel"
	}
	if st, detail := ProbeHTTPS(ctx, e.Domain); st != ui.StateOK {
		msg := fmt.Sprintf("https://%s doesn't load through your %s.", e.Domain, noun)
		if strings.HasPrefix(detail, "returns ") {
			msg += " It " + detail + "."
		}
		return fail(msg, "Set up your "+noun+" with the config from rexctl proxy config, then run rexctl doctor.")
	}
	if st, _ := ProbeWebSocket(ctx, e.Domain); st != ui.StateOK {
		return fail(fmt.Sprintf("Your %s doesn't pass WebSocket connections on /gateway through.", noun), "Forward the Upgrade and Connection headers. rexctl proxy config shows how.")
	}
	source := "127.0.0.1"
	if e.setting(settings.EdgeProxyKind) == settings.ProxyRemote {
		if first, _, _ := strings.Cut(e.setting(settings.EdgeTrusted), ","); strings.TrimSpace(first) != "" {
			source = strings.TrimSpace(first)
		}
	}
	if st, _ := ProbeClientIP(ctx, e.Domain, source); st == ui.StateFail {
		return warn(fmt.Sprintf("Fluxer sees your %s's address instead of each visitor's.", noun), "Make your "+noun+" set the X-Forwarded-For header. rexctl proxy config shows how.")
	}
	return pass(fmt.Sprintf("https://%s loads through your %s, which passes WebSockets and each visitor's address through.", e.Domain, noun))
}

func ProxyTLSConfig(domain string) *tls.Config {
	return &tls.Config{ServerName: settings.BareHost(domain), MinVersion: tls.VersionTLS12, InsecureSkipVerify: os.Getenv("REXCTL_TEST_INSECURE_PROXY") == "1"}
}

func ProbeHTTPS(ctx context.Context, domain string) (ui.State, string) {
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	client := &http.Client{Transport: &http.Transport{TLSClientConfig: ProxyTLSConfig(domain)}}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, "https://"+domain+"/_health", nil)
	if err != nil {
		return ui.StateFail, err.Error()
	}
	resp, err := client.Do(req)
	if err != nil {
		return ui.StateActive, "no answer yet"
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return ui.StateActive, fmt.Sprintf("returns status %d", resp.StatusCode)
	}
	version := "TLS 1.2"
	if resp.TLS != nil && resp.TLS.Version == tls.VersionTLS13 {
		version = "TLS 1.3"
	}
	return ui.StateOK, "works over " + version
}

func ProbeWebSocket(ctx context.Context, domain string) (ui.State, string) {
	d := &tls.Dialer{NetDialer: &net.Dialer{Timeout: 5 * time.Second}, Config: ProxyTLSConfig(domain)}
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	conn, err := d.DialContext(ctx, "tcp", net.JoinHostPort(settings.BareHost(domain), "443"))
	if err != nil {
		return ui.StateFail, "couldn't connect"
	}
	defer conn.Close()
	_ = conn.SetDeadline(time.Now().Add(5 * time.Second))
	req := "GET /gateway?v=1&encoding=json HTTP/1.1\r\nHost: " + domain + "\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: cmV4Y3RsLXByb3h5LWNoZWNr\r\n\r\n"
	if _, err := conn.Write([]byte(req)); err != nil {
		return ui.StateFail, "couldn't send a request"
	}
	line, err := bufio.NewReader(conn).ReadString('\n')
	if err != nil {
		return ui.StateFail, "no answer"
	}
	if strings.Contains(line, " 101 ") {
		return ui.StateOK, "/gateway upgrades to a WebSocket"
	}
	return ui.StateFail, "/gateway returns " + strings.TrimSpace(strings.TrimPrefix(line, "HTTP/1.1 "))
}

func ProbeClientIP(ctx context.Context, domain, source string) (ui.State, string) {
	const later = "not checked, make sure your proxy sets the X-Forwarded-For header"
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	client := &http.Client{Transport: &http.Transport{TLSClientConfig: ProxyTLSConfig(domain)}}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, "https://"+domain+"/.well-known/fluxer-client-ip", nil)
	if err != nil {
		return ui.StateSkipped, later
	}
	resp, err := client.Do(req)
	if err != nil {
		return ui.StateSkipped, later
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(io.LimitReader(resp.Body, 128))
	seen, err := netip.ParseAddr(strings.TrimSpace(string(body)))
	if resp.StatusCode != http.StatusOK || err != nil {
		return ui.StateSkipped, later
	}
	return judgeClientIP(seen.Unmap(), source, resolvesToLoopback(ctx, domain))
}

func judgeClientIP(seen netip.Addr, source string, loopbackDomain bool) (ui.State, string) {
	if seen.IsLoopback() && loopbackDomain {
		return ui.StateSkipped, "not checked, this machine connects to the address over loopback"
	}
	if fromSource(seen, source) {
		return ui.StateFail, "Fluxer sees " + seen.String() + ", the address of your proxy"
	}
	return ui.StateOK, "Fluxer sees this machine as " + seen.String()
}

func fromSource(seen netip.Addr, sources string) bool {
	for _, source := range settings.TrustedProxyList(sources) {
		if a, err := netip.ParseAddr(source); err == nil {
			a = a.Unmap()
			if seen == a || a.IsLoopback() && seen.IsLoopback() {
				return true
			}
		}
		if p, err := netip.ParsePrefix(source); err == nil && p.Contains(seen) {
			return true
		}
	}
	return false
}

func resolvesToLoopback(ctx context.Context, domain string) bool {
	addrs, err := net.DefaultResolver.LookupNetIP(ctx, "ip", settings.BareHost(domain))
	if err != nil {
		return false
	}
	for _, a := range addrs {
		if a.Unmap().IsLoopback() {
			return true
		}
	}
	return false
}
