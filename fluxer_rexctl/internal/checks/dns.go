// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"strings"
	"time"
)

func init() {
	Register(Check{ID: "dns", Phases: Preinstall | Installed, Run: runDNS})
}

func runDNS(ctx context.Context, e *Env) Result {
	if privateDomain(e.Domain) {
		return skip("")
	}
	lookupCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	addrs, err := net.DefaultResolver.LookupIPAddr(lookupCtx, e.Domain)
	var dnsErr *net.DNSError
	if err != nil && !(errors.As(err, &dnsErr) && dnsErr.IsNotFound) {
		return skip(fmt.Sprintf("Couldn't look up %s.", e.Domain))
	}
	if e.mode() != modeBundled {
		if len(addrs) == 0 {
			return fail(fmt.Sprintf("%s doesn't point anywhere yet.", e.Domain),
				"Add a DNS record for it that points to your proxy, then run rexctl doctor.")
		}
		return pass(fmt.Sprintf("%s points to %s.", e.Domain, addrs[0].IP))
	}
	public, perr := e.publicAddress(ctx)
	if len(addrs) == 0 {
		if perr != nil {
			return fail(fmt.Sprintf("%s doesn't point to this machine yet.", e.Domain),
				"Add an A record for it pointing to this machine's public IP address, then run rexctl doctor.")
		}
		return fail(fmt.Sprintf("%s doesn't point to this machine yet.", e.Domain),
			fmt.Sprintf("Add an A record for it pointing to %s, then run rexctl doctor.", public))
	}
	if pointsHere(addrs, public) {
		shown := public
		if shown == "" {
			shown = addrs[0].IP.String()
		}
		return pass(fmt.Sprintf("%s points to this machine (%s).", e.Domain, shown))
	}
	if e.forwardsHere(ctx) {
		return pass(fmt.Sprintf("%s points to %s, which forwards to this machine.", e.Domain, addrs[0].IP))
	}
	if perr != nil {
		return skip(fmt.Sprintf("Couldn't find this machine's public IP address to compare with %s.", e.Domain))
	}
	return fail(fmt.Sprintf("%s points to %s and this machine is %s.", e.Domain, addrs[0].IP, public),
		fmt.Sprintf("Change its A record to %s, then run rexctl doctor.", public))
}

func pointsHere(addrs []net.IPAddr, public string) bool {
	pub := net.ParseIP(public)
	local := map[string]bool{}
	if ifaces, err := net.InterfaceAddrs(); err == nil {
		for _, a := range ifaces {
			if n, ok := a.(*net.IPNet); ok && !n.IP.IsLoopback() && !n.IP.IsPrivate() && !n.IP.IsLinkLocalUnicast() {
				local[n.IP.String()] = true
			}
		}
	}
	for _, a := range addrs {
		if pub != nil && a.IP.Equal(pub) {
			return true
		}
		if local[a.IP.String()] {
			return true
		}
	}
	return false
}

func (e *Env) forwardsHere(ctx context.Context) bool {
	if e.Instance == nil || !e.running(ctx, "edge") {
		return false
	}
	local, err := e.exec(ctx, "edge", "", "wget", "-q", "-O", "-", "http://127.0.0.1:8088/_edge")
	if err != nil || local == "" {
		return false
	}
	return servesEdge(ctx, e.Domain, local)
}

func servesEdge(ctx context.Context, domain, local string) bool {
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, "https://"+domain+"/_edge", nil)
	if err != nil {
		return false
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return false
	}
	defer resp.Body.Close()
	remote, err := io.ReadAll(io.LimitReader(resp.Body, 1<<10))
	return err == nil && resp.StatusCode == http.StatusOK && strings.TrimSpace(string(remote)) == local
}
