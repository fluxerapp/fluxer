// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/netip"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/proxyconf"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

func init() {
	Register(Check{ID: "dns", Phases: Preinstall | Installed, Run: runDNS})
}

func runDNS(ctx context.Context, e *Env) Result {
	if ip, ok := settings.DomainIP(e.Domain); ok {
		return e.checkDomainIP(ctx, ip)
	}
	if privateDomain(e.Domain) || tailnetDomain(e.Domain) {
		return skip("")
	}
	if overlayDomain(e.Domain) {
		if e.mode() != modeBundled || strings.EqualFold(e.value("FLUXER_PUBLIC_SCHEME"), "http") || strings.EqualFold(e.value("FLUXER_EDGE_TLS"), "internal") {
			return skip(fmt.Sprintf("rexctl doesn't check DNS for %s because public DNS can't look it up.", e.Domain))
		}
		fix := "Run rexctl install --behind-proxy, then point your tunnel at http://" + settings.DefaultBind + "."
		if e.installed() {
			fix = fmt.Sprintf("Run rexctl proxy on, point your tunnel at http://%s, then run rexctl doctor.", cmp.Or(e.setting(settings.EdgeBind), settings.DefaultBind))
		}
		return fail(fmt.Sprintf("Let's Encrypt can't give %s a certificate because public DNS can't look it up.", e.Domain), fix)
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
			return warn(fmt.Sprintf("This machine can't find %s in DNS.", e.Domain),
				"If only your proxy or tunnel can look it up, you can ignore this. Otherwise, add a DNS record for it that points to your proxy or tunnel. rexctl doctor checks it again.")
		}
		return pass(fmt.Sprintf("%s points to %s.", e.Domain, addrs[0].IP))
	}
	var public string
	var perr error
	here := "this machine is"
	listen := e.publicListenIP()
	if listen != nil {
		public, here = listen.String(), "Fluxer listens on"
	} else {
		public, perr = e.publicAddress(ctx)
	}
	record := "A"
	if ip := net.ParseIP(public); ip != nil && ip.To4() == nil {
		record = "AAAA"
	}
	if len(addrs) == 0 {
		if perr != nil {
			return fail(fmt.Sprintf("%s doesn't point to this machine yet.", e.Domain),
				"Add an A record for it pointing to this machine's public IP address, or a CNAME to a dynamic DNS name that points here, then run rexctl doctor.")
		}
		return fail(fmt.Sprintf("%s doesn't point to this machine yet.", e.Domain),
			fmt.Sprintf("Add an %s record for it pointing to %s, or a CNAME to a dynamic DNS name that points there, then run rexctl doctor.", record, public))
	}
	matched := matchHere(addrs, public, listen == nil)
	if len(matched) > 0 {
		return pass(fmt.Sprintf("%s points to this machine (%s).", e.Domain, strings.Join(matched, ", ")))
	}
	if cloudflare(addrs) && !e.installed() {
		return warn(fmt.Sprintf("%s points to Cloudflare's proxy, so rexctl can't check yet that it leads to this machine.", e.Domain),
			fmt.Sprintf("In Cloudflare, point the record at %s and keep the proxy on. rexctl doctor checks it again once Fluxer runs.", public))
	}
	forwards, reason := e.forwardsHere(ctx)
	if forwards {
		return pass(fmt.Sprintf("%s points to %s, which forwards to this machine.", e.Domain, addrs[0].IP))
	}
	if perr != nil {
		return skip(fmt.Sprintf("Couldn't find this machine's public IP address to compare with %s.", e.Domain))
	}
	problem := fmt.Sprintf("%s points to %s and %s %s.", e.Domain, addrs[0].IP, here, public)
	if reason != "" {
		return fail(problem+" "+reason,
			fmt.Sprintf("If a proxy such as Cloudflare is in front of this machine, let requests to https://%s/_edge through without a challenge, then run rexctl doctor. Otherwise, change the %s record to %s.", e.Domain, record, public))
	}
	return fail(problem, fmt.Sprintf("Change its %s record to %s, then run rexctl doctor.", record, public))
}

func (e *Env) checkDomainIP(ctx context.Context, ip netip.Addr) Result {
	if !publicAddress(ip) || e.mode() != modeBundled {
		return skip("")
	}
	if listen := e.publicListenIP(); listen != nil && listen.Equal(ip.AsSlice()) {
		return pass(fmt.Sprintf("Fluxer listens on %s.", ip))
	}
	public, err := e.publicAddress(ctx)
	seen, perr := netip.ParseAddr(public)
	if err != nil || perr != nil || seen.Is4() != ip.Is4() {
		if len(matchHere([]net.IPAddr{{IP: ip.AsSlice()}}, "", true)) > 0 {
			return pass(fmt.Sprintf("%s is an address of this machine.", ip))
		}
		return skip("")
	}
	if len(matchHere([]net.IPAddr{{IP: ip.AsSlice()}}, public, true)) > 0 {
		return pass(fmt.Sprintf("%s is this machine's public IP address.", ip))
	}
	if forwards, _ := e.forwardsHere(ctx); forwards {
		return pass(fmt.Sprintf("%s forwards to this machine.", ip))
	}
	msg := fmt.Sprintf("%s isn't this machine's public IP address, which is %s.", ip, public)
	fix := fmt.Sprintf("Use %s as the address, unless %s forwards ports 80 and 443 to this machine.", public, ip)
	if !e.installed() {
		return warn(msg, fix)
	}
	return fail(msg, fmt.Sprintf("If %s forwards ports 80 and 443 to this machine, check that they're open, then run rexctl doctor. Otherwise run rexctl config set domain %s.", ip, public))
}

func (e *Env) publicListenIP() net.IP {
	ip := net.ParseIP(e.value(settings.EdgeListenVar))
	if ip == nil || !ip.IsGlobalUnicast() || ip.IsPrivate() {
		return nil
	}
	return ip
}

func matchHere(addrs []net.IPAddr, public string, anyLocal bool) []string {
	pub := net.ParseIP(public)
	local := map[string]bool{}
	if ifaces, err := net.InterfaceAddrs(); anyLocal && err == nil {
		for _, a := range ifaces {
			if n, ok := a.(*net.IPNet); ok && !n.IP.IsLoopback() && !n.IP.IsPrivate() && !n.IP.IsLinkLocalUnicast() {
				local[n.IP.String()] = true
			}
		}
	}
	var matched []string
	for _, a := range addrs {
		if pub != nil && a.IP.Equal(pub) || local[a.IP.String()] {
			matched = append(matched, a.IP.String())
		}
	}
	return matched
}

func (e *Env) forwardsHere(ctx context.Context) (bool, string) {
	if e.Native {
		code, body, err := e.viaEdge(ctx, "/_edge")
		local := strings.TrimSpace(body)
		if err != nil || code != http.StatusOK || local == "" {
			return false, ""
		}
		return servesEdge(ctx, e.derivedOrigin(), local)
	}
	if e.Instance == nil || !e.running(ctx, "edge") {
		return false, ""
	}
	local, err := e.exec(ctx, "edge", "", "wget", "-q", "-O", "-", "http://127.0.0.1:8088/_edge")
	if err != nil || local == "" {
		return false, ""
	}
	return servesEdge(ctx, e.derivedOrigin(), local)
}

func servesEdge(ctx context.Context, origin, local string) (bool, string) {
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	url := origin + "/_edge"
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return false, ""
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return false, fmt.Sprintf("rexctl couldn't open %s.", url)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		if strings.EqualFold(resp.Header.Get("Server"), "cloudflare") || resp.Header.Get("Cf-Ray") != "" {
			return false, fmt.Sprintf("For %s, Cloudflare returned status %d.", url, resp.StatusCode)
		}
		return false, fmt.Sprintf("Requests to %s get status %d.", url, resp.StatusCode)
	}
	remote, err := io.ReadAll(io.LimitReader(resp.Body, 1<<10))
	if err != nil {
		return false, fmt.Sprintf("rexctl couldn't open %s.", url)
	}
	if strings.TrimSpace(string(remote)) != local {
		return false, fmt.Sprintf("Requests to %s get an answer from a different machine.", url)
	}
	return true, ""
}

func cloudflare(addrs []net.IPAddr) bool {
	for _, a := range addrs {
		if ip, ok := netip.AddrFromSlice(a.IP); ok && proxyconf.IsCloudflare(ip) {
			return true
		}
	}
	return false
}

func OverlayDomain(domain string) bool { return overlayDomain(domain) }

func TailnetDomain(domain string) bool { return tailnetDomain(domain) }

func tailnetDomain(domain string) bool {
	return hasDomainSuffix(strings.ToLower(strings.TrimSuffix(domain, ".")), []string{".ts.net"})
}

const TailnetCertFix = "On this machine, add TS_PERMIT_CERT_UID=fluxer to /etc/default/tailscaled and run sudo systemctl restart tailscaled, so Fluxer can get its certificate from Tailscale. Also turn on HTTPS certificates under DNS in the Tailscale admin console"

func PrivateDomain(domain string) bool { return privateDomain(domain) }

type Pointing int

const (
	PointsUnknown Pointing = iota
	PointsNowhere
	PointsHere
	PointsCloudflare
	PointsElsewhere
)

type DNSAnswer struct {
	Points Pointing
	Addr   string
	Public string
}

func LookupDomain(ctx context.Context, domain string) DNSAnswer {
	if _, ip := settings.DomainIP(domain); ip || privateDomain(domain) || overlayDomain(domain) || tailnetDomain(domain) {
		return DNSAnswer{}
	}
	lookupCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	addrs, err := net.DefaultResolver.LookupIPAddr(lookupCtx, domain)
	var dnsErr *net.DNSError
	switch {
	case err != nil && errors.As(err, &dnsErr) && dnsErr.IsNotFound, err == nil && len(addrs) == 0:
		return DNSAnswer{Points: PointsNowhere}
	case err != nil:
		return DNSAnswer{}
	}
	out := DNSAnswer{Addr: addrs[0].IP.String()}
	if cloudflare(addrs) {
		out.Points = PointsCloudflare
		return out
	}
	public, perr := host.PublicIP(ctx)
	out.Public = public
	if len(matchHere(addrs, public, true)) > 0 {
		out.Points = PointsHere
		return out
	}
	if perr != nil {
		return out
	}
	out.Points = PointsElsewhere
	return out
}
