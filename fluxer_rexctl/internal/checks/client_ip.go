// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"time"
)

const (
	clientIPWindow  = 7 * 24 * time.Hour
	clientIPMinimum = 3
)

func init() {
	Register(Check{ID: "client-ip", Phases: Installed, Run: runClientIP})
}

type clientIPUser struct {
	LastActiveAt *time.Time `json:"last_active_at"`
	LastActiveIP *string    `json:"last_active_ip"`
}

func runClientIP(ctx context.Context, e *Env) Result {
	if e.Admin == nil || !e.running(ctx, "api") {
		return skip("")
	}
	var resp struct {
		Users []clientIPUser `json:"users"`
	}
	if err := e.Admin.Call(ctx, http.MethodGet, "/admin/users", map[string]string{"limit": "100"}, nil, &resp); err != nil {
		return skip("")
	}
	ip, n := sharedIP(resp.Users, time.Now())
	if n < clientIPMinimum {
		return skip("")
	}
	if ip == "" || !e.ownAddress(ctx, ip) {
		return pass("People who use your instance show their own IP addresses.")
	}
	fix := "Turn on IPv6 for Docker on this machine, then run rexctl restart."
	if e.mode() != modeBundled {
		fix = "Make your proxy set the X-Forwarded-For header, then run rexctl doctor."
	}
	return warn(fmt.Sprintf("Everyone who used your instance this week has the IP address %s.", ip), fix)
}

func sharedIP(users []clientIPUser, now time.Time) (string, int) {
	var ips []string
	for _, u := range users {
		if u.LastActiveAt == nil || u.LastActiveIP == nil || *u.LastActiveIP == "" {
			continue
		}
		if now.Sub(*u.LastActiveAt) > clientIPWindow {
			continue
		}
		ips = append(ips, *u.LastActiveIP)
	}
	if len(ips) == 0 {
		return "", 0
	}
	for _, ip := range ips[1:] {
		if ip != ips[0] {
			return "", len(ips)
		}
	}
	return ips[0], len(ips)
}

func (e *Env) ownAddress(ctx context.Context, ip string) bool {
	parsed := net.ParseIP(ip)
	if parsed == nil {
		return false
	}
	if parsed.IsLoopback() || parsed.IsPrivate() || parsed.IsLinkLocalUnicast() || parsed.IsUnspecified() {
		return true
	}
	if public, err := e.publicAddress(ctx); err == nil && parsed.Equal(net.ParseIP(public)) {
		return true
	}
	if ifaces, err := net.InterfaceAddrs(); err == nil {
		for _, a := range ifaces {
			if n, ok := a.(*net.IPNet); ok && n.IP.Equal(parsed) {
				return true
			}
		}
	}
	if privateDomain(e.Domain) {
		return false
	}
	lookupCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	addrs, _ := net.DefaultResolver.LookupIPAddr(lookupCtx, e.Domain)
	for _, a := range addrs {
		if a.IP.Equal(parsed) {
			return true
		}
	}
	return false
}
