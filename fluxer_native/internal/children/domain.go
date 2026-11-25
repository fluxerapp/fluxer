// SPDX-License-Identifier: AGPL-3.0-or-later

package children

import (
	"errors"
	"fmt"
	"net/netip"
	"strings"
)

var placeholderDomains = []string{"example.com", "example.net", "example.org"}

var nonPublicRanges = []netip.Prefix{
	netip.MustParsePrefix("100.64.0.0/10"),
	netip.MustParsePrefix("192.0.0.0/24"),
	netip.MustParsePrefix("192.0.2.0/24"),
	netip.MustParsePrefix("198.18.0.0/15"),
	netip.MustParsePrefix("198.51.100.0/24"),
	netip.MustParsePrefix("203.0.113.0/24"),
	netip.MustParsePrefix("240.0.0.0/4"),
	netip.MustParsePrefix("64:ff9b::/96"),
	netip.MustParsePrefix("64:ff9b:1::/48"),
	netip.MustParsePrefix("100::/64"),
	netip.MustParsePrefix("2001:db8::/32"),
	netip.MustParsePrefix("2002::/16"),
	netip.MustParsePrefix("3fff::/20"),
	netip.MustParsePrefix("fec0::/10"),
}

func publicAddress(ip netip.Addr) bool {
	if !ip.IsGlobalUnicast() || ip.IsPrivate() {
		return false
	}
	for _, p := range nonPublicRanges {
		if p.Contains(ip) {
			return false
		}
	}
	return true
}

func CheckDomain(domain string) error {
	domain = strings.TrimSpace(domain)
	if domain == "" {
		return errors.New("FLUXER_DOMAIN is empty. Set it to the address people open, without https:// or a port")
	}
	if _, ok := domainIP(domain); ok {
		return nil
	}
	if !validHostname(domain) {
		return fmt.Errorf("FLUXER_DOMAIN %q is not a domain name or IP address. Set it to the address people open, without https:// or a port", domain)
	}
	lower := strings.ToLower(domain)
	for _, p := range placeholderDomains {
		if lower == p || strings.HasSuffix(lower, "."+p) {
			return fmt.Errorf("FLUXER_DOMAIN is still %s from the example settings. Set it to your domain or this machine's IP address, such as 192.168.1.20", domain)
		}
	}
	return nil
}

func NormalDomain(domain string) string {
	domain = strings.TrimSpace(domain)
	ip, ok := domainIP(domain)
	if !ok {
		return domain
	}
	if ip.Is4() {
		return ip.String()
	}
	return "[" + ip.String() + "]"
}

func domainIP(domain string) (netip.Addr, bool) {
	bare := domain
	if strings.HasPrefix(domain, "[") && strings.HasSuffix(domain, "]") {
		bare = domain[1 : len(domain)-1]
	}
	ip, err := netip.ParseAddr(bare)
	if err != nil || ip.Zone() != "" || ip.Is4() && bare != domain {
		return netip.Addr{}, false
	}
	return ip.Unmap(), true
}

func validHostname(v string) bool {
	if len(v) > 253 {
		return false
	}
	for _, label := range strings.Split(v, ".") {
		if label == "" || len(label) > 63 || strings.HasPrefix(label, "-") || strings.HasSuffix(label, "-") {
			return false
		}
		for _, r := range label {
			if !(r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z' || r >= '0' && r <= '9' || r == '-') {
				return false
			}
		}
	}
	return true
}
