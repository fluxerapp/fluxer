// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"fmt"
	"runtime"
	"strconv"
	"strings"
)

type Firewall string

const (
	FirewallNone      Firewall = ""
	FirewallUFW       Firewall = "ufw"
	FirewallFirewalld Firewall = "firewalld"
)

type Port struct {
	Number int
	Proto  string
}

func (p Port) String() string { return fmt.Sprintf("%d/%s", p.Number, p.Proto) }

type portRule struct {
	from, to int
	proto    string
}

func (r portRule) allows(p Port) bool {
	return p.Number >= r.from && p.Number <= r.to && (r.proto == "" || r.proto == p.Proto)
}

func DetectFirewall(ctx context.Context) (Firewall, error) {
	if runtime.GOOS != "linux" {
		return FirewallNone, nil
	}
	if has("ufw") {
		out, err := output(ctx, true, "ufw", "status")
		if err == nil && strings.Contains(strings.ToLower(string(out)), "status: active") {
			return FirewallUFW, nil
		}
	}
	if has("firewall-cmd") {
		out, err := output(ctx, true, "firewall-cmd", "--state")
		if err == nil && strings.TrimSpace(string(out)) == "running" {
			return FirewallFirewalld, nil
		}
	}
	return FirewallNone, nil
}

func ClosedPorts(ctx context.Context, fw Firewall, ports []Port) ([]Port, error) {
	var rules []portRule
	switch fw {
	case FirewallNone:
		return nil, nil
	case FirewallUFW:
		out, err := output(ctx, true, "ufw", "status", "verbose")
		if err != nil {
			return nil, err
		}
		allowAll, r := parseUFW(string(out))
		if allowAll {
			return nil, nil
		}
		rules = r
	case FirewallFirewalld:
		portsOut, err := output(ctx, true, "firewall-cmd", "--list-ports")
		if err != nil {
			return nil, err
		}
		servicesOut, err := output(ctx, true, "firewall-cmd", "--list-services")
		if err != nil {
			return nil, err
		}
		rules = parseFirewalld(string(portsOut), string(servicesOut))
	default:
		return nil, fmt.Errorf("host: unknown firewall %q", fw)
	}
	var closed []Port
	for _, p := range ports {
		open := false
		for _, r := range rules {
			if r.allows(p) {
				open = true
				break
			}
		}
		if !open {
			closed = append(closed, p)
		}
	}
	return closed, nil
}

func OpenPorts(ctx context.Context, fw Firewall, ports []Port) error {
	switch fw {
	case FirewallNone:
		return nil
	case FirewallUFW:
		for _, p := range ports {
			if _, err := output(ctx, false, "ufw", "allow", p.String()); err != nil {
				return err
			}
		}
		return nil
	case FirewallFirewalld:
		for _, p := range ports {
			if _, err := output(ctx, false, "firewall-cmd", "--permanent", "--add-port="+p.String()); err != nil {
				return err
			}
		}
		_, err := output(ctx, false, "firewall-cmd", "--reload")
		return err
	}
	return fmt.Errorf("host: unknown firewall %q", fw)
}

func ClosePorts(ctx context.Context, fw Firewall, ports []Port) error {
	switch fw {
	case FirewallNone:
		return nil
	case FirewallUFW:
		for _, p := range ports {
			if _, err := output(ctx, false, "ufw", "delete", "allow", p.String()); err != nil {
				return err
			}
		}
		return nil
	case FirewallFirewalld:
		for _, p := range ports {
			if _, err := output(ctx, false, "firewall-cmd", "--permanent", "--remove-port="+p.String()); err != nil {
				return err
			}
		}
		_, err := output(ctx, false, "firewall-cmd", "--reload")
		return err
	}
	return fmt.Errorf("host: unknown firewall %q", fw)
}

func parseUFW(out string) (bool, []portRule) {
	var rules []portRule
	for _, line := range strings.Split(out, "\n") {
		line = strings.TrimSpace(line)
		lower := strings.ToLower(line)
		if strings.HasPrefix(lower, "default:") {
			if strings.Contains(lower, "allow (incoming)") {
				return true, nil
			}
			continue
		}
		fields := strings.Fields(line)
		if len(fields) < 3 {
			continue
		}
		idx := -1
		for i, f := range fields {
			if f == "ALLOW" {
				idx = i
				break
			}
		}
		if idx < 1 || (idx+1 < len(fields) && fields[idx+1] == "OUT") {
			continue
		}
		from := strings.Join(fields[idx+1:], " ")
		if !strings.Contains(from, "Anywhere") {
			continue
		}
		rules = append(rules, parseSpec(fields[0], ":")...)
	}
	return false, rules
}

func parseFirewalld(ports, services string) []portRule {
	var rules []portRule
	for _, f := range strings.Fields(ports) {
		rules = append(rules, parseSpec(f, "-")...)
	}
	known := map[string][]portRule{
		"http":  {{80, 80, "tcp"}},
		"https": {{443, 443, "tcp"}},
		"http3": {{443, 443, "udp"}},
	}
	for _, s := range strings.Fields(services) {
		rules = append(rules, known[s]...)
	}
	return rules
}

func parseSpec(spec, rangeSep string) []portRule {
	numbers, proto, _ := strings.Cut(spec, "/")
	var rules []portRule
	for _, part := range strings.Split(numbers, ",") {
		lo, hi, isRange := strings.Cut(part, rangeSep)
		from, err := strconv.Atoi(lo)
		if err != nil {
			continue
		}
		to := from
		if isRange {
			if to, err = strconv.Atoi(hi); err != nil {
				continue
			}
		}
		rules = append(rules, portRule{from: from, to: to, proto: proto})
	}
	return rules
}
