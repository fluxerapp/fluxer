// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"fmt"
	"runtime"
	"slices"
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
	Source string
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
		if p.Source != "" {
			closed = append(closed, p)
			continue
		}
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
			if _, err := output(ctx, false, "ufw", ufwArgs("allow", p)...); err != nil {
				return err
			}
		}
		return nil
	case FirewallFirewalld:
		for _, p := range ports {
			if _, err := output(ctx, false, "firewall-cmd", "--permanent", firewalldArg("add", p)); err != nil {
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
			if _, err := output(ctx, false, "ufw", append([]string{"delete"}, ufwArgs("allow", p)...)...); err != nil {
				return err
			}
		}
		return nil
	case FirewallFirewalld:
		for _, p := range ports {
			if _, err := output(ctx, false, "firewall-cmd", "--permanent", firewalldArg("remove", p)); err != nil {
				return err
			}
		}
		_, err := output(ctx, false, "firewall-cmd", "--reload")
		return err
	}
	return fmt.Errorf("host: unknown firewall %q", fw)
}

func ufwArgs(verb string, p Port) []string {
	if p.Source == "" {
		return []string{verb, p.String()}
	}
	return []string{verb, "from", p.Source, "to", "any", "port", strconv.Itoa(p.Number), "proto", p.Proto}
}

func firewalldArg(verb string, p Port) string {
	if p.Source == "" {
		return "--" + verb + "-port=" + p.String()
	}
	family := "ipv4"
	if strings.Contains(p.Source, ":") {
		family = "ipv6"
	}
	return fmt.Sprintf("--%s-rich-rule=rule family=%q source address=%q port port=%q protocol=%q accept", verb, family, p.Source, strconv.Itoa(p.Number), p.Proto)
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

func IptablesClosed(ctx context.Context, ports []Port) []Port {
	if runtime.GOOS != "linux" || !has("iptables") {
		return nil
	}
	out, err := output(ctx, true, "iptables", "-S", "INPUT")
	if err != nil {
		return nil
	}
	return iptablesClosed(string(out), ports)
}

func iptablesClosed(rules string, ports []Port) []Port {
	var allowed []portRule
	blocked := false
	for _, line := range strings.Split(rules, "\n") {
		f := strings.Fields(line)
		if len(f) == 3 && f[0] == "-P" && f[1] == "INPUT" && f[2] == "DROP" {
			blocked = true
		}
		if len(f) < 4 || f[0] != "-A" || f[1] != "INPUT" {
			continue
		}
		if len(f) == 4 && f[2] == "-j" && f[3] == "ACCEPT" {
			return nil
		}
		if f[2] == "-j" && (f[3] == "REJECT" || f[3] == "DROP") {
			blocked = true
			break
		}
		allowed = append(allowed, iptablesAccept(f[2:])...)
	}
	if !blocked {
		return nil
	}
	var closed []Port
	for _, p := range ports {
		if !slices.ContainsFunc(allowed, func(r portRule) bool { return r.allows(p) }) {
			closed = append(closed, p)
		}
	}
	return closed
}

func iptablesAccept(args []string) []portRule {
	proto, ports, target := "", "", ""
	for i := 0; i+1 < len(args); i++ {
		switch args[i] {
		case "-s", "-i", "--src-range":
			return nil
		case "-p":
			proto = args[i+1]
		case "--dport", "--dports":
			ports = args[i+1]
		case "-j":
			target = args[i+1]
		}
	}
	if target != "ACCEPT" || proto != "tcp" && proto != "udp" || ports == "" {
		return nil
	}
	return parseSpec(ports+"/"+proto, ":")
}
