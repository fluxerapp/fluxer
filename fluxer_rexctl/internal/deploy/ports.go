// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"fmt"
	"maps"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type portVar struct {
	name   string
	def    int
	protos []string
}

var portVars = []portVar{
	{"FLUXER_HTTP_PORT", 80, []string{"tcp"}},
	{"FLUXER_HTTPS_PORT", 443, []string{"tcp", "udp"}},
	{"FLUXER_LIVEKIT_TCP_PORT", 7881, []string{"tcp"}},
	{"FLUXER_LIVEKIT_UDP_PORT", 7882, []string{"udp"}},
}

func portKeyChanged(changed []string) bool {
	for _, name := range changed {
		switch name {
		case settings.EdgeHTTPPort, settings.EdgeHTTPSPort, settings.VoiceTCPPort, settings.VoiceUDPPort:
			return true
		}
	}
	return false
}

func portOf(env map[string]string, v portVar) int {
	if n, err := strconv.Atoi(strings.TrimSpace(env[v.name])); err == nil && n > 0 && n <= 65535 {
		return n
	}
	return v.def
}

func portDiff(oldEnv, newEnv map[string]string) (added, removed []host.Port) {
	for _, v := range portVars {
		was, will := portOf(oldEnv, v), portOf(newEnv, v)
		if was == will {
			continue
		}
		for _, proto := range v.protos {
			added = append(added, host.Port{Number: will, Proto: proto})
			removed = append(removed, host.Port{Number: was, Proto: proto})
		}
	}
	return added, removed
}

func PortChanges(inst *instance.Instance, changes map[string]*string) (opened, closed []host.Port, err error) {
	rs, err := ReleaseSet(inst)
	if err != nil {
		return nil, nil, err
	}
	next := maps.Clone(inst.State.Settings)
	for name, v := range changes {
		if v == nil {
			delete(next, name)
		} else {
			next[name] = *v
		}
	}
	oldEnv, err := Environment(rs, inst.State, inst.State.Settings)
	if err != nil {
		return nil, nil, err
	}
	newEnv, err := Environment(rs, inst.State, next)
	if err != nil {
		return nil, nil, err
	}
	opened, closed = portDiff(oldEnv, newEnv)
	return opened, closed, nil
}

func checkPortsFree(ctx context.Context, e *engine.Engine, self string, ports []host.Port) error {
	published := map[host.Port]string{}
	if e != nil {
		if owners, err := e.PublishedHostPorts(ctx); err == nil {
			for _, o := range owners {
				if o.Project == self {
					continue
				}
				key := host.Port{Number: o.Number, Proto: o.Proto}
				if _, ok := published[key]; !ok {
					published[key] = o.Project
				}
			}
		}
	}
	seen := map[host.Port]bool{}
	for _, p := range ports {
		if seen[p] {
			continue
		}
		seen[p] = true
		busy, owner, err := host.PortInUse(p.Proto, p.Number)
		if err != nil {
			continue
		}
		if !busy {
			if proj, ok := published[p]; ok {
				busy, owner = true, proj
			}
		}
		if !busy {
			continue
		}
		msg := fmt.Sprintf("Port %d is already in use on this machine.", p.Number)
		if owner != "" {
			msg = fmt.Sprintf("Port %d is already in use by %s.", p.Number, owner)
		}
		return ui.Refused(msg, "Pick another port, or stop what uses it, then try again.")
	}
	return nil
}

type portRole struct {
	host.Port
	subject string
	noun    string
	setting string
}

func effectivePorts(env map[string]string, mode string) []portRole {
	var out []portRole
	if mode != settings.EdgeProxy && mode != settings.EdgeTunnel {
		http := portOf(env, portVars[0])
		https := portOf(env, portVars[1])
		out = append(out,
			portRole{Port: host.Port{Number: http, Proto: "tcp"}, subject: "The web address", noun: "the web address", setting: settings.EdgeHTTPPort},
			portRole{Port: host.Port{Number: https, Proto: "tcp"}, subject: "The web address", noun: "the web address", setting: settings.EdgeHTTPSPort},
			portRole{Port: host.Port{Number: https, Proto: "udp"}, subject: "The web address", noun: "the web address", setting: settings.EdgeHTTPSPort},
		)
	}
	tcp := portOf(env, portVars[2])
	udp := portOf(env, portVars[3])
	out = append(out,
		portRole{Port: host.Port{Number: tcp, Proto: "tcp"}, subject: "Voice", noun: "voice (TCP)", setting: settings.VoiceTCPPort},
		portRole{Port: host.Port{Number: udp, Proto: "udp"}, subject: "Voice", noun: "voice (UDP)", setting: settings.VoiceUDPPort},
	)
	return out
}

func checkPortsDistinct(ports []portRole) error {
	byPort := map[host.Port]portRole{}
	for _, p := range ports {
		if prev, ok := byPort[p.Port]; ok && prev.noun != p.noun {
			return ui.Refused(
				fmt.Sprintf("%s would use port %d, which Fluxer already uses for %s.", p.subject, p.Number, prev.noun),
				fmt.Sprintf("Pick a different port with %s.", p.setting))
		}
		byPort[p.Port] = p
	}
	return nil
}

func PortList(ports []host.Port) string {
	var nums []int
	for _, p := range ports {
		if !slices.Contains(nums, p.Number) {
			nums = append(nums, p.Number)
		}
	}
	slices.Sort(nums)
	parts := make([]string, len(nums))
	for i, n := range nums {
		parts[i] = strconv.Itoa(n)
	}
	if len(parts) < 2 {
		return strings.Join(parts, "")
	}
	return strings.Join(parts[:len(parts)-1], ", ") + " and " + parts[len(parts)-1]
}
