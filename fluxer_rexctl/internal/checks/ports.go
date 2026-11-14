// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"
	"net"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "ports", Phases: Preinstall | Installed, Run: runPorts})
}

var containerOwners = map[string]bool{
	"docker-proxy":       true,
	"com.docker.backend": true,
	"com.docker.vpnkit":  true,
	"vpnkit-bridge":      true,
	"rootlessport":       true,
	"rootlesskit":        true,
	"conmon":             true,
	"gvproxy":            true,
	"wslrelay":           true,
}

func ContainerOwner(name string) bool {
	return containerOwners[name]
}

func runPorts(ctx context.Context, e *Env) Result {
	if e.installed() {
		return e.portOwners(ctx)
	}
	wanted := e.wantedPorts()
	pub, _ := e.published(ctx)
	var numbers, busy []string
	var first *stack.Port
	firstOwner := ""
	firstContainer := false
	sameOwner := true
	allVoice := true
	allContainer := true
	seen := map[int]bool{}
	for _, p := range wanted {
		p := p
		if seen[p.Number] {
			continue
		}
		used, owner, err := host.PortInUse(p.Proto, p.Number)
		if err != nil {
			return skip(fmt.Sprintf("Couldn't check whether port %d is free.", p.Number))
		}
		seen[p.Number] = true
		numbers = append(numbers, strconv.Itoa(p.Number))
		container := containerOwners[owner]
		if !used {
			if proj, ok := pub[host.Port{Number: p.Number, Proto: p.Proto}]; ok {
				used, owner, container = true, proj, true
			}
		}
		if !used {
			continue
		}
		busy = append(busy, strconv.Itoa(p.Number))
		if !e.isVoicePort(p) {
			allVoice = false
		}
		if !container {
			allContainer = false
		}
		if first == nil {
			first, firstOwner, firstContainer = &p, owner, container
		} else if owner != firstOwner {
			sameOwner = false
		}
	}
	switch {
	case first == nil:
		return pass(fmt.Sprintf("Ports %s are free.", ui.JoinAnd(numbers)))
	case len(busy) == 1:
		return e.portBusy(ctx, *first, firstOwner, firstContainer)
	}
	if allVoice && allContainer {
		if fix := e.voiceRemedy(ctx); fix != "" {
			return fail(fmt.Sprintf("Ports %s are already used by another instance on this machine.", ui.JoinAnd(busy)), fix)
		}
	}
	msg := fmt.Sprintf("Ports %s are already in use.", ui.JoinAnd(busy))
	fix := "Stop the programs that use them, then run rexctl doctor."
	switch {
	case sameOwner && firstContainer:
		msg = fmt.Sprintf("Ports %s are already in use by containers.", ui.JoinAnd(busy))
		fix = "Stop the containers that use them, then run rexctl doctor."
	case sameOwner && firstOwner != "":
		msg = fmt.Sprintf("Ports %s are already in use by %s.", ui.JoinAnd(busy), firstOwner)
		fix = fmt.Sprintf("Stop %s, then run rexctl doctor.", firstOwner)
	}
	if e.mode() == modeBundled && !slices.ContainsFunc(busy, func(n string) bool { return n != "80" && n != "443" }) {
		fix = e.portFix(ctx, *first, firstContainer)
	}
	return fail(msg, fix)
}

func (e *Env) portOwners(ctx context.Context) Result {
	pub, ok := e.published(ctx)
	if !ok || len(pub) == 0 {
		return skip("")
	}
	byProject := map[string][]int{}
	for port, proj := range pub {
		if proj == "" {
			continue
		}
		if !slices.Contains(byProject[proj], port.Number) {
			byProject[proj] = append(byProject[proj], port.Number)
		}
	}
	if len(byProject) == 0 {
		return skip("")
	}
	projects := make([]string, 0, len(byProject))
	for proj := range byProject {
		projects = append(projects, proj)
	}
	slices.Sort(projects)
	var parts []string
	for _, proj := range projects {
		nums := byProject[proj]
		slices.Sort(nums)
		strs := make([]string, len(nums))
		for i, n := range nums {
			strs[i] = strconv.Itoa(n)
		}
		parts = append(parts, fmt.Sprintf("%s uses %s", proj, ui.JoinAnd(strs)))
	}
	return pass(strings.Join(parts, ", ") + ".")
}

func (e *Env) isVoicePort(p stack.Port) bool {
	tcp, udp := e.voicePorts()
	return (p.Proto == "tcp" && p.Number == tcp) || (p.Proto == "udp" && p.Number == udp)
}

func (e *Env) voiceRemedy(ctx context.Context) string {
	tcp, udp := e.voicePorts()
	ftcp, fudp := e.nextFreeVoicePair(ctx, tcp, udp)
	if ftcp == 0 || fudp == 0 {
		return ""
	}
	return fmt.Sprintf("Run rexctl install --project NAME --voice-tcp-port %d --voice-udp-port %d to run a second instance alongside it.", ftcp, fudp)
}

func (e *Env) nextFreeVoicePair(ctx context.Context, tcp, udp int) (int, int) {
	pub, _ := e.published(ctx)
	free := func(number int, proto string) bool {
		if number < 1 || number > 65535 {
			return false
		}
		used, _, err := host.PortInUse(proto, number)
		if err != nil || used {
			return false
		}
		_, taken := pub[host.Port{Number: number, Proto: proto}]
		return !taken
	}
	for t, u := tcp+2, udp+2; t <= 65533 && u <= 65534; t, u = t+2, u+2 {
		if free(t, "tcp") && free(u, "udp") {
			return t, u
		}
	}
	return 0, 0
}

func (e *Env) wantedPorts() []stack.Port {
	var out []stack.Port
	ports := e.ports()
	if e.mode() == modeBundled {
		for _, p := range ports {
			if p.Number == 80 || p.Number == 443 {
				out = append(out, p)
			}
		}
	} else if port := bindPort(e.Bind); port > 0 {
		out = append(out, stack.Port{Number: port, Proto: "tcp"})
	}
	for _, p := range ports {
		if p.Number != 80 && p.Number != 443 {
			out = append(out, p)
		}
	}
	return out
}

func bindPort(bind string) int {
	_, port, err := net.SplitHostPort(bind)
	if err != nil {
		port = strings.TrimPrefix(bind, ":")
	}
	n, err := strconv.Atoi(port)
	if err != nil {
		return 0
	}
	return n
}

func (e *Env) portBusy(ctx context.Context, p stack.Port, owner string, container bool) Result {
	msg := fmt.Sprintf("Port %d is already in use.", p.Number)
	switch {
	case container:
		msg = fmt.Sprintf("Port %d is already in use by a container.", p.Number)
	case owner != "":
		msg = fmt.Sprintf("Port %d is already in use by %s.", p.Number, owner)
	}
	if fix := e.portFix(ctx, p, container); fix != "" {
		return fail(msg, fix)
	}
	what := "the program that uses it"
	switch {
	case container:
		what = "the container that uses it"
	case owner != "":
		what = owner
	}
	return fail(msg, fmt.Sprintf("Stop %s, then run rexctl doctor.", what))
}

func (e *Env) portFix(ctx context.Context, p stack.Port, container bool) string {
	if e.mode() == modeBundled && (p.Number == 80 || p.Number == 443) {
		return "Run rexctl install --behind-proxy to use your own proxy."
	}
	if e.mode() != modeBundled && p.Number == bindPort(e.Bind) {
		if free := nextFreePort(p.Number + 1); free > 0 {
			host, _, err := net.SplitHostPort(e.Bind)
			if err != nil || host == "" {
				host = "127.0.0.1"
			}
			return fmt.Sprintf("Run rexctl install --behind-proxy --bind %s to use another port.", net.JoinHostPort(host, strconv.Itoa(free)))
		}
	}
	if container && e.isVoicePort(p) {
		if fix := e.voiceRemedy(ctx); fix != "" {
			return fix
		}
	}
	return ""
}

func nextFreePort(from int) int {
	for port := from; port < from+20 && port <= 65535; port++ {
		used, _, err := host.PortInUse("tcp", port)
		if err == nil && !used {
			return port
		}
	}
	return 0
}
