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
	Register(Check{ID: "ports", Phases: Preinstall, Run: runPorts})
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
	wanted := e.wantedPorts()
	var numbers, busy []string
	var first *stack.Port
	firstOwner := ""
	sameOwner := true
	seen := map[int]bool{}
	for _, p := range wanted {
		if seen[p.Number] {
			continue
		}
		used, owner, err := host.PortInUse(p.Proto, p.Number)
		if err != nil {
			return skip(fmt.Sprintf("Couldn't check whether port %d is free.", p.Number))
		}
		seen[p.Number] = true
		numbers = append(numbers, strconv.Itoa(p.Number))
		if !used {
			continue
		}
		busy = append(busy, strconv.Itoa(p.Number))
		if first == nil {
			first, firstOwner = &p, owner
		} else if owner != firstOwner {
			sameOwner = false
		}
	}
	switch {
	case first == nil:
		return pass(fmt.Sprintf("Ports %s are free.", ui.JoinAnd(numbers)))
	case len(busy) == 1:
		return e.portBusy(*first, firstOwner)
	}
	msg := fmt.Sprintf("Ports %s are already in use.", ui.JoinAnd(busy))
	fix := "Stop the programs that use them, then run rexctl doctor."
	switch {
	case sameOwner && containerOwners[firstOwner]:
		msg = fmt.Sprintf("Ports %s are already in use by containers.", ui.JoinAnd(busy))
		fix = "Stop the containers that use them, then run rexctl doctor."
	case sameOwner && firstOwner != "":
		msg = fmt.Sprintf("Ports %s are already in use by %s.", ui.JoinAnd(busy), firstOwner)
		fix = fmt.Sprintf("Stop %s, then run rexctl doctor.", firstOwner)
	}
	if e.mode() == modeBundled && !slices.ContainsFunc(busy, func(n string) bool { return n != "80" && n != "443" }) {
		fix = e.portFix(*first)
	}
	return fail(msg, fix)
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

func (e *Env) portBusy(p stack.Port, owner string) Result {
	msg := fmt.Sprintf("Port %d is already in use.", p.Number)
	switch {
	case containerOwners[owner]:
		msg = fmt.Sprintf("Port %d is already in use by a container.", p.Number)
	case owner != "":
		msg = fmt.Sprintf("Port %d is already in use by %s.", p.Number, owner)
	}
	if fix := e.portFix(p); fix != "" {
		return fail(msg, fix)
	}
	what := "the program that uses it"
	switch {
	case containerOwners[owner]:
		what = "the container that uses it"
	case owner != "":
		what = owner
	}
	return fail(msg, fmt.Sprintf("Stop %s, then run rexctl doctor.", what))
}

func (e *Env) portFix(p stack.Port) string {
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
