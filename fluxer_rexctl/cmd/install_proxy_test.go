// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func TestProxyHintOffer(t *testing.T) {
	cases := []struct {
		name   string
		hint   proxyHint
		ok     bool
		def    bool
		reason string
	}{
		{"nothing in the way", proxyHint{Interactive: true}, false, false, ""},
		{"not interactive", proxyHint{Busy: []int{443}, LowPorts: "Podman"}, false, false, ""},
		{"flag already decides", proxyHint{Interactive: true, Decided: true, Busy: []int{80, 443}}, false, false, ""},
		{"one port busy", proxyHint{Interactive: true, Busy: []int{443}}, true, true, "Port 443 on this machine is already in use."},
		{"both ports busy by a program", proxyHint{Interactive: true, Busy: []int{80, 443}, Owner: "caddy"}, true, true, "Ports 80 and 443 on this machine are already in use by caddy."},
		{"busy by a container", proxyHint{Interactive: true, Busy: []int{80}, Owner: "docker-proxy"}, true, true, "Port 80 on this machine is already in use by a container."},
		{"busy wins over low ports", proxyHint{Interactive: true, Busy: []int{443}, LowPorts: "Podman"}, true, true, "Port 443 on this machine is already in use."},
		{"rootless podman", proxyHint{Interactive: true, LowPorts: "Podman"}, true, false, "Rootless Podman can't use ports 80 and 443 without a change to this machine."},
	}
	for _, c := range cases {
		reason, def, ok := c.hint.offer()
		if ok != c.ok || def != c.def || reason != c.reason {
			t.Errorf("%s: got (%q, %v, %v), want (%q, %v, %v)", c.name, reason, def, ok, c.reason, c.def, c.ok)
		}
	}
}

func TestValidateBind(t *testing.T) {
	for v, ok := range map[string]bool{
		"127.0.0.1:8080": true,
		"[::1]:8080":     true,
		":8080":          true,
		"127.0.0.1":      false,
		"":               false,
	} {
		if err := validateBind(v); (err == nil) != ok {
			t.Errorf("%q: got %v", v, err)
		}
	}
}

func TestOfferProxyNonInteractive(t *testing.T) {
	for _, yes := range []bool{false, true} {
		var out bytes.Buffer
		app := &App{UI: ui.New(ui.Options{Stdin: strings.NewReader("y\n"), Stdout: &out, Stderr: &out, Yes: yes})}
		c := newInstallCmd(app)
		if err := c.ParseFlags(nil); err != nil {
			t.Fatal(err)
		}
		f := &installFlags{Bind: "127.0.0.1:8080"}
		in := &installer{app: app, c: c, f: f, ran: map[string]bool{}}
		if err := in.offerProxy(); err != nil {
			t.Fatalf("yes=%v: %v", yes, err)
		}
		if in.offered || f.BehindProxy || len(in.plan) > 0 || out.Len() > 0 {
			t.Errorf("yes=%v: offered=%v behind=%v plan=%q output=%q", yes, in.offered, f.BehindProxy, in.plan, out.String())
		}
	}
}
