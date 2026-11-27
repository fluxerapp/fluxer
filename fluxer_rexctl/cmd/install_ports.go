// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func (in *installer) readyChecks(ctx context.Context, c *ui.Checklist) error {
	in.failed = nil
	in.planFor("ready")
	in.settleNetwork()
	in.resolveVoicePorts(ctx, c)
	ids := []string{"ports", "low-ports", "memory-overcommit"}
	if in.plansLowPorts() {
		ids = slices.DeleteFunc(ids, func(id string) bool { return id == "low-ports" })
	}
	in.checkRows(ctx, c, ids...)
	if err := in.checkDNS(ctx, c); err != nil {
		return err
	}
	if in.failed != nil {
		return in.failed
	}
	if err := in.planLowPorts(c); err != nil {
		return err
	}
	in.planFirewall(ctx, c)
	return nil
}

func (in *installer) storageChecks(ctx context.Context, c *ui.Checklist) error {
	in.failed = nil
	in.checkRows(ctx, c, "objects-filesystem")
	return in.failed
}

func (in *installer) checkDNS(ctx context.Context, c *ui.Checklist) error {
	chk, ok := checks.Get("dns")
	if !ok {
		return nil
	}
	for {
		r := checks.RunIn(ctx, in.checkEnv(), checks.Preinstall, chk)
		if r.Status == checks.Skip && r.Message == "" {
			return nil
		}
		c.Check("dns", dnsLabel(in.f.Domain), func(context.Context) (ui.State, string, error) {
			if r.Status == checks.Fail {
				return ui.StateFail, r.Message, nil
			}
			return in.resultRow(r)
		})
		if r.Status != checks.Fail {
			return nil
		}
		if in.f.ForMigrate {
			return ui.Refused(r.Message, in.checkFix(r.Fix))
		}
		if !in.app.UI.Interactive() {
			return ui.Refused(r.Message, strings.TrimSpace(r.Fix+" Or, if a proxy or tunnel is in front of Fluxer, run "+in.rerun()+" --behind-proxy."))
		}
		choice, err := c.Recover(r.Message+" What do you want to do?", []ui.Option{
			{Label: "Check again", Value: "again"},
			{Label: "It's behind a proxy or tunnel", Value: "proxy"},
			{Label: "Stop", Value: "stop"},
		})
		if err != nil {
			return err
		}
		switch choice {
		case "stop":
			return ui.Refused(r.Message, strings.TrimSpace(r.Fix))
		case "proxy":
			in.net.behind = true
			if in.net.kind == "" || in.net.kind == settings.ProxyLocal && len(in.net.busy) == 0 {
				in.net.kind = settings.ProxyTunnel
			}
			in.settleNetwork()
			c.Check("network", "Network", func(context.Context) (ui.State, string, error) {
				return ui.StateOK, in.networkSummary(), nil
			})
			return nil
		}
	}
}

func dnsLabel(domain string) string {
	if _, ok := settings.DomainIP(domain); ok {
		return "Address"
	}
	return "DNS"
}

func (in *installer) plansLowPorts() bool {
	return !in.f.BehindProxy && in.rootlessPodman() && in.f.SetupPodman
}

func (in *installer) planLowPorts(c *ui.Checklist) error {
	if in.f.BehindProxy || !in.rootlessPodman() || !host.LowPortsBlocked(true) {
		return nil
	}
	fix := "Run " + in.rerun() + " --behind-proxy to use your own reverse proxy or tunnel instead."
	if in.f.ForMigrate {
		fix = "Put your own reverse proxy or tunnel in front with rexctl proxy on. Then run " + migrateCommand + " again."
	}
	refusal := ui.Refused("Fluxer can't serve HTTPS from rootless Podman without ports 80 and 443.", fix)
	if err := in.allowed("setup-podman", in.f.SetupPodman, refusal); err != nil {
		return err
	}
	if !in.f.SetupPodman {
		return refusal
	}
	c.Check("low-ports", "Low ports", func(context.Context) (ui.State, string, error) {
		return ui.StateWarn, "Rootless Podman can't use ports 80 and 443 yet. rexctl lets programs that don't run as root use ports from 80 up.", nil
	})
	in.change(ui.PlanChange, "Let programs that don't run as root use ports from 80 up", &ui.Task{ID: "low-ports", Title: "Opening ports 80 and up to rootless Podman", Run: func(ctx context.Context, s *ui.Step) error {
		if err := host.RunSteps(ctx, host.LowPortsSysctl.Steps()); err != nil {
			return ui.Wrap(err, "Couldn't open ports 80 and up to programs that don't run as root.", "Run rexctl install --verbose to see why.")
		}
		s.Done("Programs that don't run as root can use ports 80 and up.")
		return nil
	}})
	return nil
}

func (in *installer) settings() map[string]string {
	out := map[string]string{}
	if in.f.VoiceTCPPort > 0 {
		out[stack.VoiceTCPPortKey] = strconv.Itoa(in.f.VoiceTCPPort)
	}
	if in.f.VoiceUDPPort > 0 {
		out[stack.VoiceUDPPortKey] = strconv.Itoa(in.f.VoiceUDPPort)
	}
	if in.f.NoVoice || in.net.calls == callsOff && in.f.BehindProxy {
		out[stack.VoiceBundledKey] = "false"
	}
	if in.f.Storage.Endpoint != "" {
		out[settings.StorageEndpoint] = in.f.Storage.Endpoint
	}
	return out
}

func (in *installer) resolveVoicePorts(ctx context.Context, c *ui.Checklist) {
	if in.f.NoVoice {
		return
	}
	tcpSet := in.f.VoiceTCPPort > 0
	udpSet := in.f.VoiceUDPPort > 0
	if tcpSet && udpSet {
		return
	}
	pub := in.publishedPorts(ctx)
	for _, v := range instance.RecordedVoicePorts() {
		pub[host.Port{Number: v.Number, Proto: v.Proto}] = true
	}
	free := func(number int, proto string) bool {
		if number < 1 || number > 65535 {
			return false
		}
		used, _, err := host.PortInUse(proto, number)
		if err != nil || used {
			return false
		}
		return !pub[host.Port{Number: number, Proto: proto}]
	}
	switch {
	case !tcpSet && !udpSet:
		tcp, udp := pickVoicePair(free)
		if tcp == 0 || tcp == 7881 {
			return
		}
		in.f.VoiceTCPPort, in.f.VoiceUDPPort = tcp, udp
		c.Check("voice-ports", "Voice ports", func(context.Context) (ui.State, string, error) {
			return ui.StateOK, fmt.Sprintf("7881 and 7882 are in use, so this instance uses %d and %d for calls", tcp, udp), nil
		})
	case !tcpSet:
		for tcp := 7881; tcp <= 65535; tcp += 2 {
			if tcp != in.f.VoiceUDPPort && free(tcp, "tcp") {
				in.f.VoiceTCPPort = tcp
				return
			}
		}
	case !udpSet:
		for udp := 7882; udp <= 65535; udp += 2 {
			if udp != in.f.VoiceTCPPort && free(udp, "udp") {
				in.f.VoiceUDPPort = udp
				return
			}
		}
	}
}

func pickVoicePair(free func(number int, proto string) bool) (int, int) {
	for tcp, udp := 7881, 7882; tcp <= 65533 && udp <= 65534; tcp, udp = tcp+2, udp+2 {
		if free(tcp, "tcp") && free(udp, "udp") {
			return tcp, udp
		}
	}
	return 0, 0
}

func (in *installer) publishedPorts(ctx context.Context) map[host.Port]bool {
	out := map[host.Port]bool{}
	e := in.engine
	if e == nil {
		e = in.found
	}
	if e == nil {
		return out
	}
	owners, err := e.PublishedHostPorts(ctx)
	if err != nil {
		return out
	}
	for _, o := range owners {
		out[host.Port{Number: o.Number, Proto: o.Proto}] = true
	}
	return out
}

func (in *installer) firewallPorts() []host.Port {
	var out []host.Port
	for _, p := range stack.Ports(in.settings()) {
		if in.f.BehindProxy && (p.Number == 80 || p.Number == 443) {
			continue
		}
		if p.Number == bindPortOf(in.f.Bind) {
			continue
		}
		out = append(out, host.Port{Number: p.Number, Proto: p.Proto})
	}
	if in.f.BehindProxy && in.f.Proxy == settings.ProxyRemote && !settings.BindIsLoopback(in.f.Bind) {
		out = append(out, proxyPorts(in.f.Bind, in.f.ProxyFrom)...)
	}
	return out
}

func bindPortOf(bind string) int {
	i := strings.LastIndex(bind, ":")
	if i < 0 {
		return 0
	}
	n, _ := strconv.Atoi(bind[i+1:])
	return n
}

func portList(ports []host.Port) string {
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
	return ui.JoinAnd(parts)
}

func portWords(ports []host.Port) string {
	var parts []string
	for _, p := range ports {
		w := strings.ToUpper(p.Proto) + " " + strconv.Itoa(p.Number)
		if p.Source != "" {
			w += " from " + p.Source
		}
		parts = append(parts, w)
	}
	return ui.JoinAnd(parts)
}

func (in *installer) providerFirewallLine() string {
	d := in.f.Domain
	if in.f.BehindProxy || checks.PrivateDomain(d) || checks.OverlayDomain(d) {
		return ""
	}
	ports := in.firewallPorts()
	if len(ports) == 0 {
		return ""
	}
	return fmt.Sprintf("If your hosting provider has a firewall, such as a Hetzner Cloud Firewall or an AWS security group, open %s in it.", portWords(ports))
}

func (in *installer) planFirewall(ctx context.Context, c *ui.Checklist) {
	if line := in.providerFirewallLine(); line != "" {
		c.Check("provider-firewall", "Provider", func(context.Context) (ui.State, string, error) {
			return ui.StateWarn, line, nil
		})
	}
	fw, err := host.DetectFirewall(ctx)
	if err == nil && fw == host.FirewallNone && in.native {
		if closed := host.IptablesClosed(ctx, in.firewallPorts()); len(closed) > 0 {
			c.Check("firewall", "Firewall", func(context.Context) (ui.State, string, error) {
				return ui.StateWarn, iptablesLine(closed), nil
			})
		}
		return
	}
	if err != nil || fw == host.FirewallNone {
		return
	}
	closed, err := host.ClosedPorts(ctx, fw, in.firewallPorts())
	if err != nil || len(closed) == 0 {
		return
	}
	if !in.f.OpenFirewall || in.allowed("open-firewall", true, nil) != nil {
		c.Check("firewall", "Firewall", func(context.Context) (ui.State, string, error) {
			return ui.StateWarn, fmt.Sprintf("%s %s closed in %s. Open them before people connect.", portWords(closed), isAre(len(closed)), fw), nil
		})
		return
	}
	c.Check("firewall", "Firewall", func(context.Context) (ui.State, string, error) {
		return ui.StateOK, fmt.Sprintf("rexctl opens %s in %s", portWords(closed), fw), nil
	})
	in.change(ui.PlanOpen, fmt.Sprintf("Open %s in %s", portWords(closed), fw), &ui.Task{ID: "firewall", Title: "Opening the firewall", Run: func(ctx context.Context, s *ui.Step) error {
		if err := host.OpenPorts(ctx, fw, closed); err != nil {
			return ui.Wrap(err, "Couldn't open the ports.", fmt.Sprintf("Open ports %s in the firewall yourself, then run rexctl install again.", portList(closed)))
		}
		for _, p := range closed {
			in.opened = append(in.opened, instance.OpenedPort{Firewall: string(fw), Number: p.Number, Proto: p.Proto, Source: p.Source})
		}
		s.Done("Opened %s in %s.", portWords(closed), fw)
		return nil
	}})
}

func isAre(n int) string {
	if n == 1 {
		return "is"
	}
	return "are"
}

func iptablesLine(closed []host.Port) string {
	var cmds []string
	for _, proto := range []string{"tcp", "udp"} {
		var nums []string
		for _, p := range closed {
			if p.Proto == proto {
				nums = append(nums, strconv.Itoa(p.Number))
			}
		}
		if len(nums) > 0 {
			cmds = append(cmds, fmt.Sprintf("sudo iptables -I INPUT -p %s -m multiport --dports %s -j ACCEPT", proto, strings.Join(nums, ",")))
		}
	}
	return fmt.Sprintf("iptables on this machine rejects %s, as on Oracle Cloud images. Run %s, then sudo netfilter-persistent save.", portWords(closed), strings.Join(cmds, " and "))
}
