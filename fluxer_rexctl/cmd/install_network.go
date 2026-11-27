// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"net"
	"net/netip"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/proxyconf"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	publicSchemeVar  = "FLUXER_PUBLIC_SCHEME"
	edgeTLSVar       = "FLUXER_EDGE_TLS"
	globalOptionsVar = "FLUXER_EDGE_GLOBAL_OPTIONS"
	plainHTTPNote    = "Fluxer uses http links for %s because Tor and I2P browsers open it over plain HTTP. To use https, run rexctl config set FLUXER_PUBLIC_SCHEME=https."
)

const (
	callsDirect = "direct"
	callsTURN   = "turn"
	callsOff    = "off"
	callsLater  = "later"
)

type netAnswer struct {
	checked   bool
	behind    bool
	kind      string
	reason    string
	busy      []int
	owner     string
	server    proxyconf.Server
	dns       checks.DNSAnswer
	lowPorts  bool
	choices   []ui.Option
	listen    string
	from      string
	calls     string
	turnHost  string
	turnPort  string
	turnUser  string
	turnPass  string
	plainHTTP bool
	orange    bool
	boot      bool
	edge      *instance.Instance
}

func (in *installer) addressFields() []ui.Field {
	return []ui.Field{{
		Flag:        "domain",
		FlagLabel:   "--domain",
		Title:       "What address will people use to open your instance?",
		Description: in.addressHint(),
		Placeholder: "chat.example.com",
		Default:     settings.NormalDomain(in.publicIP),
		Value:       &in.f.Domain,
		Validate:    validateDomain,
	}}
}

func (in *installer) addressHint() string {
	hint := "A domain name or an IP address both work."
	if in.tailnet != nil && in.tailnet.Name != "" {
		hint += " On your tailnet, this machine is " + in.tailnet.Name + "."
	}
	if in.lanIP != "" {
		hint += " Behind a home router, " + in.lanIP + " works for devices on the same network without forwarding ports."
	}
	return hint
}

func behindNAT(public, local string) string {
	ip, err := netip.ParseAddr(local)
	if err != nil || public == "" || local == public || !ip.IsPrivate() {
		return ""
	}
	return local
}

func (in *installer) tailnetInfo(ctx context.Context) host.Tailnet {
	if in.tailnet == nil {
		t := host.DetectTailnet(ctx)
		in.tailnet = &t
	}
	return *in.tailnet
}

func (in *installer) addressChecks(ctx context.Context, c *ui.Checklist) error {
	if in.publicDone || !in.app.UI.Interactive() {
		return nil
	}
	in.tailnetInfo(ctx)
	c.Check("public-ip", "Public address", func(ctx context.Context) (ui.State, string, error) {
		if ip := in.detectPublicIP(ctx); ip != "" {
			return ui.StateOK, ip, nil
		}
		return ui.StateSkipped, "rexctl couldn't find this machine's public IP address", nil
	})
	in.addrStage.Fields = in.addressFields()
	return nil
}

func (in *installer) detectPublicIP(ctx context.Context) string {
	if in.publicDone {
		return in.publicIP
	}
	in.publicDone = true
	ctx, cancel := context.WithTimeout(ctx, 4*time.Second)
	defer cancel()
	ip, err := host.PublicIP(ctx)
	if err != nil || checks.PrivateDomain(settings.NormalDomain(ip)) {
		return ""
	}
	in.publicIP = ip
	in.lanIP = behindNAT(ip, host.OutboundIPv4())
	return ip
}

func (in *installer) addressSummary() string {
	in.f.Domain = normaliseDomain(in.f.Domain)
	d := in.f.Domain
	switch {
	case checks.OverlayDomain(d):
		return d + ", a Tor, I2P or Lokinet name"
	case checks.TailnetDomain(d):
		return d + ", a Tailscale name"
	case checks.PrivateDomain(d):
		return d + ", a " + privateKind(d)
	}
	if _, ok := settings.DomainIP(d); ok {
		return d + ", a public IP address"
	}
	in.net.dns = checks.LookupDomain(in.ctx, d)
	switch in.net.dns.Points {
	case checks.PointsHere:
		return d + ", points at this machine"
	case checks.PointsCloudflare:
		return d + ", points at Cloudflare"
	case checks.PointsNowhere:
		return d + ", not in DNS yet"
	case checks.PointsElsewhere:
		return fmt.Sprintf("%s, points at %s", d, in.net.dns.Addr)
	}
	return d
}

func (in *installer) proxyDecided() bool {
	fl := in.c.Flags()
	return fl.Changed("behind-proxy") || fl.Changed("bind") || fl.Changed("proxy") || fl.Changed("proxy-from")
}

func (in *installer) networkChecks(ctx context.Context, c *ui.Checklist) error {
	n := &in.net
	n.busy, n.owner = busyWebPorts()
	n.edge = edgeHolder(in.dir)
	if in.f.Proxy == settings.ProxyInstance && n.edge == nil {
		return ui.Refused("No other Fluxer on this machine serves HTTPS for this one to go through.", "Pass --proxy local, --proxy remote or --proxy tunnel.")
	}
	n.lowPorts = in.rootlessPodman() && host.LowPortsBlocked(true)
	overlay := checks.OverlayDomain(in.f.Domain)
	if len(n.busy) > 0 {
		c.Check("ports", "Web ports", func(context.Context) (ui.State, string, error) {
			return ui.StateWarn, busyLine(n.busy, n.owner), nil
		})
	} else {
		c.Check("ports", "Web ports", func(context.Context) (ui.State, string, error) {
			return ui.StateOK, "free on this machine", nil
		})
	}
	def, reason := false, "Ports 80 and 443 are free, so rexctl can set up HTTPS for you with a free certificate."
	switch {
	case overlay:
		def, reason = true, "Tor and I2P addresses always go through a proxy."
	case len(n.busy) > 0 && n.edge != nil:
		def, reason = true, "The Fluxer in "+n.edge.Dir+" uses ports 80 and 443, so Yes is likely. It can serve this one too."
	case len(n.busy) > 0:
		def, reason = true, busyLine(n.busy, n.owner)+", so Yes is likely."
	case n.lowPorts && in.f.SetupPodman:
		reason = "Rootless Podman can't use ports 80 and 443 yet. With No, rexctl changes one setting on this machine with sudo so it can, and sets up HTTPS for you."
	case n.lowPorts:
		def, reason = true, "Rootless Podman can't use ports 80 and 443 without a change to this machine, so Yes is likely."
	case n.dns.Points == checks.PointsCloudflare:
		def, reason = true, in.f.Domain+" points at Cloudflare, so Yes is likely if you use Cloudflare Tunnel."
	case checks.TailnetDomain(in.f.Domain) && !in.native:
		def, reason = true, "Fluxer in a container can't get a certificate from Tailscale, so Yes is likely, with tailscale serve in front."
	case checks.TailnetDomain(in.f.Domain):
		reason = "Ports 80 and 443 are free. Fluxer gets the certificate for " + in.f.Domain + " from Tailscale. " + checks.TailnetCertFix + "."
	case checks.PrivateDomain(in.f.Domain):
		reason = "Ports 80 and 443 are free. " + in.f.Domain + " is a " + privateKind(in.f.Domain) + ", so Fluxer signs its own certificate." + in.publicNote()
	}
	n.reason = reason
	if !n.checked {
		n.checked = true
		n.behind = def
		if in.proxyDecided() {
			n.behind = in.f.BehindProxy
		}
		n.kind = in.f.Proxy
		n.from = in.f.ProxyFrom
		n.calls = callsLater
		n.turnPort = "3478"
		n.plainHTTP = true
		n.orange = true
		n.boot = in.f.BootService != instance.BootServiceOff
		if n.kind == "" && n.edge != nil && len(n.busy) > 0 {
			n.kind = settings.ProxyInstance
		}
		if n.kind == "" {
			n.kind = settings.ProxyLocal
			if n.dns.Points == checks.PointsNowhere || n.dns.Points == checks.PointsCloudflare && len(n.busy) == 0 || checks.TailnetDomain(in.f.Domain) {
				n.kind = settings.ProxyTunnel
			}
		}
	}
	if overlay {
		n.behind = true
	}
	n.server = proxyconf.Nginx
	if s, ok := proxyconf.ServerFor(n.owner); ok {
		n.server = s
	}
	n.choices = listenChoices()
	if in.netStage != nil {
		in.netStage.Fields = in.networkFields()
	}
	return nil
}

func busyWebPorts() ([]int, string) {
	var busy []int
	owner, same := "", true
	for _, port := range []int{80, 443} {
		used, who, err := host.PortInUse("tcp", port)
		if err != nil || !used {
			continue
		}
		if len(busy) == 0 {
			owner = who
		} else if who != owner {
			same = false
		}
		busy = append(busy, port)
	}
	if !same {
		owner = ""
	}
	return busy, owner
}

func busyLine(busy []int, owner string) string {
	nums := make([]string, len(busy))
	for i, n := range busy {
		nums[i] = strconv.Itoa(n)
	}
	subject := "Port " + nums[0] + " is"
	if len(nums) > 1 {
		subject = "Ports " + ui.JoinAnd(nums) + " are"
	}
	switch {
	case checks.ContainerOwner(owner):
		return subject + " in use by a container"
	case owner != "":
		return subject + " in use by " + owner
	}
	return subject + " in use"
}

func listenChoices() []ui.Option {
	var private, public []ui.Option
	addrs, err := net.InterfaceAddrs()
	if err != nil {
		return nil
	}
	for _, a := range addrs {
		n, ok := a.(*net.IPNet)
		if !ok {
			continue
		}
		ip, ok := netip.AddrFromSlice(n.IP)
		if !ok || !ip.Unmap().Is4() || !ip.IsGlobalUnicast() {
			continue
		}
		bind, ok := freeBind(ip.Unmap())
		if !ok {
			continue
		}
		if ip.IsPrivate() {
			private = append(private, ui.Option{Label: bind, Value: bind, Hint: "a private address on this machine"})
		} else {
			public = append(public, ui.Option{Label: bind, Value: bind, Hint: "a public address on this machine"})
		}
	}
	return append(private, public...)
}

func (in *installer) networkFields() []ui.Field {
	n := &in.net
	overlay := func() bool { return checks.OverlayDomain(in.f.Domain) }
	behind := func() bool { return n.behind }
	remote := func() bool { return n.behind && !overlay() && n.kind == settings.ProxyRemote }
	tunnelish := func() bool { return n.behind && (overlay() || n.kind == settings.ProxyTunnel) && !in.f.NoVoice }
	turn := func() bool { return tunnelish() && n.calls == callsTURN }
	fields := []ui.Field{
		{
			Flag:            "behind-proxy",
			Title:           "Is Fluxer behind your own reverse proxy or tunnel?",
			Kind:            ui.KindConfirm,
			Bool:            &n.behind,
			DescriptionFunc: func() string { return n.reason },
			Hide:            func() bool { return overlay() || in.proxyDecided() },
		},
		{
			Flag:    "proxy",
			Title:   "What is in front of Fluxer?",
			Value:   &n.kind,
			Options: in.proxyOptions(),
			Hide:    func() bool { return !behind() || overlay() || in.c.Flags().Changed("proxy") },
		},
	}
	if len(n.choices) > 0 {
		fields = append(fields, ui.Field{
			Title:   "Which address on this machine should your proxy connect to?",
			Value:   &n.listen,
			Options: n.choices,
			Default: n.choices[0].Value,
			Hide:    func() bool { return !remote() || in.c.Flags().Changed("bind") },
		})
	} else {
		fields = append(fields, ui.Field{
			Title:       "Which address on this machine should your proxy connect to?",
			Value:       &n.listen,
			Placeholder: "10.0.0.5:8080",
			Validate:    validateBind,
			Hide:        func() bool { return !remote() || in.c.Flags().Changed("bind") },
		})
	}
	fields = append(fields,
		proxyFromField(&n.from, func() bool { return !remote() }),
		ui.Field{
			Title:       "Do people open this address over plain HTTP?",
			Description: "Lokinet and .alt addresses often have no certificate.",
			Kind:        ui.KindConfirm,
			Bool:        &n.plainHTTP,
			Hide:        func() bool { return !needsSchemeQuestion(in.f.Domain) },
		},
		ui.Field{
			Title:       "Can people connect to this machine directly for calls on TCP 7881 and UDP 7882?",
			Description: "Call audio and video can't go through a tunnel. They can go through a TURN relay on a machine with a public address.",
			Value:       &n.calls,
			Options: []ui.Option{
				{Label: "Yes, those ports are open", Value: callsDirect},
				{Label: "No, use a TURN relay", Value: callsTURN},
				{Label: "No, run without calls for now", Value: callsOff},
				{Label: "I'll sort it out later", Value: callsLater},
			},
			Default: callsLater,
			Hide:    func() bool { return !tunnelish() },
		},
		ui.Field{Title: "TURN relay address", Placeholder: "turn.example.com", Value: &n.turnHost, Validate: validateSetupHost, Hide: func() bool { return !turn() }},
		ui.Field{Title: "TURN relay port", Value: &n.turnPort, Default: "3478", Validate: validateSetupPort, Hide: func() bool { return !turn() }},
		ui.Field{Title: "TURN username", Value: &n.turnUser, Optional: true, Hide: func() bool { return !turn() }},
		ui.Field{Title: "TURN password", Value: &n.turnPass, Secret: true, Optional: true, Hide: func() bool { return !turn() || n.turnUser == "" }},
		ui.Field{
			Title:       "Is Cloudflare's proxy (the orange cloud) on for this domain?",
			Description: "Fluxer then trusts the visitor address Cloudflare passes on.",
			Kind:        ui.KindConfirm,
			Bool:        &n.orange,
			Hide:        func() bool { return n.behind || n.dns.Points != checks.PointsCloudflare },
		},
		ui.Field{
			Title: bootServiceQuestion,
			Kind:  ui.KindConfirm,
			Bool:  &n.boot,
			Hide:  func() bool { return !in.bootQuestion() },
		},
	)
	return fields
}

func needsSchemeQuestion(domain string) bool {
	return checks.OverlayDomain(domain) && !checks.PlainHTTPDomain(domain)
}

func (in *installer) bootQuestion() bool {
	if in.c.Flags().Changed("boot-service") || !host.BootUnitSupported() {
		return false
	}
	return in.native || !in.rootlessPodman() && (in.engine == nil || in.engine.Kind != engine.Podman)
}

func (in *installer) proxyOptions() []ui.Option {
	var opts []ui.Option
	if e := in.net.edge; e != nil {
		opts = append(opts, ui.Option{Label: "The Fluxer in " + e.Dir, Value: settings.ProxyInstance, Hint: "it serves HTTPS for this one too"})
	}
	return append(opts,
		ui.Option{Label: "A web server on this machine", Value: settings.ProxyLocal, Hint: "nginx, Caddy, Apache or Traefik"},
		ui.Option{Label: "A proxy on another machine", Value: settings.ProxyRemote, Hint: "a load balancer or a web server elsewhere"},
		ui.Option{Label: "A tunnel", Value: settings.ProxyTunnel, Hint: "Cloudflare Tunnel, Tailscale Funnel or ngrok"},
	)
}

func (in *installer) networkSummary() string {
	in.settleNetwork()
	n := in.net
	f := in.f
	switch {
	case !f.BehindProxy && n.orange && n.dns.Points == checks.PointsCloudflare:
		return "Behind Cloudflare's proxy, rexctl sets up HTTPS for you"
	case !f.BehindProxy && checks.PrivateDomain(f.Domain):
		return "Fluxer signs its own certificate for this " + privateKind(f.Domain)
	case !f.BehindProxy:
		return "rexctl sets up HTTPS for you with a free certificate"
	}
	switch f.Proxy {
	case settings.ProxyInstance:
		if n.edge != nil {
			return "Through the Fluxer in " + n.edge.Dir + ", Fluxer listens on " + f.Bind
		}
	case settings.ProxyOverlay:
		return "Behind your Tor, I2P or Lokinet proxy, Fluxer listens on " + f.Bind
	case settings.ProxyRemote:
		return fmt.Sprintf("Behind a proxy at %s, Fluxer listens on %s", proxyFromWords(n.from), f.Bind)
	case settings.ProxyTunnel:
		return "Behind a tunnel, Fluxer listens on " + f.Bind
	}
	if _, known := proxyconf.ServerFor(n.owner); !known {
		return "Behind a web server on this machine, Fluxer listens on " + f.Bind
	}
	return fmt.Sprintf("Behind %s on this machine, Fluxer listens on %s", n.server.Name(), f.Bind)
}

func (in *installer) settleNetwork() {
	n := &in.net
	f := in.f
	if in.resuming != nil {
		return
	}
	f.Domain = normaliseDomain(f.Domain)
	if !n.checked {
		n.behind = f.BehindProxy
		n.kind = f.Proxy
		n.from = f.ProxyFrom
		n.boot = f.BootService != instance.BootServiceOff
	}
	if checks.OverlayDomain(f.Domain) {
		n.behind, n.kind = true, settings.ProxyOverlay
	}
	f.BehindProxy = n.behind
	if !n.behind {
		f.Proxy = ""
		return
	}
	if n.kind == "" {
		n.kind = settings.ProxyLocal
	}
	f.Proxy = n.kind
	f.ProxyFrom = strings.TrimSpace(n.from)
	switch {
	case in.c.Flags().Changed("bind"):
	case n.kind == settings.ProxyRemote && n.listen != "":
		if b, err := settings.NormaliseBind(n.listen); err == nil {
			f.Bind = b
		}
	default:
		f.Bind = freeLoopbackBind()
	}
}

func freeLoopbackBind() string {
	if bind, ok := freeBind(netip.MustParseAddr("127.0.0.1")); ok {
		return bind
	}
	return settings.DefaultBind
}

func freeBind(ip netip.Addr) (string, bool) {
	for port := 8080; port < 8180; port++ {
		if used, _, err := host.PortInUse("tcp", port); err != nil || used {
			continue
		}
		bind := netip.AddrPortFrom(ip, uint16(port)).String()
		l, err := net.Listen("tcp", bind)
		if err != nil {
			continue
		}
		_ = l.Close()
		return bind, true
	}
	return "", false
}

func (in *installer) restoreNetwork(inst *instance.Instance) {
	st := inst.State.Settings
	mode := st[settings.EdgeMode]
	behind := mode == settings.EdgeProxy || mode == settings.EdgeTunnel
	bind := st[settings.EdgeBind]
	if b, err := settings.NormaliseBind(bind); err == nil {
		bind = b
	}
	if in.proxyDecided() && (in.f.BehindProxy != behind || behind && in.c.Flags().Changed("bind") && in.f.Bind != bind) {
		fix := "Run rexctl proxy off after the install to serve HTTPS directly."
		if in.f.BehindProxy {
			fix = "Run rexctl proxy on --bind " + in.f.Bind + " after the install to change it."
		}
		in.app.UI.Warn("This install keeps the network setup it started with.", fix)
	}
	in.f.BehindProxy = behind
	in.f.Bind = bind
	in.f.Proxy = st[settings.EdgeProxyKind]
	in.net.behind, in.net.kind, in.net.checked = behind, in.f.Proxy, true
	in.net.boot = inst.State.BootService != instance.BootServiceOff
	if in.f.Proxy == "" && behind {
		in.f.Proxy = settings.ProxyLocal
	}
	in.net.server = proxyconf.Nginx
	if _, owner := busyWebPorts(); owner != "" {
		if s, ok := proxyconf.ServerFor(owner); ok {
			in.net.server = s
		}
	}
}

func (in *installer) networkSettings() map[string]string {
	f, n := in.f, in.net
	out := map[string]string{}
	if checks.PrivateDomain(f.Domain) && !f.BehindProxy {
		out[edgeTLSVar] = "internal"
	}
	if !f.BehindProxy {
		if n.orange && n.dns.Points == checks.PointsCloudflare {
			out[settings.EdgeTrusted] = proxyconf.CloudflareTrusted()
		}
		return out
	}
	out[settings.EdgeMode] = settings.EdgeProxy
	out[settings.EdgeBind] = f.Bind
	out[settings.EdgeProxyKind] = f.Proxy
	if in.plainHTTP() {
		out[publicSchemeVar] = "http"
	}
	if trusted, ok := trustedFor(f.ProxyFrom); ok && f.Proxy == settings.ProxyRemote {
		out[settings.EdgeTrusted] = trusted
	}
	if f.Proxy == settings.ProxyTunnel || f.Proxy == settings.ProxyOverlay {
		switch n.calls {
		case callsOff:
			out[settings.VoiceBundled] = "false"
		case callsTURN:
			port, _ := strconv.Atoi(n.turnPort)
			out[settings.TurnServersVar] = settings.TurnServersValue([]settings.TurnServer{{
				Host: strings.TrimSpace(n.turnHost), Port: port, Protocol: "udp",
				Username: strings.TrimSpace(n.turnUser), Credential: n.turnPass,
			}})
		}
	}
	return out
}

func privateSource(v string) bool {
	if a, err := netip.ParseAddr(v); err == nil {
		return a.IsPrivate() || a.IsLoopback()
	}
	if p, err := netip.ParsePrefix(v); err == nil {
		return p.Addr().IsPrivate() || p.Addr().IsLoopback()
	}
	return false
}

func proxyFromField(v *string, hide func() bool) ui.Field {
	return ui.Field{
		Flag:        "proxy-from",
		FlagLabel:   "--proxy-from",
		Title:       "What address does your proxy connect from?",
		Description: "Fluxer trusts the visitor address your proxy passes on only from here, and the firewall lets only this address in. Enter private for any address on your private network.",
		Placeholder: "203.0.113.10",
		Value:       v,
		Validate:    validateProxyFrom,
		Hide:        hide,
	}
}

func (in *installer) plainHTTP() bool {
	d := in.f.Domain
	if !in.f.BehindProxy {
		return false
	}
	if checks.PlainHTTPDomain(d) {
		return true
	}
	return needsSchemeQuestion(d) && in.net.plainHTTP
}

func (in *installer) networkRow() ui.KV {
	return ui.KV{Key: "Network", Value: in.networkSummary(), Edit: 'n'}
}

func (in *installer) publicNote() string {
	if _, ok := settings.DomainIP(in.f.Domain); !ok || in.publicIP == "" {
		return ""
	}
	return " This machine's public IP address is " + settings.NormalDomain(in.publicIP) + "."
}

func privateKind(domain string) string {
	if _, ok := settings.DomainIP(domain); ok {
		return "private address"
	}
	return "private name"
}
