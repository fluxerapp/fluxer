// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"fmt"
	"net"
	"net/netip"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/proxyconf"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newProxyCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "proxy",
		Short: "Use your own proxy or tunnel in front of Fluxer",
		Example: "rexctl proxy on --bind 127.0.0.1:8081\n" +
			"rexctl proxy config nginx\n" +
			"rexctl proxy off",
		Args: noArgs,
	}
	var bind, kind, from string
	on := &cobra.Command{
		Use:   "on",
		Short: "Serve plain HTTP for your own proxy or tunnel",
		Example: "rexctl proxy on\n" +
			"rexctl proxy on --bind 127.0.0.1:8081 --proxy tunnel\n" +
			"rexctl proxy on --bind 10.0.0.5:8080 --proxy-from 10.0.0.2",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			changes := map[string]*string{}
			if c.Flags().Changed("bind") {
				norm, err := settings.NormaliseBind(bind)
				if err != nil {
					return ui.Usage("Pass --bind as an address and port, like " + settings.DefaultBind + ".")
				}
				changes[settings.EdgeBind] = &norm
			}
			if c.Flags().Changed("proxy") || c.Flags().Changed("kind") {
				if kind != settings.ProxyLocal && kind != settings.ProxyRemote && kind != settings.ProxyTunnel {
					return ui.Usage("Pass --proxy local, --proxy remote or --proxy tunnel.")
				}
				changes[settings.EdgeProxyKind] = &kind
			}
			if from != "" {
				if err := validateProxyFrom(from); err != nil {
					return ui.Usage(fmt.Sprintf("--proxy-from can't be %s. %s", from, err.Error()))
				}
			}
			return proxyOn(c, app, changes, strings.TrimSpace(from))
		},
	}
	on.Flags().StringVar(&bind, "bind", "", "address your proxy or tunnel connects to, kept when left out ("+settings.DefaultBind+" the first time)")
	on.Flags().StringVar(&kind, "proxy", "", "what is in front of Fluxer (local, remote or tunnel)")
	on.Flags().StringVar(&from, "proxy-from", "", "addresses or ranges your proxy connects from, or private for any private address, when other machines can connect to --bind")
	on.Flags().StringVar(&kind, "kind", "", "")
	on.Flags().StringVar(&from, "from", "", "")
	_ = on.Flags().MarkHidden("kind")
	_ = on.Flags().MarkHidden("from")
	off := &cobra.Command{
		Use:     "off",
		Short:   "Serve HTTPS itself with a Let's Encrypt certificate",
		Example: "rexctl proxy off",
		Args:    noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return proxyOff(c.Context(), app)
		},
	}
	config := &cobra.Command{
		Use:       "config [nginx|caddy|apache|traefik|cloudflared]",
		Short:     "Print a ready config for your proxy or tunnel",
		Example:   "rexctl proxy config nginx\nrexctl proxy config caddy > /etc/caddy/fluxer.caddy",
		Args:      maxArgs(1),
		ValidArgs: []string{"nginx", "caddy", "apache", "traefik", "cloudflared"},
		RunE: func(c *cobra.Command, args []string) error {
			name := ""
			if len(args) > 0 {
				name = args[0]
			}
			return runProxyConfig(app, name)
		},
	}
	c.AddCommand(on, off, config)
	return c
}

func runProxyConfig(app *App, name string) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	s := proxyconf.Nginx
	if name != "" {
		var ok bool
		if s, ok = proxyconf.Parse(name); !ok {
			return ui.Usage("Pass nginx, caddy, apache, traefik or cloudflared.")
		}
	} else if inst.State.Settings[settings.EdgeProxyKind] == settings.ProxyTunnel {
		s = proxyconf.Cloudflared
	}
	bind, err := settings.NormaliseBind(cmp.Or(inst.State.Settings[settings.EdgeBind], settings.DefaultBind))
	if err != nil {
		bind = settings.DefaultBind
	}
	p := proxyParams(inst.Domain(), bind)
	text, err := proxyconf.Render(s, p)
	if err != nil {
		return err
	}
	if isProxied(inst) {
		_, _ = proxyconf.Write(inst.Dir, p)
	} else {
		app.UI.Warn("Fluxer serves HTTPS itself right now, so a proxy in front of it can't connect yet.", "Run rexctl proxy on first.")
	}
	fmt.Fprint(app.UI.Stdout(), text)
	return nil
}

func firewallHolds(ctx context.Context, app *App, inst *instance.Instance) bool {
	if rt.KindOf(inst) == rt.Native {
		return true
	}
	e, err := app.anyEngine(ctx, inst)
	return err == nil && e.Kind == engine.Podman && e.Rootless
}

func isProxied(inst *instance.Instance) bool {
	mode := inst.State.Settings[settings.EdgeMode]
	return mode == settings.EdgeProxy || mode == settings.EdgeTunnel
}

func proxyOn(c *cobra.Command, app *App, changes map[string]*string, from string) error {
	ctx := c.Context()
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	st := inst.State.Settings
	mode := settings.EdgeProxy
	if st[settings.EdgeMode] == settings.EdgeTunnel {
		mode = settings.EdgeTunnel
	}
	changes[settings.EdgeMode] = &mode
	if _, ok := changes[settings.EdgeBind]; !ok {
		bind := cmp.Or(st[settings.EdgeBind], settings.DefaultBind)
		if norm, err := settings.NormaliseBind(bind); err == nil {
			bind = norm
		}
		changes[settings.EdgeBind] = &bind
	}
	bind := *changes[settings.EdgeBind]
	public := !settings.BindIsLoopback(bind)
	if _, ok := changes[settings.EdgeProxyKind]; !ok && st[settings.EdgeProxyKind] == "" {
		kind := settings.ProxyLocal
		switch {
		case checks.OverlayDomain(inst.Domain()):
			kind = settings.ProxyOverlay
		case public:
			kind = settings.ProxyRemote
		}
		changes[settings.EdgeProxyKind] = &kind
	}
	if !public && from != "" {
		return ui.Usage("--proxy-from only works when --bind is an address other machines can connect to.")
	}
	if public {
		ok, err := confirmPublicBind(app, bind, firewallHolds(ctx, app, inst))
		if err != nil || !ok {
			return err
		}
		if err := app.UI.Ask(c, proxyFromField(&from, nil)); err != nil {
			return err
		}
		from = strings.TrimSpace(from)
		if trusted, ok := trustedFor(from); ok {
			changes[settings.EdgeTrusted] = &trusted
		}
	}
	plainHTTP := checks.PlainHTTPDomain(inst.Domain()) && st[publicSchemeVar] == ""
	if plainHTTP {
		scheme := "http"
		changes[publicSchemeVar] = &scheme
	}
	change, err := applyProxy(ctx, app, inst, changes, "Switching Fluxer to plain HTTP")
	if err != nil {
		return err
	}
	if err := closeProxySources(ctx, app, inst, bind, from); err != nil {
		return err
	}
	if public {
		if err := openProxySource(ctx, app, inst, bind, from); err != nil {
			return err
		}
	}
	reportProxy(app, inst, true, change, changes, plainHTTP)
	return nil
}

func proxyOff(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	st := inst.State.Settings
	changes := map[string]*string{
		settings.EdgeMode:      nil,
		settings.EdgeBind:      nil,
		settings.EdgeProxyKind: nil,
	}
	if st[publicSchemeVar] == "http" && !checks.PlainHTTPDomain(inst.Domain()) {
		changes[publicSchemeVar] = nil
	}
	if checks.PrivateDomain(inst.Domain()) && st[edgeTLSVar] == "" {
		own := "internal"
		changes[edgeTLSVar] = &own
	}
	var sources []string
	for _, p := range inst.State.Firewall {
		sources = append(sources, p.Source)
	}
	if trusted, ok := trustedFor(strings.Join(sources, ",")); ok && st[settings.EdgeTrusted] == trusted {
		changes[settings.EdgeTrusted] = nil
	}
	if err := edgeGuard(inst, changes); err != nil {
		return err
	}
	if isProxied(inst) {
		if err := proxyOffPreflight(); err != nil {
			return err
		}
		r, err := app.Runtime(ctx, inst)
		if err != nil {
			return err
		}
		if !r.Info(ctx).Root {
			if err := allowLowPorts(ctx, app, inst); err != nil {
				return err
			}
		}
	}
	change, err := applyProxy(ctx, app, inst, changes, "Switching Fluxer to HTTPS")
	if err != nil {
		return err
	}
	if err := closeProxySources(ctx, app, inst, "", ""); err != nil {
		return err
	}
	reportProxy(app, inst, false, change, changes, false)
	return nil
}

const proxyFromPrivate = "private"

var privateRanges = []string{"10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16", "fc00::/7"}

func proxySources(from string) []string {
	var out []string
	for _, s := range settings.TrustedProxyList(strings.ToLower(from)) {
		if !slices.Contains(out, s) {
			out = append(out, s)
		}
	}
	return out
}

func trustedFor(from string) (string, bool) {
	trusted, named := []string{"private_ranges"}, false
	for _, s := range proxySources(from) {
		switch {
		case s == proxyFromPrivate:
			named = true
		case !privateSource(s):
			trusted = append(trusted, s)
		}
	}
	if len(trusted) == 1 && !named {
		return "", false
	}
	return strings.Join(trusted, ","), true
}

func firewallSources(bind, from string) []string {
	var out []string
	for _, s := range proxySources(from) {
		add := []string{s}
		if s == proxyFromPrivate {
			add = privateRangesFor(bind)
		}
		for _, a := range add {
			if !slices.Contains(out, a) {
				out = append(out, a)
			}
		}
	}
	return out
}

func privateRangesFor(bind string) []string {
	host, _, err := net.SplitHostPort(bind)
	if err != nil {
		return privateRanges
	}
	a, err := netip.ParseAddr(host)
	if err != nil || a.IsUnspecified() && a.Is6() {
		return privateRanges
	}
	return slices.DeleteFunc(slices.Clone(privateRanges), func(r string) bool {
		return netip.MustParsePrefix(r).Addr().Is4() != a.Unmap().Is4()
	})
}

func proxyFromWords(from string) string {
	var words []string
	for _, s := range proxySources(from) {
		if s == proxyFromPrivate {
			s = "a private address"
		}
		words = append(words, s)
	}
	return ui.JoinOr(words)
}

func migratedProxyFrom(trusted string) string {
	entries := settings.TrustedProxyList(trusted)
	private := len(entries) == 0
	var public []string
	for _, e := range entries {
		switch {
		case e == "private_ranges" || privateSource(e):
			private = true
		case !slices.Contains(public, e):
			public = append(public, e)
		}
	}
	if private {
		public = append([]string{proxyFromPrivate}, public...)
	}
	return strings.Join(public, ",")
}

func applyProxy(ctx context.Context, app *App, inst *instance.Instance, changes map[string]*string, title string) (*deploy.Change, error) {
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return nil, err
	}
	var change *deploy.Change
	err = app.Locked(ctx, inst, func() error {
		if err := deploy.Changeable(inst); err != nil {
			return err
		}
		if err := edgeGuard(inst, changes); err != nil {
			return err
		}
		p, err := deploy.Prepare(inst, changes)
		if err != nil {
			return ui.Wrap(err, "rexctl can't make that change.", "Check the values you passed, then try again.")
		}
		if p.Unchanged {
			change = &deploy.Change{Keys: p.Keys, Unchanged: true}
			return nil
		}
		if err := r.Settings().Validate(ctx, p); err != nil {
			return err
		}
		preview, err := r.Settings().Preview(ctx, p)
		if err != nil {
			return err
		}
		pv := p.Preview
		pv.Restarts = preview.Restarts
		ok, err := confirmPreview(app, r.Kind(), running(inst), pv, preview.Notes...)
		if err != nil {
			return err
		}
		if !ok {
			return &ui.ExitError{Code: ui.ExitRefused, Msg: "Nothing changed.", Err: errKeptSettings}
		}
		err = app.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
			change, err = r.Settings().Apply(ctx, s, p)
			if err == nil && change.Applied {
				s.Done("Fluxer runs with the new settings.")
			}
			return err
		})
		reportVoiceDown(app, change, err)
		if err != nil {
			return err
		}
		return syncEdgeSite(context.WithoutCancel(ctx), app, inst)
	})
	return change, err
}

func openProxySource(ctx context.Context, app *App, inst *instance.Instance, bind, from string) error {
	ports := proxyPorts(bind, from)
	if len(ports) == 0 {
		return nil
	}
	fw, err := host.DetectFirewall(ctx)
	if err != nil || fw == host.FirewallNone {
		return nil
	}
	closed, err := host.ClosedPorts(ctx, fw, ports)
	if err != nil || len(closed) == 0 {
		return nil
	}
	ok, err := app.UI.ConfirmWith(ui.Question{Text: fmt.Sprintf("Open %s in %s?", portWords(closed), fw), Default: true})
	if err != nil {
		return err
	}
	if !ok {
		app.UI.Warn(fmt.Sprintf("%s %s closed in %s, so your proxy can't connect yet.", portWords(closed), isAre(len(closed)), fw), fmt.Sprintf("Open it before people connect, or run rexctl proxy on --bind %s --from %s again.", bind, from))
		return nil
	}
	if err := host.OpenPorts(ctx, fw, closed); err != nil {
		return ui.Wrap(err, "Couldn't open the port for your proxy.", fmt.Sprintf("Open TCP %d from %s in %s yourself.", bindPortOf(bind), proxyFromWords(from), fw))
	}
	for _, p := range closed {
		inst.State.Firewall = append(inst.State.Firewall, instance.OpenedPort{Firewall: string(fw), Number: p.Number, Proto: p.Proto, Source: p.Source})
	}
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then try again.")
	}
	app.UI.OK("Opened %s in %s.", portWords(closed), fw)
	return nil
}

func proxyPorts(bind, from string) []host.Port {
	port := bindPortOf(bind)
	if port == 0 {
		return nil
	}
	var out []host.Port
	for _, s := range firewallSources(bind, from) {
		out = append(out, host.Port{Number: port, Proto: "tcp", Source: s})
	}
	return out
}

func closeProxySources(ctx context.Context, app *App, inst *instance.Instance, bind, from string) error {
	keep := proxyPorts(bind, from)
	var stale []instance.OpenedPort
	var kept []instance.OpenedPort
	for _, p := range inst.State.Firewall {
		if p.Source == "" || slices.Contains(keep, host.Port{Number: p.Number, Proto: p.Proto, Source: p.Source}) {
			kept = append(kept, p)
			continue
		}
		stale = append(stale, p)
	}
	if len(stale) == 0 {
		return nil
	}
	var failed []string
	for _, p := range stale {
		port := host.Port{Number: p.Number, Proto: p.Proto, Source: p.Source}
		if err := host.ClosePorts(ctx, host.Firewall(p.Firewall), []host.Port{port}); err != nil {
			app.UI.Detail("%v", err)
			failed = append(failed, portWords([]host.Port{port}))
			kept = append(kept, p)
		}
	}
	inst.State.Firewall = kept
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then try again.")
	}
	if len(failed) > 0 {
		app.UI.Warn(fmt.Sprintf("Couldn't close %s in the firewall.", ui.JoinAnd(failed)), "Close it yourself, since your proxy no longer uses it.")
	}
	return nil
}

func reportProxy(app *App, inst *instance.Instance, on bool, change *deploy.Change, changes map[string]*string, plainHTTP bool) {
	u := app.UI
	switch {
	case on && change.Unchanged:
		u.OK("Fluxer already serves plain HTTP on %s.", *changes[settings.EdgeBind])
	case change.Unchanged:
		u.OK("Fluxer already serves HTTPS itself.")
	case on:
		u.OK("Fluxer serves plain HTTP on %s.", *changes[settings.EdgeBind])
		proxyInstructions(u, inst.Dir, inst.Domain(), *changes[settings.EdgeBind], inst.State.Settings[settings.EdgeProxyKind], "")
		if plainHTTP {
			u.Info(plainHTTPNote, inst.Domain())
		}
	default:
		u.OK("Fluxer serves HTTPS for %s itself.", inst.Domain())
		if checks.PrivateDomain(inst.Domain()) {
			u.Info("Fluxer signs its own certificate for this %s.", privateKind(inst.Domain()))
		} else {
			u.Info("Keep ports 80 and 443 open to this machine for the certificate.")
		}
	}
	if !change.Unchanged && !change.Applied {
		u.Info("Fluxer uses the new setup the next time it starts.")
	}
}

func proxyOffPreflight() error {
	var busy []string
	for _, port := range []int{80, 443} {
		used, owner, err := host.PortInUse("tcp", port)
		if err != nil || !used || checks.ContainerOwner(owner) {
			continue
		}
		if owner != "" {
			busy = append(busy, fmt.Sprintf("%d (%s)", port, owner))
		} else {
			busy = append(busy, fmt.Sprint(port))
		}
	}
	if len(busy) == 0 {
		return nil
	}
	return ui.Refused(fmt.Sprintf("Port %s is in use on this machine, and Fluxer needs ports 80 and 443 to serve HTTPS itself.", ui.JoinAnd(busy)), "Stop what uses them, then run rexctl proxy off again.")
}

func allowLowPorts(ctx context.Context, app *App, inst *instance.Instance) error {
	e, err := app.anyEngine(ctx, inst)
	if err != nil {
		return err
	}
	if !host.LowPortsBlocked(e.Rootless) {
		return nil
	}
	name := e.Kind.Name()
	app.UI.Warn(fmt.Sprintf("Rootless %s can't use ports 80 and 443, because only root can use ports below 1024 on this machine.", name), "")
	ok, err := app.UI.Confirm("Let programs that don't run as root use ports 80 and up on this machine?", true)
	if err != nil {
		return err
	}
	if !ok {
		return ui.Refused(fmt.Sprintf("Fluxer can't serve HTTPS until %s can use ports 80 and 443.", name), "Run rexctl proxy off again and let rexctl allow them.")
	}
	if err := host.RunSteps(ctx, host.LowPortsSysctl.Steps()); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't let %s use ports 80 and 443.", name), "Run rexctl proxy off --verbose to see why.")
	}
	app.UI.OK("Let %s use ports 80 and 443.", name)
	return nil
}

func proxyInstructions(u *ui.UI, dir, domain, bind, kind string, server proxyconf.Server) {
	server = proxyServer(kind, server)
	path := proxyconf.Path(dir, server)
	p := proxyParams(domain, bind)
	if _, err := proxyconf.Write(dir, p); err != nil {
		path = ""
	}
	body := []string{fmt.Sprintf("Send all traffic for %s to http://%s.", domain, bind)}
	if p.Private && (server == proxyconf.Nginx || server == proxyconf.Apache) {
		body = append(body, domain+" is a "+privateKind(domain)+", so no public authority signs a certificate for it. The commands below make one of your own, and your devices ask you to trust it.")
	}
	if kind == settings.ProxyTunnel {
		body = append(body, "Tailscale Funnel and ngrok work too. Point them at the same address.")
	}
	card := ui.Card{Title: "Point your " + proxyNoun(kind) + " at Fluxer", Rows: []ui.KV{{Key: "Listens on", Value: bind}}, Body: body}
	if path != "" {
		card.Rows = append(card.Rows, ui.KV{Key: "Config", Value: path})
		if kind == settings.ProxyLocal || kind == "" {
			if install := proxyInstall(server, p.Private); install != "" {
				card.Commands = append(card.Commands, install)
			}
		}
		card.Commands = append(card.Commands, proxyconf.Enable(server, path, p)...)
	}
	card.Footer = append(card.Footer, "rexctl proxy config "+string(server)+" prints it again.")
	u.Card(card)
}

var hasCommand = func(name string) bool {
	if _, err := exec.LookPath(name); err == nil {
		return true
	}
	for _, dir := range []string{"/usr/sbin", "/sbin"} {
		if _, err := os.Stat(filepath.Join(dir, name)); err == nil {
			return true
		}
	}
	return false
}

func proxyInstall(server proxyconf.Server, private bool) string {
	if !hasCommand("apt-get") {
		return ""
	}
	var pkgs []string
	switch server {
	case proxyconf.Nginx, proxyconf.Apache:
		bin, plugin := "nginx", "python3-certbot-nginx"
		if server == proxyconf.Apache {
			bin, plugin = "apache2", "python3-certbot-apache"
		}
		if !hasCommand(bin) {
			pkgs = append(pkgs, bin)
		}
		if !private && !hasCommand("certbot") {
			pkgs = append(pkgs, plugin)
		}
	case proxyconf.Caddy:
		if !hasCommand("caddy") {
			pkgs = append(pkgs, "caddy")
		}
	}
	if len(pkgs) == 0 {
		return ""
	}
	return "sudo apt install -y " + strings.Join(pkgs, " ")
}

func proxyServer(kind string, server proxyconf.Server) proxyconf.Server {
	switch {
	case kind == settings.ProxyTunnel && (server == "" || server == proxyconf.Nginx):
		return proxyconf.Cloudflared
	case server == "":
		return proxyconf.Nginx
	}
	return server
}

func proxyParams(domain, bind string) proxyconf.Params {
	return proxyconf.Params{Domain: domain, Upstream: bind, Private: checks.PrivateDomain(domain)}
}

func proxyNoun(kind string) string {
	switch kind {
	case settings.ProxyTunnel:
		return "tunnel"
	case settings.ProxyOverlay:
		return "Tor, I2P or Lokinet proxy"
	case settings.ProxyInstance:
		return "other Fluxer"
	}
	return "proxy"
}

func (in *installer) proxyCard() {
	proxyInstructions(in.app.UI, in.inst.Dir, in.f.Domain, in.f.Bind, in.f.Proxy, in.net.server)
}

const (
	proxyWait = 2 * time.Minute
)

func (in *installer) waitProxy(ctx context.Context) error {
	if in.f.Proxy == settings.ProxyOverlay || checks.PrivateDomain(in.f.Domain) {
		return nil
	}
	u := in.app.UI
	domain := in.f.Domain
	noun := proxyNoun(in.f.Proxy)
	var skip <-chan struct{}
	stop := func() {}
	title := "Waiting for https://" + domain + " to load through your " + noun
	if u.Interactive() {
		skip, stop = watchEnter(u.Stdin())
		if skip != nil {
			title += " (Enter to skip)"
		}
	}
	defer stop()
	later := ""
	err := u.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
		deadline := time.Now().Add(proxyWait)
		if !u.Interactive() && in.f.Proxy != settings.ProxyInstance {
			deadline = time.Now()
		}
		for {
			tlsState, tlsDetail := checks.ProbeHTTPS(ctx, domain)
			if tlsState == ui.StateOK {
				s.Item("tls", "HTTPS", ui.StateOK, tlsDetail)
				wsState, wsDetail := checks.ProbeWebSocket(ctx, domain)
				s.Item("websocket", "WebSocket", wsState, wsDetail)
				ipState, ipDetail := checks.ProbeClientIP(ctx, domain, in.proxySource())
				s.Item("client", "Visitor address", ipState, ipDetail)
				switch {
				case wsState != ui.StateOK:
					s.Warn("Your "+noun+" doesn't pass WebSocket connections on /gateway through.", "Forward the Upgrade and Connection headers. rexctl proxy config "+string(in.net.server)+" shows how.")
				case ipState == ui.StateFail:
					s.Warn("Fluxer sees your "+noun+"'s address instead of each visitor's.", in.clientIPFix())
				default:
					s.Done("Your %s passes HTTPS and WebSockets through to Fluxer.", noun)
				}
				return nil
			}
			s.Item("tls", "HTTPS", ui.StateActive, tlsDetail)
			if time.Now().After(deadline) {
				s.Warned("https://%s doesn't load through your %s yet.", domain, noun)
				later = "Set it up as shown above, then run rexctl doctor."
				if in.f.Proxy == settings.ProxyInstance {
					later = "Check that " + domain + " points at this machine, then run rexctl doctor."
				}
				return nil
			}
			select {
			case <-ctx.Done():
				return ctx.Err()
			case <-skip:
				s.Warned("Skipped the check for your %s.", noun)
				later = "Run rexctl doctor once it's set up."
				return nil
			case <-time.After(5 * time.Second):
			}
		}
	})
	if ctx.Err() != nil {
		return ctx.Err()
	}
	if err == nil && later != "" {
		u.Info("%s", later)
	}
	return err
}

func (in *installer) proxySource() string {
	if in.f.Proxy == settings.ProxyRemote {
		return strings.Join(slices.DeleteFunc(proxySources(in.f.ProxyFrom), func(s string) bool { return s == proxyFromPrivate }), ",")
	}
	return "127.0.0.1"
}

func (in *installer) clientIPFix() string {
	if in.f.Proxy == settings.ProxyRemote {
		return "Make your proxy set the X-Forwarded-For header, and check that it connects from " + proxyFromWords(in.f.ProxyFrom) + ". Then run rexctl doctor."
	}
	return "Make your " + proxyNoun(in.f.Proxy) + " set the X-Forwarded-For header. rexctl proxy config " + string(in.net.server) + " shows how."
}
