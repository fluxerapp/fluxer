// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"net/url"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func planJoin(ctx context.Context, app *App, client *voice.Client, e *engine.Engine, inst *instance.Instance, token string, plan *joinPlan, f *voiceFlags) error {
	pctx, cancel := context.WithTimeout(ctx, 15*time.Second)
	publicIP, _ := host.PublicIPv4(pctx)
	cancel()
	if f.IP != "" {
		publicIP = f.IP
	}
	publicIPv6 := detectIPv6(ctx, f)
	if publicIP == "" && publicIPv6 == "" {
		return ui.Refused("This machine has no public IPv4 or IPv6 address that rexctl can find.", "Pass --ip or --ipv6 if the address is known.")
	}
	reach, err := assessJoin(app, publicIP, publicIPv6, f.IP, voice.InterfaceAddresses(), voice.DefaultRouteIPv4())
	if err != nil {
		return err
	}
	plan.reach = reach
	plan.hostMem, plan.cpus = host.Hardware()
	plan.preview, err = client.Preview(ctx, plan.disc.APIPublic, token, voice.PreviewRequest{
		PublicIPv4: reach.PublicIPv4, PublicIPv6: reach.PublicIPv6, RegionID: f.Region, CPUCount: plan.cpus, MemoryBytes: plan.hostMem, RexctlVersion: app.Version,
	})
	if err != nil {
		return joinError(err, plan.domain, "")
	}
	plan.image, plan.imageWhy = joinImage(f)
	plan.firewall, _ = host.DetectFirewall(ctx)
	if err := planEdge(ctx, app, e, inst, plan, f); err != nil {
		return err
	}
	plan.netIPv6 = plan.reach.PublicIPv6 != ""
	id, err := chooseJoinID(plan.preview, f.ID, inst, voiceIDsInUse(ctx, e, inst, voice.JoinedProject("")))
	if err != nil {
		return err
	}
	if plan.edge == voice.EdgeOwn {
		if err := chooseAddress(ctx, app, plan, id, f); err != nil {
			return err
		}
	}
	tcp, udp := f.TCPPort, f.UDPPort
	if existing := inst.State.VoiceServer(id); existing != nil {
		tcp, udp = existing.TCPPort, existing.UDPPort
	}
	if tcp == 0 || udp == 0 {
		pt, pu := voice.PickPorts(takenVoicePorts(ctx, e, inst), freeOnHost)
		tcp, udp = orInt(tcp, pt), orInt(udp, pu)
	}
	if tcp == 0 || udp == 0 {
		return ui.Refused("No free pair of ports is left for voice on this machine.", "Pass --tcp-port and --udp-port.")
	}
	secret, err := voice.NewSecret()
	if err != nil {
		return err
	}
	if existing := inst.State.VoiceServer(id); existing != nil {
		secret = existing.Secret
	}
	memory := f.Memory
	if memory == "" {
		memory = voice.MachineMemory(plan.hostMem)
	}
	soft := f.SoftLimit
	if soft == 0 {
		if plan.preview.SoftLimit != nil {
			soft = *plan.preview.SoftLimit
		} else {
			soft = voice.SoftLimit(plan.cpus)
		}
	}
	httpPort, httpBind := 0, ""
	if f.NoEdge {
		httpPort = f.HTTPPort
		if existing := inst.State.VoiceServer(id); httpPort == 0 && existing != nil {
			httpPort = existing.HTTPPort
		}
		if httpPort == 0 {
			httpPort = pickHTTPPort(takenVoicePorts(ctx, e, inst), tcp, freeOnHost)
		}
		httpBind = f.HTTPBind
		if existing := inst.State.VoiceServer(id); !f.HTTPBindSet && existing != nil {
			httpBind = existing.HTTPBind
		}
	}
	plan.srv = instance.VoiceServer{
		ID:        id,
		Region:    plan.preview.Region.ID,
		Key:       voice.Key(id),
		Secret:    secret,
		TCPPort:   tcp,
		UDPPort:   udp,
		HTTPPort:  httpPort,
		HTTPBind:  httpBind,
		NodeIP:    plan.reach.PublicIPv4,
		NodeIPv6:  plan.reach.PublicIPv6,
		Memory:    memory,
		SoftLimit: soft,
		Project:   voice.NativeChild(id),
		Webhook:   plan.preview.WebhookURL,
		Instance:  plan.domain,
		Edge:      plan.edge,
		Joined:    true,
		Turn:      settings.TurnServersValue(plan.preview.TurnServers),
	}
	if f.NoEdge {
		plan.srv.Endpoint = strings.TrimRight(f.Endpoint, "/")
	} else {
		plan.srv.Endpoint = voice.Endpoint(plan.address, id)
	}
	if u, err := url.Parse(plan.srv.Endpoint); err == nil {
		plan.srv.Hostname = u.Hostname()
	}
	return planFluxerdServer(inst, plan, f)
}

func planFluxerdServer(inst *instance.Instance, plan *joinPlan, f *voiceFlags) error {
	s := &plan.srv
	if s.Edge == voice.EdgeOwn {
		if err := voiceHostAddressClash(inst, s.Endpoint); err != nil {
			return err
		}
	}
	if f.NoEdge {
		return nil
	}
	s.HTTPBind = ""
	if existing := inst.State.VoiceServer(s.ID); existing != nil && existing.HTTPPort > 0 {
		s.HTTPPort = existing.HTTPPort
		return nil
	}
	var taken []int
	for _, other := range inst.State.Voice {
		taken = append(taken, other.HTTPPort)
	}
	s.HTTPPort = voice.PickNativeHTTPPort(taken, freeOnHost)
	if s.HTTPPort == 0 {
		return ui.Refused("No free local port is left for another voice server.", "Remove a voice server you no longer use, then try again.")
	}
	return nil
}

func chooseJoinID(preview *voice.Preview, flagID string, inst *instance.Instance, taken []string) (string, error) {
	id, err := pickJoinID(preview, flagID, inst, taken)
	if err != nil {
		return "", err
	}
	if err := checkJoinID(preview, id); err != nil {
		return "", err
	}
	return id, nil
}

func checkJoinID(preview *voice.Preview, id string) error {
	if id != voice.EdgeID && voice.ValidID(id) {
		return nil
	}
	fix := "Pass --id with lowercase letters, digits and dashes."
	if preview.ServerIDPinned {
		fix = "Make a new token on the Fluxer machine."
	}
	return ui.Refused(fmt.Sprintf("rexctl can't use the server ID %q.", id), fix)
}

func pickJoinID(preview *voice.Preview, flagID string, inst *instance.Instance, taken []string) (string, error) {
	id := preview.ServerID
	switch {
	case flagID != "" && preview.ServerIDPinned && flagID != id:
		return "", ui.Refused(fmt.Sprintf("The token is for the server ID %s.", id), "Leave out --id.")
	case flagID != "":
		id = flagID
	}
	if existing := inst.State.VoiceServer(id); existing != nil {
		if existing.NodeURL != "" {
			return "", ui.Refused(fmt.Sprintf("This machine already runs %s.", id), "Pass --id with another ID.")
		}
		return id, nil
	}
	if !slices.Contains(taken, id) {
		return id, nil
	}
	switch {
	case preview.ServerIDPinned:
		return "", ui.Refused(fmt.Sprintf("The token is for the server ID %s, and this machine already has a voice server or Compose project with that ID.", id), "Make a new token with another server ID on the Fluxer machine.")
	case flagID != "":
		return "", ui.Refused(fmt.Sprintf("This machine already has a voice server or Compose project with the ID %s.", id), "Pass --id with another ID.")
	}
	return voice.NextID(preview.Region.ID, taken), nil
}

func assessJoin(app *App, publicIP, publicIPv6, flagIP string, local []string, routeIP string) (voice.Reachability, error) {
	reach := voice.Assess(publicIP, publicIPv6, local, routeIP)
	if flagIP != "" {
		reach.CGNAT = false
	}
	if reach.CGNAT && reach.PublicIPv6 != "" {
		app.UI.Info("This machine is behind CGNAT on IPv4, a shared address that can't take incoming calls. The server uses IPv6 only.")
		reach.PublicIPv4, reach.BehindNAT, reach.CGNAT = "", false, false
	}
	if reach.CGNAT {
		return reach, ui.Refused("This machine is behind CGNAT, a shared address that can't take incoming calls.", "Pass --ip with the machine's public address if it has one of its own.")
	}
	return reach, nil
}

func detectIPv6(ctx context.Context, f *voiceFlags) string {
	if f.NoIPv6 {
		return ""
	}
	if f.IPv6 != "" {
		return f.IPv6
	}
	pctx, cancel := context.WithTimeout(ctx, 10*time.Second)
	seen, _ := host.PublicIPv6(pctx)
	cancel()
	return voice.MachineIPv6(seen)
}

func pickHTTPPort(taken map[host.Port]bool, tcp int, free func(int, string) bool) int {
	for n := 7880; n < 7980; n++ {
		if n != tcp && !taken[host.Port{Number: n, Proto: "tcp"}] && free(n, "tcp") {
			return n
		}
	}
	return 0
}

func bindAddress(ip string, local []string) string {
	if ip != "" && slices.Contains(local, ip) {
		return ip
	}
	return ""
}

func joinFirewallText(fw host.Firewall, ports int, noFirewall bool) string {
	switch {
	case fw == host.FirewallNone:
		return "none found on this machine. Open the ports in your provider's firewall."
	case noFirewall:
		return fmt.Sprintf("%s, left alone. Open the ports in it and in your provider's firewall.", fw)
	}
	return fmt.Sprintf("%s, will open %d ports. Open the same %d in your provider's firewall.", fw, ports, ports)
}

func orInt(v, fallback int) int {
	if v != 0 {
		return v
	}
	return fallback
}

func joinImage(f *voiceFlags) (string, string) {
	if f.Image != "" {
		return f.Image, "from --image"
	}
	return voice.DefaultImage, "the pin of this rexctl"
}

func planEdge(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, plan *joinPlan, f *voiceFlags) error {
	if f.NoEdge {
		plan.edge = voice.EdgeNone
		return nil
	}
	var owners []engine.PortOwner
	if e != nil {
		owners, _ = e.PublishedHostPorts(ctx)
	}
	holder := ""
	for _, o := range owners {
		if (o.Number == 443 || o.Number == 80) && o.Proto == "tcp" {
			holder = o.Project
		}
	}
	switch {
	case holder == voice.EdgeProject:
		plan.edge = voice.EdgeOwn
	case holder != "":
		return ui.Refused(fmt.Sprintf("Ports 80 and 443 belong to the Compose project %s.", holder), "Pass --no-edge and --endpoint to use your own proxy.")
	default:
		for _, p := range []int{80, 443} {
			if used, owner, err := host.PortInUse("tcp", p); err == nil && used {
				if owner == "" {
					owner = "another program"
				}
				return ui.Refused(fmt.Sprintf("Port %d is in use by %s.", p, owner), "Pass --no-edge and --endpoint to use your own proxy.")
			}
		}
		plan.edge, plan.newEdge = voice.EdgeOwn, true
	}
	return nil
}

func chooseAddress(ctx context.Context, app *App, plan *joinPlan, id string, f *voiceFlags) error {
	pinned := ""
	if plan.preview.Hostname != nil {
		pinned = strings.ToLower(*plan.preview.Hostname)
	}
	name := strings.ToLower(strings.TrimSuffix(strings.TrimSpace(f.Hostname), "."))
	if pinned != "" && name != "" && name != pinned {
		return ui.Refused(fmt.Sprintf("The token is for the hostname %s.", pinned), "Leave out --hostname.")
	}
	if name == "" {
		name = pinned
	}
	only6 := plan.reach.PublicIPv4 == ""
	resolve := hostnameLookup()
	check := func(host string) voice.HostnameCheck {
		answer, err := resolve(ctx, host)
		if err != nil {
			return voice.HostnameMissing
		}
		return voice.CheckHostname(answer, plan.reach.PublicIPv4, plan.reach.PublicIPv6, voice.InterfaceAddresses())
	}
	if name == "" && plan.preview.SuggestedHostname != "" && check(plan.preview.SuggestedHostname).Usable() {
		name = plan.preview.SuggestedHostname
	}
	if name == "" && only6 {
		return ui.Refused("This machine has only IPv6, and a voice server on IPv6 needs a hostname.", fmt.Sprintf("Point a hostname at %s with an AAAA record, then pass it with --hostname.", plan.reach.PublicIPv6))
	}
	if name == "" {
		plan.address, plan.ipCert = plan.reach.PublicIPv4, true
		return nil
	}
	if pinned != "" || only6 {
		if waitForHostname(ctx, app, name, plan.reach, check, f.DryRun, true) || f.DryRun {
			plan.address = name
			return nil
		}
		why := fmt.Sprintf("%s doesn't point at this machine yet.", name)
		if pinned != "" {
			why = fmt.Sprintf("The token needs the hostname %s, and it doesn't point at this machine yet.", name)
		}
		return ui.Refused(why, "Point it at "+addressText(plan.reach)+" with DNS only, then run the same command again.")
	}
	if waitForHostname(ctx, app, name, plan.reach, check, f.DryRun, false) {
		plan.address = name
		return nil
	}
	app.UI.Info("Using %s for now. Once DNS points at this machine, run rexctl voice hostname %s %s.", plan.reach.PublicIPv4, id, name)
	plan.address, plan.ipCert = plan.reach.PublicIPv4, true
	return nil
}

func addressText(addrs voice.Reachability) string {
	return ui.JoinAnd(slices.DeleteFunc([]string{addrs.PublicIPv4, addrs.PublicIPv6}, func(s string) bool { return s == "" }))
}

func waitForHostname(ctx context.Context, app *App, name string, addrs voice.Reachability, check func(string) voice.HostnameCheck, dryRun, always bool) bool {
	result := check(name)
	if result.Usable() {
		hostnameNote(app, name, result)
		return true
	}
	switch result {
	case voice.HostnameProxied:
		app.UI.Warn(fmt.Sprintf("%s goes through the Cloudflare proxy.", name), "Set the record to DNS only. Calls can't go through the proxy.")
	case voice.HostnameIPv6Elsewhere:
		app.UI.Warn(fmt.Sprintf("%s has an AAAA record that points somewhere else.", name), "Remove that record. Let's Encrypt checks IPv6 first.")
	default:
		app.UI.Info("%s doesn't point at this machine yet. Add these records at your DNS provider:", name)
		app.UI.Info("")
		if addrs.PublicIPv4 != "" {
			app.UI.Info("  %s.  A     %s", name, addrs.PublicIPv4)
		}
		if addrs.PublicIPv6 != "" {
			app.UI.Info("  %s.  AAAA  %s", name, addrs.PublicIPv6)
		}
		app.UI.Info("")
		app.UI.Info("On Cloudflare, set the records to DNS only.")
	}
	if dryRun || !always && (app.UI.Yes() || !app.UI.Interactive()) {
		return false
	}
	if always {
		app.UI.Info("Waiting up to 10 minutes.")
	} else {
		app.UI.Info("Waiting up to 10 minutes. rexctl uses the IP address after that.")
	}
	deadline := time.Now().Add(dnsWait)
	for time.Now().Before(deadline) {
		select {
		case <-ctx.Done():
			return false
		case <-time.After(15 * time.Second):
		}
		if result := check(name); result.Usable() {
			app.UI.OK("%s points at this machine.", name)
			hostnameNote(app, name, result)
			return true
		}
	}
	return false
}

func hostnameNote(app *App, name string, result voice.HostnameCheck) {
	switch result {
	case voice.HostnameIPv4Front:
		app.UI.Info("%s also has an A record for another machine. That machine has to forward ports 80 and 443 here for callers with only IPv4.", name)
	case voice.HostnameNoA:
		app.UI.Warn(fmt.Sprintf("%s has no A record.", name), "Callers with only IPv4 can't connect to this server until you add one that points at this machine.")
	case voice.HostnameNoAAAA:
		app.UI.Warn(fmt.Sprintf("%s has no AAAA record.", name), "Callers with only IPv6 can't connect to this server until you add one that points at this machine.")
	}
}

func joinRegionText(p *voice.Preview, requested string) string {
	region := p.Region.ID
	if p.Region.Name != "" {
		region += " (" + p.Region.Name + ")"
	}
	if p.Region.New {
		return region + ", a new region from the token"
	}
	source := p.Region.Source
	if source == "" && requested != "" && requested == p.Region.ID {
		source = "requested"
	}
	if p.Region.DistanceKm == nil {
		switch source {
		case "token":
			return region + ", from the token"
		case "requested":
			return region + ", from --region"
		}
		return region
	}
	km := *p.Region.DistanceKm
	switch source {
	case "nearest":
		return region + fmt.Sprintf(", nearest existing region, %.0f km", km)
	case "token":
		return region + fmt.Sprintf(", from the token, %.0f km away", km)
	case "requested":
		return region + fmt.Sprintf(", from --region, %.0f km away", km)
	}
	return region + fmt.Sprintf(", %.0f km away", km)
}

func showJoinSummary(app *App, plan *joinPlan, f *voiceFlags) {
	u := app.UI
	s := plan.srv
	p := plan.preview
	region := joinRegionText(p, f.Region)
	cert := "Let's Encrypt for " + plan.address
	switch {
	case s.Edge == voice.EdgeNone:
		cert = "from your own proxy"
	case plan.ipCert:
		cert = "Let's Encrypt IP address certificate for " + plan.address + ", renewed about every 6 days"
	}
	ports := []string{fmt.Sprintf("%d/tcp", s.TCPPort), fmt.Sprintf("%d/udp", s.UDPPort)}
	if plan.newEdge {
		ports = append(ports, "80/tcp", "443/tcp")
	}
	fw := joinFirewallText(plan.firewall, len(ports), f.NoFirewall)
	ip := orNone(s.NodeIP)
	if plan.reach.BehindNAT {
		ip += ", behind NAT. Forward each port to the same port number on this machine."
	}
	image, _, _ := strings.Cut(s.Image, "@")
	memory := s.Memory
	if n, ok := voice.ParseBytes(s.Memory); ok {
		memory = voice.HumanBytes(n)
	}
	livekit := image + ", " + plan.imageWhy
	memoryRow := []string{"Memory limit", fmt.Sprintf("%s of %s", memory, voice.HumanBytes(plan.hostMem))}
	if plan.rel != nil {
		livekit = "the one in Fluxer " + plan.rel.Version
		memoryRow = []string{"Memory goal", fmt.Sprintf("LiveKit aims to stay under %s of %s", memory, voice.HumanBytes(plan.hostMem))}
	}
	rows := [][]string{
		{"Server ID", s.ID},
		{"Region", region},
		{"Endpoint", s.Endpoint},
		{"Certificate", cert},
		{"Public IPv4", ip},
		{"Public IPv6", orNone(s.NodeIPv6)},
		{"Ports", strings.Join(ports, ", ")},
		{"Firewall", fw},
		{"LiveKit", livekit},
		{"Runs", voiceRuntimeLabel(plan.inst)},
		memoryRow,
		{"Soft limit", plural(s.SoftLimit, "connection", "connections")},
		{"Webhook", s.Webhook},
	}
	if s.Edge == voice.EdgeNone && s.HTTPPort > 0 {
		rows = append(rows, []string{"Local port", voice.HTTPAddress(s.HTTPBind, s.HTTPPort) + " for your proxy"})
	}
	u.Info("Voice server for %s", plan.domain)
	u.Info("")
	for _, r := range rows {
		u.Info("  %-15s %s", r[0], r[1])
	}
	u.Info("")
	u.Info("Change any of these with flags. Run rexctl voice join --help to see them.")
}

func orNone(v string) string {
	if v == "" {
		return "none"
	}
	return v
}
