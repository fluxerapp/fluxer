// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"fmt"
	"net/netip"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

type voiceFlags struct {
	ID          string
	Region      string
	NewRegion   string
	RegionName  string
	Lat, Long   float64
	HasCoords   bool
	Hostname    string
	IP          string
	IPv6        string
	NoIPv6      bool
	TCPPort     int
	UDPPort     int
	SoftLimit   int
	Memory      string
	Image       string
	Email       string
	NoEdge      bool
	Endpoint    string
	HTTPPort    int
	HTTPBind    string
	HTTPBindSet bool
	TokenStdin  bool
	NoFirewall  bool
	DryRun      bool
	Force       bool
	Count       int
	Runtime     string
	Print       bool
}

func addVoiceOverrideFlags(c *cobra.Command, f *voiceFlags) {
	fl := c.Flags()
	fl.StringVar(&f.ID, "id", "", "server ID, such as fra-server-2. With --print, a registered server's ID prints a new config with a new secret")
	fl.StringVar(&f.Region, "region", "", "existing voice region for the server")
	fl.Float64Var(&f.Lat, "lat", 0, "latitude of the server")
	fl.Float64Var(&f.Long, "long", 0, "longitude of the server")
	fl.StringVar(&f.IP, "ip", "", "public IPv4 address LiveKit gives to clients, also the address the ports bind to when it's on this machine")
	fl.StringVar(&f.IPv6, "ipv6", "", "public IPv6 address LiveKit gives to clients")
	fl.BoolVar(&f.NoIPv6, "no-ipv6", false, "give clients only the IPv4 address, even when this machine has IPv6")
	fl.IntVar(&f.TCPPort, "tcp-port", 0, "TCP port for media")
	fl.IntVar(&f.UDPPort, "udp-port", 0, "UDP port for media")
	fl.IntVar(&f.SoftLimit, "soft-limit", 0, "connections above which placement prefers another server")
	fl.StringVar(&f.Memory, "memory", "", "memory limit for the server, such as 2g")
	fl.StringVar(&f.Image, "image", "", "LiveKit image to run")
	fl.BoolVar(&f.NoFirewall, "no-firewall", false, "leave the firewall on this machine alone")
	fl.BoolVar(&f.DryRun, "dry-run", false, "show what would happen and change nothing")
}

func checkHTTPBind(f *voiceFlags, local []string) error {
	if f.HTTPBind == "" {
		return nil
	}
	if !f.NoEdge {
		return ui.Usage("Pass --http-bind with --no-edge.")
	}
	bind, ok := voice.NormalHTTPBind(f.HTTPBind)
	if !ok {
		return ui.Usage(fmt.Sprintf("Pass --http-bind as an IPv4 or IPv6 address. %s isn't one.", f.HTTPBind))
	}
	if bind != "" && !voice.HTTPBindOnMachine(bind, local) {
		return ui.Refused(fmt.Sprintf("%s isn't an address of this machine.", f.HTTPBind), "Pass one of its own addresses, or 0.0.0.0 for every address.")
	}
	f.HTTPBind, f.HTTPBindSet = bind, true
	return nil
}

func validateVoiceFlags(c *cobra.Command, f *voiceFlags) error {
	if err := checkServerID(f.ID); err != nil {
		return err
	}
	if f.NewRegion != "" && !voice.ValidID(f.NewRegion) {
		return ui.Usage(fmt.Sprintf("%s isn't a region ID. Use lowercase letters, digits and dashes.", f.NewRegion))
	}
	if f.IP != "" {
		ip, err := netip.ParseAddr(f.IP)
		if err != nil || !ip.Is4() {
			return ui.Usage(fmt.Sprintf("Pass --ip as an IPv4 address. %s isn't one. Pass an IPv6 address with --ipv6.", f.IP))
		}
	}
	if f.IPv6 != "" {
		if f.NoIPv6 {
			return ui.Usage("Pass --ipv6 or --no-ipv6, not both.")
		}
		if !voice.GlobalIPv6(f.IPv6) {
			return ui.Usage(fmt.Sprintf("Pass --ipv6 as a public IPv6 address. %s isn't one.", f.IPv6))
		}
		f.IPv6 = netip.MustParseAddr(f.IPv6).String()
	}
	for _, p := range []int{f.TCPPort, f.UDPPort, f.HTTPPort} {
		if p < 0 || p > 65535 {
			return ui.Usage("Pass a port from 1 to 65535.")
		}
	}
	if c.Flags().Changed("lat") != c.Flags().Changed("long") {
		return ui.Usage("Pass --lat and --long together.")
	}
	f.HasCoords = c.Flags().Changed("lat")
	if f.Lat < -90 || f.Lat > 90 || f.Long < -180 || f.Long > 180 {
		return ui.Usage("Pass --lat from -90 to 90 and --long from -180 to 180.")
	}
	if f.Memory != "" {
		if _, ok := voice.ParseBytes(f.Memory); !ok {
			return ui.Usage(fmt.Sprintf("Pass --memory as a size such as 2g or 1536m. %s isn't one.", f.Memory))
		}
	}
	return nil
}

func checkServerID(id string) error {
	switch {
	case id == "":
		return nil
	case id == voice.EdgeID:
		return ui.Usage("The voice edge uses the ID edge. Pick another server ID.")
	case !voice.ValidID(id):
		return ui.Usage(fmt.Sprintf("%s isn't a server ID. Use lowercase letters, digits and dashes.", id))
	}
	return nil
}

func newVoiceAddCmd(app *App) *cobra.Command {
	f := &voiceFlags{}
	c := &cobra.Command{
		Use:   "add",
		Short: "Add voice servers on this Fluxer machine",
		Long: "Add standalone LiveKit voice servers next to your instance on this machine. " +
			"Each one gets its own ID, ports, keys and path on your domain, and rexctl registers it with your instance.",
		Example: "  rexctl voice add\n" +
			"  rexctl voice add --count 2\n" +
			"  rexctl voice add --print --endpoint wss://voice.example.com > livekit.yaml\n" +
			"  rexctl voice add --new-region fra --region-name Frankfurt --lat 50.1 --long 8.7",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			if err := validateVoiceFlags(c, f); err != nil {
				return err
			}
			if f.Print || f.Endpoint != "" {
				return runVoiceAddPrint(c.Context(), app, c, f)
			}
			if f.Count < 1 || f.Count > 16 {
				return ui.Usage("Pass --count from 1 to 16.")
			}
			if f.Count > 1 && (f.ID != "" || f.TCPPort > 0 || f.UDPPort > 0) {
				return ui.Usage("Pass --id, --tcp-port and --udp-port only when you add one server.")
			}
			if f.NewRegion != "" && f.RegionName == "" {
				return ui.Usage("Pass --region-name with --new-region.")
			}
			if f.NewRegion != "" && !f.HasCoords {
				return ui.Usage("Pass --lat and --long with --new-region. Placement sends people to the nearest region.")
			}
			return runVoiceAdd(c.Context(), app, c, f)
		},
	}
	addVoiceOverrideFlags(c, f)
	c.Flags().IntVar(&f.Count, "count", 1, "how many servers to add")
	c.Flags().StringVar(&f.NewRegion, "new-region", "", "create a voice region with this ID for the servers")
	c.Flags().StringVar(&f.RegionName, "region-name", "", "name of the new region")
	c.Flags().BoolVar(&f.Force, "force", false, "add servers even when Fluxer runs through a tunnel")
	c.Flags().BoolVar(&f.Print, "print", false, "register a server you run yourself with plain LiveKit and print its LiveKit config, with --endpoint")
	c.Flags().StringVar(&f.Endpoint, "endpoint", "", "public wss:// URL of the LiveKit server you run yourself, with --print")
	return c
}

type plannedVoice struct {
	srv       instance.VoiceServer
	publicIP  string
	softLimit int
}

func runVoiceAdd(ctx context.Context, app *App, c *cobra.Command, f *voiceFlags) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if inst.State.IsVoice() {
		return ui.Refused("This folder holds voice servers that joined another instance.", "Run rexctl voice join with a new token to add one more here.")
	}
	if err := managed(inst); err != nil {
		return err
	}
	vs, err := voiceRuntime(ctx, app, inst)
	if err != nil {
		return err
	}
	if err := vs.CanChange(); err != nil {
		return err
	}
	api, _, err := app.Admin(ctx)
	if err != nil {
		return err
	}
	switch inst.State.Settings[settings.EdgeMode] {
	case settings.EdgeTunnel:
		if !f.Force {
			return ui.Refused("Fluxer runs through a tunnel here. Signalling works through a tunnel and call audio doesn't.", "Pass --force to add the server anyway, then open its ports to the internet.")
		}
	case settings.EdgeProxy:
		app.UI.Info("Your proxy must pass WebSocket upgrades on the new paths, as it does for /livekit/.")
	}
	return app.Locked(ctx, inst, func() error {
		regions, err := api.VoiceRegions(ctx)
		if err != nil {
			return adminapi.Explain(err)
		}
		region, err := pickRegion(regions, f)
		if err != nil {
			return err
		}
		plans, err := planVoiceAdd(ctx, app, vs, inst, regions, region, f)
		if err != nil {
			return err
		}
		showVoiceAddSummary(app, inst, region, plans, f)
		if f.DryRun {
			return nil
		}
		if err := confirmVoice(app, "Add "+plural(len(plans), "voice server", "voice servers")+"?"); err != nil {
			return err
		}
		if f.NewRegion != "" {
			lat, long := float32(f.Lat), float32(f.Long)
			err := api.CreateVoiceRegion(ctx, adminapi.CreateVoiceRegionRequest{Id: f.NewRegion, Name: f.RegionName, Emoji: "🌍", Latitude: lat, Longitude: long})
			if err != nil {
				return adminapi.Explain(err)
			}
		}
		for i := range plans {
			if err := addOneVoiceServer(ctx, app, api, vs, inst, &plans[i], f); err != nil {
				return err
			}
		}
		app.UI.Info("Run rexctl voice list to see every server, and rexctl voice remove <id> to take one away.")
		return nil
	})
}

func pickRegion(regions []adminapi.VoiceRegion, f *voiceFlags) (string, error) {
	if f.NewRegion != "" {
		if slices.ContainsFunc(regions, func(r adminapi.VoiceRegion) bool { return r.Id == f.NewRegion }) {
			return "", ui.Refused(fmt.Sprintf("The voice region %s already exists.", f.NewRegion), "Pass --region "+f.NewRegion+" to use it.")
		}
		return f.NewRegion, nil
	}
	if f.Region != "" {
		if !slices.ContainsFunc(regions, func(r adminapi.VoiceRegion) bool { return r.Id == f.Region }) {
			return "", ui.Refused(fmt.Sprintf("There's no voice region called %s.", f.Region), "Run rexctl voice list to see the regions, or pass --new-region.")
		}
		return f.Region, nil
	}
	for _, r := range regions {
		if r.IsDefault {
			return r.Id, nil
		}
	}
	if len(regions) > 0 {
		return regions[0].Id, nil
	}
	return "", ui.Refused("Your instance has no voice regions.", "Pass --new-region, --region-name, --lat and --long to create one.")
}

func takenVoiceIDs(regions []adminapi.VoiceRegion, inst *instance.Instance) []string {
	var out []string
	for _, r := range regions {
		for _, s := range r.Servers {
			out = append(out, s.ServerId)
		}
	}
	for _, s := range inst.State.Voice {
		out = append(out, s.ID)
	}
	if id := deploy.BundledVoiceServerID(inst); id != "" {
		out = append(out, id)
	}
	return out
}

func planVoiceAdd(ctx context.Context, app *App, vs rt.VoiceServers, inst *instance.Instance, regions []adminapi.VoiceRegion, region string, f *voiceFlags) ([]plannedVoice, error) {
	inUse, published := vs.InUse(ctx)
	ids := append(append(takenVoiceIDs(regions, inst), voiceIDsInUse(ctx, nil, inst, "")...), inUse...)
	ports := takenVoicePorts(ctx, nil, inst)
	for _, p := range published {
		ports[p] = true
	}
	nodeIP := f.IP
	if nodeIP == "" {
		nodeIP = strings.TrimSpace(inst.State.Settings[settings.VoicePublicIP])
	}
	publicIP := nodeIP
	if publicIP == "" {
		pctx, cancel := context.WithTimeout(ctx, 10*time.Second)
		publicIP, _ = host.PublicIPv4(pctx)
		cancel()
	}
	nodeIPv6, err := addIPv6(ctx, app, vs, publicIP, f)
	if err != nil {
		return nil, err
	}
	if nodeIPv6 != "" && nodeIP == "" {
		nodeIP = publicIP
	}
	memory := f.Memory
	if memory == "" {
		memory = strings.TrimSpace(inst.Setting("FLUXER_LIVEKIT_MEMORY_LIMIT"))
		if _, ok := voice.ParseBytes(memory); !ok {
			memory = voice.DefaultMemory
		}
	}
	image := f.Image
	if image == "" && vs.Images() {
		image = instanceLiveKitImage(inst)
	}
	_, cpus := host.Hardware()
	softLimit := f.SoftLimit
	if softLimit == 0 {
		softLimit = voice.SoftLimit(cpus)
	}
	base := voice.WSOrigin(inst.Origin())
	if base == "" {
		return nil, ui.Refused("rexctl can't work out your instance's address.", "Run rexctl config set domain <domain>.")
	}
	var plans []plannedVoice
	for range f.Count {
		id := f.ID
		if id == "" {
			if !voice.UsableRegion(region) {
				return nil, ui.Refused(fmt.Sprintf("rexctl can't make a server ID from the region ID %q.", region), "Pass --id with a server ID.")
			}
			id = voice.NextID(region, ids)
		} else if slices.Contains(ids, id) {
			return nil, ui.Refused(fmt.Sprintf("A voice server or Compose project with the ID %s already exists.", id), "Pass --id with another ID.")
		}
		ids = append(ids, id)
		tcp, udp := f.TCPPort, f.UDPPort
		if tcp == 0 || udp == 0 {
			pt, pu := voice.PickPorts(ports, freeOnHost)
			if tcp == 0 {
				tcp = pt
			}
			if udp == 0 {
				udp = pu
			}
		}
		if tcp == 0 || udp == 0 {
			return nil, ui.Refused("No free pair of ports is left for voice on this machine.", "Pass --tcp-port and --udp-port.")
		}
		ports[host.Port{Number: tcp, Proto: "tcp"}] = true
		ports[host.Port{Number: udp, Proto: "udp"}] = true
		secret, err := voice.NewSecret()
		if err != nil {
			return nil, err
		}
		srv := instance.VoiceServer{
			ID:        id,
			Region:    region,
			Endpoint:  base + voice.Path(id),
			Key:       voice.Key(id),
			Secret:    secret,
			TCPPort:   tcp,
			UDPPort:   udp,
			NodeIP:    nodeIP,
			NodeIPv6:  nodeIPv6,
			IPPinned:  f.IP != "",
			BindIP:    bindAddress(f.IP, voice.InterfaceAddresses()),
			Image:     image,
			Pinned:    f.Image != "",
			Memory:    memory,
			SoftLimit: softLimit,
		}
		pending := make([]instance.VoiceServer, len(plans))
		for i, p := range plans {
			pending[i] = p.srv
		}
		if err := vs.Prepare(&srv, pending); err != nil {
			return nil, err
		}
		plans = append(plans, plannedVoice{srv: srv, publicIP: publicIP, softLimit: softLimit})
	}
	return plans, nil
}

func addIPv6(ctx context.Context, app *App, vs rt.VoiceServers, publicIP string, f *voiceFlags) (string, error) {
	v6 := detectIPv6(ctx, f)
	if v6 == "" {
		return "", nil
	}
	why, fix, err := vs.IPv6Blocked(ctx, bindAddress(f.IP, voice.InterfaceAddresses()))
	if err != nil {
		return "", err
	}
	if why == "" {
		return v6, nil
	}
	if publicIP == "" {
		return "", ui.Refused("This machine has only IPv6, and "+why+".", fix)
	}
	app.UI.Info("This machine has IPv6 too. The new servers offer only IPv4, because %s.", why)
	return "", nil
}

func showVoiceAddSummary(app *App, inst *instance.Instance, region string, plans []plannedVoice, f *voiceFlags) {
	u := app.UI
	hostMem, _ := host.Hardware()
	var total uint64
	for _, p := range plans {
		n, _ := voice.ParseBytes(p.srv.Memory)
		total += n
	}
	for i, p := range plans {
		s := p.srv
		ip := p.publicIP
		if s.NodeIP == "" && ip != "" {
			ip += ", found with STUN"
		} else if ip == "" {
			ip = "LiveKit finds it with STUN"
		}
		memory := s.Memory
		if n, ok := voice.ParseBytes(s.Memory); ok && n > 0 {
			memory = ui.Bytes(int64(n))
		}
		image, _, _ := strings.Cut(cmp.Or(s.Image, "the release's LiveKit"), "@")
		rows := []ui.KV{
			{Key: "Region", Value: region},
			{Key: "Endpoint", Value: s.Endpoint},
			{Key: "Public IPv4", Value: ip},
			{Key: "Public IPv6", Value: orNone(s.NodeIPv6)},
			{Key: "Ports", Value: fmt.Sprintf("%d/tcp, %d/udp", s.TCPPort, s.UDPPort)},
			{Key: "LiveKit", Value: image},
			{Key: "Memory", Value: memory},
			{Key: "Soft limit", Value: plural(p.softLimit, "connection", "connections")},
			{Key: "Webhook", Value: s.Webhook},
		}
		if hostMem > 0 {
			rows[6].Hint = "of " + ui.Bytes(int64(hostMem)) + " on this machine"
		}
		if f.HasCoords {
			rows = append(rows, ui.KV{Key: "Location", Value: fmt.Sprintf("%.2f, %.2f", f.Lat, f.Long)})
		}
		card := ui.Card{Title: "Voice server " + s.ID + " on " + inst.Domain(), Rows: rows}
		if i == len(plans)-1 {
			card.Body = []string{"Adding a path to the edge reloads it, and people connected to Fluxer reconnect once.", "Open the same ports in your provider's firewall. rexctl can't see that one."}
			if f.NoFirewall {
				var ports []string
				for _, p := range plans {
					ports = append(ports, portStrings(voicePortStrings(&p.srv))...)
				}
				card.Body = append(card.Body, fmt.Sprintf("rexctl leaves the firewall on this machine alone. Open %s in it too.", ui.JoinAnd(ports)))
			}
			card.Footer = []string{"Change any of these with flags. rexctl voice add --help lists them."}
		}
		u.Card(card)
	}
	if hostMem > 0 && total > hostMem/2 {
		u.Warn(fmt.Sprintf("These servers can use %s together, more than half of this machine's %s.", voice.HumanBytes(total), voice.HumanBytes(hostMem)), "Pass --memory with a smaller limit.")
	}
}

func confirmVoice(app *App, question string) error {
	ok, err := app.UI.ConfirmWith(ui.Question{Text: question, Default: true})
	if err == nil && !ok {
		app.UI.Info("Nothing changed.")
		return errDeclined
	}
	return err
}

func rollBackVoiceAdd(ctx context.Context, app *App, vs rt.VoiceServers, inst *instance.Instance, id string, cause error) error {
	undo := fmt.Sprintf("Run rexctl voice remove %s to undo what rexctl did so far.", id)
	rerr := app.UI.Step(context.WithoutCancel(ctx), "Taking voice server "+id+" back out", func(ctx context.Context, s *ui.Step) error {
		if err := vs.Remove(ctx, s, id); err != nil {
			return err
		}
		inst.State.RemoveVoiceServer(id)
		if err := saveVoiceState(inst); err != nil {
			return err
		}
		s.Done("Took voice server %s back out.", id)
		return nil
	})
	if rerr != nil {
		return withFix(cause, undo)
	}
	return withFix(cause, "rexctl took "+id+" back out, so nothing is left behind. Run rexctl doctor, then run the same command again.")
}

func addOneVoiceServer(ctx context.Context, app *App, api *adminapi.Client, vs rt.VoiceServers, inst *instance.Instance, plan *plannedVoice, f *voiceFlags) error {
	srv := plan.srv
	srv.CreatedAt = time.Now().UTC().Truncate(time.Second)
	inst.State.Voice = append(inst.State.Voice, srv)
	saved := inst.State.VoiceServer(srv.ID)
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	undo := fmt.Sprintf("Run rexctl voice remove %s to undo what rexctl did so far.", srv.ID)
	err := app.UI.Step(ctx, "Starting voice server "+srv.ID, func(ctx context.Context, s *ui.Step) error {
		if err := vs.Add(ctx, s, *saved); err != nil {
			return err
		}
		s.Done("Started voice server %s.", srv.ID)
		return nil
	})
	if err != nil {
		return rollBackVoiceAdd(ctx, app, vs, inst, srv.ID, err)
	}
	draining := true
	active := true
	internal := vs.Internal(saved)
	notes := "Added with rexctl voice add on " + time.Now().UTC().Format("2006-01-02")
	limit := plan.softLimit
	body := adminapi.CreateVoiceServerRequestBody{
		ServerId:            srv.ID,
		Endpoint:            srv.Endpoint,
		ApiKey:              srv.Key,
		ApiSecret:           srv.Secret,
		IsActive:            &active,
		Draining:            &draining,
		InternalEndpoint:    &internal,
		Notes:               &notes,
		SoftConnectionLimit: &limit,
	}
	if f.HasCoords {
		lat, long := float32(f.Lat), float32(f.Long)
		body.Latitude, body.Longitude = &lat, &long
	}
	if v4 := cmp.Or(srv.NodeIP, plan.publicIP); v4 != "" {
		body.PublicIpv4 = &v4
	}
	if srv.NodeIPv6 != "" {
		v6 := srv.NodeIPv6
		body.PublicIpv6 = &v6
	}
	if _, err := api.CreateVoiceServer(ctx, srv.Region, body); err != nil {
		return ui.Wrap(adminapi.Explain(err), fmt.Sprintf("Couldn't register %s with your instance.", srv.ID), undo)
	}
	base, dial := deploy.VoiceProbeTarget(inst, saved)
	if err := waitForEndpoint(ctx, base, dial, true, time.Minute); err != nil {
		return ui.Wrap(err, fmt.Sprintf("%s doesn't respond through the edge.", srv.Endpoint), undo)
	}
	saved.Opened = openVoiceFirewall(ctx, app, voicePortStrings(saved), f.NoFirewall)
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	open := false
	if _, err := api.UpdateVoiceServer(ctx, srv.Region, srv.ID, adminapi.UpdateVoiceServerRequestBody{Draining: &open}); err != nil {
		return ui.Wrap(adminapi.Explain(err), fmt.Sprintf("%s runs, and it's still draining.", srv.ID), fmt.Sprintf("Run rexctl voice undrain %s.", srv.ID))
	}
	app.UI.OK("Added voice server %s at %s.", srv.ID, srv.Endpoint)
	results := runTransportTests(ctx, srv.Endpoint, serverPaths(saved), func() (string, error) {
		return voice.TestToken(srv.Key, srv.Secret, voice.TestRoom(), time.Now()), nil
	}, true)
	reportTransports(app, results, true)
	app.UI.Info("%s", providerPortsText(saved))
	return nil
}

func providerPortsText(srv *instance.VoiceServer) string {
	return fmt.Sprintf("Open %d/tcp and %d/udp in your provider's firewall too, such as a Hetzner Cloud Firewall or an AWS security group.", srv.TCPPort, srv.UDPPort)
}
