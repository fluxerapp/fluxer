// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"net/netip"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

type voiceFlags struct {
	ID         string
	Region     string
	NewRegion  string
	RegionName string
	Lat, Long  float64
	HasCoords  bool
	Hostname   string
	IP         string
	TCPPort    int
	UDPPort    int
	SoftLimit  int
	Memory     string
	Image      string
	Email      string
	NoEdge     bool
	Endpoint   string
	HTTPPort   int
	TokenStdin bool
	NoFirewall bool
	DryRun     bool
	Force      bool
	Count      int
}

func addVoiceOverrideFlags(c *cobra.Command, f *voiceFlags) {
	fl := c.Flags()
	fl.StringVar(&f.ID, "id", "", "server ID, such as fra-server-2")
	fl.StringVar(&f.Region, "region", "", "existing voice region for the server")
	fl.Float64Var(&f.Lat, "lat", 0, "latitude of the server")
	fl.Float64Var(&f.Long, "long", 0, "longitude of the server")
	fl.StringVar(&f.IP, "ip", "", "public IPv4 address LiveKit gives to clients, also the address the ports bind to when it's on this machine")
	fl.IntVar(&f.TCPPort, "tcp-port", 0, "TCP port for media")
	fl.IntVar(&f.UDPPort, "udp-port", 0, "UDP port for media")
	fl.IntVar(&f.SoftLimit, "soft-limit", 0, "connections above which placement prefers another server")
	fl.StringVar(&f.Memory, "memory", "", "memory limit for the server, such as 2g")
	fl.StringVar(&f.Image, "image", "", "LiveKit image to run")
	fl.BoolVar(&f.NoFirewall, "no-firewall", false, "leave the firewall on this machine alone")
	fl.BoolVar(&f.DryRun, "dry-run", false, "show what would happen and change nothing")
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
			return ui.Usage(fmt.Sprintf("Pass --ip as an IPv4 address. %s isn't one.", f.IP))
		}
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
		Long: "Add standalone LiveKit voice servers next to your Fluxer instance on this machine. " +
			"Each one gets its own ID, ports, keys and path on your domain, and rexctl registers it with your instance.",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			if err := validateVoiceFlags(c, f); err != nil {
				return err
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
	named, err := app.Named(ctx, inst)
	if err != nil {
		return err
	}
	api := app.adminClient(inst, named)
	switch inst.State.Settings[settings.EdgeMode] {
	case settings.EdgeTunnel:
		if !f.Force {
			return ui.Refused("Fluxer runs through a tunnel here. Signalling works through a tunnel and call audio doesn't.", "Pass --force to add the server anyway, then open its ports to the internet.")
		}
	case settings.EdgeProxy:
		app.UI.Info("Your proxy must pass WebSocket upgrades on the new paths, as it does for /livekit/.")
	}
	return app.Locked(ctx, inst, func() error {
		e, err := app.Engine(ctx, inst)
		if err != nil {
			return err
		}
		regions, err := api.VoiceRegions(ctx)
		if err != nil {
			return adminapi.Explain(err)
		}
		region, err := pickRegion(regions, f)
		if err != nil {
			return err
		}
		plans, err := planVoiceAdd(ctx, app, e, inst, regions, region, f)
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
			if err := addOneVoiceServer(ctx, app, api, e, named, inst, &plans[i], f); err != nil {
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

func planVoiceAdd(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, regions []adminapi.VoiceRegion, region string, f *voiceFlags) ([]plannedVoice, error) {
	ids := append(takenVoiceIDs(regions, inst), voiceIDsInUse(ctx, e, inst, voice.ProjectName(inst.Project(), ""))...)
	ports := takenVoicePorts(ctx, e, inst)
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
	memory := f.Memory
	if memory == "" {
		memory = strings.TrimSpace(inst.Setting("FLUXER_LIVEKIT_MEMORY_LIMIT"))
		if _, ok := voice.ParseBytes(memory); !ok {
			memory = voice.DefaultMemory
		}
	}
	image := f.Image
	if image == "" {
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
		bindIP := bindAddress(f.IP, voice.InterfaceAddresses())
		plans = append(plans, plannedVoice{
			srv: instance.VoiceServer{
				ID:        id,
				Region:    region,
				Endpoint:  base + voice.Path(id),
				Key:       voice.Key(id),
				Secret:    secret,
				TCPPort:   tcp,
				UDPPort:   udp,
				NodeIP:    nodeIP,
				IPPinned:  f.IP != "",
				BindIP:    bindIP,
				Image:     image,
				Pinned:    f.Image != "",
				Memory:    memory,
				SoftLimit: softLimit,
				Project:   voice.ProjectName(inst.Project(), id),
				Network:   inst.Project() + "_fluxer",
				Webhook:   "http://api:8080/webhooks/livekit",
				Edge:      voice.EdgeInstance,
			},
			publicIP:  publicIP,
			softLimit: softLimit,
		})
	}
	return plans, nil
}

func showVoiceAddSummary(app *App, inst *instance.Instance, region string, plans []plannedVoice, f *voiceFlags) {
	u := app.UI
	hostMem, _ := host.Hardware()
	var total uint64
	for _, p := range plans {
		n, _ := voice.ParseBytes(p.srv.Memory)
		total += n
	}
	for _, p := range plans {
		s := p.srv
		u.Info("Voice server %s on %s", s.ID, inst.Domain())
		ip := p.publicIP
		if s.NodeIP == "" && ip != "" {
			ip += ", found with STUN"
		} else if ip == "" {
			ip = "LiveKit finds it with STUN"
		}
		rows := [][]string{
			{"Region", region},
			{"Endpoint", s.Endpoint},
			{"Public IPv4", ip},
			{"Ports", fmt.Sprintf("%d/tcp, %d/udp", s.TCPPort, s.UDPPort)},
			{"LiveKit", s.Image},
			{"Memory limit", fmt.Sprintf("%s of %s", s.Memory, voice.HumanBytes(hostMem))},
			{"Soft limit", plural(p.softLimit, "connection", "connections")},
			{"Webhook", s.Webhook},
		}
		if f.HasCoords {
			rows = append(rows, []string{"Location", fmt.Sprintf("%.2f, %.2f", f.Lat, f.Long)})
		}
		for _, r := range rows {
			u.Info("  %-15s %s", r[0], r[1])
		}
		u.Info("")
	}
	if hostMem > 0 && total > hostMem/2 {
		u.Warn(fmt.Sprintf("These servers can use %s together, more than half of this machine's %s.", voice.HumanBytes(total), voice.HumanBytes(hostMem)), "Pass --memory with a smaller limit.")
	}
	u.Info("Adding a path to the edge reloads it, and people connected to Fluxer reconnect once.")
	u.Info("Open the same ports in your provider's firewall. rexctl can't see that one.")
	if f.NoFirewall {
		var ports []string
		for _, p := range plans {
			ports = append(ports, portStrings(voicePortStrings(&p.srv))...)
		}
		u.Info("rexctl leaves the firewall on this machine alone. Open %s in it too.", ui.JoinAnd(ports))
	}
	u.Info("Change any of these with flags. Run rexctl voice add --help to see them.")
}

func confirmVoice(app *App, question string) error {
	if app.UI.Yes() || !app.UI.Interactive() {
		return nil
	}
	ok, err := app.UI.Confirm(question, true)
	if err != nil {
		return err
	}
	if !ok {
		return ui.Refused("Stopped without changing anything.", "")
	}
	return nil
}

func addOneVoiceServer(ctx context.Context, app *App, api *adminapi.Client, e *engine.Engine, p *engine.Project, inst *instance.Instance, plan *plannedVoice, f *voiceFlags) error {
	srv := plan.srv
	srv.CreatedAt = time.Now().UTC().Truncate(time.Second)
	if err := writeVoiceServer(inst, &srv); err != nil {
		return ui.Wrap(err, "Couldn't write the voice server's files.", "Check that you can write to "+inst.Dir+".")
	}
	inst.State.Voice = append(inst.State.Voice, srv)
	saved := inst.State.VoiceServer(srv.ID)
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	undo := fmt.Sprintf("Run rexctl voice remove %s to undo what rexctl did so far.", srv.ID)
	err := app.UI.Step(ctx, "Starting voice server "+srv.ID, func(ctx context.Context, s *ui.Step) error {
		if err := startVoiceServer(ctx, app, e, inst, saved, s); err != nil {
			return err
		}
		s.Done("Started voice server %s.", srv.ID)
		return nil
	})
	if err != nil {
		return err
	}
	draining := true
	active := true
	internal := voice.InternalURL(srv.ID)
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
	_, err = api.CreateVoiceServer(ctx, srv.Region, body)
	if err != nil {
		return ui.Wrap(adminapi.Explain(err), fmt.Sprintf("Couldn't register %s with your instance.", srv.ID), undo)
	}
	dropIn := filepath.Join(inst.Path(instance.CaddyDropInDir), voice.DropInName(srv.ID))
	if err := os.WriteFile(dropIn, voice.DropIn(srv.ID), 0o644); err != nil {
		return ui.Wrap(err, "Couldn't write the edge route.", undo)
	}
	if err := reloadCaddy(ctx, p, "edge", "/etc/caddy/Caddyfile"); err != nil {
		return ui.Wrap(engineFailure(err), "Couldn't reload the edge with the new route.", undo)
	}
	base, dial := voiceProbeTarget(inst, saved)
	if err := waitForEndpoint(ctx, base, dial, true, time.Minute); err != nil {
		return ui.Wrap(err, fmt.Sprintf("%s doesn't answer through the edge.", srv.Endpoint), undo)
	}
	saved.Opened = openVoiceFirewall(ctx, app, voicePortStrings(saved), f.NoFirewall)
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	open := false
	if _, err := api.UpdateVoiceServer(ctx, srv.Region, srv.ID, adminapi.UpdateVoiceServerRequestBody{Draining: &open}); err != nil {
		return ui.Wrap(adminapi.Explain(err), fmt.Sprintf("%s runs, and it's still draining.", srv.ID), fmt.Sprintf("Clear Draining for %s in the admin panel.", srv.ID))
	}
	app.UI.OK("Added voice server %s at %s.", srv.ID, srv.Endpoint)
	results := runTransportTests(ctx, srv.Endpoint, func() (string, error) {
		return voice.TestToken(srv.Key, srv.Secret, voice.TestRoom(), time.Now()), nil
	}, true)
	reportTransports(app, results, true)
	app.UI.Info("%s", providerPortsText(saved))
	return nil
}

func providerPortsText(srv *instance.VoiceServer) string {
	return fmt.Sprintf("Open %d/tcp and %d/udp in your provider's firewall too, such as a Hetzner Cloud Firewall or an AWS security group.", srv.TCPPort, srv.UDPPort)
}
