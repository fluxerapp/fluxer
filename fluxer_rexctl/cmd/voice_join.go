// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

const (
	voiceTokenEnv     = "FLUXER_VOICE_JOIN_TOKEN"
	voiceSysctlFile   = "/etc/sysctl.d/61-fluxer-voice.conf"
	voiceSysctl       = "net.core.rmem_max = 16777216\nnet.core.wmem_max = 16777216\n"
	voiceJoinAttempts = 5
)

var (
	dnsWait        = 10 * time.Minute
	hostnameLookup = func() voice.Resolver { return voice.PublicLookup(&http.Client{Timeout: 5 * time.Second}) }
)

func newVoiceJoinCmd(app *App) *cobra.Command {
	f := &voiceFlags{}
	c := &cobra.Command{
		Use:   "join <domain> [token]",
		Short: "Install a voice server on this machine and connect it to an instance",
		Long: "Install a standalone LiveKit voice server on this machine and register it with the Fluxer instance at <domain>. " +
			"Get the token with rexctl voice token on the Fluxer machine, or with Add a voice server in the admin panel. " +
			"The token works once. rexctl also reads it from " + voiceTokenEnv + " or, with --token-stdin, from standard input. " +
			"The server offers IPv4 and IPv6 when the machine has both. On a machine with only IPv6, pass --hostname with an AAAA record that points here.",
		Args: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return ui.Usage("Pass the domain of your Fluxer instance.")
			}
			if len(args) > 2 {
				return ui.Usage(c.CommandPath() + " takes a domain and a token.")
			}
			return nil
		},
		RunE: func(c *cobra.Command, args []string) error {
			if err := validateVoiceFlags(c, f); err != nil {
				return err
			}
			if f.NoEdge && f.Endpoint == "" {
				return ui.Usage("Pass --endpoint with --no-edge.")
			}
			if f.Endpoint != "" && !strings.HasPrefix(f.Endpoint, "wss://") {
				return ui.Usage("Pass --endpoint as a wss:// URL.")
			}
			if err := checkHTTPBind(f, voice.InterfaceAddresses()); err != nil {
				return err
			}
			token, err := joinToken(c, args, f)
			if err != nil {
				return err
			}
			return runVoiceJoin(c.Context(), app, normaliseDomain(args[0]), token, f)
		},
	}
	addVoiceOverrideFlags(c, f)
	fl := c.Flags()
	fl.StringVar(&f.Hostname, "hostname", "", "hostname for the server's endpoint and certificate")
	fl.StringVar(&f.Email, "email", "", "email address for Let's Encrypt notices")
	fl.BoolVar(&f.NoEdge, "no-edge", false, "start no edge and publish the server's HTTP port for your own proxy")
	fl.StringVar(&f.Endpoint, "endpoint", "", "public wss:// URL of the server behind your own proxy")
	fl.IntVar(&f.HTTPPort, "http-port", 0, "port for your own proxy, with --no-edge")
	fl.StringVar(&f.HTTPBind, "http-bind", "", "address the port for your own proxy listens on, with --no-edge (default 127.0.0.1)")
	fl.BoolVar(&f.TokenStdin, "token-stdin", false, "read the join token from standard input")
	return c
}

func joinToken(c *cobra.Command, args []string, f *voiceFlags) (string, error) {
	token := ""
	switch {
	case len(args) == 2:
		token = args[1]
	case f.TokenStdin:
		line, err := bufio.NewReader(c.InOrStdin()).ReadString('\n')
		if err != nil && !errors.Is(err, io.EOF) {
			return "", err
		}
		token = line
	default:
		token = os.Getenv(voiceTokenEnv)
	}
	token = strings.TrimSpace(token)
	if token == "" {
		return "", ui.Usage("Pass the join token after the domain, set " + voiceTokenEnv + ", or pass --token-stdin.")
	}
	if !voice.ValidToken(token) {
		return "", ui.Usage("That isn't a voice join token. They start with fvj_.")
	}
	return token, nil
}

func voiceMachineDir(app *App) (string, error) {
	if app.Flags.Dir != "" {
		return filepath.Abs(app.Flags.Dir)
	}
	for _, dir := range instance.ReadRecord(instance.OwnScope()).Instances {
		if inst, err := instance.Find(dir); err == nil && inst.State.IsVoice() {
			return dir, nil
		}
	}
	if euid() == 0 {
		return "/opt/fluxer-voice", nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(home, "fluxer-voice"), nil
}

func openVoiceMachine(dir string) (*instance.Instance, bool, error) {
	inst, err := instance.Find(dir)
	if err == nil {
		if !inst.State.IsVoice() {
			return nil, false, ui.Refused(fmt.Sprintf("%s holds a Fluxer instance.", dir), "Run rexctl voice add there, or pass --dir with another folder.")
		}
		return inst, false, nil
	}
	if !errors.Is(err, instance.ErrNotFound) {
		return nil, false, ui.Wrap(err, fmt.Sprintf("rexctl can't read %s.", dir), "Pass --dir with another folder.")
	}
	now := time.Now().UTC().Truncate(time.Second)
	inst = &instance.Instance{Dir: dir, State: &instance.State{
		Schema: instance.StateSchema, Kind: instance.KindVoice, Project: voice.MachineProject,
		Settings: map[string]string{}, Secrets: map[string]string{}, InstalledAt: now, UpdatedAt: now,
	}}
	return inst, true, nil
}

type joinPlan struct {
	domain   string
	disc     *voice.Discovery
	preview  *voice.Preview
	reach    voice.Reachability
	srv      instance.VoiceServer
	address  string
	ipCert   bool
	edge     string
	edgeInst *instance.Instance
	newEdge  bool
	firewall host.Firewall
	netIPv6  bool
	hostMem  uint64
	cpus     int
	image    string
	imageWhy string
	coords   bool
	lat      float64
	long     float64
}

func runVoiceJoin(ctx context.Context, app *App, domain, token string, f *voiceFlags) error {
	if runtime.GOOS != "linux" && !f.DryRun {
		return ui.Refused("Voice servers run on Linux.", "Run this on a Linux machine.")
	}
	hc := &http.Client{Timeout: 20 * time.Second}
	client := &voice.Client{HTTP: hc}
	disc, err := client.Discover(ctx, domain)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("rexctl can't read the Fluxer instance at %s.", domain), "Check the domain, and that it has a valid certificate.")
	}
	if !disc.VoiceEnabled {
		return ui.Refused(fmt.Sprintf("Voice is turned off on %s.", domain), "Turn on voice there first.")
	}
	dir, err := voiceMachineDir(app)
	if err != nil {
		return err
	}
	inst, fresh, err := openVoiceMachine(dir)
	if err != nil {
		return err
	}
	e, err := app.Engine(ctx, nil)
	if err != nil && joinInstallsDocker(err, runtime.GOOS, f.DryRun) {
		e, err = installDockerHere(ctx, app, dir, "run the same command again", func() (bool, error) {
			if app.UI.Yes() || !app.UI.Interactive() {
				return true, nil
			}
			return app.UI.Confirm("Install Docker?", true)
		})
		if err == nil {
			app.engine = e
		}
	}
	if err != nil {
		return err
	}
	plan := &joinPlan{domain: domain, disc: disc, coords: f.HasCoords, lat: f.Lat, long: f.Long}
	if err := planJoin(ctx, app, client, e, inst, token, plan, f); err != nil {
		return err
	}
	if err := podmanJoinRefusal(e, plan); err != nil {
		return err
	}
	showJoinSummary(app, plan, f)
	if f.DryRun {
		return nil
	}
	if err := confirmVoice(app, "Install this voice server?"); err != nil {
		return err
	}
	if fresh {
		if err := os.MkdirAll(inst.Path(instance.StateDirName), 0o700); err != nil {
			return ui.Wrap(err, "Couldn't create "+dir+".", "Run rexctl as root, or pass --dir with a folder you can write to.")
		}
		if err := saveVoiceState(inst); err != nil {
			return err
		}
		app.remember(dir)
	}
	l, err := inst.Lock(ctx)
	if err != nil {
		return ui.Refused("Another rexctl command is changing this machine's voice servers.", "Wait for it to finish, then try again.")
	}
	defer l.Release()
	return finishJoin(ctx, app, client, e, inst, token, plan, f)
}

func podmanJoinRefusal(e *engine.Engine, plan *joinPlan) error {
	if e.Kind != engine.Podman || !olderPodman(e.Version) || plan.edgeInst != nil {
		return nil
	}
	return ui.Refused(fmt.Sprintf("Podman %s doesn't start voice servers again when this machine restarts.", e.Version), fmt.Sprintf("Upgrade Podman to %s or later, or use Docker.", podmanRestartsContainers))
}

func joinInstallsDocker(err error, goos string, dryRun bool) bool {
	return goos == "linux" && !dryRun && !errors.Is(err, context.Canceled) && engine.ClassOf(err) == engine.ClassUnreachable
}

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
	plan.image, plan.imageWhy = joinImage(app, plan.preview, f)
	plan.firewall, _ = host.DetectFirewall(ctx)
	if err := planEdge(ctx, app, e, inst, plan, f); err != nil {
		return err
	}
	network := voice.MachineNetwork
	if plan.edgeInst != nil {
		network = plan.edgeInst.Project() + "_fluxer"
	}
	if err := planJoinIPv6(ctx, app, e, inst, plan, network, f); err != nil {
		return err
	}
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
		BindIP:    bindAddress(f.IP, voice.InterfaceAddresses()),
		Image:     plan.image,
		Pinned:    f.Image != "",
		Memory:    memory,
		SoftLimit: soft,
		Project:   voice.JoinedProject(id),
		Network:   network,
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
	return nil
}

func chooseJoinID(preview *voice.Preview, flagID string, inst *instance.Instance, taken []string) (string, error) {
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

func ipv6EngineText(e *engine.Engine) string {
	switch {
	case e.Kind != engine.Docker:
		return fmt.Sprintf("voice servers on IPv6 need Docker %s or later, and this machine runs %s", engine.MinDockerIPv6, e.Kind)
	case e.Rootless:
		return "voice servers on IPv6 need rootful Docker, and this machine runs Docker rootless"
	}
	return fmt.Sprintf("voice servers on IPv6 need Docker %s or later, and this machine runs Docker %s", engine.MinDockerIPv6, e.Version)
}

func planJoinIPv6(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, plan *joinPlan, network string, f *voiceFlags) error {
	exists, has6, err := e.NetworkIPv6(ctx, network)
	if err != nil {
		return engineFailure(err)
	}
	why, fix := "", ""
	switch {
	case !e.IPv6Voice():
		why, fix = ipv6EngineText(e), fmt.Sprintf("Upgrade Docker to %s or later.", engine.MinDockerIPv6)
	case plan.edgeInst != nil && !has6:
		why, fix = "the network of the Fluxer instance on this machine has no IPv6", "On this machine, run rexctl config set network.ipv6 true."
	case exists && !has6, !exists && plan.edge == voice.EdgeOwn && !plan.newEdge && inst.State.VoiceEdge != nil && !inst.State.VoiceEdge.IPv6:
		why, fix = "the voice network on this machine was made without IPv6", "Remove the voice servers on this machine with rexctl voice remove --all, then join again."
	case bindAddress(f.IP, voice.InterfaceAddresses()) != "":
		why, fix = "--ip binds the ports to one IPv4 address", "Leave out --ip."
	}
	if plan.reach.PublicIPv6 != "" && why != "" {
		if plan.reach.PublicIPv4 == "" {
			return ui.Refused("This machine has only IPv6, and "+why+".", fix)
		}
		app.UI.Info("This machine has IPv6 too. The server offers only IPv4, because %s.", why)
		plan.reach.PublicIPv6 = ""
	}
	plan.netIPv6 = plan.reach.PublicIPv6 != ""
	return nil
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

func joinImage(app *App, preview *voice.Preview, f *voiceFlags) (string, string) {
	if f.Image != "" {
		return f.Image, "from --image"
	}
	own := ownLiveKitImage()
	switch {
	case preview.Release == "" || !release.Valid(preview.Release):
		return own, "the pin of this rexctl, because the instance runs a development build"
	case preview.Release == app.Version:
		return own, "matches " + preview.Release
	default:
		return own, fmt.Sprintf("the pin of rexctl %s. The instance runs %s, and rexctl upgrade brings this machine to it", app.Version, preview.Release)
	}
}

func planEdge(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, plan *joinPlan, f *voiceFlags) error {
	if f.NoEdge {
		plan.edge = voice.EdgeNone
		return nil
	}
	owners, _ := e.PublishedHostPorts(ctx)
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
		for _, dir := range instance.ReadRecord(instance.OwnScope()).Instances {
			other, err := instance.Find(dir)
			if err == nil && !other.State.IsVoice() && other.Project() == holder {
				if f.Hostname != "" {
					return ui.Refused(fmt.Sprintf("Ports 80 and 443 belong to the Fluxer instance in %s. The server uses that instance's domain.", dir), "Leave out --hostname.")
				}
				plan.edge, plan.edgeInst = voice.EdgeInstance, other
				plan.address = other.Domain()
				return nil
			}
		}
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
	case s.Edge == voice.EdgeInstance:
		cert = "the certificate of " + plan.address
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
	rows := [][]string{
		{"Server ID", s.ID},
		{"Region", region},
		{"Endpoint", s.Endpoint},
		{"Certificate", cert},
		{"Public IPv4", ip},
		{"Public IPv6", orNone(s.NodeIPv6)},
		{"Ports", strings.Join(ports, ", ")},
		{"Firewall", fw},
		{"LiveKit", s.Image + ", " + plan.imageWhy},
		{"Memory limit", fmt.Sprintf("%s of %s", s.Memory, voice.HumanBytes(plan.hostMem))},
		{"Soft limit", plural(s.SoftLimit, "connection", "connections")},
		{"Webhook", s.Webhook},
	}
	if s.HTTPPort > 0 {
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

func finishJoin(ctx context.Context, app *App, client *voice.Client, e *engine.Engine, inst *instance.Instance, token string, plan *joinPlan, f *voiceFlags) error {
	srv := plan.srv
	if existing := inst.State.VoiceServer(srv.ID); existing != nil {
		srv.Opened = slices.Clone(existing.Opened)
		srv.CreatedAt = existing.CreatedAt
		inst.State.RemoveVoiceServer(srv.ID)
	}
	if srv.CreatedAt.IsZero() {
		srv.CreatedAt = time.Now().UTC().Truncate(time.Second)
	}
	inst.State.Voice = append(inst.State.Voice, srv)
	if plan.edgeInst != nil {
		inst.State.VoiceEdge = &instance.VoiceEdge{Instance: plan.edgeInst.Dir}
	} else if inst.State.VoiceEdge == nil && srv.Edge == voice.EdgeOwn {
		inst.State.VoiceEdge = &instance.VoiceEdge{Email: f.Email, IPv6: plan.netIPv6}
	}
	saved := inst.State.VoiceServer(srv.ID)
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	writeVoiceSysctl(ctx, app)
	if err := writeVoiceServer(inst, saved); err != nil {
		return ui.Wrap(err, "Couldn't write the voice server's files.", "Check that you can write to "+inst.Dir+".")
	}
	dirFlag := voiceDirFlag(inst.Dir)
	undo := fmt.Sprintf("Run rexctl voice join %s <token>%s again to retry, or rexctl voice remove %s%s to undo.", plan.domain, dirFlag, srv.ID, dirFlag)
	err := app.UI.Step(ctx, "Starting voice server "+srv.ID, func(ctx context.Context, s *ui.Step) error {
		if saved.Edge == voice.EdgeOwn {
			if err := ensureVoiceEdge(ctx, app, e, inst, s); err != nil {
				return err
			}
		}
		if err := startVoiceServer(ctx, app, e, inst, saved, s); err != nil {
			return err
		}
		if err := routeVoiceServer(ctx, app, e, inst, saved); err != nil {
			return err
		}
		s.Done("Started voice server %s.", srv.ID)
		return nil
	})
	if err != nil {
		return ui.Wrap(err, "The voice server didn't start.", undo)
	}
	ports := voicePortStrings(saved)
	if saved.Edge == voice.EdgeOwn {
		ports = append(ports, host.Port{Number: 80, Proto: "tcp"}, host.Port{Number: 443, Proto: "tcp"})
	}
	opened := openVoiceFirewall(ctx, app, ports, f.NoFirewall)
	for _, o := range opened {
		if (o == "80/tcp" || o == "443/tcp") && inst.State.VoiceEdge != nil {
			inst.State.VoiceEdge.Opened = append(inst.State.VoiceEdge.Opened, o)
		} else {
			saved.Opened = append(saved.Opened, o)
		}
	}
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	edgeAddr := "127.0.0.1:443"
	if saved.Edge == voice.EdgeNone {
		edgeAddr = ""
	}
	err = app.UI.Step(ctx, "Waiting for "+srv.Endpoint, func(ctx context.Context, s *ui.Step) error {
		var err error
		if edgeAddr == "" {
			err = waitForPublicEndpoint(ctx, srv.Endpoint, 10*time.Minute)
		} else {
			err = waitForEndpoint(ctx, voice.HTTPBase(srv.Endpoint), edgeAddr, false, 3*time.Minute)
		}
		if err != nil {
			return err
		}
		s.Done("%s answers with a valid certificate.", srv.Endpoint)
		return nil
	})
	if err != nil {
		check := "Check that ports 80 and 443 are open to the internet"
		if saved.Edge == voice.EdgeNone {
			path := "/"
			if u, perr := url.Parse(srv.Endpoint); perr == nil && u.Path != "" {
				path = u.Path
			}
			check = fmt.Sprintf("Check that your proxy sends %s to %s with WebSocket upgrades", path, voice.HTTPAddress(saved.HTTPBind, saved.HTTPPort))
		}
		return ui.Wrap(err, fmt.Sprintf("%s doesn't answer yet.", srv.Endpoint), check+", then "+strings.ToLower(undo[:1])+undo[1:])
	}
	joined, err := joinWithRetries(ctx, app, client, e, inst, token, plan, saved)
	if err != nil {
		return err
	}
	saved.NodeURL = joined.NodeURL
	saved.Region = joined.RegionID
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	app.UI.OK("Voice server %s joined %s in region %s.", saved.ID, plan.domain, joined.RegionID)
	waitForVoiceOrigin(ctx, app, client, plan.domain, saved.Endpoint)
	results := runTransportTests(ctx, saved.Endpoint, serverPaths(saved), func() (string, error) {
		return voice.TestToken(saved.Key, saved.Secret, voice.TestRoom(), time.Now()), nil
	}, false)
	reportTransports(app, results, true)
	app.UI.Info("Run rexctl voice test %s on the Fluxer machine to test from outside this network.", saved.ID)
	if saved.NodeIP == "" {
		app.UI.Info("%s", ipv6OnlyText(saved.ID, len(plan.preview.TurnServers) > 0))
	}
	if status, _, err := client.TestWebhook(ctx, saved.Webhook, saved.Key, saved.Secret, time.Now()); err != nil || status < 200 || status > 299 {
		app.UI.Warn(fmt.Sprintf("%s doesn't accept a test webhook from %s.", plan.domain, saved.ID), "Run rexctl doctor to see why.")
	} else {
		app.UI.OK("%s accepts webhooks from %s.", plan.domain, saved.ID)
	}
	app.UI.Info("Run rexctl voice status%s to check it later, and rexctl voice remove %s%s to take it away.", dirFlag, saved.ID, dirFlag)
	app.UI.Info("This machine doesn't upgrade with the Fluxer machine. Run rexctl upgrade%s here after you upgrade Fluxer, or rexctl auto-upgrade on%s to upgrade it every day.", dirFlag, dirFlag)
	return nil
}

func joinWithRetries(ctx context.Context, app *App, client *voice.Client, e *engine.Engine, inst *instance.Instance, token string, plan *joinPlan, saved *instance.VoiceServer) (*voice.Joined, error) {
	for attempt := 1; ; attempt++ {
		req := voice.JoinRequest{
			ServerID:   saved.ID,
			RegionID:   plan.preview.Region.ID,
			Endpoint:   saved.Endpoint,
			APIKey:     saved.Key,
			APISecret:  saved.Secret,
			SoftLimit:  saved.SoftLimit,
			PublicIPv4: saved.NodeIP,
			PublicIPv6: saved.NodeIPv6,
		}
		switch {
		case plan.coords:
			req.Latitude, req.Longitude = &plan.lat, &plan.long
		case plan.preview.Latitude != nil && plan.preview.Longitude != nil:
			req.Latitude, req.Longitude = plan.preview.Latitude, plan.preview.Longitude
		}
		joined, err := client.Join(ctx, plan.disc.APIPublic, token, req)
		code := voice.CodeOf(err)
		if err == nil {
			return joined, nil
		}
		if code == voice.CodeEndpointUnreachable && saved.NodeIP == "" {
			return nil, ui.Failed(fmt.Sprintf("%s couldn't connect to the server's endpoint (%s).", plan.domain, err), "The Fluxer machine may have no IPv6. On it, run rexctl config set network.ipv6 true, then run the same command here again.")
		}
		if plan.preview.ServerIDPinned {
			return nil, pinnedJoinError(err, plan.domain, saved.ID)
		}
		if (code != voice.CodeIDInUse && code != voice.CodeKeyInUse) || attempt >= voiceJoinAttempts {
			return nil, joinError(err, plan.domain, saved.ID)
		}
		next := voice.NextID(plan.preview.Region.ID, append(voiceIDsInUse(ctx, e, inst, voice.JoinedProject("")), saved.ID))
		app.UI.Info("%s is taken on %s. Trying %s.", saved.ID, plan.domain, next)
		if err := renameJoinedServer(ctx, app, e, inst, saved, next, plan); err != nil {
			return nil, err
		}
	}
}

func renameJoinedServer(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, saved *instance.VoiceServer, next string, plan *joinPlan) error {
	old := *saved
	if err := removeVoiceServerProject(ctx, e, &old); err != nil {
		return err
	}
	unrouteVoiceServer(ctx, app, e, inst, &old)
	_ = os.RemoveAll(voice.ServerDir(inst.Dir, old.ID))
	saved.ID = next
	saved.Key = voice.Key(next)
	saved.Project = voice.JoinedProject(next)
	if saved.Edge != voice.EdgeNone {
		saved.Endpoint = voice.Endpoint(plan.address, next)
	}
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	if err := writeVoiceServer(inst, saved); err != nil {
		return err
	}
	return app.UI.Step(ctx, "Starting voice server "+next, func(ctx context.Context, s *ui.Step) error {
		if err := startVoiceServer(ctx, app, e, inst, saved, s); err != nil {
			return err
		}
		if err := routeVoiceServer(ctx, app, e, inst, saved); err != nil {
			return err
		}
		if saved.Edge != voice.EdgeNone {
			if err := waitForEndpoint(ctx, voice.HTTPBase(saved.Endpoint), "127.0.0.1:443", false, 2*time.Minute); err != nil {
				return err
			}
		}
		s.Done("Started voice server %s.", next)
		return nil
	})
}

func ipv6OnlyText(id string, turn bool) string {
	if turn {
		return fmt.Sprintf("This server offers only IPv6. Callers with IPv6 connect to calls on it directly. Callers with only IPv4 can join through your TURN server when it hands out IPv6 relay addresses and an A record for the hostname points at a machine that forwards ports 80 and 443 here. Run rexctl voice test %s on the Fluxer machine to check, and see the voice servers guide.", id)
	}
	return "This server offers only IPv6. Callers with IPv6 connect to calls on it. Callers with only IPv4 can't, and your instance gives clients no TURN server. See the voice servers guide."
}

func pinnedJoinError(err error, domain, id string) error {
	if code := voice.CodeOf(err); code == voice.CodeIDInUse || code == voice.CodeKeyInUse {
		return ui.Refused(fmt.Sprintf("%s already has a server called %s.", domain, id), "Delete it in the admin panel, or run rexctl voice token --id with another server ID for a new token.")
	}
	return joinError(err, domain, id)
}

func joinError(err error, domain, id string) error {
	var api *voice.APIError
	if !errors.As(err, &api) {
		return ui.Wrap(err, fmt.Sprintf("rexctl couldn't connect to %s.", domain), "Check the domain and this machine's internet connection, then try again.")
	}
	detail := ""
	if api.Detail != "" {
		detail = " (" + api.Detail + ")"
	}
	switch api.Code {
	case voice.CodeTokenInvalid:
		return ui.Refused("The join token doesn't work. It may be used, expired or revoked.", "Run rexctl voice token on the Fluxer machine for a new one.")
	case voice.CodeTokenBusy:
		return ui.Refused("Another machine is joining with this token right now.", "Wait two minutes, then try again.")
	case voice.CodeEndpointUnreachable:
		return ui.Failed(fmt.Sprintf("%s couldn't connect to the server's endpoint%s.", domain, detail), "Check that ports 80 and 443 are open in your provider's firewall, then run the same command again.")
	case voice.CodeEndpointInvalid:
		if host, ok := strings.CutPrefix(api.Detail, "The endpoint must use the hostname "); ok {
			return ui.Refused(fmt.Sprintf("%s accepts only the hostname %s for this server.", domain, host), fmt.Sprintf("Point %s at this machine, then use it.", host))
		}
		return ui.Refused(fmt.Sprintf("%s doesn't accept the server's endpoint%s.", domain, detail), "Use a hostname that resolves to a public address, or the machine's public IPv4 address.")
	case voice.CodeKeyMismatch:
		return ui.Failed("The voice server rejected its own key.", fmt.Sprintf("Run rexctl voice remove %s, then join again.", id))
	case voice.CodeIDInUse, voice.CodeKeyInUse:
		return ui.Refused(fmt.Sprintf("%s already has a server with this ID or key.", domain), "Pass --id with another ID.")
	case voice.CodeUnknownRegion:
		return ui.Refused("That region doesn't exist on the instance.", "Leave out --region, or create the region with a new token.")
	}
	return ui.Failed(fmt.Sprintf("%s refused the request (%s).", domain, api.Error()), "Run the same command again, or ask the instance's admin.")
}

func writeVoiceSysctl(ctx context.Context, app *App) {
	if runtime.GOOS != "linux" {
		return
	}
	if euid() != 0 {
		if voice.ReadSysctl("net.core.rmem_max") != "16777216" || voice.ReadSysctl("net.core.wmem_max") != "16777216" {
			showVoiceSysctlCommands(app)
		}
		return
	}
	if data, err := os.ReadFile(voiceSysctlFile); err != nil || string(data) != voiceSysctl {
		if err := os.WriteFile(voiceSysctlFile, []byte(voiceSysctl), 0o644); err != nil {
			app.UI.Detail("writing %s: %v", voiceSysctlFile, err)
			return
		}
	}
	if voice.ReadSysctl("net.core.rmem_max") == "16777216" && voice.ReadSysctl("net.core.wmem_max") == "16777216" {
		return
	}
	if err := runQuietCommand(ctx, "sysctl", "-p", voiceSysctlFile); err != nil {
		app.UI.Detail("loading %s: %v", voiceSysctlFile, err)
	}
}

func showVoiceSysctlCommands(app *App) {
	app.UI.Info("LiveKit needs larger UDP buffers for many calls, and only root can raise them. Run these commands:")
	app.UI.Info("  printf '%s' | sudo tee %s", strings.ReplaceAll(voiceSysctl, "\n", `\n`), voiceSysctlFile)
	app.UI.Info("  sudo sysctl -p %s", voiceSysctlFile)
}

func ensureVoiceEdge(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, s *ui.Step) error {
	dir := inst.Path(voice.EdgeDir)
	if err := os.MkdirAll(filepath.Join(dir, voice.SitesDir), 0o755); err != nil {
		return err
	}
	set := voiceSet(nil)
	for _, name := range []string{voice.EdgeComposeFile, voice.EdgeCaddyfile} {
		data, err := voice.ShippedFile(set, name)
		if err != nil {
			return err
		}
		if err := os.WriteFile(filepath.Join(dir, name), data, 0o644); err != nil {
			return err
		}
	}
	if err := writeVoiceSites(inst); err != nil {
		return err
	}
	p, err := e.Load(ctx, voiceEdgeSpec(inst))
	if err != nil {
		return engineFailure(err)
	}
	if err := p.Pull(ctx, app.progress(s, "Starting")); err != nil {
		return engineFailure(err)
	}
	if err := p.Up(context.WithoutCancel(ctx), engine.UpOptions{Wait: true, WaitTimeout: deploy.VoiceStartTimeout}); err != nil {
		return ui.Wrap(engineFailure(err), "The voice edge didn't start.", "Run rexctl voice status to see why.")
	}
	return nil
}

func voiceEdgeSpec(inst *instance.Instance) engine.Spec {
	env := map[string]string{"FLUXER_VOICE_NETWORK": voice.MachineNetwork}
	if inst.State.VoiceEdge != nil && inst.State.VoiceEdge.Email != "" {
		env["FLUXER_VOICE_EDGE_EMAIL"] = inst.State.VoiceEdge.Email
	}
	if inst.State.VoiceEdge != nil && inst.State.VoiceEdge.IPv6 {
		env["FLUXER_VOICE_NETWORK_IPV6"] = "true"
	}
	for _, s := range inst.State.Voice {
		if u, err := url.Parse(s.Endpoint); err == nil && s.Edge == voice.EdgeOwn && voice.IsIPv4(u.Hostname()) {
			env["FLUXER_VOICE_EDGE_IP"] = u.Hostname()
		}
	}
	return engine.Spec{Name: voice.EdgeProject, Dir: inst.Path(voice.EdgeDir), Files: []string{voice.EdgeComposeFile}, Env: env}
}

func writeVoiceSites(inst *instance.Instance) error {
	dir := inst.Path(voice.EdgeDir, voice.SitesDir)
	byAddress := map[string][]string{}
	for _, s := range inst.State.Voice {
		if s.Edge != voice.EdgeOwn {
			continue
		}
		u, err := url.Parse(s.Endpoint)
		if err != nil {
			continue
		}
		byAddress[u.Hostname()] = append(byAddress[u.Hostname()], s.ID)
	}
	entries, _ := os.ReadDir(dir)
	for _, ent := range entries {
		keep := false
		for address := range byAddress {
			if ent.Name() == voice.SiteFileName(address) {
				keep = true
			}
		}
		if !keep {
			_ = os.Remove(filepath.Join(dir, ent.Name()))
		}
	}
	for address, ids := range byAddress {
		ip := strings.Trim(address, "0123456789.") == ""
		if err := os.WriteFile(filepath.Join(dir, voice.SiteFileName(address)), voice.SiteFile(address, ids, ip), 0o644); err != nil {
			return err
		}
	}
	return nil
}

func routeVoiceServer(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer) error {
	switch srv.Edge {
	case voice.EdgeOwn:
		if err := writeVoiceSites(inst); err != nil {
			return err
		}
		return reloadCaddy(ctx, e.Named(voice.EdgeProject, inst.Path(voice.EdgeDir)), "edge", "/etc/caddy/Caddyfile")
	case voice.EdgeInstance:
		target := inst
		if inst.State.IsVoice() && inst.State.VoiceEdge != nil && inst.State.VoiceEdge.Instance != "" {
			other, err := instance.Find(inst.State.VoiceEdge.Instance)
			if err != nil {
				return err
			}
			target = other
		}
		path := filepath.Join(target.Path(instance.CaddyDropInDir), voice.DropInName(srv.ID))
		if err := os.WriteFile(path, voice.DropIn(srv.ID), 0o644); err != nil {
			return err
		}
		return reloadCaddy(ctx, e.Named(target.Project(), target.Dir), "edge", "/etc/caddy/Caddyfile")
	}
	app.UI.Info("Point your proxy at %s for %s, with WebSocket upgrades and a read timeout of at least 120 seconds.", voice.HTTPAddress(srv.HTTPBind, srv.HTTPPort), srv.Endpoint)
	if !voice.HTTPBindPrivate(srv.HTTPBind) {
		app.UI.Warn(fmt.Sprintf("%s has no TLS of its own.", voice.HTTPAddress(srv.HTTPBind, srv.HTTPPort)), "Let only your proxy's network connect to it in your firewall.")
	}
	return nil
}

func unrouteVoiceServer(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer) {
	switch srv.Edge {
	case voice.EdgeOwn:
		if err := writeVoiceSites(inst); err == nil {
			_ = reloadCaddy(ctx, e.Named(voice.EdgeProject, inst.Path(voice.EdgeDir)), "edge", "/etc/caddy/Caddyfile")
		}
	case voice.EdgeInstance:
		target := inst
		if inst.State.IsVoice() && inst.State.VoiceEdge != nil && inst.State.VoiceEdge.Instance != "" {
			other, err := instance.Find(inst.State.VoiceEdge.Instance)
			if err != nil {
				app.UI.Detail("%v", err)
				return
			}
			target = other
		}
		_ = os.Remove(filepath.Join(target.Path(instance.CaddyDropInDir), voice.DropInName(srv.ID)))
		if err := reloadCaddy(ctx, e.Named(target.Project(), target.Dir), "edge", "/etc/caddy/Caddyfile"); err != nil {
			app.UI.Detail("reloading the edge: %v", err)
		}
	}
}

func waitForPublicEndpoint(ctx context.Context, endpoint string, limit time.Duration) error {
	u, err := url.Parse(endpoint)
	if err != nil {
		return err
	}
	return waitForEndpoint(ctx, voice.HTTPBase(endpoint), u.Host+portSuffix(u), false, limit)
}

func portSuffix(u *url.URL) string {
	if u.Port() != "" {
		return ""
	}
	return ":443"
}

type originState int

const (
	originUnknown originState = iota
	originUnlisted
	originPending
	originAllowed
)

func voiceOriginState(ctx context.Context, client *voice.Client, domain, origin string) originState {
	disc, err := client.Discover(ctx, domain)
	switch {
	case err != nil:
		return originUnknown
	case !disc.ListsOrigins:
		return originUnlisted
	case !slices.Contains(disc.VoiceOrigins, origin):
		return originPending
	}
	if ok, err := client.AllowsOrigin(ctx, domain, origin); err == nil && ok {
		return originAllowed
	}
	return originPending
}

func endpointOrigin(endpoint string) string {
	u, err := url.Parse(endpoint)
	if err != nil || u.Host == "" {
		return ""
	}
	return u.Scheme + "://" + u.Host
}

func waitForVoiceOrigin(ctx context.Context, app *App, client *voice.Client, domain, endpoint string) {
	origin := endpointOrigin(endpoint)
	if origin == "" {
		return
	}
	state := originUnknown
	deadline := time.Now().Add(3 * time.Minute)
	for time.Now().Before(deadline) {
		if state = voiceOriginState(ctx, client, domain, origin); state == originAllowed {
			app.UI.OK("Browsers on %s can connect to %s.", domain, origin)
			return
		}
		select {
		case <-ctx.Done():
			return
		case <-time.After(5 * time.Second):
		}
	}
	app.UI.Warn(fmt.Sprintf("Browsers on %s can't connect to %s yet.", domain, origin), originFix(state, domain, app.Version))
}

func originFix(state originState, domain, version string) string {
	if state == originUnlisted {
		target := "the latest release"
		if release.Valid(version) {
			target = version
		}
		return fmt.Sprintf("Upgrade Fluxer on %s to %s, then reload open pages.", domain, target)
	}
	return "Check again in a minute with rexctl voice status. Pages opened before then need a reload."
}
