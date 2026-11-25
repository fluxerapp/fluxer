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
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
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
			token, err := joinToken(c, app, args, f)
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
	fl.StringVar(&f.Runtime, "runtime", "", "where the voice server runs, systemd, docker or podman (default systemd on Debian or Ubuntu as root, otherwise a container)")
	return c
}

func joinToken(c *cobra.Command, app *App, args []string, f *voiceFlags) (string, error) {
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
	if token == "" && app.UI.Interactive() {
		field := ui.Field{Title: "Join token", Description: "From rexctl voice token on the Fluxer machine. It starts with fvj_.", Value: &token, Secret: true, Validate: func(v string) error {
			if !voice.ValidToken(strings.TrimSpace(v)) {
				return errors.New("that isn't a voice join token. They start with fvj_")
			}
			return nil
		}}
		if err := app.UI.Ask(c, field); err != nil {
			return "", err
		}
		token = strings.TrimSpace(token)
	}
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
	inst     *instance.Instance
	rel      *release.Release
	domain   string
	disc     *voice.Discovery
	preview  *voice.Preview
	reach    voice.Reachability
	srv      instance.VoiceServer
	address  string
	ipCert   bool
	edge     string
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
	if err := chooseJoinRuntime(ctx, inst, fresh, f); err != nil {
		return err
	}
	if err := joinBesideInstances(f); err != nil {
		return err
	}
	e, err := joinEngine(ctx, inst)
	if err != nil && joinStartsPodman(err, inst, f) {
		e, err = startPodmanHere(ctx, app)
		if err == nil {
			app.engine = e
		}
	} else if err != nil && joinInstallsDocker(err, runtime.GOOS, f.DryRun) && inst.State.Engine.Kind != string(engine.Podman) {
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
	if e != nil && inst.State.Runtime == instance.RuntimeContainer {
		inst.State.Engine = instance.EngineRef{Kind: string(e.Kind), Host: e.Host, ID: e.ID}
	}
	plan := &joinPlan{inst: inst, domain: domain, disc: disc, coords: f.HasCoords, lat: f.Lat, long: f.Long}
	if err := planJoin(ctx, app, client, e, inst, token, plan, f); err != nil {
		return err
	}
	if e != nil {
		if err := podmanRefusal(e); err != nil {
			return err
		}
	}
	if err := rootlessEdgeRefusal(e, plan); err != nil {
		return err
	}
	if plan.rel, err = voiceHostRelease(ctx, inst, plan.preview.Release); err != nil {
		return err
	}
	showJoinSummary(app, plan, f)
	if f.DryRun {
		return nil
	}
	if app.UI.Interactive() && !app.UI.Yes() {
		if err := confirmVoice(app, "Install this voice server?"); err != nil {
			return err
		}
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

func joinBesideInstances(f *voiceFlags) error {
	if f.NoEdge {
		return nil
	}
	for _, dir := range instance.ReadRecord(instance.OwnScope()).Instances {
		if other, err := instance.Find(dir); err == nil && !other.State.IsVoice() {
			return ui.Refused(fmt.Sprintf("The Fluxer instance in %s holds ports 80 and 443 on this machine.", dir), "Run rexctl voice add there to give it another voice server, or pass --no-edge and --endpoint to put this one behind your own proxy.")
		}
	}
	return nil
}

func rootlessEdgeRefusal(e *engine.Engine, plan *joinPlan) error {
	if e == nil || plan.srv.Edge != voice.EdgeOwn || !host.LowPortsBlocked(e.Rootless) {
		return nil
	}
	return ui.Refused(fmt.Sprintf("Rootless %s can't open ports 80 and 443 on this machine.", e.Kind.Name()), "Run the same command again with sudo, or pass --no-edge and --endpoint to put the voice server behind your own proxy.")
}

func podmanRefusal(e *engine.Engine) error {
	if e.Kind != engine.Podman || !olderPodman(e.Version) {
		return nil
	}
	return ui.Refused(fmt.Sprintf("Podman %s doesn't start voice servers again when this machine restarts.", e.Version), fmt.Sprintf("Upgrade Podman to %s or later, or use Docker.", podmanRestartsContainers))
}

func joinInstallsDocker(err error, goos string, dryRun bool) bool {
	return goos == "linux" && !dryRun && !errors.Is(err, context.Canceled) && engine.ClassOf(err) == engine.ClassUnreachable
}

func joinStartsPodman(err error, inst *instance.Instance, f *voiceFlags) bool {
	if inst.State.Runtime != instance.RuntimeContainer || inst.State.Engine.Kind == string(engine.Docker) || !joinInstallsDocker(err, runtime.GOOS, f.DryRun) {
		return false
	}
	if _, err := exec.LookPath("podman"); err != nil {
		return false
	}
	return inst.State.Engine.Kind == string(engine.Podman) || !dockerInstalled()
}

func startPodmanHere(ctx context.Context, app *App) (*engine.Engine, error) {
	rootless := os.Geteuid() != 0
	s := host.CheckPodman(ctx, rootless, true)
	if !s.Systemd {
		return nil, ui.Refused("Podman's API socket isn't running.", "Start podman.socket, then run the same command again.")
	}
	steps := s.Steps()
	if !host.PodmanRestartOn(ctx, rootless) {
		steps = append(steps, host.PodmanRestartStep(rootless))
	}
	var e *engine.Engine
	err := app.UI.Step(ctx, "Starting Podman's API socket", func(ctx context.Context, st *ui.Step) error {
		if err := host.RunSteps(ctx, steps); err != nil {
			if errors.Is(err, host.ErrNoUserSession) {
				msg, fix := host.NoUserSession("the same command again")
				return ui.Wrap(err, msg, fix)
			}
			return ui.Wrap(err, "Couldn't start Podman's API socket.", "Run the same command again with --verbose to see why.")
		}
		var err error
		if e, err = engine.Connect(ctx, engine.ConnectOptions{Prefer: engine.Podman, AllowOld: true}); err != nil {
			return rt.EngineError(engine.Podman, err)
		}
		st.Done("Started Podman's API socket.")
		return nil
	})
	return e, err
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
	if inst.State.VoiceEdge == nil && srv.Edge == voice.EdgeOwn {
		inst.State.VoiceEdge = &instance.VoiceEdge{Email: f.Email, IPv6: plan.netIPv6}
	}
	saved := inst.State.VoiceServer(srv.ID)
	if err := saveVoiceState(inst); err != nil {
		return err
	}
	writeVoiceSysctl(ctx, app)
	dirFlag := voiceDirFlag(inst.Dir)
	undo := fmt.Sprintf("Run rexctl voice join %s <token>%s again to retry, or rexctl voice remove %s%s to undo.", plan.domain, dirFlag, srv.ID, dirFlag)
	err := app.UI.Step(ctx, "Starting voice server "+srv.ID, func(ctx context.Context, s *ui.Step) error {
		if err := ensureVoiceHost(ctx, app, inst, plan.rel, s); err != nil {
			return err
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
		s.Done("%s has a valid certificate.", srv.Endpoint)
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
		return ui.Wrap(err, fmt.Sprintf("%s doesn't respond yet.", srv.Endpoint), check+", then "+strings.ToLower(undo[:1])+undo[1:])
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
	waitInstanceSees(ctx, app, inst, time.Minute)
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
		if err := checkJoinID(plan.preview, next); err != nil {
			return nil, err
		}
		app.UI.Info("%s is taken on %s. Trying %s.", saved.ID, plan.domain, next)
		if err := renameJoinedServer(ctx, app, e, inst, saved, next, plan); err != nil {
			return nil, err
		}
	}
}

func renameJoinedServer(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, saved *instance.VoiceServer, next string, plan *joinPlan) error {
	old := *saved
	vs, err := engineVoice(ctx, app, inst, e)
	if err != nil {
		return err
	}
	if err := app.UI.Step(ctx, "Taking voice server "+old.ID+" out", func(ctx context.Context, s *ui.Step) error { return vs.Remove(ctx, s, old.ID) }); err != nil {
		return err
	}
	unrouteVoiceServer(ctx, app, e, inst, &old)
	_ = os.RemoveAll(voice.ServerDir(inst.Dir, old.ID))
	saved.ID = next
	saved.Key = voice.Key(next)
	saved.Project = voice.NativeChild(next)
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
		return fmt.Sprintf("This server offers only IPv6. Callers with IPv6 connect to calls on it directly. Callers with only IPv4 can join through your TURN server when it hands out IPv6 relay addresses and an A record for the hostname points at a machine that forwards ports 80 and 443 here. Run rexctl voice test %s on the Fluxer machine to check.", id)
	}
	return "This server offers only IPv6. Callers with IPv6 connect to calls on it. Callers with only IPv4 can't, and your instance gives clients no TURN server."
}

func pinnedJoinError(err error, domain, id string) error {
	if code := voice.CodeOf(err); code == voice.CodeIDInUse || code == voice.CodeKeyInUse {
		return ui.Refused(fmt.Sprintf("%s already has a server called %s.", domain, id), fmt.Sprintf("On the Fluxer machine, run rexctl voice remove %s --forget, or rexctl voice token --id with another server ID for a new token.", id))
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

const podmanRestartsContainers = "5.8.2"

func olderPodman(version string) bool {
	return version != "" && versionBelow(version, podmanRestartsContainers)
}

func versionBelow(version, floor string) bool {
	parse := func(v string) []int {
		v = strings.TrimPrefix(strings.TrimSpace(v), "v")
		v, _, _ = strings.Cut(v, "-")
		var out []int
		for _, part := range strings.SplitN(v, ".", 3) {
			n, err := strconv.Atoi(part)
			if err != nil {
				return nil
			}
			out = append(out, n)
		}
		return out
	}
	a, b := parse(version), parse(floor)
	if a == nil || b == nil {
		return false
	}
	for i := range b {
		x := 0
		if i < len(a) {
			x = a[i]
		}
		if x != b[i] {
			return x < b[i]
		}
	}
	return false
}
