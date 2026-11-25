// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"cmp"
	"context"
	"errors"
	"fmt"
	"net/http"
	"os"
	"runtime"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

const defaultDrainTimeout = 15 * time.Minute

func newVoiceCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "voice",
		Short: "Run extra voice servers here or on other machines",
		Long: "Run standalone LiveKit voice servers next to Fluxer with rexctl voice add, " +
			"or on other machines with rexctl voice token and rexctl voice join.\n\n" +
			"To run without the bundled voice server, run rexctl config set voice.bundled false, " +
			"or install with rexctl install --no-bundled-voice. Calls then go only to the servers you add. " +
			"rexctl config set voice.bundled true brings the bundled server back.",
		Example: "  rexctl voice add\n" +
			"  rexctl voice status\n" +
			"  rexctl voice token --region fra",
		Args: noArgs,
	}
	c.AddCommand(
		newVoiceAddCmd(app),
		newVoiceJoinCmd(app),
		newVoiceTokenCmd(app),
		newVoiceListCmd(app),
		newVoiceStatusCmd(app),
		newVoiceTestCmd(app),
		newVoiceHostnameCmd(app),
		newVoiceRemoveCmd(app),
		newVoiceTurnCmd(app),
		newVoiceResyncCmd(app),
		newVoiceDrainCmd(app, true),
		newVoiceDrainCmd(app, false),
		newVoiceSetCmd(app),
		newVoiceLogsCmd(app),
		newVoiceRestartCmd(app),
	)
	return c
}

func newVoiceTokenCmd(app *App) *cobra.Command {
	var region, newRegion, regionName, id, hostname, internal, name, expires string
	var soft int
	var lat, long float64
	c := &cobra.Command{
		Use:   "token",
		Short: "Create a join token for a voice server on another machine",
		Long:  "Create a one-time join token and print the command that installs a voice server with it.",
		Args:  noArgs,
	}
	c.Flags().StringVar(&region, "region", "", "existing region for the server")
	c.Flags().StringVar(&newRegion, "new-region", "", "create a region with this ID when the server joins")
	c.Flags().StringVar(&regionName, "region-name", "", "name of the new region")
	c.Flags().Float64Var(&lat, "lat", 0, "latitude of the new region")
	c.Flags().Float64Var(&long, "long", 0, "longitude of the new region")
	c.Flags().StringVar(&id, "id", "", "server ID")
	c.Flags().StringVar(&hostname, "hostname", "", "hostname the server must use")
	c.Flags().StringVar(&internal, "internal-endpoint", "", "URL the API uses to connect to the server, such as http://10.0.0.5:7880")
	c.Flags().StringVar(&name, "name", "", "a note for the server")
	c.Flags().IntVar(&soft, "soft-limit", 0, "connections above which placement prefers another server")
	c.Flags().StringVar(&expires, "expires", "1h", "how long the token works, at most 24h")
	c.RunE = func(c *cobra.Command, _ []string) error {
		ttl, err := parseExpiry(expires)
		if err != nil || ttl <= 0 || ttl < time.Minute || ttl > 24*time.Hour {
			return ui.Usage("Pass --expires from 1m to 24h.")
		}
		if newRegion != "" && (regionName == "" || region != "") {
			return ui.Usage("Pass --new-region with --region-name, and without --region.")
		}
		coords := c.Flags().Changed("lat")
		if coords != c.Flags().Changed("long") {
			return ui.Usage("Pass --lat and --long together.")
		}
		if coords && newRegion == "" {
			return ui.Usage("Pass --lat and --long only with --new-region.")
		}
		if lat < -90 || lat > 90 || long < -180 || long > 180 {
			return ui.Usage("Pass --lat from -90 to 90 and --long from -180 to 180.")
		}
		if err := checkServerID(id); err != nil {
			return err
		}
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, inst *instance.Instance) error {
			seconds := int(ttl / time.Second)
			req := adminapi.CreateVoiceJoinTokenRequest{ExpiresInSeconds: &seconds}
			set := func(v string) *string {
				if v == "" {
					return nil
				}
				return &v
			}
			req.RegionId, req.ServerId, req.Hostname, req.Notes = set(region), set(id), set(hostname), set(name)
			req.InternalEndpoint = set(internal)
			if newRegion != "" {
				emoji := "🌍"
				req.RegionId, req.RegionName, req.RegionEmoji = &newRegion, &regionName, &emoji
				if coords {
					la, lo := float32(lat), float32(long)
					req.RegionLatitude, req.RegionLongitude = &la, &lo
				}
			}
			if soft > 0 {
				req.SoftConnectionLimit = &soft
			}
			created, err := api.CreateVoiceJoinToken(ctx, req)
			if err != nil {
				return err
			}
			app.UI.Info("Run this on the voice machine:")
			app.UI.Info("")
			app.UI.Highlight("  " + created.Command)
			app.UI.Info("")
			app.UI.Info("The token works once and expires %s.", expiryWhen(created.ExpiresAt))
			app.UI.Info("A voice server can record the calls placed on it. Give the token only to a machine you trust.")
			return nil
		})
	}

	list := &cobra.Command{Use: "list", Short: "List join tokens", Args: noArgs}
	app.jsonFlag(list)
	list.RunE = func(c *cobra.Command, _ []string) error {
		return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			tokens, err := api.VoiceJoinTokens(ctx)
			if err != nil {
				return err
			}
			if app.Flags.JSON {
				return app.UI.JSON(tokens)
			}
			if len(tokens) == 0 {
				app.UI.Info("There are no join tokens from the last 7 days. Run rexctl voice token to make one.")
				return nil
			}
			rows := make([][]string, 0, len(tokens))
			for _, t := range tokens {
				region := "automatic"
				if t.RegionId != nil {
					region = *t.RegionId
				}
				server := ""
				if t.UsedServerId != nil {
					server = *t.UsedServerId
				} else if t.ServerId != nil {
					server = *t.ServerId
				}
				rows = append(rows, []string{t.TokenId, string(t.Status), region, server, expiryText(&t.ExpiresAt)})
			}
			app.UI.Table([]string{"ID", "Status", "Region", "Server", "Expires"}, rows)
			return nil
		})
	}

	revoke := &cobra.Command{Use: "revoke <token id>", Short: "Stop a join token from working", Args: exactArgs("the token ID")}
	revoke.RunE = func(c *cobra.Command, args []string) error {
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			if err := api.RevokeVoiceJoinToken(ctx, args[0]); err != nil {
				return err
			}
			app.UI.OK("The join token %s doesn't work anymore.", args[0])
			return nil
		})
	}
	c.AddCommand(list, revoke)
	return c
}

func newVoiceListCmd(app *App) *cobra.Command {
	c := &cobra.Command{Use: "list", Short: "List voice servers", Args: noArgs}
	app.jsonFlag(c)
	c.RunE = func(c *cobra.Command, _ []string) error {
		inst, err := app.Instance()
		if err != nil {
			return err
		}
		if inst.State.IsVoice() {
			return listVoiceMachine(c.Context(), app, inst)
		}
		return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, inst *instance.Instance) error {
			regions, err := api.VoiceRegions(ctx)
			if err != nil {
				return err
			}
			if app.Flags.JSON {
				return app.UI.JSON(regions)
			}
			rows, servers := voiceListRows(regions, inst)
			if servers == 0 {
				app.UI.Info("Your instance has no voice servers.")
			}
			if len(rows) > 0 {
				app.UI.Table([]string{"ID", "Region", "Health", "Endpoint", "Runs on"}, rows)
			}
			return nil
		})
	}
	return c
}

func voiceListRows(regions []adminapi.VoiceRegion, inst *instance.Instance) ([][]string, int) {
	var rows [][]string
	servers := 0
	for _, r := range regions {
		if len(r.Servers) == 0 {
			rows = append(rows, []string{"-", r.Id, "no servers", "", ""})
		}
		for _, s := range r.Servers {
			servers++
			here := ""
			if inst.State.VoiceServer(s.ServerId) != nil {
				here = "this machine"
			}
			state := string(s.Health.Status)
			if !s.IsActive {
				state = "off"
			} else if s.Draining {
				state += ", draining"
			}
			if s.DuplicateApiKey {
				state += ", shared key"
			}
			rows = append(rows, []string{s.ServerId, r.Id, state, wsEndpoint(s.Endpoint), here})
		}
	}
	return rows, servers
}

func wsEndpoint(endpoint string) string {
	if rest, ok := strings.CutPrefix(endpoint, "https://"); ok {
		return "wss://" + rest
	}
	if rest, ok := strings.CutPrefix(endpoint, "http://"); ok {
		return "ws://" + rest
	}
	return endpoint
}

type voiceMachineRow struct {
	ID       string `json:"id"`
	Instance string `json:"instance"`
	Region   string `json:"region"`
	Endpoint string `json:"endpoint"`
	Health   string `json:"health"`
	up       bool
	draining bool
}

func voiceMachineRows(ctx context.Context, inst *instance.Instance) []voiceMachineRow {
	client := &voice.Client{HTTP: &http.Client{Timeout: 10 * time.Second}}
	out := []voiceMachineRow{}
	for _, s := range inst.State.Voice {
		r := voiceMachineRow{ID: s.ID, Instance: s.Instance, Region: s.Region, Endpoint: s.Endpoint, Health: "unknown"}
		if node, err := client.GetNode(ctx, s.NodeURL, s.Key, s.Secret); err == nil {
			r.Health, r.up, r.draining = node.Health.Status, node.IsActive && node.Health.Status == "up", node.Draining
			if node.Draining {
				r.Health += ", draining"
			}
			if !node.IsActive {
				r.Health = "off"
			}
		} else if code := voice.CodeOf(err); code != "" {
			r.Health = strings.ToLower(code)
		}
		out = append(out, r)
	}
	return out
}

func listVoiceMachine(ctx context.Context, app *App, inst *instance.Instance) error {
	out := voiceMachineRows(ctx, inst)
	if app.Flags.JSON {
		return app.UI.JSON(out)
	}
	if len(out) == 0 {
		app.UI.Info("This machine runs no voice servers. Run rexctl voice join to add one.")
		return nil
	}
	rows := make([][]string, len(out))
	for i, r := range out {
		rows[i] = []string{r.ID, r.Instance, r.Region, r.Health, r.Endpoint}
	}
	app.UI.Table([]string{"ID", "Instance", "Region", "Health", "Endpoint"}, rows)
	return nil
}

func voiceServersHere(inst *instance.Instance, args []string) (*instance.Instance, []*instance.VoiceServer, error) {
	bundled := deploy.BundledVoiceServer(inst)
	if bundled != nil && len(args) == 1 && args[0] == bundled.ID {
		return inst, []*instance.VoiceServer{bundled}, nil
	}
	owner, servers, err := localVoiceServers(inst, args)
	if err == nil && len(args) == 0 && bundled != nil {
		servers = append([]*instance.VoiceServer{bundled}, servers...)
	}
	return owner, servers, err
}

func isBundled(inst *instance.Instance, srv *instance.VoiceServer) bool {
	return srv.ID == deploy.BundledVoiceServerID(inst) && inst.State.VoiceServer(srv.ID) == nil
}

func localVoiceServers(inst *instance.Instance, args []string) (*instance.Instance, []*instance.VoiceServer, error) {
	if len(args) == 0 {
		out := make([]*instance.VoiceServer, 0, len(inst.State.Voice))
		for i := range inst.State.Voice {
			out = append(out, &inst.State.Voice[i])
		}
		return inst, out, nil
	}
	owner, srv := findVoiceServer(inst, args[0])
	if srv == nil {
		return inst, nil, ui.Refused(fmt.Sprintf("This machine doesn't run a voice server called %s.", args[0]), "Run rexctl voice list to see the servers.")
	}
	return owner, []*instance.VoiceServer{srv}, nil
}

func findVoiceServer(inst *instance.Instance, id string) (*instance.Instance, *instance.VoiceServer) {
	if srv := inst.State.VoiceServer(id); srv != nil {
		return inst, srv
	}
	for _, dir := range instance.ReadRecord(instance.OwnScope()).Instances {
		if dir == inst.Dir {
			continue
		}
		other, err := instance.Find(dir)
		if err != nil || !other.State.IsVoice() {
			continue
		}
		if srv := other.State.VoiceServer(id); srv != nil {
			return other, srv
		}
	}
	return inst, nil
}

func newVoiceStatusCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:     "status [id]",
		Short:   "Show the voice servers on this machine",
		Example: "  rexctl voice status\n  rexctl voice status fra-server-2",
		Args:    maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			inst, servers, err := voiceServersHere(inst, args)
			if err != nil {
				return err
			}
			if len(servers) == 0 {
				if app.Flags.JSON {
					return app.UI.JSON([]voiceStatus{})
				}
				app.UI.Info("This machine runs no voice servers.")
				return nil
			}
			r, err := app.Runtime(ctx, inst)
			if err != nil {
				return err
			}
			out := []voiceStatus{}
			for _, srv := range servers {
				out = append(out, readVoiceStatus(ctx, r, inst, srv))
			}
			markDraining(ctx, app, inst, out)
			if app.Flags.JSON {
				return app.UI.JSON(out)
			}
			for _, v := range out {
				showVoiceStatus(app, v)
			}
			return nil
		},
	}
	app.jsonFlag(c)
	return c
}

func markDraining(ctx context.Context, app *App, inst *instance.Instance, out []voiceStatus) {
	if inst.State.IsVoice() {
		return
	}
	api, _, err := app.Admin(ctx)
	if err != nil {
		return
	}
	regions, err := api.VoiceRegions(ctx)
	if err != nil {
		return
	}
	for _, r := range regions {
		for _, s := range r.Servers {
			for i := range out {
				if out[i].ID == s.ServerId && s.Draining {
					out[i].State += ", draining"
				}
			}
		}
	}
}

type voiceStatus struct {
	ID          string `json:"id"`
	Bundled     bool   `json:"bundled"`
	Region      string `json:"region"`
	Endpoint    string `json:"endpoint"`
	Reach       string `json:"reach"`
	State       string `json:"state"`
	TCPPort     int    `json:"tcp_port"`
	UDPPort     int    `json:"udp_port"`
	Addresses   string `json:"addresses"`
	Certificate string `json:"certificate"`
	Calls       string `json:"calls"`
	LiveKit     string `json:"livekit"`
	Webhook     string `json:"webhook"`
	Instance    string `json:"instance,omitempty"`
	Browsers    string `json:"browsers,omitempty"`
}

func voiceServerState(ctx context.Context, r rt.Runtime, inst *instance.Instance, srv *instance.VoiceServer) (string, func(int) []string) {
	if !isBundled(inst, srv) {
		vs := rt.VoiceOf(r)
		return vs.State(ctx, srv), func(tail int) []string { return voiceLogLines(ctx, vs, srv, tail) }
	}
	state := "unknown"
	if st, err := r.Status(ctx); err == nil {
		state = "not running"
		for _, p := range st.Parts {
			if p.Name == "voice" {
				state = string(p.State)
				if p.Health != "" {
					state += ", " + p.Health
				}
			}
		}
	}
	return state, func(tail int) []string {
		var buf bytes.Buffer
		if err := r.Logs(ctx, &buf, rt.LogOptions{Parts: []string{"voice"}, Tail: tail}); err != nil {
			return nil
		}
		return voice.LogLines(&buf, func(string) bool { return true })
	}
}

func readVoiceStatus(ctx context.Context, r rt.Runtime, inst *instance.Instance, srv *instance.VoiceServer) voiceStatus {
	state, logs := voiceServerState(ctx, r, inst, srv)
	base, dial := deploy.VoiceProbeTarget(inst, srv)
	v := voiceStatus{ID: srv.ID, Bundled: isBundled(inst, srv), Region: srv.Region, Endpoint: srv.Endpoint, Reach: "responds", State: state,
		TCPPort: srv.TCPPort, UDPPort: srv.UDPPort, Addresses: addressesText(srv), Certificate: "-", Calls: "unknown",
		LiveKit: cmp.Or(srv.Image, "the release's LiveKit"), Webhook: srv.Webhook + ", " + webhookState(logs), Instance: srv.Instance}
	if err := voice.ProbeThroughEdge(ctx, base, dial, true); err != nil {
		v.Reach = err.Error()
	}
	secure := strings.HasPrefix(srv.Endpoint, "wss://")
	ownTLS := srv.Edge == voice.EdgeOwn || srv.Edge == voice.EdgeInstance && cmp.Or(inst.State.Settings[settings.EdgeMode], settings.EdgeBundled) == settings.EdgeBundled
	switch {
	case !secure:
		v.Certificate = "none, the endpoint uses plain HTTP"
	case !ownTLS:
		v.Certificate = "your proxy's"
	default:
		if exp, err := voice.CertificateExpiry(ctx, srv.Endpoint, dial); err != nil {
			v.Certificate = "not valid (" + err.Error() + ")"
		} else {
			v.Certificate = "valid until " + exp.Local().Format("2 January at 15:04 MST")
		}
	}
	if l, err := voice.ListRooms(ctx, base, dial, srv.Key, srv.Secret, true); err == nil {
		v.Calls = fmt.Sprintf("%s, %s", plural(l.Rooms, "room", "rooms"), plural(l.Participants, "participant", "participants"))
	}
	if srv.Joined && srv.Instance != "" {
		v.Browsers = browsersText(ctx, srv)
	}
	return v
}

func showVoiceStatus(app *App, v voiceStatus) {
	u := app.UI
	if v.Bundled {
		u.Info("Voice server %s, bundled with Fluxer", v.ID)
	} else {
		u.Info("Voice server %s", v.ID)
	}
	rows := [][]string{
		{"Region", v.Region},
		{"Endpoint", v.Endpoint + ", " + v.Reach},
		{"State", v.State},
		{"Ports", fmt.Sprintf("%d/tcp, %d/udp", v.TCPPort, v.UDPPort)},
		{"Addresses", v.Addresses},
		{"Certificate", v.Certificate},
		{"Calls", v.Calls},
		{"LiveKit", v.LiveKit},
		{"Webhook", v.Webhook},
	}
	if v.Instance != "" {
		rows = append(rows, []string{"Instance", v.Instance})
	}
	if v.Browsers != "" {
		rows = append(rows, []string{"Browsers", v.Browsers})
	}
	for _, r := range rows {
		u.Info("  %-15s %s", r[0], r[1])
	}
	u.Info("")
}

func browsersText(ctx context.Context, srv *instance.VoiceServer) string {
	client := &voice.Client{HTTP: &http.Client{Timeout: 10 * time.Second}}
	switch voiceOriginState(ctx, client, srv.Instance, endpointOrigin(srv.Endpoint)) {
	case originAllowed:
		return "can connect from " + srv.Instance
	case originUnlisted:
		return srv.Instance + " runs a release without the list of voice servers"
	case originPending:
		return "can't connect from " + srv.Instance + " yet"
	}
	return "unknown, rexctl can't read " + srv.Instance
}

func webhookState(logs func(int) []string) string {
	failed, sent := voiceWebhookLog(logs(2000))
	switch {
	case failed != "":
		return "failing (" + failed + ")"
	case sent:
		return "delivered"
	}
	return "pending until a call runs"
}

func newVoiceTestCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:   "test [id]",
		Short: "Test that calls can connect to a voice server over UDP and TCP",
		Long: "Join a throwaway room on a voice server and report which transports connect. " +
			"On the Fluxer machine it can test any server of the instance, including servers on other machines.",
		Example: "  rexctl voice test\n  rexctl voice test ams-server-1",
		Args:    maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			owner, servers, err := voiceServersHere(inst, args)
			if err != nil && len(args) == 1 && !inst.State.IsVoice() {
				return testRemoteVoiceServer(c, app, args[0])
			}
			if err != nil {
				return err
			}
			if len(servers) == 0 {
				return ui.Refused("This machine runs no voice servers.", "Pass the ID of a server on another machine.")
			}
			ok := true
			for _, srv := range servers {
				app.UI.Info("Testing %s at %s", srv.ID, srv.Endpoint)
				results := runTransportTests(ctx, srv.Endpoint, serverPaths(srv), func() (string, error) {
					return voice.TestToken(srv.Key, srv.Secret, voice.TestRoom(), time.Now()), nil
				}, !owner.State.IsVoice())
				ok = reportTransports(app, results, true) && ok
			}
			if !inst.State.IsVoice() {
				app.UI.Info("Servers on other machines can be tested from here with rexctl voice test <id>.")
			}
			if !ok {
				app.SetExit(ui.ExitFailed)
			}
			return nil
		},
	}
}

func testRemoteVoiceServer(c *cobra.Command, app *App, id string) error {
	return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, inst *instance.Instance) error {
		regions, err := api.VoiceRegions(ctx)
		if err != nil {
			return err
		}
		region := ""
		server := voice.FamilyIPv4
		for _, r := range regions {
			for _, s := range r.Servers {
				if s.ServerId == id {
					region = r.Id
					if f := voice.FamilyOf(stringOf(s.PublicIpv4), stringOf(s.PublicIpv6)); f != voice.FamilyNone {
						server = f
					}
				}
			}
		}
		if region == "" {
			return ui.Refused(fmt.Sprintf("Your instance has no voice server called %s.", id), "Run rexctl voice list to see the servers.")
		}
		tester := voice.FamilyOf(voice.DefaultRouteIPv4(), globalOnly(voice.DefaultRouteIPv6()))
		paths := withoutUnsetTurn(app, inst, id, remotePaths(server, tester))
		for _, fam := range []string{voice.FamilyIPv4, voice.FamilyIPv6} {
			if (server == fam || server == voice.FamilyDual) && tester != fam && tester != voice.FamilyDual {
				app.UI.Info("This machine has no %s, and rexctl can't test %s over %s from here.", familyName(fam), id, familyName(fam))
			}
		}
		if len(paths) == 0 {
			return ui.Refused(fmt.Sprintf("This machine and %s have no address family in common.", id), "Run the test on a machine with "+familyName(server)+".")
		}
		var endpoint string
		var results []transportResult
		for _, p := range paths {
			tok, err := api.VoiceTestToken(ctx, region, id)
			if err == nil {
				endpoint = tok.Endpoint
				err = voice.TestTransport(ctx, tok.Endpoint, tok.Token, p, false, 20*time.Second)
			}
			results = append(results, transportResult{Path: p, Err: err})
		}
		app.UI.Info("Tested %s at %s from this machine.", id, ui.Inline(endpoint))
		if !reportTransports(app, results, false) {
			switch {
			case slices.ContainsFunc(results, func(r transportResult) bool { return errors.Is(r.Err, voice.ErrTokenRejected) }):
				app.UI.Info("%s rejected a token signed with the API secret your instance has for it. Run rexctl voice resync %s on the machine that runs it.", id, id)
			case slices.ContainsFunc(results, func(r transportResult) bool { return r.Path.Relay && errors.Is(r.Err, voice.ErrSignalling) }):
				app.UI.Info("rexctl couldn't open the signalling connection to %s over IPv4. Callers with only IPv4 need an A record for its hostname at a machine that forwards ports 80 and 443 to it.", id)
			case slices.ContainsFunc(results, func(r transportResult) bool {
				return r.Path.Relay && r.Err != nil && !errors.Is(r.Err, voice.ErrNoTurn)
			}):
				app.UI.Info("Media through your TURN server didn't connect. IPv4 through TURN needs a TURN server that hands out IPv6 relay addresses.")
			default:
				app.UI.Info("Open the server's TCP and UDP ports in the firewall of its provider and on the machine itself.")
			}
			app.SetExit(ui.ExitFailed)
		}
		return nil
	})
}

func remotePaths(server, tester string) []voice.MediaPath {
	paths := voice.MediaPaths(server, tester)
	if server == voice.FamilyIPv6 && (tester == voice.FamilyIPv4 || tester == voice.FamilyDual) {
		paths = append(paths, voice.MediaPath{Transport: voice.TransportUDP, Family: voice.FamilyIPv4, Relay: true})
	}
	return paths
}

func withoutUnsetTurn(app *App, inst *instance.Instance, id string, paths []voice.MediaPath) []voice.MediaPath {
	relay := func(p voice.MediaPath) bool { return p.Relay }
	if inst == nil || inst.State == nil || inst.State.IsVoice() || !slices.ContainsFunc(paths, relay) {
		return paths
	}
	if list, err := settings.ParseTurnServers(inst.Setting(settings.TurnServersVar)); err != nil || len(list) > 0 {
		return paths
	}
	app.UI.Info("%s offers only IPv6, and your instance gives clients no TURN server. Callers with only IPv4 can't join calls on it, and rexctl doesn't test IPv4 through TURN.", id)
	return slices.DeleteFunc(paths, relay)
}

func familyName(f string) string {
	switch f {
	case voice.FamilyIPv6:
		return "IPv6"
	case voice.FamilyDual:
		return "IPv4 and IPv6"
	}
	return "IPv4"
}

func globalOnly(ip string) string {
	if voice.GlobalIPv6(ip) {
		return ip
	}
	return ""
}

func newVoiceHostnameCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:     "hostname <id> <host>",
		Short:   "Move a voice server to a new hostname",
		Long:    "Move a voice server on this machine to a hostname once its DNS record points here. rexctl gets a certificate and updates the server's endpoint on the instance.",
		Example: "  rexctl voice hostname ams-server-1 voice.example.com",
		Args:    exactArgs("the server ID", "the hostname"),
		RunE: func(c *cobra.Command, args []string) error {
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			if err := managed(inst); err != nil {
				return err
			}
			inst, srv := findVoiceServer(inst, args[0])
			if srv != nil && srv.Edge == voice.EdgeInstance {
				return ui.Refused(fmt.Sprintf("%s uses the domain of the Fluxer instance on this machine.", srv.ID), "rexctl voice hostname applies only to a server with its own edge.")
			}
			if srv == nil || srv.Edge != voice.EdgeOwn {
				return ui.Refused(fmt.Sprintf("%s isn't a voice server with its own edge on this machine.", args[0]), "Run rexctl voice list to see the servers.")
			}
			name := strings.ToLower(strings.TrimSuffix(strings.TrimSpace(args[1]), "."))
			check := func(h string) voice.HostnameCheck {
				answer, err := voice.PublicLookup(&http.Client{Timeout: 5 * time.Second})(ctx, h)
				if err != nil {
					return voice.HostnameMissing
				}
				return voice.CheckHostname(answer, srv.NodeIP, srv.NodeIPv6, voice.InterfaceAddresses())
			}
			if fluxerdVoice(inst) && slices.ContainsFunc(inst.State.Voice, func(o instance.VoiceServer) bool { return o.ID != srv.ID && o.Edge == voice.EdgeOwn }) {
				return ui.Refused("Every voice server on this machine uses one address, so one server can't move on its own.", "Remove the other servers with rexctl voice remove first, or keep the hostname.")
			}
			if !waitForHostname(ctx, app, name, voice.Reachability{PublicIPv4: srv.NodeIP, PublicIPv6: srv.NodeIPv6}, check, false, true) {
				return ui.Refused(fmt.Sprintf("%s doesn't point at this machine yet.", name), "Fix the DNS record, then run the same command again.")
			}
			e, err := voiceMachineEngine(ctx, app, inst)
			if err != nil {
				return err
			}
			return app.Locked(ctx, inst, func() error {
				old := srv.Endpoint
				srv.Endpoint, srv.Hostname = voice.Endpoint(name, srv.ID), name
				if err := rerouteVoiceServer(ctx, app, e, inst, srv); err != nil {
					return err
				}
				if err := waitForEndpoint(ctx, voice.HTTPBase(srv.Endpoint), "127.0.0.1:443", false, 3*time.Minute); err != nil {
					srv.Endpoint = old
					_ = rerouteVoiceServer(ctx, app, e, inst, srv)
					return ui.Wrap(err, fmt.Sprintf("%s didn't get a certificate.", name), "Check that ports 80 and 443 are open, then try again.")
				}
				client := &voice.Client{HTTP: &http.Client{Timeout: 30 * time.Second}}
				if _, err := client.UpdateNode(ctx, srv.NodeURL, srv.Key, srv.Secret, voice.NodeUpdate{Endpoint: &srv.Endpoint}); err != nil {
					srv.Endpoint = old
					_ = rerouteVoiceServer(ctx, app, e, inst, srv)
					return joinError(err, srv.Instance, srv.ID)
				}
				if err := saveVoiceState(inst); err != nil {
					return err
				}
				app.UI.OK("%s now uses %s.", srv.ID, srv.Endpoint)
				waitForVoiceOrigin(ctx, app, client, srv.Instance, srv.Endpoint)
				return nil
			})
		},
	}
}

func newVoiceRemoveCmd(app *App) *cobra.Command {
	var all, now, forget bool
	var drain time.Duration
	c := &cobra.Command{
		Use:   "remove [id]",
		Short: "Drain, unregister and delete a voice server",
		Args:  maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			if all == (len(args) == 1) {
				return ui.Usage("Pass the ID of the server, or --all.")
			}
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			if len(args) == 1 && args[0] == deploy.BundledVoiceServerID(inst) && inst.State.VoiceServer(args[0]) == nil {
				return ui.Refused(fmt.Sprintf("%s is the voice server bundled with Fluxer.", args[0]), "Run rexctl config set voice.bundled false to run without it.")
			}
			if all && !inst.State.IsVoice() {
				return ui.Refused("--all works on a voice machine.", "Pass the ID of the server to remove.")
			}
			if forget {
				return forgetVoiceServer(c, app, inst, args)
			}
			owner, servers, err := localVoiceServers(inst, args)
			if err != nil && len(args) == 1 && !inst.State.IsVoice() && instanceHasVoiceServer(c, app, args[0]) {
				return ui.Refused(fmt.Sprintf("%s runs on another machine.", args[0]), fmt.Sprintf("Run rexctl voice remove %s on the machine that runs it, or pass --forget if that machine is gone.", args[0]))
			}
			if err != nil {
				return err
			}
			inst = owner
			if !now && drain <= 0 {
				drain = defaultDrainTimeout
			}
			if now {
				drain = 0
			}
			return removeVoiceServers(c.Context(), app, inst, servers, drain, all)
		},
	}
	c.Flags().BoolVar(&all, "all", false, "remove every voice server and the edge on this machine")
	c.Flags().BoolVar(&now, "now", false, "don't wait for calls to end")
	c.Flags().DurationVar(&drain, "drain-timeout", defaultDrainTimeout, "how long to wait for calls to end")
	c.Flags().BoolVar(&forget, "forget", false, "only remove a server on another machine from the instance, when that machine is gone")
	c.MarkFlagsMutuallyExclusive("forget", "all")
	c.Example = "  rexctl voice remove fra-server-2\n  rexctl voice remove ams-server-1 --forget\n  rexctl voice remove --all"
	return c
}

func forgetVoiceServer(c *cobra.Command, app *App, inst *instance.Instance, args []string) error {
	if inst.State.IsVoice() {
		return ui.Refused("--forget works on the Fluxer machine.", "Run rexctl voice remove "+args[0]+" here to remove the server from this machine.")
	}
	id := args[0]
	if inst.State.VoiceServer(id) != nil {
		return ui.Refused(fmt.Sprintf("%s runs on this machine.", id), "Run rexctl voice remove "+id+" without --forget.")
	}
	if id == deploy.BundledVoiceServerID(inst) {
		return ui.Refused(fmt.Sprintf("%s is the voice server bundled with Fluxer.", id), "Run rexctl config set voice.bundled false to run without it.")
	}
	if err := confirmVoice(app, "Remove "+id+" from your instance? Calls on it end."); err != nil {
		return err
	}
	return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
		region, err := voiceRegionOf(ctx, api, id)
		if err != nil {
			return err
		}
		if err := api.DeleteVoiceServer(ctx, region, id); err != nil && !adminapi.IsStatus(err, http.StatusNotFound) {
			return err
		}
		app.UI.OK("Removed %s from your instance. If its machine comes back, run rexctl voice remove %s there.", id, id)
		return nil
	})
}

func instanceHasVoiceServer(c *cobra.Command, app *App, id string) bool {
	found := false
	_ = adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
		regions, err := api.VoiceRegions(ctx)
		if err != nil {
			return err
		}
		for _, r := range regions {
			found = found || slices.ContainsFunc(r.Servers, func(s adminapi.VoiceServerAdminResponse) bool { return s.ServerId == id })
		}
		return nil
	})
	return found
}

func removeVoiceServers(ctx context.Context, app *App, inst *instance.Instance, servers []*instance.VoiceServer, drain time.Duration, all bool) error {
	if err := managed(inst); err != nil {
		return err
	}
	ids := make([]string, len(servers))
	for i, s := range servers {
		ids[i] = s.ID
	}
	vs, err := voiceRuntime(ctx, app, inst)
	if err != nil {
		return err
	}
	if err := vs.CanChange(); err != nil {
		return err
	}
	if err := confirmVoice(app, "Remove "+ui.JoinAnd(ids)+"?"); err != nil {
		return err
	}
	var e *engine.Engine
	var api *adminapi.Client
	if inst.State.IsVoice() {
		if e, err = voiceMachineEngine(ctx, app, inst); err != nil {
			return err
		}
	} else if api, _, err = app.Admin(ctx); err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		for _, id := range ids {
			srv := inst.State.VoiceServer(id)
			if srv == nil {
				continue
			}
			if err := removeOneVoiceServer(ctx, app, vs, e, api, inst, srv, drain); err != nil {
				return err
			}
		}
		if inst.State.IsVoice() && len(inst.State.Voice) == 0 {
			return removeVoiceMachine(ctx, app, e, inst)
		}
		return nil
	})
}

func setVoiceDraining(ctx context.Context, api *adminapi.Client, inst *instance.Instance, srv *instance.VoiceServer, draining bool) error {
	if inst.State.IsVoice() {
		client := &voice.Client{HTTP: &http.Client{Timeout: 30 * time.Second}}
		_, err := client.UpdateNode(ctx, srv.NodeURL, srv.Key, srv.Secret, voice.NodeUpdate{Draining: &draining})
		return err
	}
	_, err := api.UpdateVoiceServer(ctx, srv.Region, srv.ID, adminapi.UpdateVoiceServerRequestBody{Draining: &draining})
	return err
}

func waitForDrain(ctx context.Context, app *App, inst *instance.Instance, srv *instance.VoiceServer, limit time.Duration) {
	if limit <= 0 {
		return
	}
	base, dial := deploy.VoiceProbeTarget(inst, srv)
	_ = app.UI.Step(ctx, "Waiting for calls on "+srv.ID+" to end", func(ctx context.Context, s *ui.Step) error {
		deadline := time.Now().Add(limit)
		for {
			load, err := voice.ListRooms(ctx, base, dial, srv.Key, srv.Secret, true)
			if err == nil && load.Participants == 0 {
				s.Done("No calls left on %s.", srv.ID)
				return nil
			}
			if time.Now().After(deadline) {
				if err != nil {
					s.Done("rexctl couldn't read the calls on %s (%v), and the wait is over.", srv.ID, err)
				} else {
					s.Done("The wait is over with %s still on %s.", plural(load.Participants, "participant", "participants"), srv.ID)
				}
				return nil
			}
			if err != nil {
				s.Title(fmt.Sprintf("Waiting for calls on %s to end. rexctl can't read them right now", srv.ID))
			} else {
				s.Title(fmt.Sprintf("Waiting for %s on %s to leave", plural(load.Participants, "participant", "participants"), srv.ID))
			}
			select {
			case <-ctx.Done():
				return ctx.Err()
			case <-time.After(5 * time.Second):
			}
		}
	})
}

func waitInstanceSees(ctx context.Context, app *App, inst *instance.Instance, limit time.Duration) {
	if !inst.State.IsVoice() || len(inst.State.Voice) == 0 {
		return
	}
	client := &voice.Client{HTTP: &http.Client{Timeout: 10 * time.Second}}
	who := voiceInstances(inst)
	_ = app.UI.Step(ctx, "Waiting for "+who+" to see the voice servers up", func(ctx context.Context, s *ui.Step) error {
		deadline := time.Now().Add(limit)
		for {
			var down []string
			for _, srv := range inst.State.Voice {
				node, err := client.GetNode(ctx, srv.NodeURL, srv.Key, srv.Secret)
				if err != nil || (node.IsActive && node.Health.Status != "up") {
					down = append(down, srv.ID)
				}
			}
			if len(down) == 0 {
				s.Done("%s sees the voice servers up.", who)
				return nil
			}
			if time.Now().After(deadline) {
				app.UI.Warn(fmt.Sprintf("%s doesn't see %s up yet.", who, ui.JoinAnd(down)), "Run rexctl voice test "+down[0]+" on the Fluxer machine to see why.")
				return nil
			}
			select {
			case <-ctx.Done():
				return ctx.Err()
			case <-time.After(3 * time.Second):
			}
		}
	})
}

func unregisterVoiceServer(ctx context.Context, app *App, api *adminapi.Client, inst *instance.Instance, srv *instance.VoiceServer, drain time.Duration) error {
	if inst.State.IsVoice() && srv.NodeURL == "" {
		app.UI.Info("%s never joined %s. rexctl removes it here.", srv.ID, cmp.Or(srv.Instance, "the instance"))
		return nil
	}
	gone := false
	if err := setVoiceDraining(ctx, api, inst, srv, true); err != nil {
		switch {
		case voice.CodeOf(err) == voice.CodeNodeUnknown, adminapi.IsStatus(err, http.StatusNotFound):
			gone = true
			app.UI.Info("%s is already gone from the instance. rexctl removes it here.", srv.ID)
		case voice.CodeOf(err) == voice.CodeTokenExpired:
			return ui.Refused(voice.ClockProblem(err, srv.Instance, time.Now())+".", voice.ClockFix)
		case voice.CodeOf(err) == voice.CodeSignatureInvalid:
			return ui.Refused(fmt.Sprintf("The instance has another secret for %s.", srv.ID), fmt.Sprintf("Run rexctl voice resync %s%s, then run this again.", srv.ID, voiceDirFlag(inst.Dir)))
		default:
			app.UI.Warn(fmt.Sprintf("Couldn't mark %s as draining (%v).", srv.ID, err), "")
		}
	}
	if !gone {
		waitForDrain(ctx, app, inst, srv, drain)
		var err error
		if inst.State.IsVoice() {
			client := &voice.Client{HTTP: &http.Client{Timeout: 30 * time.Second}}
			err = client.DeleteNode(ctx, srv.NodeURL, srv.Key, srv.Secret)
			if voice.CodeOf(err) == voice.CodeNodeUnknown {
				err = nil
			}
		} else {
			err = api.DeleteVoiceServer(ctx, srv.Region, srv.ID)
			if adminapi.IsStatus(err, http.StatusNotFound) {
				err = nil
			}
		}
		if err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't remove %s from the instance.", srv.ID), "Run the same command again.")
		}
	}
	return nil
}

func removeOneVoiceServer(ctx context.Context, app *App, vs rt.VoiceServers, e *engine.Engine, api *adminapi.Client, inst *instance.Instance, srv *instance.VoiceServer, drain time.Duration) error {
	if err := unregisterVoiceServer(ctx, app, api, inst, srv, drain); err != nil {
		return err
	}
	err := app.UI.Step(ctx, "Removing voice server "+srv.ID, func(ctx context.Context, s *ui.Step) error {
		removed := *srv
		if err := vs.Remove(ctx, s, srv.ID); err != nil {
			return err
		}
		inst.State.RemoveVoiceServer(removed.ID)
		if inst.State.IsVoice() {
			unrouteVoiceServer(ctx, app, e, inst, &removed)
		}
		closeVoiceFirewall(ctx, app, removed.Opened)
		if err := saveVoiceState(inst); err != nil {
			return err
		}
		s.Done("Removed voice server %s.", removed.ID)
		return nil
	})
	return err
}

func removeVoiceMachine(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance) error {
	done := "Removed every voice server in " + inst.Dir + "."
	if fluxerdVoice(inst) {
		done = "Removed every voice server and the voice service from this machine."
		if err := removeVoiceHost(ctx, inst, true); err != nil {
			return ui.Wrap(err, "Couldn't remove the voice service.", "Run the same command again.")
		}
		if edge := inst.State.VoiceEdge; edge != nil {
			closeVoiceFirewall(ctx, app, edge.Opened)
		}
	} else if edge := inst.State.VoiceEdge; edge != nil && edge.Instance == "" {
		done = "Removed every voice server and the edge from this machine."
		if err := rt.EngineError("", e.Named(voice.EdgeProject, inst.Path(voice.EdgeDir)).Down(context.WithoutCancel(ctx), true)); err != nil {
			return ui.Wrap(err, "Couldn't stop the voice edge.", "Run the same command again.")
		}
		closeVoiceFirewall(ctx, app, edge.Opened)
	}
	if _, err := removeAutoUpgrade(ctx, inst, e); err != nil {
		app.UI.Detail("removing automatic upgrades: %v", err)
		app.UI.Warn("The timer that upgrades the voice servers by itself is still on this machine.", "")
	}
	app.forget(inst.Dir)
	removeVoiceSysctl(app)
	if err := os.RemoveAll(inst.Dir); err != nil {
		return ui.Wrap(err, "Couldn't delete "+inst.Dir+".", "Delete it yourself.")
	}
	app.UI.OK("%s", done)
	return nil
}

func removeVoiceSysctl(app *App) {
	if runtime.GOOS != "linux" || euid() != 0 {
		return
	}
	for _, dir := range instance.ReadRecord(instance.OwnScope()).Instances {
		if other, err := instance.Find(dir); err == nil && other.State.IsVoice() && len(other.State.Voice) > 0 {
			return
		}
	}
	if err := os.Remove(voiceSysctlFile); err != nil && !errors.Is(err, os.ErrNotExist) {
		app.UI.Detail("removing %s: %v", voiceSysctlFile, err)
	}
}

func voiceCommandAllowed(path string) bool {
	parts := strings.Fields(path)
	if len(parts) < 2 {
		return true
	}
	if len(parts) == 2 && parts[1] == "backup" {
		return true
	}
	return slices.Contains([]string{"voice", "logs", "restart", "uninstall", "upgrade", "auto-upgrade", "doctor", "status", "version", "docs", "use", "help", "completion", "migrate"}, parts[1])
}

func stringOf(p *string) string {
	if p == nil {
		return ""
	}
	return *p
}

func addressesText(srv *instance.VoiceServer) string {
	switch {
	case srv.NodeIP != "" && srv.NodeIPv6 != "":
		return "IPv4 " + srv.NodeIP + ", IPv6 " + srv.NodeIPv6
	case srv.NodeIPv6 != "":
		return "IPv6 only, " + srv.NodeIPv6
	case srv.NodeIP != "":
		return "IPv4 " + srv.NodeIP
	}
	return "found by LiveKit with STUN"
}
