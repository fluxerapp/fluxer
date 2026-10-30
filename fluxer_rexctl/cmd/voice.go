// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
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
		Long:  "Create a join token and print the command that installs a voice server with it. The token works once.",
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
			rows = append(rows, []string{s.ServerId, r.Id, state, s.Endpoint, here})
		}
	}
	return rows, servers
}

func listVoiceMachine(ctx context.Context, app *App, inst *instance.Instance) error {
	client := &voice.Client{HTTP: &http.Client{Timeout: 10 * time.Second}}
	type row struct {
		ID       string `json:"id"`
		Instance string `json:"instance"`
		Region   string `json:"region"`
		Endpoint string `json:"endpoint"`
		Health   string `json:"health"`
	}
	var out []row
	for _, s := range inst.State.Voice {
		health := "unknown"
		if node, err := client.GetNode(ctx, s.NodeURL, s.Key, s.Secret); err == nil {
			health = node.Health.Status
			if node.Draining {
				health += ", draining"
			}
			if !node.IsActive {
				health = "off"
			}
		} else if code := voice.CodeOf(err); code != "" {
			health = strings.ToLower(code)
		}
		out = append(out, row{s.ID, s.Instance, s.Region, s.Endpoint, health})
	}
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
	return &cobra.Command{
		Use:   "status [id]",
		Short: "Show the voice servers on this machine",
		Args:  maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			inst, servers, err := localVoiceServers(inst, args)
			if err != nil {
				return err
			}
			if len(servers) == 0 {
				app.UI.Info("This machine runs no voice servers.")
				return nil
			}
			e, err := app.Engine(ctx, inst)
			if err != nil {
				return err
			}
			for _, srv := range servers {
				showVoiceStatus(ctx, app, e, inst, srv)
			}
			return nil
		},
	}
}

func showVoiceStatus(ctx context.Context, app *App, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer) {
	u := app.UI
	container := "not running"
	if cs, err := voiceProject(e, inst, srv).PS(ctx); err == nil && len(cs) > 0 {
		container = cs[0].State
		if cs[0].Health != "" {
			container += ", " + cs[0].Health
		}
	}
	base, dial := voiceProbeTarget(inst, srv)
	reach := "answers"
	if err := voice.ProbeThroughEdge(ctx, base, dial, true); err != nil {
		reach = err.Error()
	}
	cert := "-"
	if srv.Edge == voice.EdgeOwn {
		if exp, err := voice.CertificateExpiry(ctx, srv.Endpoint, dial); err != nil {
			cert = "not valid (" + err.Error() + ")"
		} else {
			cert = "valid until " + exp.Local().Format("2 January 15:04")
		}
	}
	load := "unknown"
	if l, err := voice.ListRooms(ctx, base, dial, srv.Key, srv.Secret, true); err == nil {
		load = fmt.Sprintf("%s, %s", plural(l.Rooms, "room", "rooms"), plural(l.Participants, "participant", "participants"))
	}
	u.Info("Voice server %s", srv.ID)
	rows := [][]string{
		{"Region", srv.Region},
		{"Endpoint", srv.Endpoint + ", " + reach},
		{"Container", container},
		{"Ports", fmt.Sprintf("%d/tcp, %d/udp", srv.TCPPort, srv.UDPPort)},
		{"Certificate", cert},
		{"Calls", load},
		{"LiveKit", srv.Image},
		{"Webhook", srv.Webhook + ", " + webhookState(ctx, e, inst, srv)},
	}
	if srv.Instance != "" {
		rows = append(rows, []string{"Instance", srv.Instance})
	}
	if srv.Joined && srv.Instance != "" {
		rows = append(rows, []string{"Browsers", browsersText(ctx, srv)})
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

func webhookState(ctx context.Context, e *engine.Engine, inst *instance.Instance, srv *instance.VoiceServer) string {
	failed, sent := voiceWebhookLog(ctx, e, inst, srv)
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
		Args: maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			ctx := c.Context()
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			owner, servers, err := localVoiceServers(inst, args)
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
				results := runTransportTests(ctx, srv.Endpoint, func() (string, error) {
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
	return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
		regions, err := api.VoiceRegions(ctx)
		if err != nil {
			return err
		}
		region := ""
		for _, r := range regions {
			for _, s := range r.Servers {
				if s.ServerId == id {
					region = r.Id
				}
			}
		}
		if region == "" {
			return ui.Refused(fmt.Sprintf("Your instance has no voice server called %s.", id), "Run rexctl voice list to see the servers.")
		}
		var endpoint string
		var results []transportResult
		for _, t := range []voice.Transport{voice.TransportUDP, voice.TransportTCP} {
			tok, err := api.VoiceTestToken(ctx, region, id)
			if err == nil {
				endpoint = tok.Endpoint
				err = voice.TestTransport(ctx, tok.Endpoint, tok.Token, t, false, 20*time.Second)
			}
			results = append(results, transportResult{Transport: t, Err: err})
		}
		app.UI.Info("Tested %s at %s from this machine.", id, endpoint)
		if !reportTransports(app, results, false) {
			if slices.ContainsFunc(results, func(r transportResult) bool { return errors.Is(r.Err, voice.ErrTokenRejected) }) {
				app.UI.Info("%s rejected a token signed with the API secret your instance has for it. Put the server's own secret back in the admin panel. It's FLUXER_VOICE_API_SECRET in voice/%s/.env in the rexctl folder on the machine that runs it.", id, id)
			} else {
				app.UI.Info("Open the server's TCP and UDP ports in the firewall of its provider and on the machine itself.")
			}
			app.SetExit(ui.ExitFailed)
		}
		return nil
	})
}

func newVoiceHostnameCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:   "hostname <id> <host>",
		Short: "Move a voice server to a new hostname",
		Long:  "Move a voice server on this machine to a hostname once its DNS record points here. rexctl gets a certificate and updates the server's endpoint on the instance.",
		Args:  exactArgs("the server ID", "the hostname"),
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
				return voice.CheckHostname(answer, srv.NodeIP, voice.InterfaceAddresses())
			}
			if !waitForHostname(ctx, app, name, srv.NodeIP, check, false, true) {
				return ui.Refused(fmt.Sprintf("%s doesn't point at this machine yet.", name), "Fix the DNS record, then run the same command again.")
			}
			e, err := app.Engine(ctx, inst)
			if err != nil {
				return err
			}
			return app.Locked(ctx, inst, func() error {
				old := srv.Endpoint
				srv.Endpoint, srv.Hostname = voice.Endpoint(name, srv.ID), name
				if err := routeVoiceServer(ctx, app, e, inst, srv); err != nil {
					return err
				}
				if err := waitForEndpoint(ctx, voice.HTTPBase(srv.Endpoint), "127.0.0.1:443", false, 3*time.Minute); err != nil {
					srv.Endpoint = old
					_ = routeVoiceServer(ctx, app, e, inst, srv)
					return ui.Wrap(err, fmt.Sprintf("%s didn't get a certificate.", name), "Check that ports 80 and 443 are open, then try again.")
				}
				client := &voice.Client{HTTP: &http.Client{Timeout: 30 * time.Second}}
				if _, err := client.UpdateNode(ctx, srv.NodeURL, srv.Key, srv.Secret, voice.NodeUpdate{Endpoint: &srv.Endpoint}); err != nil {
					srv.Endpoint = old
					_ = routeVoiceServer(ctx, app, e, inst, srv)
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
	var all, now bool
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
			if all && !inst.State.IsVoice() {
				return ui.Refused("--all works on a voice machine.", "Pass the ID of the server to remove.")
			}
			owner, servers, err := localVoiceServers(inst, args)
			if err != nil && len(args) == 1 && !inst.State.IsVoice() && instanceHasVoiceServer(c, app, args[0]) {
				return ui.Refused(fmt.Sprintf("%s runs on another machine.", args[0]), fmt.Sprintf("Run rexctl voice remove %s on the machine that runs it, or delete it in the admin panel.", args[0]))
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
	return c
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
	if err := confirmVoice(app, "Remove "+ui.JoinAnd(ids)+"?"); err != nil {
		return err
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return err
	}
	var api *adminapi.Client
	if !inst.State.IsVoice() {
		p, err := app.Named(ctx, inst)
		if err != nil {
			return err
		}
		api = app.adminClient(inst, p)
	}
	return app.Locked(ctx, inst, func() error {
		for _, id := range ids {
			srv := inst.State.VoiceServer(id)
			if srv == nil {
				continue
			}
			if err := removeOneVoiceServer(ctx, app, e, api, inst, srv, drain); err != nil {
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
	base, dial := voiceProbeTarget(inst, srv)
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
			return ui.Refused(fmt.Sprintf("The instance has another secret for %s.", srv.ID), fmt.Sprintf("Put FLUXER_VOICE_API_SECRET from %s back in the admin panel, or delete the server there and run this again.", filepath.Join(voice.ServerDir(inst.Dir, srv.ID), voice.EnvFile)))
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

func removeOneVoiceServer(ctx context.Context, app *App, e *engine.Engine, api *adminapi.Client, inst *instance.Instance, srv *instance.VoiceServer, drain time.Duration) error {
	if err := unregisterVoiceServer(ctx, app, api, inst, srv, drain); err != nil {
		return err
	}
	err := app.UI.Step(ctx, "Removing voice server "+srv.ID, func(ctx context.Context, s *ui.Step) error {
		if err := removeVoiceServerProject(ctx, e, srv); err != nil {
			return err
		}
		_ = os.RemoveAll(voice.ServerDir(inst.Dir, srv.ID))
		removed := *srv
		inst.State.RemoveVoiceServer(srv.ID)
		if !inst.State.IsVoice() {
			_ = os.Remove(filepath.Join(inst.Path(instance.CaddyDropInDir), voice.DropInName(removed.ID)))
			if err := reloadCaddy(ctx, e.Named(inst.Project(), inst.Dir), "edge", "/etc/caddy/Caddyfile"); err != nil {
				app.UI.Detail("reloading the edge: %v", err)
			}
		} else {
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
	if edge := inst.State.VoiceEdge; edge != nil && edge.Instance == "" {
		done = "Removed every voice server and the edge from this machine."
		if err := engineFailure(e.Named(voice.EdgeProject, inst.Path(voice.EdgeDir)).Down(context.WithoutCancel(ctx), true)); err != nil {
			return ui.Wrap(err, "Couldn't stop the voice edge.", "Run the same command again.")
		}
		closeVoiceFirewall(ctx, app, edge.Opened)
	}
	if _, err := removeAutoUpgradeFor(ctx, inst, e); err != nil {
		app.UI.Detail("removing automatic upgrades: %v", err)
	}
	app.forget(inst.Dir)
	removeVoiceSysctl(app)
	if err := os.RemoveAll(inst.Dir); err != nil {
		return ui.Wrap(err, "Couldn't delete "+inst.Dir+".", "Delete it yourself.")
	}
	app.UI.OK("%s", done)
	return nil
}

var removeAutoUpgradeFor = removeAutoUpgrade

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
	return slices.Contains([]string{"voice", "uninstall", "upgrade", "auto-upgrade", "doctor", "status", "version", "docs", "use", "help", "completion"}, parts[1])
}
