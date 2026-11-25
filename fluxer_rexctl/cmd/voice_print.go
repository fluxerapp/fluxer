// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"fmt"
	"net/http"
	"net/url"
	"slices"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func runVoiceAddPrint(ctx context.Context, app *App, c *cobra.Command, f *voiceFlags) error {
	if !f.Print || f.Endpoint == "" {
		return ui.Usage("Pass --print and --endpoint together.")
	}
	u, err := url.Parse(f.Endpoint)
	if err != nil || u.Scheme != "wss" || u.Hostname() == "" {
		return ui.Usage("Pass --endpoint as a wss:// URL, such as wss://voice.example.com.")
	}
	for _, name := range []string{"count", "memory", "image", "no-firewall", "ipv6", "no-ipv6", "new-region", "region-name", "force"} {
		if c.Flags().Changed(name) {
			return ui.Usage("--" + name + " doesn't apply to a server you run yourself. Leave it out with --print.")
		}
	}
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if inst.State.IsVoice() {
		return ui.Refused("This folder holds voice servers that joined another instance.", "Run rexctl voice add --print on the Fluxer machine.")
	}
	api, _, err := app.Admin(ctx)
	if err != nil {
		return err
	}
	regions, err := api.VoiceRegions(ctx)
	if err != nil {
		return adminapi.Explain(err)
	}
	region, update := plainServerRegion(regions, inst, f.ID)
	if update && f.Region != "" && f.Region != region {
		return ui.Refused(fmt.Sprintf("%s is in the voice region %s.", f.ID, region), "Leave out --region to print a new config for it.")
	}
	if !update {
		if region, err = pickRegion(regions, f); err != nil {
			return err
		}
	}
	ids := takenVoiceIDs(regions, inst)
	id := f.ID
	switch {
	case id == "":
		id = voice.NextID(region, ids)
	case !update && slices.Contains(ids, id):
		return ui.Refused(fmt.Sprintf("A voice server with the ID %s already exists.", id), "Pass --id with another ID.")
	}
	webhook := voiceWebhookURL(ctx, inst.Domain(), inst.Origin())
	if webhook == "" {
		return ui.Refused("rexctl can't work out your instance's public address.", "Check that "+inst.Domain()+" opens in a browser, then try again.")
	}
	secret, err := voice.NewSecret()
	if err != nil {
		return err
	}
	turn, _ := settings.NormalTurnServers(inst.Setting(settings.TurnServersVar))
	plain := voice.Plain{ID: id, Key: voice.Key(id), Secret: secret, Webhook: webhook, TCPPort: cmp.Or(f.TCPPort, voice.FirstTCPPort), UDPPort: cmp.Or(f.UDPPort, voice.FirstUDPPort), NodeIP: f.IP, Turn: turn}
	if update {
		return rotatePlainServer(ctx, app, api, region, f, plain)
	}
	draining, active := true, true
	notes := "Added with rexctl voice add --print on " + time.Now().UTC().Format("2006-01-02")
	body := adminapi.CreateVoiceServerRequestBody{ServerId: id, Endpoint: f.Endpoint, ApiKey: plain.Key, ApiSecret: secret, IsActive: &active, Draining: &draining, Notes: &notes}
	if f.HasCoords {
		lat, long := float32(f.Lat), float32(f.Long)
		body.Latitude, body.Longitude = &lat, &long
	}
	if f.IP != "" {
		ip := f.IP
		body.PublicIpv4 = &ip
	}
	if f.SoftLimit > 0 {
		limit := f.SoftLimit
		body.SoftConnectionLimit = &limit
	}
	if _, err := api.CreateVoiceServer(ctx, region, body); err != nil {
		return ui.Wrap(adminapi.Explain(err), fmt.Sprintf("Couldn't register %s with your instance.", id), "")
	}
	fmt.Fprint(app.UI.Stdout(), plain.YAML())
	app.UI.OK("Registered %s in region %s at %s. It stays draining, so no call goes to it yet.", id, region, f.Endpoint)
	app.UI.Info("Run LiveKit with this config behind a proxy that has a certificate for %s, and open %d/tcp and %d/udp.", u.Hostname(), plain.TCPPort, plain.UDPPort)
	app.UI.Info("Then run rexctl voice test %s, and rexctl voice undrain %s once it passes.", id, id)
	if turn != "" {
		app.UI.Info("The config holds your TURN servers. When you change them with rexctl voice turn, run rexctl voice add --print --endpoint %s --id %s again for a new config.", f.Endpoint, id)
	}
	return nil
}

func voiceWebhookURL(ctx context.Context, domain, origin string) string {
	client := &voice.Client{HTTP: &http.Client{Timeout: 15 * time.Second}}
	if disc, err := client.Discover(ctx, domain); err == nil {
		return disc.APIPublic + "/webhooks/livekit"
	}
	if origin == "" {
		return ""
	}
	return origin + "/api/webhooks/livekit"
}

func plainServerRegion(regions []adminapi.VoiceRegion, inst *instance.Instance, id string) (string, bool) {
	if id == "" || id == deploy.BundledVoiceServerID(inst) || slices.ContainsFunc(inst.State.Voice, func(v instance.VoiceServer) bool { return v.ID == id }) {
		return "", false
	}
	for _, r := range regions {
		for _, s := range r.Servers {
			if s.ServerId == id {
				return r.Id, true
			}
		}
	}
	return "", false
}

func rotatePlainServer(ctx context.Context, app *App, api *adminapi.Client, region string, f *voiceFlags, plain voice.Plain) error {
	body := adminapi.UpdateVoiceServerRequestBody{Endpoint: &f.Endpoint, ApiKey: &plain.Key, ApiSecret: &plain.Secret}
	if f.HasCoords {
		lat, long := float32(f.Lat), float32(f.Long)
		body.Latitude, body.Longitude = &lat, &long
	}
	if f.IP != "" {
		ip := f.IP
		body.PublicIpv4 = &ip
	}
	if f.SoftLimit > 0 {
		limit := f.SoftLimit
		body.SoftConnectionLimit = &limit
	}
	if _, err := api.UpdateVoiceServer(ctx, region, plain.ID, body); err != nil {
		return ui.Wrap(adminapi.Explain(err), fmt.Sprintf("Couldn't update %s on your instance.", plain.ID), "")
	}
	fmt.Fprint(app.UI.Stdout(), plain.YAML())
	app.UI.OK("Gave %s in region %s a new secret.", plain.ID, region)
	app.UI.Info("Restart LiveKit there with this config, because the old one no longer works.")
	return nil
}
