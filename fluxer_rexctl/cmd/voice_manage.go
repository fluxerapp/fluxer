// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"net/http"
	"net/netip"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func voiceNodeClient() *voice.Client {
	return &voice.Client{HTTP: &http.Client{Timeout: 30 * time.Second}}
}

func localVoiceServer(app *App, id string) (*instance.Instance, *instance.VoiceServer, error) {
	inst, err := app.Instance()
	if err != nil {
		return nil, nil, err
	}
	if err := managed(inst); err != nil {
		return nil, nil, err
	}
	if id == deploy.BundledVoiceServerID(inst) && inst.State.VoiceServer(id) == nil {
		return nil, nil, ui.Refused(fmt.Sprintf("%s is the voice server bundled with Fluxer, and rexctl looks after it with the instance.", id), "Run rexctl config to change the bundled server.")
	}
	owner, servers, err := localVoiceServers(inst, []string{id})
	if err != nil {
		return nil, nil, err
	}
	return owner, servers[0], nil
}

func newVoiceDrainCmd(app *App, draining bool) *cobra.Command {
	use, short, done := "drain <id>", "Stop sending new calls to a voice server", "%s takes no new calls. Calls on it go on until they end."
	if !draining {
		use, short, done = "undrain <id>", "Send new calls to a voice server again", "%s takes new calls again."
	}
	c := &cobra.Command{
		Use:     use,
		Short:   short,
		Example: fmt.Sprintf("  rexctl voice %s fra-server-2", use[:len(use)-5]),
		Args:    exactArgs("the server ID"),
	}
	c.RunE = func(c *cobra.Command, args []string) error {
		ctx := c.Context()
		inst, err := app.Instance()
		if err != nil {
			return err
		}
		if !inst.State.IsVoice() && inst.State.VoiceServer(args[0]) == nil {
			return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
				region, err := voiceRegionOf(ctx, api, args[0])
				if err != nil {
					return err
				}
				if _, err := api.UpdateVoiceServer(ctx, region, args[0], adminapi.UpdateVoiceServerRequestBody{Draining: &draining}); err != nil {
					return err
				}
				app.UI.OK(done, args[0])
				return nil
			})
		}
		owner, srv, err := localVoiceServer(app, args[0])
		if err != nil {
			return err
		}
		var api *adminapi.Client
		if !owner.State.IsVoice() {
			if api, _, err = app.Admin(ctx); err != nil {
				return err
			}
		}
		return app.Locked(ctx, owner, func() error {
			if err := setVoiceDraining(ctx, api, owner, srv, draining); err != nil {
				return voiceNodeError(err, owner, srv)
			}
			srv.Drained = false
			if err := saveVoiceState(owner); err != nil {
				return err
			}
			app.UI.OK(done, srv.ID)
			return nil
		})
	}
	return c
}

func voiceRegionOf(ctx context.Context, api *adminapi.Client, id string) (string, error) {
	regions, err := api.VoiceRegions(ctx)
	if err != nil {
		return "", err
	}
	for _, r := range regions {
		for _, s := range r.Servers {
			if s.ServerId == id {
				return r.Id, nil
			}
		}
	}
	return "", ui.Refused(fmt.Sprintf("Your instance has no voice server called %s.", id), "Run rexctl voice list to see the servers.")
}

func voiceNodeError(err error, inst *instance.Instance, srv *instance.VoiceServer) error {
	switch voice.CodeOf(err) {
	case voice.CodeTokenExpired:
		return ui.Refused(voice.ClockProblem(err, srv.Instance, time.Now())+".", voice.ClockFix)
	case voice.CodeSignatureInvalid:
		return ui.Refused(fmt.Sprintf("%s has another secret for %s.", voiceInstanceName(srv), srv.ID), fmt.Sprintf("Run rexctl voice resync %s%s.", srv.ID, voiceDirFlag(inst.Dir)))
	case voice.CodeNodeUnknown:
		return ui.Refused(fmt.Sprintf("%s no longer knows %s.", voiceInstanceName(srv), srv.ID), fmt.Sprintf("Run rexctl voice remove %s%s, then join again.", srv.ID, voiceDirFlag(inst.Dir)))
	}
	return adminapi.Explain(err)
}

func voiceInstanceName(srv *instance.VoiceServer) string {
	if srv.Instance != "" {
		return srv.Instance
	}
	return "Your instance"
}

func newVoiceResyncCmd(app *App) *cobra.Command {
	var secretFile string
	var printSecret bool
	c := &cobra.Command{
		Use:   "resync <id>",
		Short: "Give the instance a voice server's own key and secret again",
		Long: "Calls fail when the instance signs tokens with another secret than the voice server has. " +
			"On the machine that runs the server, rexctl voice resync sends the server's key, secret, endpoint and addresses to the instance again. " +
			"For a server on another machine, run it on the Fluxer machine and pass the server's secret with --secret-file.",
		Example: "  rexctl voice resync fra-server-2\n" +
			"  rexctl voice resync ams-server-1 --print-secret\n" +
			"  rexctl voice resync ams-server-1 --secret-file -",
		Args: exactArgs("the server ID"),
	}
	c.Flags().StringVar(&secretFile, "secret-file", "", "file that holds the server's secret, or - for standard input, for a server on another machine")
	c.Flags().BoolVar(&printSecret, "print-secret", false, "print the secret of a server on this machine, for --secret-file on the Fluxer machine")
	c.RunE = func(c *cobra.Command, args []string) error {
		ctx := c.Context()
		inst, err := app.Instance()
		if err != nil {
			return err
		}
		id := args[0]
		if id == deploy.BundledVoiceServerID(inst) && inst.State.VoiceServer(id) == nil {
			return app.Locked(ctx, inst, func() error { return registerBundledVoiceServer(ctx, app, inst, true) })
		}
		if secretFile != "" {
			if inst.State.IsVoice() || inst.State.VoiceServer(id) != nil {
				return ui.Usage("Pass --secret-file only on the Fluxer machine, for a server on another machine.")
			}
			secret, err := readSecretFile(app, secretFile, "secret-file")
			if err != nil {
				return err
			}
			return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
				region, err := voiceRegionOf(ctx, api, id)
				if err != nil {
					return err
				}
				key := voice.Key(id)
				if _, err := api.UpdateVoiceServer(ctx, region, id, adminapi.UpdateVoiceServerRequestBody{ApiKey: &key, ApiSecret: &secret}); err != nil {
					return err
				}
				app.UI.OK("Your instance signs calls on %s with its own secret again.", id)
				return nil
			})
		}
		owner, srv, err := localVoiceServer(app, id)
		if err != nil {
			if !inst.State.IsVoice() && inst.State.VoiceServer(id) == nil {
				return ui.Refused(fmt.Sprintf("%s doesn't run on this machine.", id), fmt.Sprintf("On the machine that runs it, run rexctl voice resync %s --print-secret. Then run rexctl voice resync %s --secret-file - here and paste the secret.", id, id))
			}
			return err
		}
		if printSecret {
			app.UI.Result(srv.Secret)
			return nil
		}
		if owner.State.IsVoice() {
			return resyncJoinedServer(ctx, app, owner, srv)
		}
		api, _, err := app.Admin(ctx)
		if err != nil {
			return err
		}
		vs, err := voiceRuntime(ctx, app, owner)
		if err != nil {
			return err
		}
		return app.Locked(ctx, owner, func() error {
			internal := vs.Internal(srv)
			body := adminapi.UpdateVoiceServerRequestBody{ApiKey: &srv.Key, ApiSecret: &srv.Secret, Endpoint: &srv.Endpoint, InternalEndpoint: &internal}
			if srv.NodeIP != "" {
				body.PublicIpv4 = &srv.NodeIP
			}
			if srv.NodeIPv6 != "" {
				body.PublicIpv6 = &srv.NodeIPv6
			}
			if _, err := api.UpdateVoiceServer(ctx, srv.Region, srv.ID, body); err != nil {
				return adminapi.Explain(err)
			}
			app.UI.OK("Your instance has the key, secret and endpoint of %s again.", srv.ID)
			return nil
		})
	}
	return c
}

func resyncJoinedServer(ctx context.Context, app *App, inst *instance.Instance, srv *instance.VoiceServer) error {
	if srv.NodeURL == "" {
		return ui.Refused(fmt.Sprintf("%s never joined %s.", srv.ID, voiceInstanceName(srv)), fmt.Sprintf("Run rexctl voice remove %s%s, then join again.", srv.ID, voiceDirFlag(inst.Dir)))
	}
	update := voice.NodeUpdate{Endpoint: &srv.Endpoint, PublicIPv4: &srv.NodeIP, PublicIPv6: &srv.NodeIPv6}
	if _, err := voiceNodeClient().UpdateNode(ctx, srv.NodeURL, srv.Key, srv.Secret, update); err != nil {
		if voice.CodeOf(err) == voice.CodeSignatureInvalid {
			return ui.Refused(fmt.Sprintf("%s has another secret for %s, and rexctl can't change it from here.", voiceInstanceName(srv), srv.ID), fmt.Sprintf("Run rexctl voice resync %s --print-secret here. Then on the Fluxer machine, run rexctl voice resync %s --secret-file - and paste it.", srv.ID, srv.ID))
		}
		return voiceNodeError(err, inst, srv)
	}
	app.UI.OK("%s has the endpoint and addresses of %s again.", voiceInstanceName(srv), srv.ID)
	return nil
}

func newVoiceSetCmd(app *App) *cobra.Command {
	var ip, httpBind string
	c := &cobra.Command{
		Use:   "set <id>",
		Short: "Change the address of a voice server on this machine",
		Long: "Change the public IPv4 address a voice server gives to clients, or the address its port for your own proxy listens on. " +
			"rexctl restarts the server, and calls on it reconnect.",
		Example: "  rexctl voice set fra-server-2 --ip 203.0.113.7\n" +
			"  rexctl voice set fra-server-2 --ip auto\n" +
			"  rexctl voice set ams-server-1 --http-bind 10.0.0.5",
		Args: exactArgs("the server ID"),
	}
	c.Flags().StringVar(&ip, "ip", "", "public IPv4 address for clients, or auto to follow this machine")
	c.Flags().StringVar(&httpBind, "http-bind", "", "address the port for your own proxy listens on")
	c.RunE = func(c *cobra.Command, args []string) error {
		ctx := c.Context()
		if ip == "" && httpBind == "" {
			return ui.Usage("Pass --ip or --http-bind.")
		}
		owner, srv, err := localVoiceServer(app, args[0])
		if err != nil {
			return err
		}
		next := *srv
		if ip != "" {
			if err := setVoiceIP(owner, &next, ip); err != nil {
				return err
			}
		}
		if httpBind != "" {
			if next.Edge != voice.EdgeNone {
				return ui.Refused(fmt.Sprintf("%s has no port for your own proxy.", srv.ID), "Pass --http-bind only for a server that joined with --no-edge.")
			}
			bind, ok := voice.NormalHTTPBind(httpBind)
			if !ok {
				return ui.Usage(fmt.Sprintf("Pass --http-bind as an IPv4 or IPv6 address. %s isn't one.", httpBind))
			}
			if bind != "" && !voice.HTTPBindOnMachine(bind, voice.InterfaceAddresses()) {
				return ui.Refused(fmt.Sprintf("%s isn't an address of this machine.", httpBind), "Pass one of its own addresses, or 0.0.0.0 for every address.")
			}
			next.HTTPBind = bind
		}
		if next.NodeIP == srv.NodeIP && next.IPPinned == srv.IPPinned && next.HTTPBind == srv.HTTPBind {
			app.UI.OK("Nothing to change.")
			return nil
		}
		vs, err := voiceRuntime(ctx, app, owner)
		if err != nil {
			return err
		}
		return app.Locked(ctx, owner, func() error {
			err := app.UI.Step(ctx, "Restarting voice server "+srv.ID, func(ctx context.Context, s *ui.Step) error {
				if err := vs.Start(ctx, s, &next); err != nil {
					return err
				}
				s.Done("Restarted voice server %s.", srv.ID)
				return nil
			})
			if err != nil {
				_ = vs.Write(srv)
				return err
			}
			*srv = next
			if err := saveVoiceState(owner); err != nil {
				return err
			}
			reportVoiceAddresses(ctx, app, owner, srv)
			return nil
		})
	}
	return c
}

func setVoiceIP(inst *instance.Instance, srv *instance.VoiceServer, ip string) error {
	if ip == "auto" {
		srv.IPPinned = false
		if v := strings.TrimSpace(inst.State.Settings[settings.VoicePublicIP]); v != "" && !inst.State.IsVoice() {
			srv.NodeIP = v
		}
		return nil
	}
	a, err := netip.ParseAddr(ip)
	if err != nil || !a.Is4() {
		return ui.Usage(fmt.Sprintf("Pass --ip as an IPv4 address, or auto. %s isn't one.", ip))
	}
	srv.NodeIP, srv.IPPinned = a.String(), true
	return nil
}

func reportVoiceAddresses(ctx context.Context, app *App, inst *instance.Instance, srv *instance.VoiceServer) {
	var err error
	if inst.State.IsVoice() {
		if srv.NodeURL == "" {
			return
		}
		_, err = voiceNodeClient().UpdateNode(ctx, srv.NodeURL, srv.Key, srv.Secret, voice.NodeUpdate{PublicIPv4: &srv.NodeIP, PublicIPv6: &srv.NodeIPv6})
	} else {
		var api *adminapi.Client
		if api, _, err = app.Admin(ctx); err == nil {
			_, err = api.UpdateVoiceServer(ctx, srv.Region, srv.ID, adminapi.UpdateVoiceServerRequestBody{PublicIpv4: &srv.NodeIP})
		}
	}
	if err != nil {
		app.UI.Warn(fmt.Sprintf("%s runs with the new address, and the instance still lists the old one (%v).", srv.ID, err), fmt.Sprintf("Run rexctl voice resync %s%s.", srv.ID, voiceDirFlag(inst.Dir)))
		return
	}
	app.UI.OK("%s now gives clients %s.", srv.ID, addressesText(srv))
}
