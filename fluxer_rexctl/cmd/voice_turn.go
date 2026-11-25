// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newVoiceTurnCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "turn",
		Short: "Choose the TURN servers calls fall back to",
		Long: "Callers behind strict firewalls connect to voice servers through a TURN server. " +
			"rexctl keeps the list, gives it to every voice server of the instance and restarts the bundled one.",
		Example: "  rexctl voice turn add --host turn.example.com --username fluxer --password-file turn.txt\n" +
			"  rexctl voice turn list\n" +
			"  rexctl voice turn remove turn.example.com",
		Args: noArgs,
	}
	c.AddCommand(newVoiceTurnAddCmd(app), newVoiceTurnListCmd(app), newVoiceTurnRemoveCmd(app))
	return c
}

func turnInstance(app *App) (*instance.Instance, error) {
	inst, err := app.Instance()
	if err != nil {
		return nil, err
	}
	if inst.State.IsVoice() {
		return nil, ui.Refused("TURN servers belong to the Fluxer instance, and this machine only runs voice servers.", "Run the command on the Fluxer machine. Voice servers here pick up the list on their next upgrade.")
	}
	return inst, nil
}

func turnServers(app *App) ([]settings.TurnServer, error) {
	inst, err := turnInstance(app)
	if err != nil {
		return nil, err
	}
	list, err := settings.ParseTurnServers(inst.Setting(settings.TurnServersVar))
	if err != nil {
		return nil, ui.Refused("rexctl can't read the TURN servers this instance has. "+ui.Sentence(err.Error()), "Run rexctl voice turn remove --all, then add them again.")
	}
	return list, nil
}

func saveTurnServers(c *cobra.Command, app *App, list []settings.TurnServer) error {
	ch := configChange{name: settings.TurnServersVar}
	if v := settings.TurnServersValue(list); v != "" {
		ch.value = &v
	}
	ctx := c.Context()
	if err := runConfigChange(ctx, app, []configChange{ch}, nil); err != nil {
		return err
	}
	inst, err := app.Instance()
	if err != nil || len(inst.State.Voice) == 0 {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		refreshStaleVoiceServers(ctx, app, inst)
		return nil
	})
}

func newVoiceTurnAddCmd(app *App) *cobra.Command {
	var t settings.TurnServer
	var passwordFile, secretFile string
	c := &cobra.Command{
		Use:   "add",
		Short: "Add a TURN server",
		Example: "  rexctl voice turn add --host turn.example.com --username fluxer --password-file turn.txt\n" +
			"  rexctl voice turn add --host turn.example.com --port 5349 --protocol tls --secret-file secret.txt",
		Args: noArgs,
	}
	c.Flags().StringVar(&t.Host, "host", "", "hostname or IPv4 address of the TURN server")
	c.Flags().IntVar(&t.Port, "port", 3478, "port of the TURN server")
	c.Flags().StringVar(&t.Protocol, "protocol", "udp", "how to connect to the TURN server, udp, tcp or tls")
	c.Flags().StringVar(&t.Username, "username", "", "username for the TURN server")
	c.Flags().StringVar(&passwordFile, "password-file", "", "file that holds the password for --username, or - for standard input")
	c.Flags().StringVar(&secretFile, "secret-file", "", "file that holds the shared secret, for TURN servers that make short-lived passwords")
	c.Flags().IntVar(&t.TTL, "ttl", 0, "seconds a password made from the shared secret works")
	c.MarkFlagsMutuallyExclusive("username", "secret-file")
	c.MarkFlagsMutuallyExclusive("password-file", "secret-file")
	c.RunE = func(c *cobra.Command, _ []string) error {
		list, err := turnServers(app)
		if err != nil {
			return err
		}
		if err := app.UI.Ask(c, ui.Field{Flag: "host", Title: "Hostname of the TURN server", Value: &t.Host, Validate: validTurnHost}); err != nil {
			return err
		}
		t.Host = strings.ToLower(strings.TrimSuffix(strings.TrimSpace(t.Host), "."))
		switch {
		case t.Port < 1 || t.Port > 65535:
			return ui.Usage("Pass --port from 1 to 65535.")
		case t.Protocol != "udp" && t.Protocol != "tcp" && t.Protocol != "tls":
			return ui.Usage(fmt.Sprintf("--protocol can't be %q. Pass udp, tcp or tls.", t.Protocol))
		case t.TTL < 0, t.TTL > 0 && secretFile == "":
			return ui.Usage("Pass --ttl only with --secret-file, as a number of seconds.")
		case passwordFile != "" && t.Username == "":
			return ui.Usage("Pass --username with --password-file.")
		}
		switch {
		case secretFile != "":
			if t.Secret, err = readSecretFile(app, secretFile, "secret-file"); err != nil {
				return err
			}
		case passwordFile != "":
			if t.Credential, err = readSecretFile(app, passwordFile, "password-file"); err != nil {
				return err
			}
		case t.Username != "":
			if err := app.UI.Ask(c, ui.Field{Flag: "password-file", FlagLabel: "The password", Title: "Password for " + t.Username, Value: &t.Credential, Secret: true}); err != nil {
				return err
			}
		}
		if slices.ContainsFunc(list, func(o settings.TurnServer) bool {
			return o.Host == t.Host && o.Port == t.Port && o.Protocol == t.Protocol
		}) {
			return ui.Refused(fmt.Sprintf("%s is already in the list.", turnName(t)), "Run rexctl voice turn remove "+t.Host+" first to change it.")
		}
		next := append(slices.Clone(list), t)
		if _, err := settings.ParseTurnServers(settings.TurnServersValue(next)); err != nil {
			return ui.Usage(ui.Sentence(err.Error()))
		}
		return saveTurnServers(c, app, next)
	}
	return c
}

func validTurnHost(v string) error {
	v = strings.TrimSpace(v)
	if v == "" || strings.ContainsAny(v, ":/[] \t") {
		return fmt.Errorf("give a hostname or an IPv4 address")
	}
	return nil
}

func turnName(t settings.TurnServer) string {
	return fmt.Sprintf("%s:%d over %s", t.Host, t.Port, t.Protocol)
}

func turnAuth(t settings.TurnServer) string {
	switch {
	case t.Secret != "" && t.TTL > 0:
		return fmt.Sprintf("shared secret, passwords work %ds", t.TTL)
	case t.Secret != "":
		return "shared secret"
	case t.Username != "":
		return "username " + t.Username
	}
	return "none"
}

func newVoiceTurnListCmd(app *App) *cobra.Command {
	c := &cobra.Command{Use: "list", Short: "List the TURN servers", Args: noArgs}
	app.jsonFlag(c)
	c.RunE = func(c *cobra.Command, _ []string) error {
		list, err := turnServers(app)
		if err != nil {
			return err
		}
		if app.Flags.JSON {
			type row struct {
				Host     string `json:"host"`
				Port     int    `json:"port"`
				Protocol string `json:"protocol"`
				Auth     string `json:"auth"`
			}
			out := make([]row, len(list))
			for i, t := range list {
				out[i] = row{t.Host, t.Port, t.Protocol, turnAuth(t)}
			}
			return app.UI.JSON(out)
		}
		if len(list) == 0 {
			app.UI.Info("Calls use no TURN server. Run rexctl voice turn add to add one.")
			return nil
		}
		rows := make([][]string, len(list))
		for i, t := range list {
			rows[i] = []string{t.Host, strconv.Itoa(t.Port), t.Protocol, turnAuth(t)}
		}
		app.UI.Table([]string{"Host", "Port", "Protocol", "Sign-in"}, rows)
		return nil
	}
	return c
}

func newVoiceTurnRemoveCmd(app *App) *cobra.Command {
	var all bool
	var port int
	c := &cobra.Command{
		Use:     "remove [host]",
		Short:   "Remove a TURN server",
		Example: "  rexctl voice turn remove turn.example.com\n  rexctl voice turn remove turn.example.com --port 5349\n  rexctl voice turn remove --all",
		Args:    maxArgs(1),
	}
	c.Flags().BoolVar(&all, "all", false, "remove every TURN server")
	c.Flags().IntVar(&port, "port", 0, "remove only the entry on this port")
	c.RunE = func(c *cobra.Command, args []string) error {
		if all == (len(args) == 1) {
			return ui.Usage("Pass the host of the TURN server, or --all.")
		}
		inst, err := turnInstance(app)
		if err != nil {
			return err
		}
		if all {
			if strings.TrimSpace(inst.Setting(settings.TurnServersVar)) == "" {
				app.UI.OK("Calls already use no TURN server.")
				return nil
			}
			return saveTurnServers(c, app, nil)
		}
		list, err := turnServers(app)
		if err != nil {
			return err
		}
		host := strings.ToLower(strings.TrimSuffix(strings.TrimSpace(args[0]), "."))
		match := func(t settings.TurnServer) bool { return t.Host == host && (port == 0 || t.Port == port) }
		var found []settings.TurnServer
		for _, t := range list {
			if match(t) {
				found = append(found, t)
			}
		}
		if len(found) == 0 {
			return ui.Refused(fmt.Sprintf("There's no TURN server %s in the list.", host), "Run rexctl voice turn list to see them.")
		}
		if port == 0 && len(found) > 1 && slices.ContainsFunc(found, func(t settings.TurnServer) bool { return t.Port != found[0].Port }) {
			return ui.Usage(fmt.Sprintf("%s is in the list on more than one port. Pass --port.", host))
		}
		return saveTurnServers(c, app, slices.DeleteFunc(slices.Clone(list), match))
	}
	return c
}

func readSecretFile(app *App, path, flag string) (string, error) {
	secret, err := readSetupSecret(app, path)
	if err != nil {
		return "", ui.Wrap(err, fmt.Sprintf("Couldn't read %s.", path), fmt.Sprintf("Check the path you passed to --%s.", flag))
	}
	if secret == "" {
		return "", ui.Usage(fmt.Sprintf("The file passed with --%s is empty.", flag))
	}
	return secret, nil
}
