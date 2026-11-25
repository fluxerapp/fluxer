// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const mediaSigningKey = "media-signing-key"

func newConfigRotateCmd(app *App) *cobra.Command {
	var finish bool
	c := &cobra.Command{
		Use:   "rotate media-signing-key",
		Short: "Replace the key that signs attachment links",
		Long: "Replace the key that signs attachment links, without breaking links people already have. rexctl adds a new key, restarts the media services so every part can check it, then signs with it. " +
			"The old key keeps checking older links. A day later, run it again with --finish to drop the old key. Data package links signed with it stop working then.",
		Example: `rexctl config rotate media-signing-key
rexctl config rotate media-signing-key --finish`,
		Args:      exactArgs(mediaSigningKey),
		ValidArgs: []string{mediaSigningKey},
	}
	c.Flags().BoolVar(&finish, "finish", false, "drop the old keys and keep only the one that signs")
	c.RunE = func(c *cobra.Command, args []string) error {
		if args[0] != mediaSigningKey {
			return ui.Usage("rexctl can rotate media-signing-key.")
		}
		return rotateMediaKey(c.Context(), app, finish)
	}
	return c
}

func signingKeys(v string) []string {
	var out []string
	for _, k := range strings.Split(v, ",") {
		if k = strings.TrimSpace(k); k != "" {
			out = append(out, k)
		}
	}
	return out
}

func rotateMediaKey(ctx context.Context, app *App, finish bool) error {
	v, err := loadConfigView(ctx, app, nil)
	if err != nil {
		return err
	}
	k, err := v.lookup(settings.SignatureSecretsVar)
	if err != nil {
		return err
	}
	keys := signingKeys(v.value(k))
	if len(keys) == 0 {
		return ui.Refused("Attachment links aren't signed, so there's no key to rotate.", "Run rexctl config set media.signed_attachments report to start signing them.")
	}
	if finish {
		if len(keys) == 1 {
			app.UI.OK("There's only one signing key. Nothing to drop.")
			return nil
		}
		ok, err := app.UI.ConfirmWith(ui.Question{Text: "Drop the old signing keys?", Detail: "Data package links and other links signed with them stop working.", Default: true})
		if err != nil {
			return err
		}
		if !ok {
			app.UI.Info("Nothing changed.")
			return nil
		}
		return runConfigChange(ctx, app, []configChange{{name: settings.SignatureSecretsVar, value: &keys[0]}}, nil)
	}
	fresh, err := secrets.Base64(32)
	if err != nil {
		return ui.Wrap(err, "Couldn't make a new signing key.", "Try again.")
	}
	ok, err := app.UI.ConfirmWith(ui.Question{Text: "Rotate the attachment signing key?", Detail: "The services that sign and check attachment links restart twice.", Default: true})
	if err != nil {
		return err
	}
	if !ok {
		app.UI.Info("Nothing changed.")
		return nil
	}
	checking := strings.Join(append(keys, fresh), ",")
	if err := changeConfig(ctx, app, []configChange{{name: settings.SignatureSecretsVar, value: &checking}}, nil); err != nil {
		if errors.Is(err, errKeptSettings) {
			app.UI.Info("Nothing changed.")
			return nil
		}
		return err
	}
	signing := strings.Join(append([]string{fresh}, keys...), ",")
	err = changeConfig(ctx, app, []configChange{{name: settings.SignatureSecretsVar, value: &signing}}, nil)
	if errors.Is(err, errKeptSettings) {
		app.UI.Info("Fluxer accepts links signed with the new key but still signs them with the old one.")
		return nil
	}
	if err != nil {
		return err
	}
	app.UI.OK("Fluxer signs attachment links with the new key.")
	app.UI.Info("Run rexctl config rotate media-signing-key --finish tomorrow to drop the old key.")
	return nil
}
