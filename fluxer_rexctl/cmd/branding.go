// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"net/http"
	"os"
	"regexp"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type brandingFlags struct {
	Name   string
	Colour string
	Logo   string
	Icon   string
}

var hexColour = regexp.MustCompile(`^#?(?:[0-9a-fA-F]{3}|[0-9a-fA-F]{6})$`)

func readImage(path, flag string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, ui.Failed(fmt.Sprintf("Couldn't read %s.", path), fmt.Sprintf("Pass --%s with the path to an image file.", flag))
	}
	switch http.DetectContentType(data) {
	case "image/png", "image/jpeg", "image/gif", "image/webp":
		return data, nil
	}
	return nil, ui.Usage(fmt.Sprintf("%s isn't a PNG, JPEG, GIF or WebP image.", path))
}

func newBrandingCmd(app *App) *cobra.Command {
	c := &cobra.Command{Use: "branding", Short: "Change your instance's name, colour and images", Args: noArgs}
	var f brandingFlags
	set := &cobra.Command{Use: "set", Short: "Change the name, colour, logo or icon", Args: noArgs}
	set.Flags().StringVar(&f.Name, "name", "", "your instance's name")
	set.Flags().StringVar(&f.Colour, "colour", "", "accent colour as a hex code, like #4641d9")
	set.Flags().StringVar(&f.Logo, "logo", "", "image file for the logo")
	set.Flags().StringVar(&f.Icon, "icon", "", "image file for the icon")
	set.RunE = func(c *cobra.Command, _ []string) error {
		if f.Name == "" && f.Colour == "" && f.Logo == "" && f.Icon == "" {
			return ui.Usage("Pass --name, --colour, --logo or --icon.")
		}
		name := strings.TrimSpace(f.Name)
		colour := strings.TrimSpace(f.Colour)
		if colour != "" {
			if !hexColour.MatchString(colour) {
				return ui.Usage("Pass --colour as a hex code, like #4641d9.")
			}
			colour = "#" + strings.ToLower(strings.TrimPrefix(colour, "#"))
		}
		var logo, icon []byte
		var err error
		if f.Logo != "" {
			if logo, err = readImage(f.Logo, "logo"); err != nil {
				return err
			}
		}
		if f.Icon != "" {
			if icon, err = readImage(f.Icon, "icon"); err != nil {
				return err
			}
		}
		return adminWrite(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			if err := api.SetBranding(ctx, name, colour); err != nil {
				return err
			}
			if name != "" {
				app.UI.OK("Your instance is called %s.", name)
			}
			if colour != "" {
				app.UI.OK("Set the accent colour to %s.", colour)
			}
			if logo != nil {
				if err := api.UploadBranding(ctx, adminapi.Logo, logo); err != nil {
					return err
				}
				app.UI.OK("Uploaded the logo.")
			}
			if icon != nil {
				if err := api.UploadBranding(ctx, adminapi.Icon, icon); err != nil {
					return err
				}
				app.UI.OK("Uploaded the icon.")
			}
			return nil
		})
	}
	c.AddCommand(set)
	return c
}
