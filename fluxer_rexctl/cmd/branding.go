// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
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

type brandingImage struct {
	flag  string
	kind  adminapi.BrandingAssetUploadRequestKind
	label string
	key   string
}

var brandingImages = []brandingImage{
	{"logo", adminapi.Logo, "logo", "logo_url"},
	{"icon", adminapi.Icon, "icon", "icon_url"},
	{"wordmark", adminapi.Wordmark, "wordmark", "wordmark_url"},
	{"symbol", adminapi.Symbol, "symbol", "symbol_url"},
	{"favicon", adminapi.Favicon, "favicon", "favicon_url"},
	{"favicon-dark", adminapi.FaviconDark, "dark mode favicon", "favicon_dark_url"},
	{"sign-in-backdrop", adminapi.SignInBackdrop, "sign-in backdrop", "sign_in_backdrop_url"},
}

type brandingFlags struct {
	Name   string
	Colour string
	Images map[string]*string
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

func validColour(s string) error {
	if !hexColour.MatchString(strings.TrimSpace(s)) {
		return errors.New("Use a hex code such as 4641d9.")
	}
	return nil
}

func normaliseColour(s string) string {
	return "#" + strings.ToLower(strings.TrimPrefix(strings.TrimSpace(s), "#"))
}

func newBrandingCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "branding",
		Short: "Change your instance's name, colour and images",
		Long:  "Shows your instance's name, accent colour and images, and offers to change them.",
		Example: `rexctl branding
rexctl branding set --name "Example Chat" --colour 4641d9
rexctl branding set --logo logo.png --favicon favicon.png`,
		Args: noArgs,
	}
	app.jsonFlag(c)
	f := brandingFlags{Images: map[string]*string{}}
	set := &cobra.Command{
		Use:   "set",
		Short: "Change the name, colour or images",
		Example: `rexctl branding set --name "Example Chat"
rexctl branding set --colour 4641d9
rexctl branding set --icon icon.png --sign-in-backdrop backdrop.jpg`,
		Args: noArgs,
	}
	set.Flags().StringVar(&f.Name, "name", "", "your instance's name")
	set.Flags().StringVar(&f.Colour, "colour", "", "accent colour as a hex code, like 4641d9")
	set.Flags().StringVar(&f.Colour, "color", "", "accent colour as a hex code, like 4641d9")
	_ = set.Flags().MarkHidden("color")
	for _, img := range brandingImages {
		v := new(string)
		f.Images[img.flag] = v
		set.Flags().StringVar(v, img.flag, "", "image file for the "+img.label)
	}
	set.RunE = func(c *cobra.Command, _ []string) error {
		return setBranding(c, app, f)
	}
	c.RunE = func(c *cobra.Command, _ []string) error {
		var b map[string]string
		err := adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
			var err error
			b, err = api.Branding(ctx)
			return err
		})
		if err != nil {
			return err
		}
		if app.Flags.JSON {
			return app.UI.JSON(b)
		}
		app.UI.Card(ui.Card{Title: "Branding", Rows: brandingRows(b)})
		if !app.UI.Interactive() {
			app.UI.Next("rexctl branding set --name <name>", "rexctl branding set --logo <file>")
			return nil
		}
		return brandingActions(c, app, b)
	}
	c.AddCommand(set)
	return c
}

func brandingRows(b map[string]string) []ui.KV {
	orNot := func(v, none string) string {
		if v == "" {
			return none
		}
		return v
	}
	rows := []ui.KV{
		{Key: "Name", Value: orNot(b["product_name"], "Fluxer")},
		{Key: "Colour", Value: orNot(b["theme_color"], "Fluxer's own")},
	}
	for _, img := range brandingImages {
		rows = append(rows, ui.KV{Key: ui.Capitalise(img.label), Value: orNot(b[img.key], "Fluxer's own")})
	}
	return rows
}

func brandingActions(c *cobra.Command, app *App, b map[string]string) error {
	items := []ui.PickItem{{Value: "name", Label: "Change the name"}, {Value: "colour", Label: "Change the accent colour"}}
	for _, img := range brandingImages {
		items = append(items, ui.PickItem{Value: img.flag, Label: "Upload a new " + img.label})
	}
	items = append(items, ui.PickItem{Value: "", Label: "Done"})
	choice, err := app.UI.Pick("What do you want to change?", items, "")
	if err != nil || choice == "" {
		return err
	}
	f := brandingFlags{Images: map[string]*string{}}
	switch choice {
	case "name":
		f.Name = b["product_name"]
		err = app.UI.Ask(nil, ui.Field{Title: "What's your instance called?", Value: &f.Name, Validate: func(s string) error {
			if strings.TrimSpace(s) == "" {
				return errors.New("Type a name.")
			}
			return nil
		}})
	case "colour":
		f.Colour = b["theme_color"]
		err = app.UI.Ask(nil, ui.Field{Title: "Which accent colour?", Value: &f.Colour, Placeholder: "#4641d9", Validate: validColour})
	default:
		v := new(string)
		f.Images[choice] = v
		err = app.UI.Ask(nil, ui.Field{Title: "Which image file?", Value: v, Description: "A PNG, JPEG, GIF or WebP file on this machine.", Validate: func(s string) error {
			if _, err := os.Stat(strings.TrimSpace(s)); err != nil {
				return errors.New("There's no file at that path.")
			}
			return nil
		}})
	}
	if err != nil {
		return err
	}
	return setBranding(c, app, f)
}

func setBranding(c *cobra.Command, app *App, f brandingFlags) error {
	name := strings.TrimSpace(f.Name)
	colour := strings.TrimSpace(f.Colour)
	images := map[adminapi.BrandingAssetUploadRequestKind][]byte{}
	var order []brandingImage
	for _, img := range brandingImages {
		path := f.Images[img.flag]
		if path == nil || strings.TrimSpace(*path) == "" {
			continue
		}
		data, err := readImage(strings.TrimSpace(*path), img.flag)
		if err != nil {
			return err
		}
		images[img.kind] = data
		order = append(order, img)
	}
	if f.Name != "" && name == "" {
		return ui.Usage("--name can't be empty.")
	}
	if name == "" && colour == "" && len(images) == 0 {
		return ui.Usage("Pass --name, --colour or an image flag such as --logo.")
	}
	if colour != "" {
		if err := validColour(colour); err != nil {
			return ui.Usage("--colour can't be \"" + colour + "\". " + err.Error())
		}
		colour = normaliseColour(colour)
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
		for _, img := range order {
			if err := api.UploadBranding(ctx, img.kind, images[img.kind]); err != nil {
				return err
			}
			app.UI.OK("Uploaded the %s.", img.label)
		}
		return nil
	})
}
