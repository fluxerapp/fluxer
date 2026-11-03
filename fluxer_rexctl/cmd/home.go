// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/charmbracelet/x/ansi"
	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var (
	homeInstalled = []string{"status", "upgrade", "backup", "users", "invites", "doctor"}
	homeLegacy    = []string{"install", "status"}
	homeEmpty     = []string{"install", "restore"}
)

func runHome(_ context.Context, app *App, c *cobra.Command) error {
	if !app.UI.Interactive() {
		return c.Help()
	}
	dir, err := app.Dir()
	if err != nil {
		return err
	}
	inst, err := instance.Find(dir)
	switch {
	case err == nil:
		homeGreet(app, fmt.Sprintf("Your instance is at %s.", inst.Origin()))
		homeCommands(app, c, homeInstalled)
	case errors.Is(err, instance.ErrLegacy):
		homeGreet(app, fmt.Sprintf("There's a Fluxer instance in %s.", dir))
		app.UI.Info("Run %s to take it over.", app.takeOverCommand())
		app.UI.Info("")
		homeCommands(app, c, homeLegacy)
	default:
		homeGreet(app, ui.WelcomeLine)
		homeCommands(app, c, homeEmpty)
	}
	app.UI.Info("")
	app.UI.Info("Run rexctl --help to see every command.")
	return nil
}

func homeGreet(app *App, line string) {
	if app.UI.ShowsRex() {
		app.UI.Rex(ui.Greeting, line)
		return
	}
	app.UI.Info("%s", line)
	app.UI.Info("")
}

func homeCommands(app *App, root *cobra.Command, names []string) {
	type row struct{ use, short string }
	var rows []row
	width := 0
	for _, name := range names {
		sub, _, err := root.Find([]string{name})
		if err != nil || sub == root {
			continue
		}
		use := "rexctl " + sub.Name()
		rows = append(rows, row{use, sub.Short})
		width = max(width, ansi.StringWidth(use))
	}
	accent := app.UI.Theme().Accent
	for _, r := range rows {
		pad := strings.Repeat(" ", width-ansi.StringWidth(r.use))
		app.UI.Info("  %s%s   %s", accent.Render(r.use), pad, r.short)
	}
}
