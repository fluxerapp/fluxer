// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"slices"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var (
	homeInstalled = []string{"status", "upgrade", "backup", "users", "invites", "doctor"}
	homeLegacy    = []string{"install", "status"}
	homeEmpty     = []string{"install", "restore"}
)

func runHome(ctx context.Context, app *App, c *cobra.Command) error {
	if !app.UI.Interactive() {
		return c.Help()
	}
	dir, err := app.Dir()
	if err != nil {
		return err
	}
	inst, err := instance.Find(dir)
	var names []string
	switch {
	case err == nil:
		homeGreet(app, fmt.Sprintf("Your instance is at %s.", ui.Inline(inst.Origin())))
		names = homeSuggested(inst)
	case errors.Is(err, instance.ErrLegacy):
		homeGreet(app, fmt.Sprintf("There's a Fluxer install from before rexctl in %s. %s", ui.Inline(dir), takeoverFixOffline))
		names = homeLegacy
	default:
		homeGreet(app, ui.WelcomeLine)
		names = homeEmpty
	}
	return homePick(ctx, app, c, names)
}

const homeEverything = "help"

var homeLabels = map[string]string{
	"install": "Install Fluxer",
	"restore": "Restore from a backup",
	"status":  "See how your instance is doing",
	"upgrade": "Upgrade Fluxer",
	"backup":  "Back up now",
	"users":   "Manage accounts",
	"invites": "Make invite links",
	"doctor":  "Check for problems",
	"start":   "Start Fluxer",
	"setup":   "Finish setting up",
}

func homePick(ctx context.Context, app *App, root *cobra.Command, names []string) error {
	var items []ui.PickItem
	for _, name := range names {
		sub, _, err := root.Find([]string{name})
		if err != nil || sub == root {
			continue
		}
		label := homeLabels[sub.Name()]
		if label == "" {
			label = sub.Short
		}
		items = append(items, ui.PickItem{Value: sub.Name(), Label: label, Note: "rexctl " + sub.Name(), Hint: sub.Short})
	}
	items = append(items, ui.PickItem{Value: homeEverything, Label: "Show every command", Note: "rexctl --help"})
	picked, err := app.UI.Pick("What do you want to do?", items, items[0].Value)
	if err != nil {
		return err
	}
	sub, _, err := root.Find([]string{picked})
	switch {
	case picked == homeEverything || err != nil || sub == root:
		return root.Help()
	case sub.RunE == nil:
		return sub.Help()
	}
	app.command = sub.CommandPath()
	sub.SetContext(ctx)
	return sub.RunE(sub, nil)
}

func homeGreet(app *App, line string) {
	if app.UI.ShowsRex() {
		app.UI.Rex(ui.Greeting, line)
		return
	}
	app.UI.Info("%s", line)
	app.UI.Info("")
}

func homeSuggested(inst *instance.Instance) []string {
	var first []string
	st := inst.State
	switch {
	case st.Upgrade != nil:
		first = append(first, "upgrade")
	case st.Stopped:
		first = append(first, "start")
	}
	if !st.SetupDone {
		first = append(first, "setup")
	}
	if list, err := backup.List(inst.BackupDir()); err == nil && len(list) == 0 {
		first = append(first, "backup")
	}
	out := first
	for _, name := range homeInstalled {
		if !slices.Contains(out, name) {
			out = append(out, name)
		}
	}
	return out
}
