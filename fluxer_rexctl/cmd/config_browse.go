// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const changedGroup = "changed"

func browseConfig(c *cobra.Command, app *App) error {
	ctx := c.Context()
	group, lastGroup, lastKey := "", "", ""
	explained := false
	for {
		inst, err := app.InstanceRepairing(nil)
		if err != nil {
			return err
		}
		if moved := inst.Moved(); moved != nil {
			return movedConfig(app, moved)
		}
		v, err := configViewOf(ctx, app, inst)
		if err != nil {
			return err
		}
		rows, apiErr := v.rows(ctx, app)
		if !explained {
			explainInstanceKeys(app, v.inst, apiErr)
			explained = true
		}
		if group == "" {
			items := groupItems(rows)
			g, err := app.UI.Pick("Which settings do you want to look at?", items, cmp.Or(lastGroup, items[0].Value))
			if errors.Is(err, ui.ErrBack) {
				return nil
			}
			if err != nil || g == "" {
				return err
			}
			group, lastGroup, lastKey = g, g, ""
			continue
		}
		items := settingItems(rows, group)
		key, err := app.UI.Pick(groupTitle(group), items, cmp.Or(lastKey, items[0].Value))
		if err != nil && !errors.Is(err, ui.ErrBack) {
			return err
		}
		if key == "" {
			group = ""
			continue
		}
		lastKey = key
		if err := editFromBrowser(ctx, app, v, rows, key); err != nil {
			if ui.CodeOf(err) == ui.ExitInterrupted || errors.Is(err, context.Canceled) {
				return err
			}
			app.UI.ReportError(err)
		}
	}
}

func groupTitle(group string) string {
	if group == changedGroup {
		return "Settings you changed"
	}
	return group + " settings"
}

func groupItems(rows []configRow) []ui.PickItem {
	var items []ui.PickItem
	changed := 0
	for _, r := range rows {
		if r.Changed {
			changed++
		}
	}
	if changed > 0 {
		items = append(items, ui.PickItem{Value: changedGroup, Label: "Settings you changed", Note: plural(changed, "setting", "settings")})
	}
	for _, g := range settings.Groups {
		n := 0
		for _, r := range rows {
			if r.Group == g {
				n++
			}
		}
		if n > 0 {
			items = append(items, ui.PickItem{Value: g, Label: g, Note: plural(n, "setting", "settings")})
		}
	}
	return append(items, ui.PickItem{Value: "", Label: "Done", Hint: "leave the settings as they are"})
}

func settingItems(rows []configRow, group string) []ui.PickItem {
	var items []ui.PickItem
	for _, r := range rows {
		if group == changedGroup && !r.Changed || group != changedGroup && r.Group != group {
			continue
		}
		r := r
		value := clip(r.shown())
		if value == "" {
			value = "empty"
		}
		items = append(items, ui.PickItem{Value: r.Key, Label: r.Key, Note: value, Hint: ui.Sentence(r.Help), Group: r.Group, Detail: func(int) string { return settingDetail(r) }})
	}
	return append(items, ui.PickItem{Value: "", Label: "Back", Hint: "pick another group"})
}

func settingDetail(r configRow) string {
	lines := []string{}
	if r.Help != "" {
		lines = append(lines, ui.Sentence(r.Help))
	}
	lines = append(lines, "Takes "+r.Takes+".", r.Applies+".")
	if r.Default != nil {
		lines = append(lines, fmt.Sprintf("Default %v.", r.Default))
	}
	return strings.Join(lines, "\n")
}

func editFromBrowser(ctx context.Context, app *App, v *configView, rows []configRow, key string) error {
	k, err := v.lookup(key)
	if err != nil {
		return err
	}
	p, err := v.prompt(ctx, app, k)
	if err != nil {
		return err
	}
	value, err := p.ask(app, true)
	if errors.Is(err, errKeptSettings) {
		app.UI.Info("Nothing changed.")
		return nil
	}
	if err != nil {
		return err
	}
	applies := "Right away"
	for _, r := range rows {
		if r.Key == key {
			applies = r.Applies
		}
	}
	next := "the default"
	switch {
	case value != nil && p.secret:
		next = "a new secret"
	case value != nil:
		next = clip(*value)
	}
	now := p.current
	if now == "" {
		now = "empty"
	}
	app.UI.Card(ui.Card{Title: key, Rows: []ui.KV{{Key: "Now", Value: now}, {Key: "New", Value: next}, {Key: "Applies", Value: applies}}})
	ok, err := app.UI.ConfirmWith(ui.Question{Text: "Apply this change?", Default: true})
	if err != nil {
		return err
	}
	if !ok {
		app.UI.Info("Nothing changed.")
		return nil
	}
	return runConfigChange(ctx, app, []configChange{{name: key, value: value}}, nil)
}
