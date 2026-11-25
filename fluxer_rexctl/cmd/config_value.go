// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const backToDefault = "\x00default"

func readSecret(app *App, source, name string) (string, error) {
	if source == "-" && app.UI.Interactive() {
		var v string
		err := app.UI.Ask(nil, ui.Field{Title: fmt.Sprintf("What should %s be?", name), Value: &v, Secret: true,
			Validate: func(s string) error {
				if strings.TrimSpace(s) == "" {
					return errors.New("Type a value, or press Ctrl+C to stop.")
				}
				return nil
			}})
		return v, err
	}
	var data []byte
	var err error
	if source == "-" {
		data, err = io.ReadAll(app.UI.Stdin())
	} else {
		data, err = os.ReadFile(source)
	}
	if err != nil {
		if source == "-" {
			return "", ui.Failed("Couldn't read the value from stdin.", "Pass the value again.")
		}
		return "", ui.Failed(fmt.Sprintf("Couldn't read %s.", source), "Check the path and try again.")
	}
	value := strings.TrimRight(string(data), "\r\n")
	if value == "" {
		if source == "-" {
			return "", ui.Usage(fmt.Sprintf("The value for %s on stdin is empty.", name))
		}
		return "", ui.Usage(fmt.Sprintf("%s is empty.", source))
	}
	return value, nil
}

type settingPrompt struct {
	name     string
	help     string
	takes    string
	current  string
	options  []string
	secret   bool
	changed  bool
	validate func(string) error
}

func (v *configView) prompt(ctx context.Context, app *App, k settings.Key) (settingPrompt, error) {
	if k.Kind != settings.Instance {
		value := v.value(k)
		p := settingPrompt{name: k.Name, help: keyHelp(k), takes: settings.Describe(k), current: settings.Mask(k, value),
			options: k.Options, secret: k.Secret, validate: k.Validate}
		_, p.changed = v.inst.State.Settings[k.Name]
		if k.Type == "bool" {
			p.options = []string{"true", "false"}
		}
		return p, nil
	}
	values, err := instanceValues(ctx, app)
	if err != nil {
		return settingPrompt{}, adminapi.Explain(err)
	}
	ak, err := adminapi.FindConfigKey(values, k.Name)
	if err != nil {
		return settingPrompt{}, adminapi.Explain(err)
	}
	r := instanceRow(ak)
	p := settingPrompt{name: ak.Key, help: ak.Help, takes: r.Takes, current: adminapi.FormatValue(ak), options: ak.Options,
		secret: r.Secret, changed: r.Changed, validate: func(s string) error {
			_, err := adminapi.ParseConfigValue(ak, s)
			if err != nil {
				return errors.New("That isn't " + r.Takes + ".")
			}
			return nil
		}}
	if ak.Type == "boolean" {
		p.options = []string{"true", "false"}
	}
	return p, nil
}

func askSettingValue(ctx context.Context, app *App, v *configView, k settings.Key) (*string, error) {
	if !app.UI.Interactive() {
		return nil, &ui.ExitError{Code: ui.ExitUsage, Msg: fmt.Sprintf("Pass a value for %s.", k.Name), Fix: fmt.Sprintf("Run rexctl config get %s to see what it takes.", k.Name)}
	}
	p, err := v.prompt(ctx, app, k)
	if err != nil {
		return nil, err
	}
	return p.ask(app, false)
}

func (p settingPrompt) description() string {
	var lines []string
	if p.help != "" {
		lines = append(lines, ui.Sentence(p.help))
	}
	lines = append(lines, "Takes "+p.takes+".")
	switch {
	case p.secret && p.current != "":
		lines = append(lines, "It's set now. Leave it empty to keep it.")
	case p.current != "":
		lines = append(lines, "Now "+clip(p.current)+".")
	default:
		lines = append(lines, "It's empty now.")
	}
	return strings.Join(lines, "\n")
}

func (p settingPrompt) ask(app *App, offerDefault bool) (*string, error) {
	title := fmt.Sprintf("What should %s be?", p.name)
	var value string
	f := ui.Field{Title: title, Value: &value, Description: p.description(), Validate: p.check}
	switch {
	case len(p.options) > 0:
		for _, o := range p.options {
			f.Options = append(f.Options, ui.Option{Label: o, Value: o})
		}
		if offerDefault && p.changed {
			f.Options = append(f.Options, ui.Option{Label: "Back to the default", Value: backToDefault})
		}
		if slices.Contains(p.options, p.current) {
			f.Default = p.current
		}
		f.Validate = nil
	case p.secret:
		f.Secret = true
		f.Optional = p.current != ""
	default:
		f.Default = p.current
		f.Optional = offerDefault
		f.Placeholder = "empty goes back to the default"
		if !offerDefault {
			f.Placeholder = ""
		}
	}
	if err := app.UI.Ask(nil, f); err != nil {
		return nil, err
	}
	switch {
	case value == backToDefault:
		return nil, nil
	case value == "" && p.secret:
		return nil, errKeptSettings
	case value == "" && offerDefault:
		return nil, nil
	case value == "":
		return nil, errKeptSettings
	}
	return &value, nil
}

func (p settingPrompt) check(s string) error {
	if s == "" || p.validate == nil {
		return nil
	}
	if err := p.validate(s); err != nil {
		return errors.New(ui.Sentence(err.Error()))
	}
	return nil
}
