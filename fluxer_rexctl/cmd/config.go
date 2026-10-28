// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"maps"
	"reflect"
	"slices"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const maxValueWidth = 60

func clip(s string) string {
	s = strings.Join(strings.Fields(s), " ")
	if n := len([]rune(s)); n > maxValueWidth {
		return fmt.Sprintf("%d characters", n)
	}
	return s
}

func describeType(k adminapi.ConfigKey) string {
	switch k.Type {
	case "list":
		return "a comma-separated list"
	case "json":
		return "JSON"
	case "secret":
		return "a secret. Pass - to read it from stdin"
	case "integer":
		return "a whole number"
	case "boolean":
		return "true or false"
	}
	if len(k.Options) > 0 {
		return ui.JoinOr(k.Options)
	}
	switch k.Type {
	case "number":
		return "a number"
	}
	return "text"
}

var deploymentTypes = map[string]string{
	"hostname": "a domain name, like chat.example.com",
	"address":  "an address and port, like 127.0.0.1:8080",
	"port":     "a port number",
	"list":     "a comma-separated list",
	"ip":       "an IP address, or nothing to let Fluxer find it",
	"url":      "a full address that starts with https://",
	"bool":     "true or false",
	"secret":   "a secret. Pass - to read it from stdin",
	"image":    "an image reference, like postgres:16-alpine",
	"registry": "a registry host, like registry.example.com",
	"endpoint": "an engine address, like " + settings.EngineExample(),
}

func describeDeployType(k settings.Key) string {
	if len(k.Options) > 0 {
		return ui.JoinOr(k.Options)
	}
	if k.Secret {
		return deploymentTypes["secret"]
	}
	if t, ok := deploymentTypes[k.Type]; ok {
		return t
	}
	return "text"
}

type configView struct {
	inst   *instance.Instance
	set    *stack.Set
	custom []byte
	vars   []string
	schema []adminapi.ConfigKey
}

func loadConfigView(app *App, repairing []string) (*configView, error) {
	inst, err := app.InstanceRepairing(repairing)
	if err != nil {
		return nil, err
	}
	set, err := deploy.ReleaseSet(inst)
	if err != nil {
		return nil, err
	}
	custom, err := deploy.CustomFile(inst)
	if err != nil {
		return nil, err
	}
	vars, err := settings.Vars(set, custom)
	if err != nil {
		return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't read %s.", instance.CustomCompose), "Fix the file, then try again.")
	}
	schema, err := adminapi.ConfigSchema()
	if err != nil {
		return nil, err
	}
	return &configView{inst: inst, set: set, custom: custom, vars: vars, schema: schema}, nil
}

func (v *configView) lookup(name string) (settings.Key, error) {
	k, err := settings.Lookup(v.set, v.vars, strings.TrimSpace(name), v.schema)
	var se *settings.Error
	if errors.As(err, &se) {
		return settings.Key{}, &ui.ExitError{Code: ui.ExitUsage, Msg: se.Problem, Fix: se.Fix, Err: err}
	}
	return k, err
}

func (v *configView) value(k settings.Key) string {
	st := v.inst.State
	if k.Name == settings.EngineHost {
		return st.Engine.Host
	}
	if value, ok := st.Settings[k.Name]; ok {
		return value
	}
	if k.Kind == settings.Raw {
		if value, ok := st.Secrets[k.Name]; ok {
			return value
		}
	}
	return v.fallback(k, v.inst)
}

func (v *configView) fallback(k settings.Key, inst *instance.Instance) string {
	switch k.Name {
	case settings.EdgeMode:
		return settings.EdgeBundled
	case settings.IPv6:
		return "false"
	}
	if len(k.Vars) == 0 {
		return ""
	}
	return inst.Setting(k.Vars[0])
}

func (v *configView) defaultOf(k settings.Key) string {
	st := *v.inst.State
	st.Settings = maps.Clone(st.Settings)
	delete(st.Settings, k.Name)
	tmp := *v.inst
	tmp.State = &st
	return v.fallback(k, &tmp)
}

func (v *configView) applies(k settings.Key) string {
	if k.Kind == settings.Instance {
		return "Right away"
	}
	if k.Name == settings.EngineHost {
		return "Next command"
	}
	return restartText(deploy.Promised(v.set, v.custom, k))
}

func partsOf(services []string) []string {
	var parts []string
	for _, p := range logParts {
		for _, svc := range p.services {
			if slices.Contains(services, svc) && !slices.Contains(parts, p.name) {
				parts = append(parts, p.name)
			}
		}
	}
	return parts
}

func extraRestartText(p deploy.Preview) string {
	promised := partsOf(p.Promised)
	if len(promised) > 2 {
		return ""
	}
	all := partsOf(append(slices.Clone(p.Restarts), p.Removes...))
	var extra []string
	for _, part := range all {
		if !slices.Contains(promised, part) {
			extra = append(extra, part)
		}
	}
	switch {
	case len(all) > 2:
		return "This restarts all of Fluxer."
	case len(extra) == 0:
		return "This also restarts other Fluxer services."
	}
	return "This also restarts " + ui.JoinAnd(extra) + "."
}

func restartText(services []string) string {
	parts := partsOf(services)
	switch {
	case len(parts) == 0:
		return "Right away"
	case len(parts) > 2:
		return "Restarts Fluxer"
	}
	return "Restarts " + ui.JoinAnd(parts)
}

type configRow struct {
	Key      string   `json:"key"`
	Kind     string   `json:"kind"`
	Type     string   `json:"type"`
	Value    any      `json:"value"`
	Default  any      `json:"default,omitempty"`
	Secret   bool     `json:"secret"`
	Restarts []string `json:"restarts"`
	Help     string   `json:"help,omitempty"`
}

func (v *configView) row(k settings.Key, reveal bool) configRow {
	value := v.value(k)
	shown := settings.Mask(k, value)
	if reveal {
		shown = value
	}
	r := configRow{Key: k.Name, Kind: "deployment", Type: k.Type, Value: shown, Secret: k.Secret, Help: k.Help, Restarts: []string{}}
	if k.Name != settings.EngineHost {
		r.Restarts = deploy.Promised(v.set, v.custom, k)
	}
	if def := v.defaultOf(k); def != "" {
		r.Default = settings.Mask(k, def)
	}
	return r
}

func (v *configView) deploymentKeys() []settings.Key {
	var out []settings.Key
	for _, k := range settings.Named() {
		if _, err := settings.Lookup(v.set, v.vars, k.Name, nil); err == nil {
			out = append(out, k)
		}
	}
	st := v.inst.State
	var raw []string
	for name := range st.Settings {
		raw = append(raw, name)
	}
	raw = append(raw, v.vars...)
	for name := range st.Secrets {
		raw = append(raw, name)
	}
	slices.Sort(raw)
	for _, name := range slices.Compact(raw) {
		k, err := settings.Lookup(v.set, v.vars, name, nil)
		if err == nil && k.Kind == settings.Raw {
			out = append(out, k)
		}
	}
	return out
}

func newConfigCmd(app *App) *cobra.Command {
	c := &cobra.Command{Use: "config", Short: "Read and change Fluxer's settings", Args: noArgs}
	c.AddCommand(newConfigListCmd(app), newConfigGetCmd(app), newConfigSetCmd(app), newConfigUnsetCmd(app), newConfigEditCmd(app), newConfigRenderCmd(app))
	return c
}

func newConfigListCmd(app *App) *cobra.Command {
	list := &cobra.Command{Use: "list", Short: "List every setting with its value and help", Args: noArgs}
	app.jsonFlag(list)
	list.RunE = func(c *cobra.Command, _ []string) error {
		ctx := c.Context()
		v, err := loadConfigView(app, nil)
		if err != nil {
			return err
		}
		var rows []configRow
		var table [][]string
		for _, k := range v.deploymentKeys() {
			r := v.row(k, false)
			if _, generated := v.inst.State.Secrets[k.Name]; generated && k.Kind == settings.Raw {
				if _, chosen := v.inst.State.Settings[k.Name]; !chosen {
					r.Value = "generated"
				}
			}
			rows = append(rows, r)
			table = append(table, []string{r.Key, clip(fmt.Sprint(r.Value)), v.applies(k), r.Help})
		}
		instanceKeys, apiErr := instanceValues(ctx, app)
		for _, k := range instanceKeys {
			rows = append(rows, configRow{Key: k.Key, Kind: "instance", Type: k.Type, Value: k.Value, Default: k.Default,
				Secret: k.Type == "secret", Restarts: []string{}, Help: k.Help})
			table = append(table, []string{k.Key, clip(adminapi.FormatValue(k)), "Right away", k.Help})
		}
		if app.Flags.JSON {
			return app.UI.JSON(rows)
		}
		app.UI.Table([]string{"Setting", "Value", "Applies", "Help"}, table)
		if errors.Is(apiErr, adminapi.ErrNoRelay) {
			app.UI.Info("The other settings need a newer Fluxer release. Run rexctl upgrade to see them.")
		} else if apiErr != nil {
			app.UI.Info("The other settings need Fluxer running. Run rexctl start to see them.")
			app.UI.Detail("%v", apiErr)
		}
		return nil
	}
	return list
}

func instanceValues(ctx context.Context, app *App) ([]adminapi.ConfigKey, error) {
	api, _, err := app.Admin(ctx)
	if err != nil {
		return nil, err
	}
	return api.ConfigValues(ctx)
}

func newConfigGetCmd(app *App) *cobra.Command {
	var reveal bool
	get := &cobra.Command{Use: "get <key>", Short: "Show one setting", Args: exactArgs("a setting name")}
	app.jsonFlag(get)
	get.Flags().BoolVar(&reveal, "reveal", false, "print a secret rexctl keeps")
	get.RunE = func(c *cobra.Command, args []string) error {
		v, err := loadConfigView(app, nil)
		if err != nil {
			return err
		}
		k, err := v.lookup(args[0])
		if err != nil {
			return err
		}
		if k.Kind == settings.Instance {
			if reveal {
				return ui.Usage(fmt.Sprintf("Fluxer doesn't show %s once it's saved.", k.Name))
			}
			return getInstanceKey(c, app, k.Name)
		}
		r := v.row(k, reveal)
		if app.Flags.JSON {
			return app.UI.JSON(r)
		}
		value := v.value(k)
		if reveal {
			app.UI.Result(value)
			return nil
		}
		shown := settings.Mask(k, value)
		if k.Secret && value == "" {
			shown = "not set"
		}
		rows := [][]string{{"Value", shown}, {"Takes", describeDeployType(k)}}
		if r.Default != nil {
			rows = append(rows, []string{"Default", fmt.Sprint(r.Default)})
		}
		rows = append(rows, []string{"Applies", v.applies(k)})
		if k.Help != "" {
			rows = append(rows, []string{"Help", k.Help})
		}
		app.UI.Table([]string{"Setting", k.Name}, rows)
		return nil
	}
	return get
}

func getInstanceKey(c *cobra.Command, app *App, name string) error {
	return adminRead(c, app, func(ctx context.Context, api *adminapi.Client, _ *instance.Instance) error {
		values, err := api.ConfigValues(ctx)
		if err != nil {
			return err
		}
		k, err := adminapi.FindConfigKey(values, name)
		if err != nil {
			return err
		}
		if app.Flags.JSON {
			return app.UI.JSON(configRow{Key: k.Key, Kind: "instance", Type: k.Type, Value: k.Value, Default: k.Default,
				Secret: k.Type == "secret", Restarts: []string{}, Help: k.Help})
		}
		rows := [][]string{{"Value", adminapi.FormatValue(k)}, {"Takes", describeType(k)}}
		if k.Default != nil {
			rows = append(rows, []string{"Default", fmt.Sprint(k.Default)})
		}
		rows = append(rows, []string{"Applies", "Right away"}, []string{"Help", k.Help})
		app.UI.Table([]string{"Setting", k.Key}, rows)
		return nil
	})
}

type configChange struct {
	name  string
	value *string
}

func parseSetArgs(app *App, args []string) ([]configChange, error) {
	var out []configChange
	if len(args) == 2 && !strings.Contains(args[0], "=") {
		out = []configChange{{name: args[0], value: &args[1]}}
	} else {
		for _, a := range args {
			name, value, ok := strings.Cut(a, "=")
			if !ok || strings.TrimSpace(name) == "" {
				return nil, ui.Usage(fmt.Sprintf("%q isn't a setting=value pair.", a))
			}
			out = append(out, configChange{name: name, value: &value})
		}
	}
	stdin := false
	seen := map[string]bool{}
	for i, ch := range out {
		out[i].name = strings.TrimSpace(ch.name)
		if seen[out[i].name] {
			return nil, ui.Usage(fmt.Sprintf("%s is in the command twice.", out[i].name))
		}
		seen[out[i].name] = true
		if *ch.value != "-" {
			continue
		}
		if stdin {
			return nil, ui.Usage("Only one setting can read its value from stdin.")
		}
		stdin = true
		data, err := io.ReadAll(app.UI.Stdin())
		if err != nil {
			return nil, ui.Failed("Couldn't read the value from stdin.", "Pass the value again.")
		}
		value := strings.TrimRight(string(data), "\r\n")
		if value == "" {
			return nil, ui.Usage(fmt.Sprintf("The value for %s on stdin is empty.", out[i].name))
		}
		out[i].value = &value
	}
	return out, nil
}

func newConfigSetCmd(app *App) *cobra.Command {
	set := &cobra.Command{
		Use:   "set <key> <value> | <key>=<value>...",
		Short: "Change one or more settings",
		Args: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return ui.Usage("Pass a setting and its value.")
			}
			return nil
		},
	}
	set.RunE = func(c *cobra.Command, args []string) error {
		changes, err := parseSetArgs(app, args)
		if err != nil {
			return err
		}
		return runConfigChange(c.Context(), app, changes)
	}
	return set
}

func newConfigUnsetCmd(app *App) *cobra.Command {
	unset := &cobra.Command{
		Use:   "unset <key>...",
		Short: "Go back to the default for one or more settings",
		Args: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return ui.Usage("Pass a setting name.")
			}
			return nil
		},
	}
	unset.RunE = func(c *cobra.Command, args []string) error {
		var changes []configChange
		for _, a := range args {
			changes = append(changes, configChange{name: strings.TrimSpace(a)})
		}
		return runConfigChange(c.Context(), app, changes)
	}
	return unset
}

func runConfigChange(ctx context.Context, app *App, changes []configChange) error {
	typed := make([]string, 0, len(changes))
	for _, ch := range changes {
		typed = append(typed, ch.name)
	}
	v, err := loadConfigView(app, typed)
	if err != nil {
		return err
	}
	deployment := map[string]*string{}
	var admin []adminChange
	for _, ch := range changes {
		k, err := v.lookup(ch.name)
		if err != nil {
			return err
		}
		if k.Kind != settings.Instance {
			deployment[k.Name] = ch.value
			continue
		}
		ac, err := parseAdminChange(v.schema, k.Name, ch.value)
		if err != nil {
			return err
		}
		admin = append(admin, ac)
	}
	finish := func(change *deploy.Change) error {
		sent, err := applyAdminChanges(ctx, app, v.inst, admin)
		if err != nil {
			return err
		}
		reportConfigChange(app, change, deployment, sent, v.inst.State.Upgrade != nil)
		names := slices.Collect(maps.Keys(deployment))
		for _, ac := range admin {
			names = append(names, ac.key.Key)
		}
		removed, err := instance.SettleEnv(v.inst, names)
		if err != nil {
			return ui.Wrap(err, "Couldn't remove the .env file rexctl doesn't read.", "Delete "+v.inst.Path(instance.EnvFileName)+" yourself.")
		}
		if removed != "" {
			app.UI.Info("Removed %s. You changed every setting in it with rexctl.", removed)
		}
		return nil
	}
	if len(deployment) == 0 {
		return app.Locked(ctx, v.inst, func() error { return finish(nil) })
	}
	return app.ChangeSettings(ctx, v.inst, func(e *engine.Engine) error {
		change, err := applySettings(ctx, app, v.inst, e, deployment, "Applying the new settings")
		if err != nil {
			return err
		}
		return finish(change)
	})
}

func reportConfigChange(app *App, change *deploy.Change, values map[string]*string, admin []adminChange, upgrading bool) {
	var keys []string
	if change != nil && !change.Unchanged {
		keys = change.Keys
	}
	switch {
	case len(keys) == 0 && len(admin) == 0:
		app.UI.OK("Nothing to change.")
		return
	case len(keys)+len(admin) > 1:
		app.UI.OK("Saved %d settings.", len(keys)+len(admin))
	case len(admin) == 1:
		app.UI.OK("%s", admin[0].done())
	default:
		app.UI.OK("%s", deploymentDone(keys[0], values[keys[0]]))
	}
	switch {
	case len(keys) > 0 && upgrading:
		app.UI.Info("rexctl upgrade uses the new setting when it finishes.")
	case len(keys) > 0 && !change.Applied:
		app.UI.Info("Fluxer uses the new settings the next time it starts.")
	}
}

func deploymentDone(name string, value *string) string {
	switch {
	case value == nil:
		return fmt.Sprintf("%s is back to its default.", name)
	case settings.SecretName(name) || name == settings.StorageSecret:
		return fmt.Sprintf("Saved %s.", name)
	}
	return fmt.Sprintf("Set %s to %s.", name, clip(settings.Mask(settings.Key{}, *value)))
}

type adminChange struct {
	key   adminapi.ConfigKey
	value any
	patch map[string]any
	mode  string
}

func (a adminChange) done() string {
	switch {
	case a.mode != "":
		return registrationDone[a.mode]
	case a.patch != nil && a.value == nil:
		return fmt.Sprintf("Cleared %s.", a.key.Key)
	case a.key.Type == "secret":
		return fmt.Sprintf("Saved %s.", a.key.Key)
	}
	k := a.key
	k.Value = a.value
	return fmt.Sprintf("Set %s to %s.", k.Key, clip(adminapi.FormatValue(k)))
}

func parseAdminChange(schema []adminapi.ConfigKey, name string, raw *string) (adminChange, error) {
	k, err := adminapi.FindConfigKey(schema, name)
	if err != nil {
		return adminChange{}, adminapi.Explain(err)
	}
	if raw == nil {
		if !k.Nullable {
			return adminChange{}, ui.Refused(fmt.Sprintf("Fluxer can't clear %s.", k.Key), fmt.Sprintf("Set another value with rexctl config set %s <value>.", k.Key))
		}
		return adminChange{key: k, patch: adminapi.ConfigPatch(k.Key, nil)}, nil
	}
	if k.Key == adminapi.RegistrationModeKey {
		mode := strings.ToLower(strings.TrimSpace(*raw))
		patch, err := adminapi.RegistrationPatch(mode)
		if err != nil {
			return adminChange{}, ui.Usage(fmt.Sprintf("%s needs one of %s.", k.Key, ui.JoinOr(adminapi.RegistrationModes)))
		}
		return adminChange{key: k, value: mode, patch: patch, mode: mode}, nil
	}
	value, err := adminapi.ParseConfigValue(k, *raw)
	if err != nil {
		return adminChange{}, adminapi.Explain(err)
	}
	return adminChange{key: k, value: value, patch: adminapi.ConfigPatch(k.Key, value)}, nil
}

func applyAdminChanges(ctx context.Context, app *App, inst *instance.Instance, changes []adminChange) ([]adminChange, error) {
	pendingImport := inst.State.AdminImport != nil && inst.State.AdminImport.SentAt == ""
	if len(changes) == 0 && !pendingImport {
		return nil, nil
	}
	api, _, err := app.Admin(ctx)
	if err != nil {
		if len(changes) == 0 {
			return nil, nil
		}
		return nil, err
	}
	if pendingImport {
		if err := deploy.SendAdminImport(ctx, inst, api); err != nil {
			app.UI.Detail("%v", err)
		}
	}
	if len(changes) == 0 {
		return nil, nil
	}
	current, err := api.ConfigValues(ctx)
	if err != nil {
		return nil, adminapi.Explain(err)
	}
	patch := map[string]any{}
	var sent []adminChange
	for _, ch := range changes {
		if ch.mode == "" && ch.key.Type != "secret" {
			if k, err := adminapi.FindConfigKey(current, ch.key.Key); err == nil && sameValue(k.Value, ch.value) {
				continue
			}
		}
		adminapi.MergePatch(patch, ch.patch)
		sent = append(sent, ch)
	}
	if len(sent) == 0 {
		return nil, nil
	}
	if err := api.PatchConfig(ctx, patch); err != nil {
		return nil, adminapi.Explain(err)
	}
	return sent, nil
}

func sameValue(a, b any) bool {
	ja, err1 := json.Marshal(a)
	jb, err2 := json.Marshal(b)
	if err1 != nil || err2 != nil {
		return reflect.DeepEqual(a, b)
	}
	var na, nb any
	if json.Unmarshal(ja, &na) != nil || json.Unmarshal(jb, &nb) != nil {
		return false
	}
	return reflect.DeepEqual(na, nb)
}

type setResult struct {
	change *deploy.Change
	err    error
}

func applySettings(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, changes map[string]*string, title string) (*deploy.Change, error) {
	closePorts, err := openPorts(ctx, app, inst, changes)
	if err != nil {
		return nil, err
	}
	ask := make(chan deploy.Preview)
	answer := make(chan bool)
	done := make(chan setResult, 1)
	var logPath string
	opts := deploy.ApplyOptions{
		Failed: func(p *engine.Project, err error) {
			logPath = writeFailureLog(ctx, inst, p, "start.log", err, nil)
		},
		Wait: true,
		Confirm: func(p deploy.Preview) bool {
			ask <- p
			return <-answer
		},
		OnEvent: func(ev engine.Event) {
			if ev.Text != "" {
				app.UI.Detail("%s %s", ev.ID, ev.Text)
			}
		},
	}
	go func() {
		change, err := deploy.Set(ctx, inst, e, changes, opts)
		done <- setResult{change, err}
	}()
	var r setResult
	select {
	case r = <-done:
	case p := <-ask:
		ok, err := confirmPreview(app, p)
		answer <- ok && err == nil
		if err != nil {
			<-done
			return nil, err
		}
		if ok {
			_ = app.UI.Step(ctx, title, func(context.Context, *ui.Step) error {
				r = <-done
				return nil
			})
		} else {
			r = <-done
		}
	}
	if errors.Is(r.err, deploy.ErrDeclined) {
		return nil, &ui.ExitError{Code: ui.ExitRefused, Msg: "rexctl left your settings as they were.", Err: r.err}
	}
	if r.err != nil {
		return nil, settingsFailure(r.change, r.err, logPath)
	}
	if r.change.Applied && !r.change.Unchanged {
		closePorts()
	}
	return r.change, nil
}

func settingsFailure(change *deploy.Change, err error, logPath string) error {
	var ee *ui.ExitError
	if errors.As(err, &ee) {
		return err
	}
	switch {
	case change != nil && change.Reverted:
		fix := "Run rexctl logs to see why."
		if logPath != "" {
			fix = "Read " + logPath + " to see why."
		}
		return ui.Wrap(err, "Fluxer didn't start with the new settings. rexctl put the old ones back.", fix)
	case change != nil:
		return ui.Wrap(err, "Fluxer didn't start with the new settings, and rexctl couldn't put the old ones back.", "Run rexctl doctor to see what's wrong.")
	}
	return engineFailure(err)
}

func confirmPreview(app *App, p deploy.Preview) (bool, error) {
	if !p.NeedsConfirm() {
		return true, nil
	}
	for _, note := range p.Notes() {
		app.UI.Info("%s", note)
	}
	if p.Larger() {
		if len(p.Networks) > 0 {
			app.UI.Info("Fluxer restarts completely to change its network.")
		} else {
			if text := extraRestartText(p); text != "" {
				app.UI.Info("%s", text)
			}
		}
		if app.UI.Verbose() {
			app.UI.Detail("restarts: %s", strings.Join(p.Restarts, ", "))
			app.UI.Detail("removes: %s", strings.Join(p.Removes, ", "))
			app.UI.Detail("networks: %s", strings.Join(p.Networks, ", "))
		}
	}
	return app.UI.Confirm("Go ahead?", true)
}

func openPorts(ctx context.Context, app *App, inst *instance.Instance, changes map[string]*string) (func(), error) {
	nothing := func() {}
	opened, closed, err := deploy.PortChanges(inst, changes)
	if err != nil || len(opened) == 0 {
		return nothing, err
	}
	fw, err := host.DetectFirewall(ctx)
	if err != nil || fw == host.FirewallNone {
		return nothing, nil
	}
	blocked, err := host.ClosedPorts(ctx, fw, opened)
	if err != nil {
		return nothing, nil
	}
	if len(blocked) > 0 {
		ok, err := app.UI.Confirm(fmt.Sprintf("Open port %s in the firewall on this machine?", deploy.PortList(blocked)), true)
		if err != nil {
			return nothing, err
		}
		if !ok {
			app.UI.Warn(fmt.Sprintf("Port %s stays closed in the firewall on this machine.", deploy.PortList(blocked)), "Open it before people connect.")
		} else if err := host.OpenPorts(ctx, fw, blocked); err != nil {
			return nothing, ui.Wrap(err, "Couldn't open the port.", fmt.Sprintf("Open port %s in the firewall yourself, then try again.", deploy.PortList(blocked)))
		}
	}
	return func() {
		ok, err := app.UI.Confirm(fmt.Sprintf("Close port %s in the firewall? Fluxer doesn't use it any more.", deploy.PortList(closed)), true)
		if err != nil || !ok {
			return
		}
		if err := host.ClosePorts(ctx, fw, closed); err != nil {
			app.UI.Warn(fmt.Sprintf("Couldn't close port %s.", deploy.PortList(closed)), "Close it in the firewall yourself.")
		}
	}, nil
}

func newConfigRenderCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:    "render",
		Short:  "Print the Compose project rexctl runs, with secrets hidden",
		Hidden: true,
		Args:   noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			s, err := deploy.Spec(inst)
			if err != nil {
				return err
			}
			for name, value := range s.Env {
				_, generated := inst.State.Secrets[name]
				if generated || settings.SecretName(name) {
					s.Env[name] = settings.Masked
				} else {
					s.Env[name] = settings.Mask(settings.Key{}, value)
				}
			}
			var e engine.Engine
			p, err := e.Load(c.Context(), s)
			if err != nil {
				return ui.Wrap(err, "rexctl can't read Fluxer's files.", "Run rexctl doctor to see what's wrong.")
			}
			out, err := p.Model.MarshalYAML()
			if err != nil {
				return err
			}
			app.UI.Result(string(out))
			return nil
		},
	}
}
