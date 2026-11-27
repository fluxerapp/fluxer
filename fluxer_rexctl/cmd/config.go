// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"maps"
	"reflect"
	"slices"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
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
		return "a secret"
	case "integer":
		return "a whole number"
	case "boolean":
		return "true or false"
	}
	if len(k.Options) > 0 {
		return ui.JoinOr(k.Options)
	}
	if k.Type == "number" {
		return "a number"
	}
	return "text"
}

type configView struct {
	inst   *instance.Instance
	rt     rt.Runtime
	set    *stack.Set
	custom []byte
	vars   []string
	schema []adminapi.ConfigKey
}

func loadConfigView(ctx context.Context, app *App, repairing []string) (*configView, error) {
	inst, err := app.InstanceRepairing(repairing)
	if err != nil {
		return nil, err
	}
	return configViewOf(ctx, app, inst)
}

func configViewOf(ctx context.Context, app *App, inst *instance.Instance) (*configView, error) {
	r, err := app.Runtime(ctx, inst)
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
		return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't read %s.", instance.CustomCompose), "Run rexctl config edit --compose to fix it.")
	}
	schema, err := adminapi.ConfigSchema()
	if err != nil {
		return nil, err
	}
	return &configView{inst: inst, rt: r, set: set, custom: custom, vars: settings.WithHeld(vars, inst.State.Settings), schema: schema}, nil
}

func (v *configView) lookup(name string) (settings.Key, error) {
	k, err := settings.Lookup(v.set, v.vars, strings.TrimSpace(name), v.schema)
	var se *settings.Error
	if errors.As(err, &se) {
		return settings.Key{}, &ui.ExitError{Code: ui.ExitUsage, Msg: se.Problem, Fix: se.Fix, Err: err}
	}
	if err != nil {
		return k, err
	}
	if ok, why := v.rt.Settings().Supports(k.Name); !ok {
		return settings.Key{}, &ui.ExitError{Code: ui.ExitRefused, Msg: why, Fix: "Run rexctl config list to see the settings this instance has.", Err: rt.ErrUnsupported}
	}
	return k, nil
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
	case settings.SignedMedia:
		if mode := inst.Setting(settings.SignatureModeVar); mode != "" {
			return mode
		}
		return "off"
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
	return restartText(v.restarts(k))
}

func (v *configView) restarts(k settings.Key) []string {
	return partsOf(deploy.Promised(v.set, v.custom, k))
}

var rexctlKeyHelp = map[string]string{
	stack.SettingsLoadedVar: "rexctl sets this. Docker Compose refuses to start without it.",
	instance.SetupTokenVar:  "The token the setup wizard asks for before it creates the first admin account. rexctl generates it when it installs Fluxer, and rexctl setup token shows it.",
}

func keyHelp(k settings.Key) string {
	if k.Help != "" || k.Kind != settings.Raw {
		return k.Help
	}
	if help := rexctlKeyHelp[k.Name]; help != "" {
		return help
	}
	return instance.EnvHelp(k.Name)
}

func firstSentence(text string) string {
	if i := strings.Index(text, ". "); i >= 0 {
		return text[:i+1]
	}
	return text
}

type configRow struct {
	Key      string   `json:"key"`
	Kind     string   `json:"kind"`
	Type     string   `json:"type"`
	Value    any      `json:"value"`
	Default  any      `json:"default,omitempty"`
	Secret   bool     `json:"secret"`
	Changed  bool     `json:"changed"`
	Group    string   `json:"group"`
	Applies  string   `json:"applies"`
	Restarts []string `json:"restarts"`
	Takes    string   `json:"takes"`
	Help     string   `json:"help,omitempty"`
	Options  []string `json:"options,omitempty"`
	common   bool
}

func (r configRow) shown() string {
	if r.Value == nil {
		return ""
	}
	if set, ok := r.Value.(bool); ok && r.Secret {
		if set {
			return "set"
		}
		return ""
	}
	if s, ok := r.Value.(string); ok {
		return s
	}
	b, err := json.Marshal(r.Value)
	if err != nil {
		return fmt.Sprint(r.Value)
	}
	return string(b)
}

func (v *configView) row(k settings.Key, reveal bool) configRow {
	value := v.value(k)
	shown := settings.Mask(k, value)
	if reveal {
		shown = value
	}
	r := configRow{Key: k.Name, Kind: "deployment", Type: k.Type, Value: shown, Secret: k.Secret, Help: keyHelp(k),
		Restarts: []string{}, Group: settings.Group(k.Name), Applies: v.applies(k), Takes: settings.Describe(k), Options: k.Options,
		common: k.Kind != settings.Raw && (!strings.HasPrefix(k.Name, "images.") || k.Name == settings.ImagesRegistry)}
	if section := instance.EnvSection(k.Name); k.Kind == settings.Raw && section != "" {
		r.Group = settings.SectionGroup(section)
	}
	if k.Name != settings.EngineHost {
		r.Restarts = v.restarts(k)
		if r.Restarts == nil {
			r.Restarts = []string{}
		}
	}
	if def := v.defaultOf(k); def != "" {
		r.Default = settings.Mask(k, def)
	}
	_, chosen := v.inst.State.Settings[k.Name]
	r.Changed = chosen || k.Name == settings.EngineHost && v.inst.State.Engine.Host != ""
	return r
}

func instanceRow(k adminapi.ConfigKey) configRow {
	r := configRow{Key: k.Key, Kind: "instance", Type: k.Type, Value: k.Value, Default: k.Default, Secret: k.Type == "secret",
		Restarts: []string{}, Help: k.Help, Group: settings.Group(k.Key), Applies: "Right away", Takes: describeType(k), Options: k.Options,
		common: slices.Contains(commonInstanceKeys, k.Key)}
	r.Changed = k.Default != nil && !sameValue(k.Value, k.Default)
	if r.Secret {
		r.Changed = k.Value == true
	}
	return r
}

func (v *configView) deploymentKeys() []settings.Key {
	var out []settings.Key
	supported := func(name string) bool {
		ok, _ := v.rt.Settings().Supports(name)
		return ok
	}
	for _, k := range settings.Named() {
		if k.Hidden {
			continue
		}
		if _, err := settings.Lookup(v.set, v.vars, k.Name, nil); err == nil && supported(k.Name) {
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
		if name == stack.SettingsLoadedVar || !supported(name) {
			continue
		}
		k, err := settings.Lookup(v.set, v.vars, name, nil)
		if err == nil && k.Kind == settings.Raw {
			out = append(out, k)
		}
	}
	return out
}

func (v *configView) rows(ctx context.Context, app *App) ([]configRow, error) {
	var rows []configRow
	for _, k := range v.deploymentKeys() {
		r := v.row(k, false)
		if _, generated := v.inst.State.Secrets[k.Name]; generated && k.Kind == settings.Raw {
			if _, chosen := v.inst.State.Settings[k.Name]; !chosen {
				r.Value = "generated"
			}
		}
		rows = append(rows, r)
	}
	instanceKeys, apiErr := instanceValues(ctx, app)
	for _, k := range instanceKeys {
		rows = append(rows, instanceRow(k))
	}
	return rows, apiErr
}

var commonInstanceKeys = []string{
	adminapi.RegistrationModeKey, "registration.allowed_email_domains", "captcha.provider",
	"accounts.date_of_birth", "accounts.inactivity_deletion.mode",
	"app_public.branding.product_name", "app_public.branding.theme_color", "app_public.branding.logo_url", "app_public.branding.icon_url",
	"app_public.legal.terms_url", "app_public.legal.privacy_url", "app_public.legal.guidelines_url",
	"integrations.email.delivery", "integrations.email.from_email", "integrations.email.from_name",
	"integrations.email.smtp.host", "integrations.email.smtp.port", "integrations.email.smtp.username", "integrations.email.smtp.password",
	"sso.enabled", "policy.guild_create_access", "policy.direct_messages_disabled", "media.attachment_decay.enabled", "notice", "voice_p2p.enabled",
}

type listFilter struct {
	text    string
	changed bool
	all     bool
}

func (f listFilter) keep(r configRow) bool {
	if f.changed && !r.Changed {
		return false
	}
	if f.text == "" {
		return f.all || f.changed || r.common || r.Changed
	}
	t := strings.ToLower(f.text)
	return strings.Contains(strings.ToLower(r.Key), t) || strings.Contains(strings.ToLower(r.Help), t) || strings.EqualFold(r.Group, f.text)
}

func newConfigCmd(app *App) *cobra.Command {
	c := &cobra.Command{
		Use:   "config",
		Short: "Read and change Fluxer's settings",
		Long:  "Read and change Fluxer's settings. On a terminal, rexctl config opens a browser of every setting, grouped by what it changes. Pick one to see what it takes and change it.",
		Example: `rexctl config
rexctl config list email
rexctl config set registration.mode invite`,
		Args: noArgs,
	}
	app.jsonFlag(c)
	c.RunE = func(c *cobra.Command, _ []string) error {
		if app.UI.Interactive() && !app.Flags.JSON {
			return browseConfig(c, app)
		}
		return listConfig(c.Context(), app, listFilter{})
	}
	c.AddCommand(newConfigListCmd(app), newConfigGetCmd(app), newConfigSetCmd(app), newConfigUnsetCmd(app), newConfigEditCmd(app), newConfigRenderCmd(app), newConfigRotateCmd(app))
	return c
}

func newConfigListCmd(app *App) *cobra.Command {
	var f listFilter
	list := &cobra.Command{
		Use:   "list [filter]",
		Short: "List settings with their values, grouped by what they change",
		Long:  "List the settings people change most and the ones you changed, with their values, grouped into Network, Email, Voice, Storage, Accounts, Branding and Advanced.\n\nPass --all to list every setting. Pass a word to search every setting for ones whose name or help has it, or a group name to show one group.",
		Example: `rexctl config list
rexctl config list --all
rexctl config list voice
rexctl config list --changed`,
		Args: maxArgs(1),
	}
	list.Flags().BoolVar(&f.changed, "changed", false, "only show settings that differ from their defaults")
	list.Flags().BoolVar(&f.all, "all", false, "list every setting")
	app.jsonFlag(list)
	list.RunE = func(c *cobra.Command, args []string) error {
		if len(args) == 1 {
			f.text = strings.TrimSpace(args[0])
		}
		return listConfig(c.Context(), app, f)
	}
	return list
}

func listConfig(ctx context.Context, app *App, f listFilter) error {
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
	all, apiErr := v.rows(ctx, app)
	rows := []configRow{}
	for _, r := range all {
		if f.keep(r) {
			rows = append(rows, r)
		}
	}
	if app.Flags.JSON {
		return app.UI.JSON(rows)
	}
	if len(rows) == 0 {
		switch {
		case f.changed && f.text == "":
			app.UI.Info("Every setting has its default value.")
		case f.changed:
			app.UI.Info("None of the settings you changed match %s. Run rexctl config list %s to see them all.", f.text, f.text)
		default:
			app.UI.Info("No setting matches %s. Run rexctl config list --all to see every setting.", f.text)
		}
	}
	for _, group := range settings.Groups {
		var table [][]string
		for _, r := range rows {
			if r.Group == group {
				table = append(table, []string{r.Key, clip(r.shown()), r.Applies, firstSentence(r.Help)})
			}
		}
		if len(table) == 0 {
			continue
		}
		app.UI.Section(group)
		app.UI.Table([]string{"Setting", "Value", "Applies", "Help"}, table)
	}
	if hidden := len(all) - len(rows); f.text == "" && !f.all && !f.changed && hidden > 0 {
		app.UI.Info("These are the settings people change most. Run rexctl config list --all to see %d more.", hidden)
	}
	explainInstanceKeys(app, v.inst, apiErr)
	return nil
}

func movedConfig(app *App, moved *instance.MovedError) error {
	if app.Flags.JSON {
		return app.UI.JSON([]configRow{})
	}
	app.UI.Info("Fluxer runs from %s now, and its settings live with that instance. Run %s to see them.", moved.To, runThere(moved.To, "config", "list"))
	return nil
}

func explainInstanceKeys(app *App, inst *instance.Instance, apiErr error) {
	switch {
	case apiErr == nil:
	case errors.Is(apiErr, adminapi.ErrNoRelay):
		app.UI.Info("The other settings need a newer Fluxer release. Run rexctl upgrade to see them.")
	case adminapi.PermissionDenied(apiErr):
		app.UI.Info("The other settings need root on this machine. Run %s to see them.", sudoAgain(inst.Dir))
	default:
		app.UI.Info("The other settings need Fluxer running. Run rexctl start to see them.")
		app.UI.Detail("%v", apiErr)
	}
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
	get := &cobra.Command{
		Use:   "get <setting>",
		Short: "Show a setting, what it takes and when it applies",
		Example: `rexctl config get edge.bind
rexctl config get storage.secret_key --reveal`,
		Args: exactArgs("a setting name"),
	}
	app.jsonFlag(get)
	get.Flags().BoolVar(&reveal, "reveal", false, "print a secret rexctl keeps")
	get.RunE = func(c *cobra.Command, args []string) error {
		ctx := c.Context()
		v, err := loadConfigView(ctx, app, nil)
		if err != nil {
			return err
		}
		k, err := v.lookup(args[0])
		if err != nil {
			return err
		}
		if k.Kind == settings.Instance {
			return getInstanceKey(ctx, app, v, k.Name, reveal)
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
		showSetting(app, r, shown)
		return nil
	}
	return get
}

func showSetting(app *App, r configRow, shown string) {
	rows := [][]string{{"Value", shown}, {"Takes", r.Takes}}
	if r.Default != nil {
		rows = append(rows, []string{"Default", fmt.Sprint(r.Default)})
	}
	rows = append(rows, []string{"Applies", r.Applies}, []string{"Group", r.Group})
	if r.Help != "" {
		rows = append(rows, []string{"Help", r.Help})
	}
	app.UI.Table([]string{"Setting", r.Key}, rows)
}

func getInstanceKey(ctx context.Context, app *App, v *configView, name string, reveal bool) error {
	values, err := instanceValues(ctx, app)
	if err != nil {
		return adminapi.Explain(err)
	}
	k, err := adminapi.FindConfigKey(values, name)
	if err != nil {
		return adminapi.Explain(err)
	}
	if reveal && k.Type == "secret" {
		return ui.Refused(fmt.Sprintf("Fluxer doesn't show %s once it's saved.", k.Key), fmt.Sprintf("Set a new value with rexctl config set %s.", k.Key))
	}
	r := instanceRow(k)
	if app.Flags.JSON {
		return app.UI.JSON(r)
	}
	if reveal {
		app.UI.Result(adminapi.FormatValue(k))
		return nil
	}
	showSetting(app, r, adminapi.FormatValue(k))
	return nil
}

type configChange struct {
	name  string
	value *string
}

func parseSetArgs(c *cobra.Command, app *App, v *configView, args []string) ([]configChange, error) {
	var out []configChange
	switch {
	case len(args) == 1 && !strings.Contains(args[0], "="):
		k, err := v.lookup(args[0])
		if err != nil {
			return nil, err
		}
		value, err := askSettingValue(c.Context(), app, v, k)
		if err != nil {
			return nil, err
		}
		return []configChange{{name: k.Name, value: value}}, nil
	case len(args) == 2 && !strings.Contains(args[0], "="):
		out = []configChange{{name: args[0], value: &args[1]}}
	default:
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
		value, err := readSecret(app, "-", out[i].name)
		if err != nil {
			return nil, err
		}
		out[i].value = &value
	}
	return out, nil
}

func newConfigSetCmd(app *App) *cobra.Command {
	set := &cobra.Command{
		Use:   "set <setting> [value] | <setting>=<value>...",
		Short: "Change one or more settings",
		Long:  "Change one or more settings. Leave the value out on a terminal and rexctl asks for it, with what the setting takes and its current value. Pass - as the value to read a secret from stdin, which keeps it out of your shell history.",
		Example: `rexctl config set registration.mode invite
rexctl config set edge.bind=127.0.0.1:8081 edge.trusted_proxies=10.0.0.5
rexctl config set storage.secret_key -`,
		Args: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return ui.Usage("Pass a setting and its value.")
			}
			return nil
		},
	}
	var migrate, noMigrate bool
	set.Flags().BoolVar(&migrate, "migrate-identities", false, "when sso.issuer changes, move SSO links to the new issuer")
	set.Flags().BoolVar(&noMigrate, "no-migrate-identities", false, "when sso.issuer changes, leave SSO links on the old issuer")
	set.MarkFlagsMutuallyExclusive("migrate-identities", "no-migrate-identities")
	set.RunE = func(c *cobra.Command, args []string) error {
		v, err := loadConfigView(c.Context(), app, nil)
		if err != nil {
			return err
		}
		changes, err := parseSetArgs(c, app, v, args)
		if errors.Is(err, errKeptSettings) {
			app.UI.Info("Nothing changed.")
			return nil
		}
		if err != nil {
			return err
		}
		restore, err := stagePEMFiles(c.Context(), app, v, changes)
		if err != nil {
			return err
		}
		var extra map[string]any
		if migrate || noMigrate {
			extra = map[string]any{"sso": map[string]any{"migrate_identities": migrate}}
		}
		if err := runConfigChange(c.Context(), app, changes, extra); err != nil {
			restore()
			return err
		}
		return nil
	}
	return set
}

func stagePEMFiles(ctx context.Context, app *App, v *configView, changes []configChange) (func(), error) {
	var undo []func()
	restore := func() {
		for _, u := range slices.Backward(undo) {
			u()
		}
	}
	for i, ch := range changes {
		k, err := v.lookup(ch.name)
		if err != nil || ch.value == nil || strings.TrimSpace(*ch.value) == "" {
			continue
		}
		if k.Name == settings.GeoIPDatabase {
			value, u, err := stageGeoIP(ctx, app, v.inst, strings.TrimSpace(*ch.value))
			if err != nil {
				restore()
				return nil, err
			}
			undo = append(undo, u)
			changes[i].value = &value
			continue
		}
		if k.Type != "pem" {
			continue
		}
		u, err := savePEM(strings.TrimSpace(*ch.value), v.inst.Path(instance.CertsDir), settings.ACMERootFile, "Pass the path to a PEM file on this machine.")
		if err != nil {
			restore()
			return nil, err
		}
		undo = append(undo, u)
		value := settings.CertsMountPoint + settings.ACMERootFile
		changes[i].value = &value
	}
	return restore, nil
}

func newConfigUnsetCmd(app *App) *cobra.Command {
	unset := &cobra.Command{
		Use:   "unset <setting>...",
		Short: "Go back to the default for one or more settings",
		Example: `rexctl config unset edge.trusted_proxies
rexctl config unset memory.profile disk.profile`,
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
		return runConfigChange(c.Context(), app, changes, nil)
	}
	return unset
}

func runConfigChange(ctx context.Context, app *App, changes []configChange, extra map[string]any) error {
	err := changeConfig(ctx, app, changes, extra)
	if errors.Is(err, errKeptSettings) {
		app.UI.Info("Nothing changed.")
		return nil
	}
	return err
}

func changeConfig(ctx context.Context, app *App, changes []configChange, extra map[string]any) error {
	typed := make([]string, 0, len(changes))
	for _, ch := range changes {
		typed = append(typed, ch.name)
	}
	v, err := loadConfigView(ctx, app, typed)
	if err != nil {
		return err
	}
	deployment, admin, err := v.split(changes)
	if err != nil {
		return err
	}
	if len(deployment) > 0 {
		if err := managed(v.inst); err != nil {
			return err
		}
	}
	return app.Locked(ctx, v.inst, func() error {
		if err := applyConfig(ctx, app, v.inst, v.rt, deployment, admin, extra); err != nil {
			return err
		}
		if geoIPChanged(deployment) {
			pruneGeoIP(v.inst)
		}
		return nil
	})
}

func (v *configView) split(changes []configChange) (map[string]*string, []adminChange, error) {
	deployment := map[string]*string{}
	var admin []adminChange
	for _, ch := range changes {
		k, err := v.lookup(ch.name)
		if err != nil {
			return nil, nil, err
		}
		if k.Kind != settings.Instance {
			deployment[k.Name] = ch.value
			if k.Name == settings.GeoIPDatabase && ch.value == nil && strings.TrimSpace(v.inst.State.Settings[settings.GeoIPVar]) != "" {
				deployment[settings.GeoIPVar] = nil
			}
			continue
		}
		ac, err := parseAdminChange(v.schema, k.Name, ch.value)
		if err != nil {
			return nil, nil, err
		}
		admin = append(admin, ac)
	}
	return deployment, admin, nil
}

func applyConfig(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime, deployment map[string]*string, admin []adminChange, extra map[string]any) error {
	if err := twoSchedules(r, inst, deployment); err != nil {
		return err
	}
	var change *deploy.Change
	if len(deployment) > 0 {
		var err error
		if change, err = applyDeployment(ctx, app, inst, r, deployment, "Applying the new settings"); err != nil {
			return err
		}
	}
	sent, err := applyAdminChanges(ctx, app, inst, admin, extra)
	if err != nil {
		return err
	}
	reportConfigChange(app, change, sent, inst.State.Upgrade != nil)
	names := slices.Collect(maps.Keys(deployment))
	for _, ac := range admin {
		names = append(names, ac.key.Key)
	}
	removed, err := instance.SettleEnv(inst, names)
	if err != nil {
		return ui.Wrap(err, "Couldn't remove the .env file rexctl doesn't read.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	if removed != "" {
		app.UI.Info("Removed %s. You changed every setting in it with rexctl.", removed)
	}
	return nil
}

func reportConfigChange(app *App, change *deploy.Change, admin []adminChange, upgrading bool) {
	changed := change != nil && !change.Unchanged
	switch {
	case !changed && len(admin) == 0:
		app.UI.OK("Nothing to change.")
		return
	case len(admin) > 1:
		app.UI.OK("Saved %d settings.", len(admin))
	case len(admin) == 1:
		app.UI.OK("%s", admin[0].done())
	}
	switch {
	case changed && upgrading:
		app.UI.Info("rexctl upgrade uses the new setting when it finishes.")
	case changed && !change.Applied:
		app.UI.Info("Fluxer uses the new settings the next time it starts.")
	}
}

func deploymentDone(name string, value *string) string {
	switch {
	case name == settings.TurnServersVar && value == nil:
		return "Fluxer uses no TURN servers now."
	case name == settings.TurnServersVar:
		return "Saved the list of TURN servers."
	case value == nil:
		return fmt.Sprintf("%s is back to its default.", name)
	case settings.SecretName(name) || name == settings.StorageSecret:
		return fmt.Sprintf("Saved %s.", name)
	}
	shown := *value
	if strings.TrimSpace(shown) == "" {
		return fmt.Sprintf("Cleared %s.", name)
	}
	if name == settings.EdgeBind {
		if norm, err := settings.NormaliseBind(shown); err == nil {
			shown = norm
		}
	}
	return fmt.Sprintf("Set %s to %s.", name, clip(settings.Mask(settings.Key{Name: name}, shown)))
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
		switch {
		case k.Nullable:
			return adminChange{key: k, patch: adminapi.ConfigPatch(k.Key, nil)}, nil
		case k.Default != nil:
			return adminChange{key: k, value: k.Default, patch: adminapi.ConfigPatch(k.Key, k.Default)}, nil
		}
		return adminChange{}, ui.Refused(fmt.Sprintf("rexctl doesn't know the default for %s, so it can't go back to it.", k.Key), fmt.Sprintf("Set the value you want with rexctl config set %s <value>.", k.Key))
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

func applyAdminChanges(ctx context.Context, app *App, inst *instance.Instance, changes []adminChange, extra map[string]any) ([]adminChange, error) {
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
	adminapi.MergePatch(patch, extra)
	warnings, err := api.PatchConfigWithWarnings(ctx, patch)
	if err != nil {
		return nil, adminapi.Explain(err)
	}
	for _, w := range warnings {
		app.UI.Warn(w, "")
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

func openPorts(ctx context.Context, app *App, inst *instance.Instance, changes map[string]*string) (func(*deploy.Change, error), error) {
	nothing := func(*deploy.Change, error) {}
	opened, closed, err := deploy.PortChanges(inst, changes)
	if err != nil || (len(opened) == 0 && len(closed) == 0) {
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
	var mine []host.Port
	if len(blocked) > 0 {
		ok, err := app.UI.Confirm(fmt.Sprintf("Open port %s in the firewall on this machine?", deploy.PortList(blocked)), true)
		if err != nil {
			return nothing, err
		}
		if !ok {
			app.UI.Warn(fmt.Sprintf("Port %s stays closed in the firewall on this machine.", deploy.PortList(blocked)), "Open it before people connect.")
		} else if err := host.OpenPorts(ctx, fw, blocked); err != nil {
			return nothing, ui.Wrap(err, "Couldn't open the port.", fmt.Sprintf("Open port %s in the firewall yourself, then try again.", deploy.PortList(blocked)))
		} else {
			mine = blocked
		}
	}
	return func(change *deploy.Change, failed error) {
		if failed != nil {
			if len(mine) == 0 {
				return
			}
			if err := host.ClosePorts(context.WithoutCancel(ctx), fw, mine); err != nil {
				app.UI.Warn(fmt.Sprintf("Couldn't close port %s again.", deploy.PortList(mine)), "Close it in the firewall yourself.")
				return
			}
			app.UI.Info("Closed port %s in the firewall again.", deploy.PortList(mine))
			return
		}
		if change == nil || !change.Applied || change.Unchanged || len(closed) == 0 {
			return
		}
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
		Short:  "Print what rexctl runs Fluxer with, with secrets hidden",
		Long:   "Print what rexctl runs Fluxer with, with secrets hidden. That's the Compose project for a Docker or Podman instance, and the settings fluxerd starts with for a systemd install.",
		Hidden: true,
		Args:   noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			inst, err := app.Instance()
			if err != nil {
				return err
			}
			r, err := app.Runtime(c.Context(), inst)
			if err != nil {
				return err
			}
			return r.Settings().Render(c.Context(), app.UI.Stdout())
		},
	}
}
