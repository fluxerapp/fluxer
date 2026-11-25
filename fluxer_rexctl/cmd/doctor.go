// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type doctorJSON struct {
	OK     bool            `json:"ok"`
	Checks []checks.Result `json:"checks"`
}

func newDoctorCmd(app *App) *cobra.Command {
	var fix, dryRun bool
	c := &cobra.Command{
		Use:   "doctor",
		Short: "Find problems on your instance and this machine",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runDoctor(c.Context(), app, fix || dryRun, dryRun)
		},
	}
	c.Flags().BoolVar(&fix, "fix", false, "fix the problems rexctl can fix safely")
	c.Flags().BoolVar(&dryRun, "dry-run", false, "show what --fix would change without changing anything")
	app.jsonFlag(c)
	return c
}

func runDoctor(ctx context.Context, app *App, fix, dryRun bool) error {
	l, err := app.Locate()
	if err != nil {
		return err
	}
	dir := l.Dir
	env := &checks.Env{Dir: dir}
	phase := checks.Installed
	inst, err := instance.Find(dir)
	var se *instance.SettingError
	switch {
	case err == nil:
		env.Instance = inst
		env.Downloads = leftDownloads(inst, time.Now())
		app.saveRepaired(inst)
		if l.From == instance.FromOnly {
			app.UI.Detail("Using %s (%s).", inst.Dir, l.Why())
		} else {
			app.UI.Info("Using %s (%s).", inst.Dir, l.Why())
		}
		if inst.State.IsVoice() {
			phase = checks.Voice
		}
	case errors.Is(err, instance.ErrNotFound):
		phase = checks.Preinstall
	case errors.Is(err, instance.ErrLegacy):
		return preRexctlRefusal(ctx, dir)
	case errors.As(err, &se):
		return settingRefusal(dir, se)
	default:
		return ui.Wrap(err, "rexctl can't read the instance in "+dir+".", "Run rexctl doctor as the account that installed Fluxer.")
	}
	if err := rootlessGuard(env.Instance); err != nil {
		return err
	}
	if env.Instance != nil {
		app.rememberFirst(env.Instance)
		if fix {
			if err := managed(env.Instance); err != nil {
				return err
			}
		}
		r, err := app.Runtime(ctx, env.Instance)
		if err != nil {
			return err
		}
		if r.Kind() != rt.Compose {
			nativeDoctor(ctx, app, env, r)
		} else {
			e, err := app.Engine(ctx, env.Instance)
			env.SetEngine(e, err)
			if err == nil && phase != checks.Voice {
				env.Admin = adminapi.New(adminapi.NewExec(e.Named(env.Instance.Project(), env.Instance.Dir)), app.Version)
			}
		}
	}
	run := func() error {
		results, err := diagnose(ctx, app, env, phase, fix, dryRun)
		if err != nil {
			return err
		}
		if env.Instance != nil {
			warnUnusedRootFiles(ctx, app, env.Instance)
		}
		if env.Instance != nil && app.UI.Interactive() {
			instance.SeenEdits(env.Instance)
		}
		var ej *instance.EjectedError
		if errors.As(env.Instance.Managed(), &ej) && !app.Flags.JSON {
			defer app.UI.Info("%s %s", ej.Error(), ej.Fix())
		}
		return reportDoctor(app, results, fix, dryRun)
	}
	if fix && !dryRun && env.Instance != nil {
		return app.Locked(ctx, env.Instance, run)
	}
	return run()
}

func nativeDoctor(ctx context.Context, app *App, env *checks.Env, r rt.Runtime) {
	inst := env.Instance
	env.Native = true
	env.Container = r.Kind() == rt.Container
	env.Stack = rt.FluxerdStack(r)
	if t, err := r.Admin(ctx); err == nil {
		env.Admin = adminapi.New(t, app.Version)
	}
	env.Start = func(ctx context.Context) error {
		return r.Start(ctx, nil, rt.StartOptions{})
	}
	env.Parts = func(services []string) []string {
		var parts []string
		for _, svc := range services {
			if part := rt.PartOf(svc); !slices.Contains(parts, part) && slices.Contains(rt.PartNames(), part) {
				parts = append(parts, part)
			}
		}
		return parts
	}
	env.Restart = func(ctx context.Context, parts []string) error {
		return r.Restart(ctx, nil, parts)
	}
	env.Resolve = func(ctx context.Context) error {
		return rt.ResolvePending(ctx, r, nil)
	}
	env.ApplySettings = func(ctx context.Context, changes map[string]*string) error {
		if len(changes) > 0 {
			_, err := applyDeployment(ctx, app, inst, r, changes, "Applying the new settings")
			return err
		}
		if inst.State.Stopped {
			return nil
		}
		return r.Restart(ctx, nil, nil)
	}
}

func diagnose(ctx context.Context, app *App, env *checks.Env, phase checks.Phase, fix, dryRun bool) ([]checks.Result, error) {
	check := func() []checks.Result {
		var results []checks.Result
		_ = app.UI.Check(ctx, "Checking your instance", func(ctx context.Context, _ *ui.Step) error {
			results = checks.Run(ctx, env, phase)
			return nil
		})
		return results
	}
	results := check()
	if !fix {
		return results, nil
	}
	if dryRun {
		for _, i := range fixOrder(results) {
			if !results[i].Fixable {
				continue
			}
			p := planFix(ctx, env, results[i].ID)
			results[i].Plan = &p
		}
		return results, nil
	}
	fixed := map[string]bool{}
	skipped := map[string]string{}
	failed := map[string]error{}
	asRoot := map[string]*host.NoSudoError{}
	approved, err := approveFixes(ctx, app, env, results)
	if err != nil {
		return nil, err
	}
	for round := 0; round < 3; round++ {
		progressed := false
		for _, i := range fixOrder(results) {
			r := results[i]
			if !r.Fixable || skipped[r.ID] != "" || asRoot[r.ID] != nil {
				continue
			}
			c, _ := checks.Get(r.ID)
			if !c.SafeFix && !approved[r.ID] {
				skipped[r.ID] = r.Fix
				if !app.UI.Interactive() && !app.UI.Yes() {
					skipped[r.ID] = "Run rexctl doctor --fix in a terminal, or pass --yes, to confirm this fix."
				}
				continue
			}
			var after checks.Result
			var applyErr error
			err := app.UI.Step(ctx, "Fixing it", func(ctx context.Context, _ *ui.Step) error {
				after, applyErr = checks.ApplyFix(ctx, env, r.ID)
				return fixError(applyErr)
			})
			if err != nil {
				if errors.Is(applyErr, host.ErrNoSudo) {
					asRoot[r.ID] = noSudo(applyErr)
				}
				results[i] = checks.Recheck(ctx, env, c)
				failed[r.ID] = err
				continue
			}
			delete(failed, r.ID)
			results[i] = after
			if after.Fixed {
				fixed[r.ID] = true
				progressed = true
			}
		}
		if env.Staged() {
			staged := env.StagedBy()
			err := app.UI.Step(ctx, "Applying the fixes", func(ctx context.Context, s *ui.Step) error {
				if err := checks.Commit(ctx, env, deploy.ApplyOptions{Wait: true}); err != nil {
					return err
				}
				s.Done("Applied the fixes.")
				return nil
			})
			if err != nil {
				for _, id := range staged {
					delete(fixed, id)
					failed[id] = err
				}
			} else {
				for _, id := range staged {
					fixed[id] = true
				}
				progressed = true
			}
		}
		if !progressed {
			break
		}
		results = check()
	}
	for i, r := range results {
		if failed[r.ID] != nil && r.Fixable && asRoot[r.ID] == nil && !errors.Is(failed[r.ID], host.ErrPodmanServiceFailed) && !errors.Is(failed[r.ID], host.ErrNoUserSession) {
			results[i].Details = append([]string{failed[r.ID].Error()}, r.Details...)
		}
		switch {
		case failed[r.ID] != nil && strings.HasPrefix(r.Message, checks.PodmanServiceStopped):
			results[i].Fix = "Fix what it reports, then run rexctl doctor --fix again."
		case fixed[r.ID] && r.Status == checks.Pass:
			results[i].Fixed = true
		case skipped[r.ID] != "" && (r.Status == checks.Warn || r.Status == checks.Fail):
			results[i].Fix = skipped[r.ID]
		case failed[r.ID] != nil && r.Fixable && r.Manual != "":
			results[i].Fix = "rexctl couldn't fix this. " + r.Manual
		case failed[r.ID] != nil && asRoot[r.ID] != nil && asRoot[r.ID].Command != "":
			results[i].Fix = "Run " + asRoot[r.ID].Command + " as root, then run rexctl doctor again."
		case failed[r.ID] != nil && asRoot[r.ID] != nil:
			results[i].Fix = "rexctl couldn't fix this, because this account can't use sudo."
		case failed[r.ID] != nil && r.Fixable && app.Flags.Verbose:
			results[i].Fix = "rexctl couldn't fix this."
		case failed[r.ID] != nil && r.Fixable && strings.Contains(r.Fix, "rexctl doctor --fix"):
			results[i].Fix = "rexctl couldn't fix this. Run rexctl doctor --fix --verbose to see why."
		case failed[r.ID] != nil && r.Fixable:
			results[i].Fix = "rexctl couldn't fix this. " + r.Fix
		}
	}
	return results, nil
}

func noSudo(err error) *host.NoSudoError {
	var ns *host.NoSudoError
	if errors.As(err, &ns) {
		return ns
	}
	return &host.NoSudoError{}
}

func fixError(err error) error {
	if errors.Is(err, host.ErrNoSudo) {
		return ui.Refused(ui.Sentence(noSudo(err).Error()), "")
	}
	if errors.Is(err, host.ErrNoUserSession) {
		msg, fix := host.NoUserSession("rexctl doctor --fix again")
		return ui.Wrap(err, msg, fix)
	}
	var ce *host.CommandError
	if errors.As(err, &ce) && errors.Is(err, host.ErrPodmanServiceFailed) && strings.TrimSpace(ce.Output) != "" {
		return &ui.ExitError{Code: ui.ExitFailed, Msg: "Couldn't start Podman's API.", Err: host.ErrPodmanServiceFailed}
	}
	return err
}

func approveFixes(ctx context.Context, app *App, env *checks.Env, results []checks.Result) (map[string]bool, error) {
	approved := map[string]bool{}
	var items []ui.PickItem
	var all []string
	for _, i := range fixOrder(results) {
		r := results[i]
		c, _ := checks.Get(r.ID)
		if !r.Fixable || c.SafeFix {
			continue
		}
		if app.UI.Yes() {
			approved[r.ID] = true
			continue
		}
		items = append(items, ui.PickItem{Value: r.ID, Label: r.Message, Hint: fixSummary(ctx, env, r.ID)})
		all = append(all, r.ID)
	}
	if len(items) == 0 || !app.UI.Interactive() {
		return approved, nil
	}
	title := "Fix this problem?"
	if len(items) > 1 {
		title = fmt.Sprintf("Fix these %d problems?", len(items))
	}
	picked, err := app.UI.PickMany(title, items, all)
	if errors.Is(err, ui.ErrBack) {
		return approved, nil
	}
	if err != nil {
		return nil, err
	}
	for _, id := range picked {
		approved[id] = true
	}
	return approved, nil
}

func fixSummary(ctx context.Context, env *checks.Env, id string) string {
	p := planFix(ctx, env, id)
	if p.Error != "" || len(p.Steps) == 0 {
		return ""
	}
	return p.Steps[0].Do
}

func fixOrder(results []checks.Result) []int {
	var first, last []int
	for i, r := range results {
		if r.ID == "services" || r.ID == "stale-containers" {
			last = append(last, i)
		} else {
			first = append(first, i)
		}
	}
	return append(first, last...)
}

func planFix(ctx context.Context, env *checks.Env, id string) checks.Plan {
	p, err := checks.PlanFix(ctx, env, id)
	if err != nil {
		p.Steps, p.Error = []host.Step{}, err.Error()
	}
	return p
}

func printPlan(u *ui.UI, p checks.Plan) {
	if p.Error != "" {
		if u.Verbose() {
			u.Info("  rexctl can't show what this fix does.")
		} else {
			u.Info("  rexctl can't show what this fix does. Run rexctl doctor --fix --dry-run --verbose to see why.")
		}
		u.Detail("  %s", p.Error)
		return
	}
	if len(p.Steps) == 0 {
		return
	}
	u.Info("  rexctl doctor --fix does this:")
	for _, s := range p.Steps {
		u.Info("    %s", s.Do)
		if s.Command != "" {
			u.Info("      %s", s.Command)
		}
	}
	if p.Settings {
		u.Info("  rexctl applies these settings changes together with the other fixes and restarts the services that use them.")
	}
	switch {
	case p.Asks:
		u.Info("  rexctl asks before it does this, unless you pass --yes.")
	default:
		u.Info("  rexctl does this without asking when you run rexctl doctor --fix.")
	}
	var undo []string
	for i := len(p.Steps) - 1; i >= 0; i-- {
		if p.Steps[i].Undo != "" {
			undo = append(undo, p.Steps[i].Undo)
		}
	}
	if len(undo) == 0 {
		return
	}
	u.Info("  To undo it, run:")
	for _, c := range undo {
		u.Info("      %s", c)
	}
}

var doctorGroups = []struct {
	title string
	ids   []string
}{
	{"This machine", []string{"engine", "service-account", "fluxer-namespaces", "podman-subids", "podman-setup", "podman-api", "windows-linux-containers", "memory", "memory-overcommit", "disk", "clock", "ports", "firewall", "low-ports"}},
	{"Fluxer", []string{"fluxer-config", "fluxer-stopped", "postgres-version", "env-file", "stack-files", "stack-edits", "pending-change", "upgrade-unfinished", "restore-unfinished", "move-download", "boot-unit", "vapid", "attachment-signatures", "services", "stale-containers", "media-proxy-spool", "postgres-password", "postgres-scram", "objects-filesystem", "uploads-read-only"}},
	{"Address and sign-in", []string{"dns", "origin", "health", "proxy", "certificate", "local-certificate", "discovery", "app-proxy-discovery", "client-ip", "sso-provider", "connection-providers", "push-relay"}},
	{"Backups", []string{"backup-age"}},
}

func doctorGroup(id string) int {
	for i, g := range doctorGroups {
		if slices.Contains(g.ids, id) {
			return i
		}
	}
	return len(doctorGroups)
}

func doctorGroupTitle(i int) string {
	if i < len(doctorGroups) {
		return doctorGroups[i].title
	}
	return "Voice"
}

func reportDoctor(app *App, results []checks.Result, fix, dryRun bool) error {
	shown := slices.Clone(results)
	slices.SortStableFunc(shown, func(a, b checks.Result) int { return doctorGroup(a.ID) - doctorGroup(b.ID) })
	group := -1
	for _, r := range shown {
		if app.Flags.JSON {
			break
		}
		if g := doctorGroup(r.ID); g != group && r.Status != checks.Skip {
			group = g
			app.UI.Section(doctorGroupTitle(g))
		}
		switch r.Status {
		case checks.Pass:
			if r.Fixed {
				app.UI.OK("Fixed. %s", r.Message)
			} else {
				app.UI.OK("%s", r.Message)
			}
		case checks.Warn:
			app.UI.Warn(r.Message, r.Fix)
		case checks.Fail:
			app.UI.Fail(r.Message, r.Fix)
		case checks.Skip:
			if r.Message != "" {
				app.UI.Detail("%s", r.Message)
			}
		}
		for _, d := range r.Details {
			if strings.TrimSpace(d) == "" {
				continue
			}
			app.UI.Detail("  %s", d)
		}
		if r.Plan != nil {
			printPlan(app.UI, *r.Plan)
		}
	}
	if !app.Flags.JSON && !dryRun {
		doctorSummary(app, results)
	}
	if !app.Flags.JSON {
		switch {
		case dryRun:
			app.UI.Info("Dry run. Nothing changed.")
		case !fix && slices.ContainsFunc(results, func(r checks.Result) bool { return r.Fixable }):
			app.UI.Info("To see what rexctl doctor --fix changes before you run it, run rexctl doctor --fix --dry-run.")
		}
	}
	if app.Flags.JSON {
		if err := app.UI.JSON(doctorJSON{OK: !checks.Failed(results), Checks: results}); err != nil {
			return err
		}
	}
	if checks.Failed(results) {
		app.SetExit(ui.ExitRefused)
		return nil
	}
	return nil
}

func doctorSummary(app *App, results []checks.Result) {
	var passed, warned, failed int
	for _, r := range results {
		switch r.Status {
		case checks.Pass:
			passed++
		case checks.Warn:
			warned++
		case checks.Fail:
			failed++
		}
	}
	parts := []string{plural(passed, "check", "checks") + " passed"}
	if warned > 0 {
		parts = append(parts, plural(warned, "warning", "warnings"))
	}
	if failed > 0 {
		parts = append(parts, plural(failed, "problem", "problems"))
	}
	tone := ui.ToneOK
	switch {
	case failed > 0:
		tone = ui.ToneFail
	case warned > 0:
		tone = ui.ToneWarn
	}
	summary := ui.Sentence(ui.JoinAnd(parts))
	if checks.AllGood(results) && app.UI.ShowsRex() {
		app.UI.Rex("Everything looks good.", summary)
		return
	}
	app.UI.Notice(tone, summary, "")
}
