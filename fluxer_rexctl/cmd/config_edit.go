// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"slices"
	"strings"
	"time"

	"github.com/spf13/cobra"
	"go.yaml.in/yaml/v4"
	"golang.org/x/term"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	editDir     = "edit"
	quickEditor = time.Second
)

var errNotAsked = errors.New("cmd: no one to ask")

var yamlLine = regexp.MustCompile(`\bL(\d+)\.C\d+`)

func newConfigEditCmd(app *App) *cobra.Command {
	var compose bool
	c := &cobra.Command{
		Use:   "edit",
		Short: "Edit your settings in an editor, check them and apply them",
		Long: "Opens the settings you changed from their defaults as setting=value lines in $VISUAL or $EDITOR. Add a line for any other setting rexctl config set takes, or remove one to go back to its default. rexctl checks them as rexctl config set does, says what restarts and applies them once you agree.\n\n" +
			"Nothing changes until your draft passes the check. With --compose, it opens " + instance.CustomCompose + " instead, for changes to Fluxer's containers that no setting covers.",
		Example: `rexctl config edit
EDITOR=vim rexctl config edit
rexctl config edit --compose`,
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runConfigEdit(c.Context(), app, compose)
		},
	}
	c.Flags().BoolVar(&compose, "compose", false, "edit "+instance.CustomCompose+", the Compose overrides of a Docker or Podman instance")
	return c
}

func openEditor(ctx context.Context, path, fix string) error {
	if !term.IsTerminal(int(os.Stdin.Fd())) {
		return &ui.ExitError{Code: ui.ExitUsage, Msg: "rexctl config edit needs a terminal.", Fix: fix}
	}
	editor, ok := editorCommand()
	if !ok {
		return ui.Refused("rexctl can't find an editor.", "Set $EDITOR to the editor you use, then try again.")
	}
	c := editorCmd(ctx, editor, path)
	c.Stdin, c.Stdout, c.Stderr = os.Stdin, os.Stdout, os.Stderr
	if err := c.Run(); err != nil {
		var ee *exec.ExitError
		if errors.Is(err, exec.ErrNotFound) || errors.As(err, &ee) && ee.ExitCode() == 127 {
			return ui.Wrap(err, "rexctl can't find the editor "+editor+".", "Set $EDITOR to the editor you use, then try again.")
		}
		return ui.Wrap(err, "The editor "+editor+" didn't finish.", "Set $EDITOR to the editor you use, then try again.")
	}
	return nil
}

func editorCommand() (string, bool) {
	for _, name := range []string{"VISUAL", "EDITOR"} {
		if v := strings.TrimSpace(os.Getenv(name)); v != "" {
			return v, true
		}
	}
	if runtime.GOOS == "windows" {
		return "notepad", true
	}
	for _, name := range []string{"nano", "vi"} {
		if _, err := exec.LookPath(name); err == nil {
			return name, true
		}
	}
	return "", false
}

func editorCmd(ctx context.Context, editor, path string) *exec.Cmd {
	if runtime.GOOS == "windows" {
		args := splitCommand(editor)
		return exec.CommandContext(ctx, args[0], append(args[1:], path)...)
	}
	return exec.CommandContext(ctx, "sh", "-c", editor+` "$@"`, editor, path)
}

func splitCommand(s string) []string {
	var out []string
	var cur strings.Builder
	quoted, started := false, false
	for _, r := range s {
		switch {
		case r == '"':
			quoted, started = !quoted, true
		case (r == ' ' || r == '\t') && !quoted:
			if started {
				out = append(out, cur.String())
				cur.Reset()
				started = false
			}
		default:
			cur.WriteRune(r)
			started = true
		}
	}
	if started {
		out = append(out, cur.String())
	}
	return out
}

func askEdit(u *ui.UI, question string, def bool) (bool, error) {
	if u.Yes() || !u.Interactive() {
		return false, errNotAsked
	}
	return u.Confirm(question, def)
}

func runConfigEdit(ctx context.Context, app *App, compose bool) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if err := managed(inst); err != nil {
		return err
	}
	if inst.State.Upgrade != nil {
		return ui.Refused("An upgrade of this instance hasn't finished.", "Run rexctl upgrade to finish it, then try again.")
	}
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return err
	}
	if !compose {
		if err := rt.CanChangeSettings(r); err != nil {
			return err
		}
		return declinedEdit(app, app.Locked(ctx, inst, func() error { return editSettings(ctx, app, inst, r) }))
	}
	if r.Kind() != rt.Compose {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: "This instance has no Compose files to change.", Fix: "Run rexctl config edit to change its settings.", Err: rt.ErrUnsupported}
	}
	return declinedEdit(app, app.Locked(ctx, inst, func() error {
		e, err := app.guard(ctx, inst, true)
		if err != nil {
			return err
		}
		return editCustom(ctx, app, inst, e)
	}))
}

func declinedEdit(app *App, err error) error {
	if errors.Is(err, errEditDeclined) {
		app.UI.Info("Nothing changed.")
		return nil
	}
	return err
}

var errEditDeclined = errors.New("cmd: the edit was declined")

func keptAsItWas() error {
	return errEditDeclined
}

func editCustom(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine) (err error) {
	path := inst.Path(instance.CustomCompose)
	start, rerr := os.ReadFile(path)
	existed := rerr == nil
	switch {
	case errors.Is(rerr, fs.ErrNotExist):
		start = []byte(instance.CustomTemplate)
	case rerr != nil:
		return ui.Wrap(rerr, fmt.Sprintf("rexctl can't read %s.", instance.CustomCompose), "Check that you can read it, then try again.")
	}
	draft, err := newDraft(inst, "custom.compose-*.yml", start)
	if err != nil {
		return ui.Wrap(err, "Couldn't make a copy to edit.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	after := start
	defer func() {
		if now, rerr := os.ReadFile(draft); rerr == nil {
			after = now
		}
		if err == nil || bytes.Equal(after, start) {
			os.Remove(draft)
			return
		}
		kept, kerr := keepDraft(inst, draft, instance.CustomCompose)
		if kerr != nil {
			kept = draft
		}
		instance.SeenEdits(inst)
		app.UI.Info("Your draft is in %s.", kept)
	}()
	failed := false
	for {
		began := time.Now()
		if err := openEditor(ctx, draft, "Run rexctl config edit --compose in a terminal."); err != nil {
			return err
		}
		next, err := os.ReadFile(draft)
		if err != nil {
			return err
		}
		same := bytes.Equal(next, after)
		after = next
		if bytes.Equal(after, start) {
			app.UI.OK("Nothing changed.")
			if time.Since(began) < quickEditor {
				app.UI.Info("If your editor opens a window, use code --wait.")
			}
			return nil
		}
		if failed && same {
			return keptAsItWas()
		}
		problem := checkCustom(ctx, inst, draft, after)
		if problem == "" {
			break
		}
		failed = true
		app.UI.Fail(fmt.Sprintf("Your change to %s doesn't work.", instance.CustomCompose), "")
		for _, line := range strings.Split(problem, "\n") {
			app.UI.Info("  %s", line)
		}
		again, err := askEdit(app.UI, "Edit it again? No keeps the file as it was.", true)
		if errors.Is(err, errNotAsked) {
			return ui.Refused(fmt.Sprintf("Kept %s as it was.", instance.CustomCompose), "Fix the problem above, then run rexctl config edit --compose again.")
		}
		if err == nil && !again {
			return keptAsItWas()
		}
		if err != nil {
			return err
		}
	}
	if err := confirmRisks(ctx, app, inst, draft); err != nil {
		return err
	}
	return applyCustom(ctx, app, inst, e, draft, start, existed, after)
}

func newDraft(inst *instance.Instance, pattern string, data []byte) (string, error) {
	dir := inst.Path(instance.StateDirName, editDir)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	f, err := os.CreateTemp(dir, pattern)
	if err != nil {
		return "", err
	}
	if _, err := f.Write(data); err != nil {
		f.Close()
		os.Remove(f.Name())
		return "", err
	}
	if err := f.Close(); err != nil {
		os.Remove(f.Name())
		return "", err
	}
	return f.Name(), nil
}

func keepDraft(inst *instance.Instance, draft, name string) (string, error) {
	dir := inst.Path(instance.StateDirName, instance.EditedDir)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	kept := filepath.Join(dir, name+"."+time.Now().UTC().Format("20060102-150405"))
	return kept, os.Rename(draft, kept)
}

func checkCustom(ctx context.Context, inst *instance.Instance, draft string, data []byte) string {
	var doc map[string]any
	if err := yaml.Unmarshal(data, &doc); err != nil {
		lead := "The file isn't valid YAML."
		if m := yamlLine.FindStringSubmatch(err.Error()); m != nil {
			lead = fmt.Sprintf("Line %s isn't valid YAML.", m[1])
		}
		return lead + "\n" + err.Error()
	}
	s, err := customSpec(inst, draft)
	if err != nil {
		return err.Error()
	}
	if _, err := engine.Load(ctx, s); err != nil {
		return composeProblem(err)
	}
	return ""
}

func customSpec(inst *instance.Instance, draft string) (engine.Spec, error) {
	s, err := deploy.Spec(inst)
	if err != nil {
		return s, err
	}
	s.Files = slices.DeleteFunc(s.Files, func(f string) bool { return f == inst.Path(instance.CustomCompose) })
	s.Files = append(s.Files, draft)
	return s, nil
}

type customRisks struct {
	removals []string
	changes  []string
	env      []string
}

func risksOf(ctx context.Context, inst *instance.Instance, draft string) (customRisks, error) {
	var r customRisks
	s, err := customSpec(inst, draft)
	if err != nil {
		return r, err
	}
	merged, err := engine.Load(ctx, s)
	if err != nil {
		return r, err
	}
	s.Files = s.Files[:len(s.Files)-1]
	base, err := engine.Load(ctx, s)
	if err != nil {
		return r, err
	}
	for _, m := range merged.DroppedMounts(base) {
		r.removals = append(r.removals, "loses the volume at "+m)
	}
	envs := map[string]bool{}
	for _, name := range stack.Services {
		b, ok := base.Model.Services[name]
		m, mok := merged.Model.Services[name]
		if !ok || !mok {
			continue
		}
		for _, n := range slices.Sorted(maps.Keys(b.Networks)) {
			if _, ok := m.Networks[n]; !ok {
				r.removals = append(r.removals, fmt.Sprintf("%s loses the network %s", name, n))
			}
		}
		if m.Image != b.Image {
			r.changes = append(r.changes, fmt.Sprintf("%s runs %s instead of %s", name, m.Image, b.Image))
		}
		if !slices.Equal(m.Command, b.Command) || !slices.Equal(m.Entrypoint, b.Entrypoint) {
			r.changes = append(r.changes, fmt.Sprintf("%s runs another command than the one Fluxer ships", name))
		}
		for k, v := range m.Environment {
			if !strings.HasPrefix(k, "FLUXER_") {
				continue
			}
			if was, ok := b.Environment[k]; !ok || (was == nil) != (v == nil) || was != nil && *was != *v {
				envs[k] = true
			}
		}
	}
	r.env = slices.Sorted(maps.Keys(envs))
	return r, nil
}

func confirmRisks(ctx context.Context, app *App, inst *instance.Instance, draft string) error {
	r, err := risksOf(ctx, inst, draft)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("rexctl can't compare %s with Fluxer's own files.", instance.CustomCompose), "Run rexctl config edit --compose --verbose to see why.")
	}
	for _, line := range r.changes {
		app.UI.Warn(fmt.Sprintf("With your change, %s.", line), "")
	}
	if len(r.env) > 0 {
		app.UI.Warn(fmt.Sprintf("Your change sets %s, which rexctl manages.", ui.JoinAnd(r.env)), "Change these with rexctl config set, so upgrades and checks know about them.")
	}
	if len(r.removals) > 0 {
		app.UI.Warn(fmt.Sprintf("With your change, %s.", ui.JoinAnd(r.removals)), "Fluxer may lose data or stop working without them.")
		ok, err := askEdit(app.UI, "Remove them anyway?", false)
		if errors.Is(err, errNotAsked) {
			return ui.Refused(fmt.Sprintf("rexctl doesn't remove volumes or networks of Fluxer's services without asking. Kept %s as it was.", instance.CustomCompose), "Run rexctl config edit --compose in a terminal, without --yes, to confirm.")
		}
		if err != nil {
			return err
		}
		if !ok {
			return keptAsItWas()
		}
		return nil
	}
	if len(r.changes) > 0 || len(r.env) > 0 {
		ok, err := app.UI.Confirm("Keep these changes?", false)
		if err != nil {
			return err
		}
		if !ok {
			return keptAsItWas()
		}
	}
	return nil
}

func planLines(plan *engine.Plan) []string {
	var out []string
	if len(plan.Recreate) > 0 {
		out = append(out, fmt.Sprintf("This restarts %s.", ui.JoinAnd(slices.Sorted(slices.Values(plan.Recreate)))))
	}
	if len(plan.Create) > 0 {
		out = append(out, fmt.Sprintf("This starts %s.", ui.JoinAnd(slices.Sorted(slices.Values(plan.Create)))))
	}
	if len(plan.Remove) > 0 {
		out = append(out, fmt.Sprintf("This removes %s.", ui.JoinAnd(plan.Remove)))
	}
	if len(out) == 0 {
		out = append(out, "No service restarts.")
	}
	return out
}

func startFailure(existed bool) string {
	if existed {
		return fmt.Sprintf("Fluxer didn't start with your change. rexctl put the old %s back.", instance.CustomCompose)
	}
	return fmt.Sprintf("Fluxer didn't start with your change. rexctl removed the new %s.", instance.CustomCompose)
}

func unchangedSince(path string, start []byte, existed bool) error {
	now, err := os.ReadFile(path)
	if existed && err == nil && bytes.Equal(now, start) || !existed && errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	return ui.Refused(fmt.Sprintf("%s changed while you were editing it.", instance.CustomCompose), "Run rexctl config edit --compose again to start from the new file.")
}

func applyCustom(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, draft string, start []byte, existed bool, data []byte) error {
	path := inst.Path(instance.CustomCompose)
	var p *engine.Project
	if !inst.State.Stopped {
		s, err := customSpec(inst, draft)
		if err != nil {
			return err
		}
		if p, err = e.Load(ctx, s); err != nil {
			return rt.EngineError("", err)
		}
		if plan, err := p.Plan(ctx); err == nil {
			for _, line := range planLines(plan) {
				app.UI.Info("%s", line)
			}
		} else {
			app.UI.Detail("%v", err)
		}
	}
	ok, err := app.UI.Confirm("Apply the change?", true)
	if err != nil {
		return err
	}
	if !ok {
		return keptAsItWas()
	}
	if err := unchangedSince(path, start, existed); err != nil {
		return err
	}
	if p != nil {
		err := app.UI.Step(ctx, "Downloading what your change needs", func(ctx context.Context, s *ui.Step) error {
			if err := p.Pull(ctx, app.progress(s, "Downloading")); err != nil {
				return err
			}
			s.Done("Downloaded what your change needs.")
			return nil
		})
		if err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't download the images your change needs. Fluxer still runs as it was, and %s didn't change.", instance.CustomCompose), "Check the image names and this machine's internet connection, then try again.")
		}
	}
	if err := envfile.WriteFileAtomic(path, data, false); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save %s.", instance.CustomCompose), "Check that you can write to "+inst.Dir+", then try again.")
	}
	if inst.State.Stopped {
		app.UI.OK("Saved %s. Fluxer uses it the next time it starts.", instance.CustomCompose)
		return nil
	}
	err = app.UI.Step(ctx, "Applying "+instance.CustomCompose, func(ctx context.Context, s *ui.Step) error {
		if _, err := deploy.Apply(ctx, inst, e, deploy.ApplyOptions{Wait: true, OnEvent: app.progress(s, "Starting")}); err != nil {
			return readyFailure(ctx, inst, e.Named(inst.Project(), inst.Dir), err)
		}
		s.Done("Applied %s.", instance.CustomCompose)
		return nil
	})
	if err == nil {
		return nil
	}
	restore := os.Remove(path)
	if existed {
		restore = envfile.WriteFileAtomic(path, start, false)
	}
	if restore != nil {
		return errors.Join(err, restore)
	}
	app.UI.Warn(startFailure(existed), "Run rexctl restart to start Fluxer with the file as it was.")
	return err
}

const settingsDraftName = "settings.env"

func operatorChosen(r rt.Runtime, chosen map[string]string) map[string]string {
	if r.Kind() == rt.Compose {
		return chosen
	}
	shown := maps.Clone(chosen)
	maps.DeleteFunc(shown, func(k, v string) bool {
		pin, ok := native.Pinned[k]
		return ok && v == pin
	})
	return shown
}

func settingsDraft(chosen map[string]string) []byte {
	var b bytes.Buffer
	for _, k := range slices.Sorted(maps.Keys(chosen)) {
		b.WriteString(k + "=" + envfile.Quote(chosen[k]) + "\n")
	}
	return b.Bytes()
}

func draftChanges(chosen map[string]string, data []byte) (map[string]*string, string) {
	for i, line := range strings.Split(strings.ReplaceAll(string(data), "\r", ""), "\n") {
		body := strings.TrimSpace(line)
		if body == "" || body[0] == '#' {
			continue
		}
		name, _, ok := strings.Cut(strings.TrimPrefix(body, "export "), "=")
		if !ok || !envfile.ValidKey(strings.TrimSpace(name)) {
			return nil, fmt.Sprintf("Line %d isn't a setting=value line.", i+1)
		}
	}
	f, err := envfile.Parse(settingsDraftName, data)
	if err != nil {
		return nil, err.Error()
	}
	next := f.Map()
	changes := map[string]*string{}
	for k, v := range next {
		if old, ok := chosen[k]; !ok || old != v {
			changes[k] = &v
		}
	}
	for k := range chosen {
		if _, ok := next[k]; !ok {
			changes[k] = nil
		}
	}
	return changes, ""
}

func settingsProblem(ctx context.Context, v *configView, inst *instance.Instance, r rt.Runtime, changes map[string]*string) (map[string]*string, []adminChange, string) {
	var all []configChange
	for _, k := range slices.Sorted(maps.Keys(changes)) {
		all = append(all, configChange{name: k, value: changes[k]})
	}
	deployment, admin, err := v.split(all)
	if err == nil && len(deployment) > 0 {
		var p *deploy.Prepared
		p, err = deploy.Prepare(inst, deployment)
		if err == nil && !p.Unchanged {
			err = r.Settings().Validate(ctx, p)
		}
	}
	var ee *ui.ExitError
	switch {
	case err == nil:
		return deployment, admin, ""
	case errors.As(err, &ee) && ee.Msg != "":
		lines := append([]string{ee.Msg}, ee.Tail...)
		if ee.Fix != "" {
			lines = append(lines, ee.Fix)
		}
		return nil, nil, strings.Join(lines, "\n")
	}
	return nil, nil, err.Error()
}

func editSettings(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime) (err error) {
	v, err := loadConfigView(ctx, app, nil)
	if err != nil {
		return err
	}
	chosen := map[string]string{}
	maps.Copy(chosen, operatorChosen(r, inst.State.Settings))
	if values, err := instanceValues(ctx, app); err == nil {
		for _, k := range values {
			if row := instanceRow(k); row.Changed && !row.Secret {
				chosen[k.Key] = adminapi.FormatValue(k)
			}
		}
	}
	start := settingsDraft(chosen)
	draft, err := newDraft(inst, "settings-*.env", start)
	if err != nil {
		return ui.Wrap(err, "Couldn't make a copy to edit.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	after := start
	defer func() {
		if now, rerr := os.ReadFile(draft); rerr == nil {
			after = now
		}
		if err == nil || bytes.Equal(after, start) {
			os.Remove(draft)
			return
		}
		kept, kerr := keepDraft(inst, draft, settingsDraftName)
		if kerr != nil {
			kept = draft
		}
		app.UI.Info("Your draft is in %s.", kept)
	}()
	failed := false
	var deployment map[string]*string
	var admin []adminChange
	for {
		began := time.Now()
		if err := openEditor(ctx, draft, "Run rexctl config set <setting> <value> to change a setting without an editor."); err != nil {
			return err
		}
		next, err := os.ReadFile(draft)
		if err != nil {
			return err
		}
		same := bytes.Equal(next, after)
		after = next
		changes, problem := draftChanges(chosen, after)
		if problem == "" && len(changes) == 0 {
			app.UI.OK("Nothing changed.")
			if time.Since(began) < quickEditor {
				app.UI.Info("If your editor opens a window, use code --wait.")
			}
			return nil
		}
		if failed && same {
			return errEditDeclined
		}
		if problem == "" {
			deployment, admin, problem = settingsProblem(ctx, v, inst, r, changes)
		}
		if problem == "" {
			break
		}
		failed = true
		app.UI.Fail("Your change to the settings doesn't work.", "")
		for _, line := range strings.Split(problem, "\n") {
			app.UI.Info("  %s", line)
		}
		again, err := askEdit(app.UI, "Edit it again? No keeps the settings as they were.", true)
		if errors.Is(err, errNotAsked) {
			return ui.Refused("Kept your settings as they were.", "Fix the problem above, then run rexctl config edit again.")
		}
		if err == nil && !again {
			return errEditDeclined
		}
		if err != nil {
			return err
		}
	}
	err = applyConfig(ctx, app, inst, r, deployment, admin, nil)
	if errors.Is(err, errKeptSettings) {
		return errEditDeclined
	}
	return err
}
