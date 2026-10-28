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

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
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
	return &cobra.Command{
		Use:   "edit",
		Short: "Edit " + instance.CustomCompose + ", check it and apply it",
		Long: "Opens " + instance.CustomCompose + " in $VISUAL or $EDITOR. rexctl checks the file, says which services restart and applies it once you agree. " +
			"Your file stays as it was until it passes the check.",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runConfigEdit(c.Context(), app)
		},
	}
}

var openEditor = func(ctx context.Context, path string) error {
	if !term.IsTerminal(int(os.Stdin.Fd())) {
		return &ui.ExitError{Code: ui.ExitUsage, Msg: "rexctl config edit needs a terminal.",
			Fix: "Edit " + instance.CustomCompose + " in your instance folder yourself, then run rexctl restart."}
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

var askEdit = func(u *ui.UI, question string, def bool) (bool, error) {
	if u.Yes() || !u.Interactive() {
		return false, errNotAsked
	}
	return u.Confirm(question, def)
}

func runConfigEdit(ctx context.Context, app *App) error {
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
	return app.Locked(ctx, inst, func() error {
		e, err := app.guard(ctx, inst, true)
		if err != nil {
			return err
		}
		return editCustom(ctx, app, inst, e)
	})
}

func keptAsItWas() error {
	return ui.Refused(fmt.Sprintf("Kept %s as it was.", instance.CustomCompose), "")
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
	draft, err := newDraft(inst, start)
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
		kept, kerr := keepDraft(inst, draft)
		if kerr != nil {
			kept = draft
		}
		instance.SeenEdits(inst)
		app.UI.Info("Your draft is in %s.", kept)
	}()
	failed := false
	for {
		began := time.Now()
		if err := openEditor(ctx, draft); err != nil {
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
		if errors.Is(err, errNotAsked) || err == nil && !again {
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

func newDraft(inst *instance.Instance, data []byte) (string, error) {
	dir := inst.Path(instance.StateDirName, editDir)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	f, err := os.CreateTemp(dir, "custom.compose-*.yml")
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

func keepDraft(inst *instance.Instance, draft string) (string, error) {
	dir := inst.Path(instance.StateDirName, instance.EditedDir)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	kept := filepath.Join(dir, instance.CustomCompose+"."+time.Now().UTC().Format("20060102-150405"))
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
	var none engine.Engine
	if _, err := none.Load(ctx, s); err != nil {
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
	var none engine.Engine
	merged, err := none.Load(ctx, s)
	if err != nil {
		return r, err
	}
	s.Files = s.Files[:len(s.Files)-1]
	base, err := none.Load(ctx, s)
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
		return ui.Wrap(err, fmt.Sprintf("rexctl can't compare %s with Fluxer's own files.", instance.CustomCompose), "Run rexctl config edit --verbose to see why.")
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
			return ui.Refused(fmt.Sprintf("rexctl doesn't remove volumes or networks of Fluxer's services without asking. Kept %s as it was.", instance.CustomCompose), "Run rexctl config edit in a terminal, without --yes, to confirm.")
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
	return ui.Refused(fmt.Sprintf("%s changed while you were editing it.", instance.CustomCompose), "Run rexctl config edit again to start from the new file.")
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
			return engineFailure(err)
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
