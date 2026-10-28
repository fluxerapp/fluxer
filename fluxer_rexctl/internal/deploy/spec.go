// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const upgradeFix = "Run rexctl upgrade to put this instance on the release this rexctl knows."

func ReleaseSet(inst *instance.Instance) (*stack.Set, error) {
	set, err := inst.Set()
	if errors.Is(err, instance.ErrNoReleaseSet) {
		return nil, &ui.ExitError{Code: ui.ExitRefused, Err: err,
			Msg: fmt.Sprintf("rexctl can't find the files of Fluxer %s on this machine.", inst.State.Release),
			Fix: upgradeFix}
	}
	if err != nil {
		return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't read the files of Fluxer %s.", inst.State.Release), upgradeFix)
	}
	return set, nil
}

func CustomFile(inst *instance.Instance) ([]byte, error) {
	data, err := inst.Custom()
	if err != nil {
		return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't read %s.", instance.CustomCompose), "Check that you can read it, then try again.")
	}
	return data, nil
}

func adminImportValues(s *instance.State) map[string]string {
	if s.AdminImport == nil {
		return nil
	}
	return s.AdminImport.Values
}

func Environment(set *stack.Set, s *instance.State, chosen map[string]string) (map[string]string, error) {
	env, err := settings.Effective(set, chosen, s.Secrets, adminImportValues(s))
	if err != nil {
		var se *settings.Error
		if errors.As(err, &se) {
			return nil, &ui.ExitError{Code: ui.ExitRefused, Msg: se.Problem, Fix: se.Fix, Err: err}
		}
		return nil, err
	}
	return env, nil
}

func composeFiles(inst *instance.Instance, chosen map[string]string) []string {
	files := inst.StackFiles(stack.OverlaysFor(chosen))
	if _, err := os.Stat(inst.Path(instance.CustomCompose)); err == nil {
		files = append(files, inst.Path(instance.CustomCompose))
	}
	return files
}

func specFor(inst *instance.Instance, set *stack.Set, chosen map[string]string) (engine.Spec, error) {
	env, err := Environment(set, inst.State, chosen)
	if err != nil {
		return engine.Spec{}, err
	}
	return engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: composeFiles(inst, chosen), Env: env}, nil
}

func Spec(inst *instance.Instance) (engine.Spec, error) {
	set, err := ReleaseSet(inst)
	if err != nil {
		return engine.Spec{}, err
	}
	return specFor(inst, set, inst.State.Settings)
}

func Load(ctx context.Context, inst *instance.Instance, e *engine.Engine) (*engine.Project, error) {
	s, err := Spec(inst)
	if err != nil {
		return nil, err
	}
	return e.Load(ctx, s)
}

func render(inst *instance.Instance, set *stack.Set, chosen map[string]string) (*stack.Rendered, error) {
	r, err := inst.Render(set, stack.OverlaysFor(chosen), inst.State.Images.Refs)
	if err != nil {
		return nil, ui.Wrap(err, "Couldn't write Fluxer's files.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	inst.State.Rendered = r.Hashes
	return r, nil
}

func save(inst *instance.Instance) error {
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your settings.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	return nil
}

func mountedHashes(inst *instance.Instance) (map[string]string, error) {
	out := map[string]string{}
	for _, dir := range stack.MountedDirs {
		h := sha256.New()
		root := inst.Path(dir)
		err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				if errors.Is(err, fs.ErrNotExist) && path == root {
					return fs.SkipAll
				}
				return err
			}
			if d.IsDir() {
				return nil
			}
			data, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			rel, _ := filepath.Rel(root, path)
			fmt.Fprintf(h, "%s\x00%d\x00", filepath.ToSlash(rel), len(data))
			h.Write(data)
			return nil
		})
		if err != nil {
			return nil, err
		}
		out[dir] = "sha256:" + hex.EncodeToString(h.Sum(nil))
	}
	return out, nil
}

func checkDrift(inst *instance.Instance, withEnv bool) error {
	if _, err := instance.KeepEdits(inst); err != nil {
		return ui.Wrap(err, "rexctl can't keep a copy of the files you changed.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	r, err := instance.Drift(inst)
	if err != nil {
		return ui.Wrap(err, "rexctl can't check Fluxer's files.", "Check that you can read "+inst.Dir+", then try again.")
	}
	if !withEnv {
		r.Env = nil
	}
	var de *instance.DriftError
	if err := r.Err(); errors.As(err, &de) {
		fix := de.Fix
		if len(de.Lines) > 0 {
			fix += "\n" + strings.Join(de.Lines, "\n")
		}
		return &ui.ExitError{Code: ui.ExitRefused, Msg: de.Msg, Fix: fix, Err: err}
	} else if err != nil {
		return err
	}
	return nil
}

func refuseEjected(inst *instance.Instance) error {
	var ee *instance.EjectedError
	if err := inst.Managed(); errors.As(err, &ee) {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: ee.Error(), Fix: ee.Fix(), Err: err}
	}
	return nil
}

func refuseUpgrade(inst *instance.Instance) error {
	if inst.State.Upgrade == nil || upgradeNotPulled(inst) {
		return nil
	}
	return ui.Refused("An upgrade of this instance hasn't finished.", "Run rexctl upgrade to finish it, then try again.")
}

func upgradeNotPulled(inst *instance.Instance) bool {
	u := inst.State.Upgrade
	i := slices.Index(release.Phases, u.Phase)
	return i >= 0 && i < slices.Index(release.Phases, release.PhasePulled)
}

func now() time.Time { return time.Now().UTC().Truncate(time.Second) }
