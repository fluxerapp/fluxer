// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

type EjectOptions struct {
	Rexctl          string
	SendAdminImport func() error
	RemoveBootUnit  func() error
}

func EjectCheck(inst *Instance) error {
	if err := inst.Managed(); err != nil {
		return err
	}
	switch {
	case inst.State.Upgrade != nil:
		return &AdoptError{Msg: "An upgrade is still in progress.", Fix: "Run rexctl upgrade to finish it, then run rexctl eject again."}
	case inst.State.Pending != nil:
		return &AdoptError{Msg: "A settings change is still in progress.", Fix: "Run rexctl doctor to finish it, then run rexctl eject again."}
	}
	return nil
}

func Eject(inst *Instance, o EjectOptions) error {
	if err := EjectCheck(inst); err != nil {
		return err
	}
	s := inst.State
	if a := s.AdminImport; a != nil && a.SentAt == "" && len(a.Values) > 0 && o.SendAdminImport != nil {
		if err := o.SendAdminImport(); err != nil {
			return err
		}
	}
	env, err := inst.Environment()
	if err != nil {
		return err
	}
	rendered, err := writePlain(inst)
	if err != nil {
		return err
	}
	f := envfile.New(inst.Path(EnvFileName))
	for _, k := range slices.Sorted(maps.Keys(env)) {
		f.Set(k, env[k])
	}
	f.Set("COMPOSE_PROJECT_NAME", s.Project)
	f.Set("COMPOSE_FILE", composeFileList(inst))
	if err := f.Save(); err != nil {
		return err
	}
	if o.RemoveBootUnit != nil {
		if err := o.RemoveBootUnit(); err != nil {
			return err
		}
	}
	s.Rendered = rendered
	s.Ejected = &Ejected{At: time.Now().UTC().Truncate(time.Second), Rexctl: o.Rexctl}
	s.UpdatedAt = time.Now().UTC().Truncate(time.Second)
	if err := s.Save(inst.StatePath()); err != nil {
		return err
	}
	for name := range rendered {
		if _, mounted := stack.MountedFiles[name]; !mounted && !inst.OldLayout() {
			if err := removeIfExists(inst.Generated(name)); err != nil {
				return err
			}
		}
	}
	return nil
}

func writePlain(inst *Instance) (map[string]string, error) {
	out := map[string]string{}
	for _, name := range stack.FileNames() {
		data, err := os.ReadFile(inst.Generated(name))
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			return nil, err
		}
		data = stack.Plain(name, data)
		if err := envfile.WriteFileAtomic(inst.Path(name), data, false); err != nil {
			return nil, err
		}
		out[name] = stack.Hash(data)
	}
	return out, nil
}

func composeFileList(inst *Instance) string {
	names := []string{stack.ComposeFile}
	for _, o := range inst.Overlays() {
		names = append(names, stack.OverlayFiles[o])
	}
	if exists(inst.Path(CustomCompose)) {
		names = append(names, CustomCompose)
	}
	return strings.Join(names, string(os.PathListSeparator))
}

func importEjected(ctx context.Context, dir string, e *engine.Engine, s *State, dry bool) (*State, *ImportReport, error) {
	r := &ImportReport{Source: SourceEjected, Dir: dir, Project: s.Project, Mine: map[string]string{}}
	if e != nil && s.Engine.ID != "" && e.ID != "" && e.ID != s.Engine.ID {
		return nil, nil, otherEngine(s.Engine.Kind, string(e.Kind))
	}
	custom := readCustom(dir)
	if what := customProblem(custom); what != "" {
		return nil, nil, refusal(fmt.Sprintf("remove %s from %s", what, CustomCompose))
	}
	set, err := releaseSet(dir, s.Release)
	if err != nil {
		return nil, nil, err
	}
	updated, err := handUpdate(ctx, dir, e, s, custom)
	if err != nil {
		return nil, nil, err
	}
	envPath := filepath.Join(dir, EnvFileName)
	data, err := os.ReadFile(envPath)
	haveEnv := err == nil
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return nil, nil, err
	}
	if haveEnv {
		env, deduped, err := parseEnv(data)
		if err != nil {
			return nil, nil, err
		}
		r.DedupedEnv = deduped
		changes, _, err := compareEnv(set, custom, s, env)
		if err != nil {
			return nil, nil, err
		}
		applyChanges(s, changes)
		for _, c := range changes {
			r.Changed = append(r.Changed, c.Key)
		}
		for _, k := range slices.Sorted(maps.Keys(s.Settings)) {
			if removedFromEnv(k, env) {
				delete(s.Settings, k)
				r.Changed = append(r.Changed, k)
			}
		}
		if s.AdminImport != nil {
			r.AdminKeys = slices.Sorted(maps.Keys(s.AdminImport.Values))
		}
	}
	if updated != nil {
		minted, err := mint(s.Secrets, true)
		if err != nil {
			return nil, nil, err
		}
		r.Minted, r.Release = minted, updated.Release
		s.PreviousRelease, s.Release = s.Release, updated.Release
		s.Images = Images{Release: updated.Release, Refs: maps.Clone(updated.Images)}
		if s.Rendered, err = stack.Hashes(stack.Embedded(), stack.OverlaysFor(s.Settings), s.Images.Refs, s.Project); err != nil {
			return nil, nil, err
		}
	}
	for _, name := range slices.Sorted(maps.Keys(s.Rendered)) {
		data, err := os.ReadFile(filepath.Join(dir, name))
		if err != nil || stack.Hash(data) == s.Rendered[name] {
			continue
		}
		r.KeepAsMine = append(r.KeepAsMine, name)
		if !dry {
			kept, err := KeepEdited(&Instance{Dir: dir, State: s}, name)
			if err != nil {
				return nil, nil, err
			}
			r.Mine[name] = kept
		}
		s.Rendered[name] = stack.Hash(data)
	}
	if dry {
		return nil, r, nil
	}
	if haveEnv {
		if err := copyPrivate(envPath, filepath.Join(dir, StateDirName, ReleasesDir, s.Release, EjectedEnvFile)); err != nil {
			return nil, nil, err
		}
	}
	s.Ejected = nil
	pin(s, e)
	if err := finishImport(dir, s); err != nil {
		return nil, nil, err
	}
	return s, r, nil
}

func handUpdate(ctx context.Context, dir string, e *engine.Engine, s *State, custom []byte) (*stack.ImageSet, error) {
	shipped, err := stack.EmbeddedImages()
	if err != nil || len(shipped.Images) == 0 {
		return nil, err
	}
	newer := shipped.Release == stack.Embedded().Release && VersionBelow(s.Release, shipped.Release)
	live, err := inspect(ctx, e, s.Project, dir)
	if err != nil {
		return nil, err
	}
	compose, err := os.ReadFile(filepath.Join(dir, stack.ComposeFile))
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return nil, err
	}
	own := stack.ComposeImages(custom)
	var updated *stack.ImageSet
	for _, seen := range []map[string]string{stack.ComposeImages(compose), live.images} {
		maps.DeleteFunc(seen, func(svc, ref string) bool {
			_, fluxer := shipped.Images[svc]
			_, mine := own[svc]
			return !fluxer || mine || strings.Contains(ref, "${")
		})
		switch {
		case sameImages(seen, s.Images.Refs):
		case newer && sameImages(seen, shipped.Images):
			updated = &shipped
		default:
			return nil, &AdoptError{
				Msg: "rexctl doesn't know the Fluxer images your instance runs.",
				Fix: "Download the latest rexctl, then run rexctl takeover again.",
			}
		}
	}
	return updated, nil
}

func sameImages(seen, refs map[string]string) bool {
	for svc, ref := range seen {
		_, got, _ := strings.Cut(ref, "@")
		_, want, _ := strings.Cut(refs[svc], "@")
		if got != "" && want != "" {
			if got != want {
				return false
			}
		} else if !sameImage(ref, refs[svc]) {
			return false
		}
	}
	return true
}

func removedFromEnv(key string, env map[string]string) bool {
	if isRawVar(key) {
		_, ok := env[key]
		return !ok
	}
	return false
}
