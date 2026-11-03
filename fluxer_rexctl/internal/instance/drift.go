// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"bytes"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type DriftReport struct {
	Changed []string
	Missing []string
	Env     *EnvDrift
}

type EnvDrift struct {
	Path    string
	Same    bool
	Changes []EnvChange
	Unused  []string
}

type DriftError struct {
	Msg   string
	Fix   string
	Lines []string
}

func (e *DriftError) Error() string { return e.Msg }

func Drift(inst *Instance) (*DriftReport, error) {
	r := &DriftReport{}
	if inst == nil || inst.State == nil || inst.State.Ejected != nil {
		return r, nil
	}
	for _, name := range slices.Sorted(maps.Keys(inst.State.Rendered)) {
		data, err := os.ReadFile(inst.Generated(name))
		switch {
		case errors.Is(err, fs.ErrNotExist):
			r.Missing = append(r.Missing, name)
		case err != nil:
			return nil, err
		case stack.Hash(data) != inst.State.Rendered[name]:
			r.Changed = append(r.Changed, name)
		}
	}
	env, err := envDrift(inst)
	if err != nil {
		return nil, err
	}
	r.Env = env
	return r, nil
}

func envDrift(inst *Instance) (*EnvDrift, error) {
	path := inst.Path(EnvFileName)
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	env, _, err := parseEnv(data)
	if err != nil {
		return nil, err
	}
	set, err := inst.Set()
	if err != nil {
		return nil, err
	}
	custom, err := inst.Custom()
	if err != nil {
		return nil, err
	}
	changes, unused, err := compareEnv(set, custom, inst.State, env)
	if err != nil {
		return nil, err
	}
	return &EnvDrift{Path: path, Same: len(changes) == 0 && len(unused) == 0, Changes: changes, Unused: unused}, nil
}

func (r *DriftReport) Err() error {
	if r == nil || r.Env == nil || r.Env.Same {
		return nil
	}
	lines := make([]string, 0, len(r.Env.Changes))
	for _, c := range r.Env.Changes {
		lines = append(lines, c.Command())
	}
	return &DriftError{
		Msg:   "Your instance has a .env file, and rexctl doesn't read it.",
		Fix:   r.Env.Fix(),
		Lines: lines,
	}
}

func (r *EnvDrift) Fix() string {
	if len(r.Unused) == 0 {
		return "Run rexctl doctor --fix to keep these values and remove the file, or change them with these commands. rexctl removes the file once you've set each of them:"
	}
	unused := fmt.Sprintf("Fluxer doesn't use %s. Run rexctl config list to find the setting you meant and change it with rexctl config set.", ui.JoinAnd(r.Unused))
	if len(r.Changes) == 0 {
		return unused + " Then delete " + r.Path + "."
	}
	return unused + " Change the others with these commands, then delete " + r.Path + ":"
}

func (r *EnvDrift) Settings() map[string]*string {
	out := map[string]*string{}
	for _, c := range r.Changes {
		if c.Kind == ChangeSetting {
			v := c.Value
			out[c.Key] = &v
		}
	}
	return out
}

func (r *EnvDrift) Values(kind ChangeKind) map[string]string {
	out := map[string]string{}
	for _, c := range r.Changes {
		if c.Kind == kind {
			out[c.Key] = c.Value
		}
	}
	return out
}

func KeepEdited(inst *Instance, name string) (string, error) {
	data, err := os.ReadFile(inst.Path(name))
	if err != nil {
		return "", err
	}
	dst := name + ".mine"
	for n := 2; ; n++ {
		existing, err := os.ReadFile(inst.Path(dst))
		if errors.Is(err, fs.ErrNotExist) || (err == nil && bytes.Equal(existing, data)) {
			break
		}
		if err != nil {
			return "", err
		}
		dst = fmt.Sprintf("%s.mine.%d", name, n)
	}
	if err := envfile.WriteFileAtomic(inst.Path(dst), data, false); err != nil {
		return "", err
	}
	return dst, nil
}

func SettleEnv(inst *Instance, set []string) (string, error) {
	if inst.State == nil || inst.State.Ejected != nil {
		return "", nil
	}
	r, err := envDrift(inst)
	if err != nil || r == nil || len(r.Unused) > 0 {
		return "", err
	}
	for _, c := range r.Changes {
		if !slices.Contains(set, c.Key) {
			return "", nil
		}
	}
	return r.Path, RemoveEnv(inst)
}

func RemoveEnv(inst *Instance) error {
	return removeIfExists(inst.Path(EnvFileName))
}

func copyPrivate(src, dst string) error {
	data, err := os.ReadFile(src)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(dst), 0o700); err != nil {
		return err
	}
	return envfile.WriteFileAtomic(dst, data, true)
}

const (
	EditedDir    = "edited"
	diffLines    = 12
	editedFormat = "20060102-150405"
)

type Edit struct {
	Name string
	Copy string
	Diff []string
}

func KeepEdits(inst *Instance) ([]Edit, error) {
	r, err := Drift(inst)
	if err != nil || len(r.Changed) == 0 {
		return nil, err
	}
	set, err := inst.Set()
	if err != nil {
		return nil, err
	}
	want, err := stack.Files(set, inst.Overlays(), inst.State.Images.Refs, inst.Project())
	if err != nil {
		return nil, err
	}
	var out []Edit
	for _, name := range r.Changed {
		data, err := os.ReadFile(inst.Generated(name))
		if err != nil {
			return nil, err
		}
		kept, err := keepEdit(inst, name, data)
		if err != nil {
			return nil, err
		}
		w := want[name]
		if inst.OldLayout() {
			w = stack.Plain(name, w)
		}
		out = append(out, Edit{Name: name, Copy: kept, Diff: ShortDiff(w, data)})
	}
	return out, nil
}

func keepEdit(inst *Instance, name string, data []byte) (string, error) {
	dir := inst.Path(StateDirName, EditedDir)
	if entries, err := os.ReadDir(dir); err == nil {
		for _, e := range entries {
			if !strings.HasPrefix(e.Name(), name+".") {
				continue
			}
			if old, err := os.ReadFile(filepath.Join(dir, e.Name())); err == nil && bytes.Equal(old, data) {
				return filepath.Join(dir, e.Name()), nil
			}
		}
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	path := filepath.Join(dir, name+"."+time.Now().UTC().Format(editedFormat))
	return path, envfile.WriteFileAtomic(path, data, false)
}

func ShortDiff(from, to []byte) []string {
	a, b := splitLines(from), splitLines(to)
	lcs := make([][]int, len(a)+1)
	for i := range lcs {
		lcs[i] = make([]int, len(b)+1)
	}
	for i := len(a) - 1; i >= 0; i-- {
		for j := len(b) - 1; j >= 0; j-- {
			if a[i] == b[j] {
				lcs[i][j] = lcs[i+1][j+1] + 1
			} else {
				lcs[i][j] = max(lcs[i+1][j], lcs[i][j+1])
			}
		}
	}
	var out []string
	i, j := 0, 0
	for i < len(a) || j < len(b) {
		switch {
		case i < len(a) && j < len(b) && a[i] == b[j]:
			i, j = i+1, j+1
		case i < len(a) && (j == len(b) || lcs[i+1][j] >= lcs[i][j+1]):
			out = append(out, "- "+a[i])
			i++
		default:
			out = append(out, "+ "+b[j])
			j++
		}
	}
	if len(out) > diffLines {
		out = append(out[:diffLines], fmt.Sprintf("and %d more changed lines", len(out)-diffLines))
	}
	return out
}

func splitLines(data []byte) []string {
	text := strings.TrimSuffix(string(data), "\n")
	if text == "" {
		return nil
	}
	return strings.Split(text, "\n")
}

const seenMarker = ".seen"

func UnseenEdits(inst *Instance) []string {
	dir := inst.Path(StateDirName, EditedDir)
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var seen time.Time
	if info, err := os.Stat(filepath.Join(dir, seenMarker)); err == nil {
		seen = info.ModTime()
	}
	var out []string
	for _, e := range entries {
		if e.Name() == seenMarker || e.IsDir() {
			continue
		}
		if info, err := e.Info(); err == nil && info.ModTime().After(seen) {
			out = append(out, filepath.Join(dir, e.Name()))
		}
	}
	return out
}

func SeenEdits(inst *Instance) {
	path := inst.Path(StateDirName, EditedDir, seenMarker)
	now := time.Now()
	if os.Chtimes(path, now, now) != nil {
		_ = os.WriteFile(path, nil, 0o600)
	}
}
