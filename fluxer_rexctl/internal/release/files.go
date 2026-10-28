// SPDX-License-Identifier: AGPL-3.0-or-later

package release

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"sort"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	StackDirName  = "stack"
	snapshotState = "state.json"
	imagesFile    = "images.json"
	unknownName   = "before-rexctl"
)

var (
	ErrNoSnapshot    = errors.New("release: no saved files for this release")
	ErrPostgresMajor = errors.New("release: the database major version changes")
)

type PostgresMajorError struct {
	From int
	To   int
}

func (e *PostgresMajorError) Error() string {
	return fmt.Sprintf("release: database major version changes from %d to %d", e.From, e.To)
}

func (e *PostgresMajorError) Is(target error) bool { return target == ErrPostgresMajor }

type imageList struct {
	Release string   `json:"release"`
	Images  []string `json:"images"`
}

func DirName(version string) string {
	if version == "" {
		return unknownName
	}
	return version
}

func Dir(inst *instance.Instance, version string) string {
	return inst.ReleaseDir(DirName(version))
}

func StackDir(inst *instance.Instance, version string) string {
	return filepath.Join(Dir(inst, version), StackDirName)
}

func private(version string) bool {
	return DirName(version) == stack.LegacyRelease
}

func writeFile(path string, data []byte, private bool) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return err
	}
	return envfile.WriteFileAtomic(path, data, private)
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func WriteRelease(inst *instance.Instance, set *stack.Set) error {
	if set == nil || set.Release == "" {
		return errors.New("release: the release set has no name")
	}
	dir := StackDir(inst, set.Release)
	if exists(filepath.Join(dir, stack.ProfilesFile)) {
		return nil
	}
	return stack.WriteSet(dir, set)
}

func Snapshot(inst *instance.Instance, version string, images []string) error {
	dir := Dir(inst, version)
	state, err := json.MarshalIndent(inst.State, "", "  ")
	if err != nil {
		return err
	}
	if err := writeFile(filepath.Join(dir, snapshotState), append(state, '\n'), true); err != nil {
		return err
	}
	if images != nil || !exists(filepath.Join(dir, imagesFile)) {
		return writeImages(dir, version, images)
	}
	return nil
}

func HasSnapshot(inst *instance.Instance, version string) bool {
	dir := Dir(inst, version)
	if !exists(filepath.Join(dir, snapshotState)) {
		return false
	}
	return exists(filepath.Join(dir, StackDirName, stack.ProfilesFile))
}

func SnapshotState(inst *instance.Instance, version string) (*instance.State, error) {
	dir := Dir(inst, version)
	data, err := os.ReadFile(filepath.Join(dir, snapshotState))
	if errors.Is(err, fs.ErrNotExist) {
		return nil, ErrNoSnapshot
	}
	if err != nil {
		return nil, err
	}
	return instance.ParseState(data, inst.Dir)
}

func LoadSet(inst *instance.Instance, version string) (*stack.Set, error) {
	set, err := stack.LoadSet(StackDir(inst, version))
	if errors.Is(err, fs.ErrNotExist) {
		return nil, ErrNoSnapshot
	}
	return set, err
}

type Restored struct {
	Files    []string
	Settings []string
	Restart  []string
}

func Restore(inst *instance.Instance, version string) (*Restored, error) {
	if !HasSnapshot(inst, version) {
		return nil, ErrNoSnapshot
	}
	saved, err := SnapshotState(inst, version)
	if err != nil {
		return nil, err
	}
	set, err := LoadSet(inst, version)
	if err != nil {
		return nil, err
	}
	if len(saved.Images.Refs) == 0 && Valid(version) {
		return nil, fmt.Errorf("release: the snapshot of %s records no images", version)
	}
	r, err := inst.Render(set, stack.OverlaysFor(saved.Settings), saved.Images.Refs)
	if err != nil {
		return nil, err
	}
	out := &Restored{Files: r.Changed, Restart: r.RestartNeeded, Settings: changedKeys(inst.State.Settings, saved.Settings)}
	for k, v := range inst.State.Secrets {
		if _, ok := saved.Secrets[k]; !ok {
			saved.Secrets[k] = v
		}
	}
	inst.State.Release = saved.Release
	inst.State.Images = saved.Images
	inst.State.Settings = saved.Settings
	inst.State.Secrets = saved.Secrets
	inst.State.Rendered = r.Hashes
	return out, nil
}

func changedKeys(a, b map[string]string) []string {
	var out []string
	for k, v := range a {
		if w, ok := b[k]; !ok || w != v {
			out = append(out, k)
		}
	}
	for k := range b {
		if _, ok := a[k]; !ok {
			out = append(out, k)
		}
	}
	sort.Strings(out)
	return out
}

type MissingProblem struct {
	Files      []string
	Message    string
	Fix        string
	Restorable bool
}

func expectedFiles(inst *instance.Instance) []string {
	if len(inst.State.Rendered) > 0 {
		return slices.Sorted(maps.Keys(inst.State.Rendered))
	}
	out := []string{stack.ComposeFile, stack.Caddyfile}
	for _, o := range stack.OverlaysFor(inst.State.Settings) {
		out = append(out, stack.OverlayFiles[o])
	}
	return out
}

func MissingFiles(inst *instance.Instance) []string {
	if inst.State == nil {
		return nil
	}
	var out []string
	for _, name := range expectedFiles(inst) {
		if _, err := os.Lstat(inst.Generated(name)); errors.Is(err, fs.ErrNotExist) {
			out = append(out, name)
		}
	}
	return out
}

func Missing(inst *instance.Instance) *MissingProblem {
	files := MissingFiles(inst)
	if len(files) == 0 {
		return nil
	}
	p := &MissingProblem{Files: files, Message: "Some of Fluxer's files are missing from " + inst.Dir + "."}
	if len(files) == 1 {
		p.Message = files[0] + " is missing from " + inst.Dir + "."
	}
	p.Restorable = canRestoreMissing(inst, files)
	switch {
	case !p.Restorable:
		p.Fix = "Restore your instance from a backup with rexctl restore."
	case len(files) == 1:
		p.Fix = "Run rexctl doctor --fix to put it back."
	default:
		p.Fix = "Run rexctl doctor --fix to put them back."
	}
	return p
}

func canRestoreMissing(inst *instance.Instance, files []string) bool {
	set, err := LoadSet(inst, inst.State.Release)
	if err != nil {
		return false
	}
	for _, name := range files {
		if _, err := fs.Stat(set.Files, name); err != nil {
			return false
		}
	}
	return true
}

func RestoreMissing(inst *instance.Instance) ([]string, error) {
	missing := MissingFiles(inst)
	if len(missing) == 0 {
		return nil, nil
	}
	set, err := LoadSet(inst, inst.State.Release)
	if err != nil {
		return nil, err
	}
	source := set.Files
	if set.Release != stack.LegacyRelease {
		tmp, err := os.MkdirTemp(inst.StateDir(), "render-")
		if err != nil {
			return nil, err
		}
		defer os.RemoveAll(tmp)
		if _, err := stack.RenderAt(set, stack.OverlaysFor(inst.State.Settings), inst.State.Images.Refs, inst.Project(), tmp, inst.OldLayout()); err != nil {
			return nil, err
		}
		source = os.DirFS(stack.FilesDir(tmp, inst.OldLayout()))
	}
	var restored []string
	for _, name := range missing {
		data, err := fs.ReadFile(source, name)
		if err != nil || len(data) == 0 {
			continue
		}
		if set.Release == stack.LegacyRelease && !inst.OldLayout() {
			data = stack.Managed(name, data)
		}
		if err := writeFile(inst.Generated(name), data, false); err != nil {
			return restored, err
		}
		if inst.State.Rendered == nil {
			inst.State.Rendered = map[string]string{}
		}
		inst.State.Rendered[name] = stack.Hash(data)
		restored = append(restored, name)
	}
	if len(restored) < len(missing) {
		return restored, ErrNoSnapshot
	}
	return restored, nil
}

func RestartFor(changed []string) []string {
	set := map[string]bool{}
	for _, rel := range changed {
		for file, services := range stack.MountedFiles {
			if filepath.Clean(rel) == filepath.Clean(file) {
				for _, s := range services {
					set[s] = true
				}
			}
		}
	}
	return slices.Sorted(maps.Keys(set))
}

func writeImages(dir, version string, refs []string) error {
	uniq := map[string]bool{}
	list := imageList{Release: version, Images: []string{}}
	for _, r := range refs {
		if r != "" && !uniq[r] {
			uniq[r] = true
			list.Images = append(list.Images, r)
		}
	}
	sort.Strings(list.Images)
	data, err := json.MarshalIndent(list, "", "  ")
	if err != nil {
		return err
	}
	return writeFile(filepath.Join(dir, imagesFile), append(data, '\n'), private(version))
}

func Images(inst *instance.Instance, version string) ([]string, error) {
	data, err := os.ReadFile(filepath.Join(Dir(inst, version), imagesFile))
	if err != nil {
		return nil, err
	}
	var list imageList
	if err := json.Unmarshal(data, &list); err != nil {
		return nil, err
	}
	return list.Images, nil
}

func KeptImages(inst *instance.Instance) []string {
	entries, _ := os.ReadDir(inst.Path(instance.StateDirName, instance.ReleasesDir))
	var out []string
	for _, d := range entries {
		if !d.IsDir() {
			continue
		}
		refs, _ := Images(inst, d.Name())
		for _, r := range refs {
			if !slices.Contains(out, r) {
				out = append(out, r)
			}
		}
	}
	return out
}

func CheckPostgres(fromRef, toRef string) error {
	if fromRef == "" || toRef == "" {
		return nil
	}
	from, err := stack.PostgresMajor(fromRef)
	if err != nil {
		return nil
	}
	to, err := stack.PostgresMajor(toRef)
	if err != nil {
		return err
	}
	if to != from {
		return &PostgresMajorError{From: from, To: to}
	}
	return nil
}

func Prune(ctx context.Context, inst *instance.Instance, e *engine.Engine) error {
	remove, err := pruneReleases(inst)
	if err != nil || len(remove) == 0 || e == nil {
		return err
	}
	return e.RemoveImages(ctx, remove)
}

func pruneReleases(inst *instance.Instance) ([]string, error) {
	root := inst.Path(instance.StateDirName, instance.ReleasesDir)
	entries, err := os.ReadDir(root)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	keep := map[string]bool{
		DirName(inst.State.Release):         true,
		DirName(inst.State.PreviousRelease): true,
	}
	if u := inst.State.Upgrade; u != nil {
		keep[DirName(u.From)] = true
		keep[DirName(u.To)] = true
	}
	kept := map[string]bool{}
	for _, r := range inst.State.Images.Refs {
		kept[r] = true
	}
	var drop []string
	for _, d := range entries {
		if !d.IsDir() {
			continue
		}
		v := d.Name()
		refs, _ := Images(inst, v)
		if keep[v] {
			for _, r := range refs {
				kept[r] = true
			}
			continue
		}
		drop = append(drop, refs...)
		if v == stack.LegacyRelease {
			err = os.Remove(filepath.Join(root, v, imagesFile))
			if errors.Is(err, fs.ErrNotExist) {
				err = nil
			}
		} else {
			err = removeAll(filepath.Join(root, v))
		}
		if err != nil {
			return nil, err
		}
	}
	var remove []string
	seen := map[string]bool{}
	for _, r := range drop {
		if !kept[r] && !seen[r] {
			seen[r] = true
			remove = append(remove, r)
		}
	}
	return remove, nil
}

func removeAll(dir string) error {
	_ = filepath.WalkDir(dir, func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return nil
		}
		if d.IsDir() {
			_ = os.Chmod(p, 0o700)
		} else if d.Type().IsRegular() {
			_ = os.Chmod(p, 0o600)
		}
		return nil
	})
	return os.RemoveAll(dir)
}

func Switched(u *instance.Upgrade) bool {
	return slices.Index(Phases, u.Phase) >= slices.Index(Phases, PhaseRecreated)
}

func UnfinishedFix(u *instance.Upgrade) string {
	switch {
	case u.BackFailed && u.Backup != "":
		return fmt.Sprintf("Run rexctl upgrade to start Fluxer %s again, or rexctl rollback --restore-database to restore the backup from before the upgrade.", u.To)
	case u.BackFailed:
		return fmt.Sprintf("Run rexctl upgrade to start Fluxer %s again.", u.To)
	case !Switched(u):
		return fmt.Sprintf("Run rexctl upgrade to finish it, or rexctl rollback to stop it and stay on Fluxer %s.", u.From)
	}
	return "Run rexctl upgrade to finish it, or rexctl rollback to go back."
}
