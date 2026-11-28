// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"time"

	"github.com/compose-spec/compose-go/v2/types"
	"go.yaml.in/yaml/v4"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	HandRunDir       = "handrun"
	EjectedStateFile = "state.ejected.json"
)

type HandRunFolder struct {
	Dir      string
	Project  string
	Ejected  *State
	Env      map[string]string
	Files    []string
	Overlays []stack.Overlay
	Custom   string
}

type HandRunPlan struct {
	Folder  *HandRunFolder
	Release string
	Staged  *Instance
	Kept    int
	Dropped []string
	Unknown []string
	Minted  []string

	set    *stack.Set
	setDir string
	tmp    string
}

var ErrHandRunEdited = errors.New("instance: a compose file differs from the release")

func handRunRefusal(msg, fix string) error { return ui.Refused(msg, fix) }

func ReadHandRun(dir string) (*HandRunFolder, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return nil, err
	}
	kind := Classify(abs)
	if kind != KindHandRunOld && kind != KindEjectedOld {
		return nil, ErrNotFound
	}
	h := &HandRunFolder{Dir: abs}
	if kind == KindEjectedOld {
		data, err := os.ReadFile(filepath.Join(abs, StateDirName, StateFileName))
		if err != nil {
			return nil, err
		}
		if h.Ejected, err = ParseState(data, abs); err != nil {
			return nil, err
		}
	}
	l, err := detectLegacy(abs, h.Ejected != nil)
	if err != nil {
		return nil, err
	}
	if !projectName.MatchString(l.Project) {
		return nil, handRunRefusal(fmt.Sprintf("rexctl can't use the compose project name %s.", l.Project), "Set COMPOSE_PROJECT_NAME in .env to lowercase letters, digits, dashes and underscores, then run this again.")
	}
	h.Project = l.Project
	data, err := os.ReadFile(filepath.Join(abs, EnvFileName))
	if err != nil {
		return nil, err
	}
	if h.Env, _, err = parseEnv(data); err != nil {
		return nil, &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("rexctl can't read .env in %s.", abs), Fix: "Fix that file so docker compose config works, then run this again.", Err: err}
	}
	var extra []string
	for i, f := range l.ComposeFiles {
		name := filepath.Base(f)
		if filepath.Dir(f) != abs {
			return nil, handRunRefusal(fmt.Sprintf("Your compose files include %s, which is outside %s.", f, abs), "Copy what you need from it into custom.compose.yml in "+abs+", then run this again.")
		}
		if !isFile(f) {
			return nil, handRunRefusal(fmt.Sprintf("Your compose files include %s, but that file is missing.", name), "Put the file back, then run this again.")
		}
		if i == 0 {
			if name != stack.ComposeFile {
				return nil, handRunRefusal(fmt.Sprintf("Your main compose file is %s, and the older layout starts from %s.", name, stack.ComposeFile), "Put back "+stack.ComposeFile+" from the release, then run this again.")
			}
			h.Files = append(h.Files, name)
			continue
		}
		if o, ok := overlayNamed(name); ok {
			h.Overlays = append(h.Overlays, o)
			h.Files = append(h.Files, name)
			continue
		}
		extra = append(extra, name)
	}
	switch {
	case len(extra) > 1:
		return nil, handRunRefusal(fmt.Sprintf("This folder has %d files of your own changes, %s, and rexctl takes one.", len(extra), ui.JoinAnd(extra)), "Put them together in "+CustomCompose+", then run this again.")
	case len(extra) == 1 && extra[0] != CustomCompose:
		return nil, handRunRefusal(fmt.Sprintf("rexctl reads your own changes only from %s, and this folder has them in %s.", CustomCompose, extra[0]), "Rename it to "+CustomCompose+", list it in COMPOSE_FILE in .env, then run this again.")
	case len(extra) == 1:
		data, err := os.ReadFile(filepath.Join(abs, CustomCompose))
		if err != nil {
			return nil, err
		}
		if what := unreadableKey(data); what != "" {
			return nil, handRunRefusal(fmt.Sprintf("%s uses %s, which rexctl can't read.", CustomCompose, what), "Copy what it points to into the file, then run this again.")
		}
		h.Custom = CustomCompose
		h.Files = append(h.Files, CustomCompose)
	}
	return h, nil
}

func overlayNamed(name string) (stack.Overlay, bool) {
	for o, f := range stack.OverlayFiles {
		if f == name {
			return o, true
		}
	}
	return "", false
}

func unreadableKey(data []byte) string {
	var doc map[string]any
	if err := yaml.Unmarshal(data, &doc); err != nil {
		return ""
	}
	if _, ok := doc["include"]; ok {
		return "include"
	}
	services, _ := doc["services"].(map[string]any)
	for _, name := range slices.Sorted(maps.Keys(services)) {
		svc, _ := services[name].(map[string]any)
		for _, key := range []string{"build", "env_file", "extends"} {
			if _, ok := svc[key]; ok {
				return key
			}
		}
	}
	return ""
}

func (h *HandRunFolder) Tag() string { return strings.TrimSpace(h.Env["FLUXER_IMAGE_TAG"]) }

func (h *HandRunFolder) EngineRef() EngineRef {
	if h.Ejected != nil {
		return h.Ejected.Engine
	}
	return EngineRef{}
}

func (h *HandRunFolder) Plan(ctx context.Context, e *engine.Engine, release, setDir string) (*HandRunPlan, error) {
	set, err := stack.LoadSet(setDir)
	if err != nil {
		return nil, err
	}
	var refs map[string]string
	if h.Ejected != nil && h.Ejected.Release == release {
		refs = maps.Clone(h.Ejected.Images.Refs)
	}
	if err := h.match(set, release, refs); err != nil {
		return nil, err
	}
	var custom []byte
	if h.Custom != "" {
		if custom, err = os.ReadFile(filepath.Join(h.Dir, h.Custom)); err != nil {
			return nil, err
		}
	}
	vals, err := importHandRun(h.Env, set, custom, h.Overlays)
	if errors.Is(err, errListenConflict) {
		return nil, handRunRefusal("FLUXER_HTTP_PORT and FLUXER_HTTPS_PORT in .env use different addresses, and rexctl runs Fluxer on one address for both.", "Give FLUXER_HTTP_PORT and FLUXER_HTTPS_PORT in .env the same address, then run this again.")
	}
	if err != nil {
		return nil, err
	}
	if got := stack.OverlaysFor(vals.settings); !slices.Equal(sortedOverlays(got), sortedOverlays(h.Overlays)) {
		return nil, handRunRefusal(fmt.Sprintf("rexctl can't tell how Fluxer runs, because the compose files in %s don't match the settings in .env.", h.Dir), "Check COMPOSE_FILE and the settings for the files it lists, then run this again.")
	}
	have := true
	if e != nil {
		vols, err := e.Volumes(ctx, h.Project)
		if err != nil {
			return nil, err
		}
		have = len(vols) > 0
		if err := h.sameImages(ctx, e); err != nil {
			return nil, err
		}
	}
	minted, err := secrets.Mint(vals.secrets, have)
	var missing *secrets.StatefulMissingError
	if errors.As(err, &missing) {
		return nil, handRunRefusal(fmt.Sprintf(".env has no %s, and the data in %s needs it.", strings.Join(missing.Keys, ", "), h.Dir), "Put the value back in .env, then run this again.")
	}
	if err != nil {
		return nil, err
	}
	s, err := h.state(e, release, refs, vals)
	if err != nil {
		return nil, err
	}
	tmp, err := os.MkdirTemp("", "rexctl-handrun-")
	if err != nil {
		return nil, err
	}
	p := &HandRunPlan{Folder: h, Release: release, Kept: vals.kept, Dropped: vals.dropped, Unknown: vals.unknown, Minted: minted, set: set, setDir: setDir, tmp: tmp}
	p.Staged = &Instance{Dir: h.Dir, State: s, staged: &staging{set: setDir, files: tmp}}
	r, err := p.Staged.Render(set, p.Staged.Overlays(), refs)
	if err != nil {
		p.Close()
		return nil, err
	}
	s.Rendered = r.Hashes
	if err := p.Staged.SameAsFiles(ctx); err != nil {
		p.Close()
		return nil, err
	}
	return p, nil
}

func sortedOverlays(in []stack.Overlay) []stack.Overlay {
	out := slices.Clone(in)
	slices.Sort(out)
	return slices.Compact(out)
}

func (p *HandRunPlan) Close() {
	if p != nil && p.tmp != "" {
		os.RemoveAll(p.tmp)
	}
}

func (h *HandRunFolder) match(set *stack.Set, release string, refs map[string]string) error {
	rendered, err := stack.Files(set, h.Overlays, refs, h.Project)
	if err != nil {
		return err
	}
	for _, name := range stack.FileNames() {
		data, err := os.ReadFile(filepath.Join(h.Dir, name))
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			return err
		}
		data = bytes.ReplaceAll(data, []byte("\r\n"), []byte("\n"))
		if want, ok := rendered[name]; ok && bytes.Equal(data, stack.Plain(name, want)) {
			continue
		}
		if raw, err := stack.ReadFile(set, name); err == nil && bytes.Equal(data, raw) {
			continue
		}
		fix := "Move your changes into " + CustomCompose + ", put back the file from the release, then run this again."
		if name == stack.Caddyfile {
			fix = "Move your changes into a .caddy file in " + CaddyDropInDir + ", put back the file from the release, then run this again."
		}
		return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("%s differs from the file Fluxer %s shipped.", name, release), Fix: fix, Err: ErrHandRunEdited}
	}
	return nil
}

func (h *HandRunFolder) sameImages(ctx context.Context, e *engine.Engine) error {
	containers, err := e.Named(h.Project, h.Dir).PS(ctx)
	if err != nil || len(containers) == 0 {
		return err
	}
	model, err := engine.Load(ctx, h.userSpec())
	if err != nil {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("rexctl can't read the compose files in %s.", h.Dir), Fix: "Check that docker compose config works in that folder, then run this again.", Err: err}
	}
	for _, c := range containers {
		svc, ok := model.Model.Services[c.Service]
		if !ok || !strings.Contains(c.Image, "@") || !strings.Contains(svc.Image, "@") {
			continue
		}
		_, have, _ := strings.Cut(c.Image, "@")
		_, want, _ := strings.Cut(svc.Image, "@")
		if have != want {
			return handRunRefusal(fmt.Sprintf("rexctl can't tell which release runs in %s, because the containers there run other images than its compose files.", h.Dir), "Finish the upgrade you started, then run this again.")
		}
	}
	return nil
}

func (h *HandRunFolder) userSpec() engine.Spec {
	files := make([]string, len(h.Files))
	for i, f := range h.Files {
		files[i] = filepath.Join(h.Dir, f)
	}
	return engine.Spec{Name: h.Project, Dir: h.Dir, Files: files, Env: h.Env}
}

func (h *HandRunFolder) state(e *engine.Engine, release string, refs map[string]string, vals *handRunValues) (*State, error) {
	now := time.Now().UTC().Truncate(time.Second)
	installed := now
	if fi, err := os.Stat(filepath.Join(h.Dir, EnvFileName)); err == nil {
		installed = fi.ModTime().UTC().Truncate(time.Second)
	}
	s := &State{
		Schema:      StateSchema,
		Project:     h.Project,
		Runtime:     RuntimeCompose,
		Release:     release,
		Images:      Images{Release: release, Refs: refs},
		Settings:    vals.settings,
		Secrets:     vals.secrets,
		SetupDone:   true,
		InstalledAt: installed,
		UpdatedAt:   now,
		Engine:      h.EngineRef(),
	}
	if e != nil {
		s.Engine = EngineRef{Kind: string(e.Kind), Host: e.Host, ID: e.ID}
	}
	if old := h.Ejected; old != nil {
		s.InstalledAt, s.BackupDir, s.KeepBackups = old.InstalledAt, old.BackupDir, old.KeepBackups
	}
	if len(vals.admin) > 0 {
		s.AdminImport = &AdminImport{Values: vals.admin}
	}
	hr, err := h.hashes(now)
	if err != nil {
		return nil, err
	}
	s.HandRun = hr
	s.normalize()
	return s, nil
}

func (h *HandRunFolder) recorded() []string {
	names := slices.Clone(h.Files)
	if isFile(filepath.Join(h.Dir, stack.Caddyfile)) && !slices.Contains(names, stack.Caddyfile) {
		names = append(names, stack.Caddyfile)
	}
	return names
}

func (h *HandRunFolder) hashes(at time.Time) (*HandRun, error) {
	hr := &HandRun{At: at}
	for _, name := range h.recorded() {
		sum, err := fileSum(filepath.Join(h.Dir, name))
		if err != nil {
			return nil, err
		}
		hr.Files = append(hr.Files, HandRunFile{Name: name, SHA256: sum})
	}
	sum, err := fileSum(filepath.Join(h.Dir, EnvFileName))
	if err != nil {
		return nil, err
	}
	hr.EnvSHA256 = sum
	return hr, nil
}

func fileSum(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	sum := sha256.Sum256(data)
	return hex.EncodeToString(sum[:]), nil
}

func (i *Instance) HandRunCopies() string { return i.Path(StateDirName, HandRunDir) }

func RecordHandRun(p *HandRunPlan) (inst *Instance, err error) {
	h := p.Folder
	state := filepath.Join(h.Dir, StateDirName, StateFileName)
	ejected := filepath.Join(h.Dir, StateDirName, EjectedStateFile)
	if h.Ejected != nil {
		if exists(ejected) {
			return nil, handRunRefusal(fmt.Sprintf("%s already has %s from an earlier try.", filepath.Join(h.Dir, StateDirName), EjectedStateFile), "Move it out of the folder, then run this again.")
		}
		if err := os.Rename(state, ejected); err != nil {
			return nil, err
		}
	}
	made := !isDir(filepath.Join(h.Dir, StateDirName))
	defer func() {
		if err == nil {
			return
		}
		if h.Ejected == nil && made {
			os.RemoveAll(filepath.Join(h.Dir, StateDirName))
			return
		}
		os.RemoveAll(filepath.Join(h.Dir, StateDirName, HandRunDir))
		os.Remove(state)
		if h.Ejected != nil {
			os.Rename(ejected, state)
		}
	}()
	if err := os.MkdirAll(filepath.Join(h.Dir, StateDirName), 0o700); err != nil {
		return nil, err
	}
	if err := envfile.RestrictToOwner(filepath.Join(h.Dir, StateDirName)); err != nil {
		return nil, err
	}
	s := *p.Staged.State
	inst = &Instance{Dir: h.Dir, State: &s}
	if target := inst.SetDir(p.Release); filepath.Clean(target) != filepath.Clean(p.setDir) && !isDir(target) {
		if err := stack.WriteSet(target, p.set); err != nil {
			return nil, err
		}
	}
	copies := inst.HandRunCopies()
	if err := os.MkdirAll(copies, 0o700); err != nil {
		return nil, err
	}
	for _, name := range append(h.recorded(), EnvFileName) {
		if err := copyHandRunFile(filepath.Join(h.Dir, name), filepath.Join(copies, name)); err != nil {
			return nil, err
		}
	}
	set, err := inst.Set()
	if err != nil {
		return nil, err
	}
	r, err := inst.Render(set, inst.Overlays(), inst.State.Images.Refs)
	if err != nil {
		return nil, err
	}
	inst.State.Rendered = r.Hashes
	if err := inst.Save(); err != nil {
		return nil, err
	}
	return inst, nil
}

func copyHandRunFile(from, to string) error {
	data, err := os.ReadFile(from)
	if err != nil {
		return err
	}
	return envfile.WriteFileAtomic(to, data, true)
}

func HandRunChanged(inst *Instance) string {
	hr := inst.State.HandRun
	if hr == nil {
		return ""
	}
	for _, f := range hr.Files {
		if sum, err := fileSum(inst.Path(f.Name)); err != nil || sum != f.SHA256 {
			return f.Name
		}
	}
	if sum, err := fileSum(inst.Path(EnvFileName)); err != nil || sum != hr.EnvSHA256 {
		return EnvFileName
	}
	return ""
}

func RemoveHandRunRecord(dir string) error {
	root := filepath.Join(dir, StateDirName)
	if !isFile(filepath.Join(root, EjectedStateFile)) {
		return os.RemoveAll(root)
	}
	entries, err := os.ReadDir(root)
	if err != nil {
		return err
	}
	for _, e := range entries {
		if e.Name() == EjectedStateFile {
			continue
		}
		if err := os.RemoveAll(filepath.Join(root, e.Name())); err != nil {
			return err
		}
	}
	return nil
}

func UndoHandRunRecord(dir string) error {
	root := filepath.Join(dir, StateDirName)
	ejected := filepath.Join(root, EjectedStateFile)
	if !isFile(ejected) {
		return os.RemoveAll(root)
	}
	if err := os.RemoveAll(filepath.Join(root, HandRunDir)); err != nil {
		return err
	}
	return os.Rename(ejected, filepath.Join(root, StateFileName))
}

func (i *Instance) handRunFolder() (*HandRunFolder, error) {
	data, err := os.ReadFile(i.Path(EnvFileName))
	if err != nil {
		return nil, err
	}
	env, _, err := parseEnv(data)
	if err != nil {
		return nil, err
	}
	files, err := resolveComposeFiles(i.Dir, env, os.Environ())
	if err != nil {
		return nil, err
	}
	h := &HandRunFolder{Dir: i.Dir, Project: i.Project(), Env: env}
	for _, f := range files {
		h.Files = append(h.Files, relPath(i.Dir, f))
	}
	return h, nil
}

func (i *Instance) SameAsFiles(ctx context.Context) error {
	h, err := i.handRunFolder()
	if err != nil {
		return err
	}
	user, err := engine.Load(ctx, h.userSpec())
	if err != nil {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("rexctl can't read the compose files in %s.", i.Dir), Fix: "Check that docker compose config works in that folder, then run this again.", Err: err}
	}
	env, err := i.Environment()
	if err != nil {
		return err
	}
	own, err := engine.Load(ctx, engine.Spec{Name: i.Project(), Dir: i.Dir, Files: i.ComposeFiles(), Env: env})
	if err != nil {
		return err
	}
	if diff := modelDiff(user.Model, own.Model, i.Dir); len(diff) > 0 {
		return handRunRefusal(fmt.Sprintf("The settings rexctl read from %s would run different containers than the files there, for %s.", i.Dir, ui.JoinAnd(diff)), "Check those settings in .env, then run this again.")
	}
	return nil
}

func modelDiff(user, own *types.Project, dir string) []string {
	gen := stack.GeneratedPath(dir) + string(filepath.Separator)
	var out []string
	names := slices.Sorted(maps.Keys(user.Services))
	for name := range own.Services {
		if !slices.Contains(names, name) {
			names = append(names, name)
		}
	}
	slices.Sort(names)
	for _, name := range names {
		a, okA := user.Services[name]
		b, okB := own.Services[name]
		if !okA || !okB {
			out = append(out, name)
			continue
		}
		for i := range b.Volumes {
			if rest, ok := strings.CutPrefix(b.Volumes[i].Source, gen); ok {
				b.Volumes[i].Source = filepath.Join(dir, rest)
			}
		}
		ma, mb := asMap(a), asMap(b)
		for _, key := range slices.Sorted(maps.Keys(merge(ma, mb))) {
			if reflect.DeepEqual(ma[key], mb[key]) {
				continue
			}
			if key == "environment" {
				ea, _ := ma[key].(map[string]any)
				eb, _ := mb[key].(map[string]any)
				for _, v := range slices.Sorted(maps.Keys(merge(ea, eb))) {
					if !reflect.DeepEqual(ea[v], eb[v]) && !mintedForEmpty(v, ea[v]) {
						out = append(out, name+" "+v)
					}
				}
				continue
			}
			out = append(out, name+" "+key)
		}
	}
	return out
}

func mintedForEmpty(name string, user any) bool {
	if v, ok := user.(string); user != nil && (!ok || v != "") {
		return false
	}
	for _, s := range secrets.Required {
		if s.Key == name {
			return !s.Stateful
		}
	}
	return false
}

func asMap(svc types.ServiceConfig) map[string]any {
	data, err := json.Marshal(svc)
	if err != nil {
		return nil
	}
	var out map[string]any
	if json.Unmarshal(data, &out) != nil {
		return nil
	}
	return out
}

func merge(a, b map[string]any) map[string]any {
	out := maps.Clone(a)
	if out == nil {
		out = map[string]any{}
	}
	maps.Copy(out, b)
	return out
}
