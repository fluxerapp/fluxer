// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"testing/fstest"
	"time"

	"go.yaml.in/yaml/v4"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

type Source string

const (
	SourceInstaller Source = "installer"
	SourceEjected   Source = "ejected"
	SourceManaged   Source = "managed"
)

const EjectedEnvFile = "ejected.env"

type ImportReport struct {
	Source        Source
	Dir           string
	Project       string
	PodmanCompose bool
	CustomFiles   []string
	KeepAsMine    []string
	Mine          map[string]string
	DeleteFiles   []string
	Minted        []string
	Placeholders  []string
	Dropped       []string
	Defaults      []string
	AdminKeys     []string
	Changed       []string
	DedupedEnv    bool
	EngineProxies bool
	RemovedEnv    bool
	Images        map[string]string
	Release       string
}

func (r *ImportReport) DroppedLine() string {
	if len(r.Dropped) == 0 {
		return ""
	}
	saved := "They are"
	if len(r.Dropped) == 1 {
		saved = "It's"
	}
	return fmt.Sprintf("rexctl left out %s Fluxer doesn't read: %s. %s saved in %s.",
		count(len(r.Dropped), "setting", "settings"), strings.Join(r.Dropped, ", "), saved,
		filepath.Join(StateDirName, ReleasesDir, LegacyRelease, EnvFileName))
}

func count(n int, one, many string) string {
	if n == 1 {
		return "1 " + one
	}
	return strconv.Itoa(n) + " " + many
}

func Import(dir string, e *engine.Engine) (*State, *ImportReport, error) {
	return runImport(context.Background(), dir, e, false)
}

func PreviewImport(dir string, e *engine.Engine) (*ImportReport, error) {
	_, r, err := runImport(context.Background(), dir, e, true)
	return r, err
}

func runImport(ctx context.Context, dir string, e *engine.Engine, dry bool) (*State, *ImportReport, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return nil, nil, err
	}
	statePath := filepath.Join(abs, StateDirName, StateFileName)
	data, err := os.ReadFile(statePath)
	if errors.Is(err, fs.ErrNotExist) {
		return importInstaller(ctx, abs, e, dry)
	}
	if err != nil {
		return nil, nil, err
	}
	s, err := ParseState(data, abs)
	if err != nil {
		return nil, nil, err
	}
	if s.Ejected != nil {
		return importEjected(ctx, abs, e, s, dry)
	}
	r := &ImportReport{Source: SourceManaged, Dir: abs, Project: s.Project}
	inst := &Instance{Dir: abs, State: s}
	if d, err := envDrift(inst); err == nil && d != nil && d.Same && !dry {
		if err := removeIfExists(inst.Path(EnvFileName)); err != nil {
			return nil, nil, err
		}
		r.RemovedEnv = true
	}
	return s, r, nil
}

func refusal(what string) *AdoptError {
	return &AdoptError{
		Msg: "rexctl can't manage this setup.",
		Fix: "Keep running it with docker compose, or " + what + " and run rexctl takeover again.",
	}
}

func importInstaller(ctx context.Context, dir string, e *engine.Engine, dry bool) (*State, *ImportReport, error) {
	l, err := DetectLegacy(dir)
	if err != nil {
		return nil, nil, err
	}
	p, err := PlanAdopt(l)
	if err != nil {
		return nil, nil, err
	}
	r := &ImportReport{
		Source:      SourceInstaller,
		Dir:         dir,
		Project:     p.Project,
		CustomFiles: p.CustomFiles,
		KeepAsMine:  p.KeepAsMine,
		Mine:        p.Mine,
		DeleteFiles: p.DeleteFiles,
		DedupedEnv:  p.DedupedEnv,
	}
	var custom []byte
	if p.customFrom != "" {
		if custom, err = l.read(p.customFrom); err != nil {
			return nil, nil, err
		}
		if what := customProblem(custom); what != "" {
			return nil, nil, refusal(fmt.Sprintf("remove %s from %s", what, displayPath(dir, p.customFrom)))
		}
	}
	live, err := inspect(ctx, e, p.Project, dir)
	if err != nil {
		return nil, nil, err
	}
	r.PodmanCompose = live.podmanCompose
	r.Images = live.images
	if live.podmanCompose && e.Kind == engine.Podman && VersionBelow(e.Version, engine.MinPodman) {
		return nil, nil, &AdoptError{
			Msg: "rexctl can't manage this setup.",
			Fix: fmt.Sprintf("Keep running it with podman-compose, or upgrade Podman to %s or newer and run rexctl takeover again.", engine.MinPodman),
		}
	}
	set, err := legacySet(l)
	if err != nil {
		return nil, nil, err
	}
	examples := map[string]string{}
	if data, err := l.read(filepath.Join(dir, ".env.example")); err == nil {
		if m, _, err := parseEnv(data); err == nil {
			examples = m
		}
	}
	envData, err := l.read(filepath.Join(dir, EnvFileName))
	if err != nil {
		return nil, nil, err
	}
	env, _, err := parseEnv(envData)
	if err != nil {
		return nil, nil, err
	}
	overlays := make([]stack.Overlay, 0, len(p.Overlays))
	for _, o := range p.Overlays {
		overlays = append(overlays, stack.Overlay(o))
	}
	vals, err := importValues(valueInput{
		env: env, set: set, custom: custom, overlays: overlays,
		examples: examples, proxies: live.proxies,
	})
	if err != nil {
		return nil, nil, err
	}
	minted, err := mint(vals.secrets, live.haveVolumes)
	if err != nil {
		return nil, nil, err
	}
	fill(r, vals, minted)
	if dry {
		return nil, r, nil
	}
	if err := adoptFiles(ctx, p); err != nil {
		return nil, nil, err
	}
	installed := time.Now().UTC().Truncate(time.Second)
	if fi, err := os.Stat(filepath.Join(l.snapshotDir(), EnvFileName)); err == nil {
		installed = fi.ModTime().UTC().Truncate(time.Second)
	}
	release, refs := LegacyRelease, live.images
	if images, ok := bundledRelease(l, p, custom); ok {
		release, refs = images.Release, images.Images
	}
	s := &State{
		Schema:      StateSchema,
		Project:     p.Project,
		Release:     release,
		Images:      Images{Release: release, Refs: refs},
		Settings:    vals.settings,
		Secrets:     vals.secrets,
		SetupDone:   true,
		InstalledAt: installed,
		UpdatedAt:   time.Now().UTC().Truncate(time.Second),
	}
	if len(vals.admin) > 0 {
		s.AdminImport = &AdminImport{Values: vals.admin}
	}
	if release != LegacyRelease {
		ConvertLegacy(s, stack.Embedded(), refs)
	}
	pin(s, e)
	s.normalize()
	if err := finishImport(dir, s); err != nil {
		return nil, nil, err
	}
	return s, r, nil
}

func bundledRelease(l *Legacy, p *AdoptPlan, custom []byte) (stack.ImageSet, bool) {
	images, err := stack.EmbeddedImages()
	if err != nil || len(images.Images) == 0 || images.Release != stack.Embedded().Release {
		return images, false
	}
	if slices.ContainsFunc(p.KeepAsMine, func(name string) bool { return name != ".env.example" }) || setsImage(custom, images.Images) {
		return images, false
	}
	data, err := l.read(l.ComposeFiles[0])
	if err != nil {
		return images, false
	}
	want, err := stack.RenderCompose(stack.Embedded(), images.Images, p.Project)
	return images, err == nil && bytes.Equal(normalizeLF(data), want)
}

func setsImage(custom []byte, images map[string]string) bool {
	var doc struct {
		Services map[string]map[string]any `yaml:"services"`
	}
	if err := yaml.Unmarshal(custom, &doc); err != nil {
		return true
	}
	for name, svc := range doc.Services {
		if _, ok := svc["image"]; ok && images[name] != "" {
			return true
		}
	}
	return false
}

func finishImport(dir string, s *State) error {
	if err := writeReleaseSet(dir, s.Release); err != nil {
		return err
	}
	if err := recordRendered(dir, s); err != nil {
		return err
	}
	s.UpdatedAt = time.Now().UTC().Truncate(time.Second)
	s.rootLayout = true
	if err := s.Save(filepath.Join(dir, StateDirName, StateFileName)); err != nil {
		return err
	}
	return removeIfExists(filepath.Join(dir, EnvFileName))
}

func fill(r *ImportReport, vals *valueResult, minted []string) {
	r.Minted = minted
	r.Placeholders = vals.placeholders
	r.Dropped = vals.dropped
	r.Defaults = vals.defaults
	r.AdminKeys = slices.Sorted(maps.Keys(vals.admin))
	r.EngineProxies = vals.engineProxies
}

func pin(s *State, e *engine.Engine) {
	if e == nil {
		return
	}
	s.Engine = EngineRef{Kind: string(e.Kind), Host: e.Host, ID: e.ID}
}

func mint(values map[string]string, haveVolumes bool) ([]string, error) {
	minted, err := secrets.Mint(values, haveVolumes)
	var missing *secrets.StatefulMissingError
	if errors.As(err, &missing) {
		return nil, &AdoptError{
			Msg: fmt.Sprintf("Your instance's data needs %s, and rexctl can't find %s.", strings.Join(missing.Keys, ", "), pick(len(missing.Keys), "its value", "their values")),
			Fix: "Restore the instance from a backup with rexctl restore.",
		}
	}
	return minted, err
}

func pick(n int, one, many string) string {
	if n == 1 {
		return one
	}
	return many
}

type liveState struct {
	haveVolumes   bool
	podmanCompose bool
	images        map[string]string
	proxies       engine.Proxies
}

func inspect(ctx context.Context, e *engine.Engine, project, dir string) (*liveState, error) {
	live := &liveState{haveVolumes: true, images: map[string]string{}}
	if e == nil {
		return live, nil
	}
	live.proxies = e.Proxies()
	vols, err := e.Volumes(ctx, project)
	if err != nil {
		return nil, err
	}
	live.haveVolumes = len(vols) > 0
	containers, err := e.Named(project, dir).PS(ctx)
	if err != nil {
		return nil, err
	}
	for _, c := range containers {
		if c.Labels[engine.PodmanComposeLabel] != "" {
			live.podmanCompose = true
		}
		if c.Service == "" || c.Image == "" {
			continue
		}
		live.images[c.Service] = c.Image
		if strings.Contains(c.Image, "@") || c.ImageID == "" {
			continue
		}
		if d, err := e.RepoDigest(ctx, c.ImageID, c.Image); err == nil && d != "" {
			live.images[c.Service] = d
		}
	}
	return live, nil
}

func VersionBelow(version, floor string) bool {
	parse := func(v string) []int {
		v = strings.TrimPrefix(strings.TrimSpace(v), "v")
		v, _, _ = strings.Cut(v, "-")
		var out []int
		for _, part := range strings.SplitN(v, ".", 3) {
			n, err := strconv.Atoi(part)
			if err != nil {
				return nil
			}
			out = append(out, n)
		}
		return out
	}
	a, b := parse(version), parse(floor)
	if a == nil || b == nil {
		return false
	}
	for i := range b {
		x := 0
		if i < len(a) {
			x = a[i]
		}
		if x != b[i] {
			return x < b[i]
		}
	}
	return false
}

func customProblem(data []byte) string {
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
		if _, ok := svc["build"]; ok {
			return "build"
		}
		if _, ok := svc["env_file"]; ok {
			return "env_file"
		}
		switch ext := svc["extends"].(type) {
		case map[string]any:
			if _, ok := ext["file"]; ok {
				return "extends"
			}
		}
	}
	return ""
}

func legacySet(l *Legacy) (*stack.Set, error) {
	files := fstest.MapFS{}
	for i, f := range l.ComposeFiles {
		data, err := l.read(f)
		if err != nil {
			continue
		}
		name := filepath.Base(f)
		if i == 0 {
			name = stack.ComposeFile
		} else if overlayFor(f, l.Dir) == "" {
			continue
		}
		files[name] = &fstest.MapFile{Data: normalizeLF(data), Mode: 0o444}
	}
	if data, err := l.read(filepath.Join(l.Dir, stack.Caddyfile)); err == nil {
		files[stack.Caddyfile] = &fstest.MapFile{Data: normalizeLF(data), Mode: 0o444}
	}
	return newSet(LegacyRelease, files, nil)
}

func newSet(release string, files fs.FS, profiles map[string]map[string]string) (*stack.Set, error) {
	vars, err := stack.FilesVars(files)
	if err != nil {
		return nil, err
	}
	if profiles == nil {
		profiles = map[string]map[string]string{}
	}
	return &stack.Set{Release: release, Files: files, Profiles: profiles, Vars: vars}, nil
}

func renderedNames(dir string) []string {
	var out []string
	for _, n := range stack.FileNames() {
		if isFile(filepath.Join(dir, n)) {
			out = append(out, n)
		}
	}
	return out
}

func diskSet(dir, release string, profiles map[string]map[string]string) (*stack.Set, error) {
	files := fstest.MapFS{}
	for _, name := range renderedNames(dir) {
		data, err := os.ReadFile(filepath.Join(dir, name))
		if err != nil {
			return nil, err
		}
		files[name] = &fstest.MapFile{Data: data, Mode: 0o444}
	}
	if _, ok := files[stack.ComposeFile]; !ok {
		return nil, fmt.Errorf("%w: %s", ErrNoReleaseSet, release)
	}
	return newSet(release, files, profiles)
}

func writeReleaseSet(dir, release string) error {
	target := filepath.Join(dir, StateDirName, ReleasesDir, release, StackDir)
	if isDir(target) {
		return nil
	}
	var set *stack.Set
	if embedded := stack.Embedded(); release == embedded.Release {
		set = embedded
	} else {
		var profiles map[string]map[string]string
		if release != LegacyRelease {
			profiles = stack.Embedded().Profiles
		}
		var err error
		if set, err = diskSet(dir, release, profiles); err != nil {
			return err
		}
	}
	return stack.WriteSet(target, set)
}

func recordRendered(dir string, s *State) error {
	if len(s.Rendered) > 0 {
		return nil
	}
	if s.Rendered == nil {
		s.Rendered = map[string]string{}
	}
	for _, name := range renderedNames(dir) {
		data, err := os.ReadFile(filepath.Join(dir, name))
		if err != nil {
			return err
		}
		s.Rendered[name] = stack.Hash(data)
	}
	return nil
}

func releaseSet(dir, release string) (*stack.Set, error) {
	set, err := stack.LoadSet(filepath.Join(dir, StateDirName, ReleasesDir, release, StackDir))
	if err == nil {
		return set, nil
	}
	if embedded := stack.Embedded(); release == embedded.Release {
		return embedded, nil
	}
	var profiles map[string]map[string]string
	if release != LegacyRelease {
		profiles = stack.Embedded().Profiles
	}
	return diskSet(dir, release, profiles)
}

func RetiredService(dir string) string {
	var doc map[string]any
	if err := yaml.Unmarshal(readCustom(dir), &doc); err != nil {
		return ""
	}
	services, _ := doc["services"].(map[string]any)
	for _, name := range slices.Sorted(maps.Keys(services)) {
		svc, _ := services[name].(map[string]any)
		if _, ok := svc["image"]; ok || slices.Contains(stack.Services, name) {
			continue
		}
		return name
	}
	return ""
}

func DropMergedShards(dir string) ([]string, error) {
	path := filepath.Join(dir, CustomCompose)
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	out, dropped := withoutMergedShards(data)
	if len(dropped) == 0 {
		return nil, nil
	}
	return dropped, envfile.WriteFileAtomic(path, out, false)
}

func withoutMergedShards(data []byte) ([]byte, []string) {
	var doc yaml.Node
	var before map[string]any
	if yaml.Unmarshal(data, &doc) != nil || yaml.Unmarshal(data, &before) != nil || len(doc.Content) == 0 {
		return nil, nil
	}
	root := doc.Content[0]
	if root.Kind != yaml.MappingNode {
		return nil, nil
	}
	var services *yaml.Node
	for i := 0; i+1 < len(root.Content); i += 2 {
		if root.Content[i].Value == "services" {
			services = root.Content[i+1]
		}
	}
	decoded, _ := before["services"].(map[string]any)
	if services == nil || services.Kind != yaml.MappingNode || services.Style&yaml.FlowStyle != 0 || decoded == nil {
		return nil, nil
	}
	lines := strings.Split(string(data), "\n")
	remove := make([]bool, len(lines))
	var dropped []string
	for i := 0; i+1 < len(services.Content); i += 2 {
		key := services.Content[i]
		base, ok := strings.CutSuffix(key.Value, "-shard")
		if !ok || !slices.Contains(stack.Services, base) || slices.Contains(stack.Services, key.Value) {
			continue
		}
		shard, have := decoded[key.Value]
		if want, ok := decoded[base]; !have || !ok || !reflect.DeepEqual(shard, want) {
			continue
		}
		start, end := key.Line-1, key.Line
		for end < len(lines) && (strings.TrimSpace(lines[end]) == "" || indent(lines[end]) >= key.Column) {
			end++
		}
		for end > start+1 && strings.TrimSpace(lines[end-1]) == "" {
			end--
		}
		for j := start; j < end; j++ {
			remove[j] = true
		}
		dropped = append(dropped, key.Value)
	}
	if len(dropped) == 0 {
		return nil, nil
	}
	kept := make([]string, 0, len(lines))
	for i, l := range lines {
		if !remove[i] {
			kept = append(kept, l)
		}
	}
	out := []byte(strings.Join(kept, "\n"))
	var after map[string]any
	if yaml.Unmarshal(out, &after) != nil {
		return nil, nil
	}
	for _, name := range dropped {
		delete(decoded, name)
	}
	if !reflect.DeepEqual(before, after) {
		return nil, nil
	}
	slices.Sort(dropped)
	return out, dropped
}

func indent(line string) int {
	return len(line) - len(strings.TrimLeft(line, " "))
}

func readCustom(dir string) []byte {
	data, _ := os.ReadFile(filepath.Join(dir, CustomCompose))
	return data
}

func otherEngine(pinned, connected string) *AdoptError {
	return refusal(fmt.Sprintf("connect rexctl to %s instead of %s", engineName(pinned), engineName(connected)))
}

func engineName(kind string) string {
	switch kind {
	case string(engine.Podman):
		return "Podman"
	case string(engine.Docker):
		return "Docker"
	}
	return "the engine that runs it"
}
