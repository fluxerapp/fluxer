// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	_ "embed"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path"
	"path/filepath"
	"slices"
	"sync"

	"github.com/compose-spec/compose-go/v2/template"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"go.yaml.in/yaml/v4"
)

const (
	LegacyRelease = "legacy"
	ProfilesFile  = "profiles.json"
	SettingsFile  = "settings.json"
	varsFile      = "vars.json"
)

type Set struct {
	Release  string
	Files    fs.FS
	Profiles map[string]map[string]string
	Vars     []string
	Catalog  []Setting
}

type Setting struct {
	Name    string   `json:"name"`
	Default string   `json:"default,omitempty"`
	Sized   string   `json:"sized,omitempty"`
	PerCPU  bool     `json:"per_cpu,omitempty"`
	Readers []string `json:"readers"`
	Secret  bool     `json:"secret,omitempty"`
}

func (s *Set) Setting(name string) (Setting, bool) {
	for _, c := range s.Catalog {
		if c.Name == name {
			return c, true
		}
	}
	return Setting{}, false
}

func ParseCatalog(data []byte) ([]Setting, error) {
	var c []Setting
	if err := json.Unmarshal(data, &c); err != nil {
		return nil, fmt.Errorf("stack: the settings catalog: %w", err)
	}
	if len(c) == 0 {
		return nil, errors.New("stack: the settings catalog is empty")
	}
	return c, nil
}

func CatalogSet(release string, catalog []Setting) *Set {
	vars := make([]string, len(catalog))
	for i, c := range catalog {
		vars[i] = c.Name
	}
	slices.Sort(vars)
	return &Set{Release: release, Profiles: embeddedProfiles(), Vars: vars, Catalog: catalog}
}

func LoadCatalogSet(dir, release string) (*Set, error) {
	data, err := os.ReadFile(filepath.Join(dir, SettingsFile))
	if err != nil {
		return nil, err
	}
	c, err := ParseCatalog(data)
	if err != nil {
		return nil, fmt.Errorf("%s: %w", filepath.Join(dir, SettingsFile), err)
	}
	set := CatalogSet(release, c)
	if data, err := os.ReadFile(filepath.Join(dir, ProfilesFile)); err == nil {
		profiles := map[string]map[string]string{}
		if err := json.Unmarshal(data, &profiles); err != nil {
			return nil, fmt.Errorf("stack: %s: %w", filepath.Join(dir, ProfilesFile), err)
		}
		set.Profiles = profiles
	}
	return set, nil
}

//go:embed profiles.json
var profilesJSON []byte

var embeddedProfiles = sync.OnceValue(func() map[string]map[string]string {
	profiles := map[string]map[string]string{}
	if err := json.Unmarshal(profilesJSON, &profiles); err != nil {
		panic(fmt.Errorf("stack: %s: %w", ProfilesFile, err))
	}
	return profiles
})

var release string

func EmbeddedRelease() string {
	if release == "" {
		return "dev"
	}
	return release
}

func LoadSet(dir string) (*Set, error) {
	info, err := os.Stat(dir)
	if err != nil {
		return nil, err
	}
	if !info.IsDir() {
		return nil, fmt.Errorf("stack: %s is not a folder", dir)
	}
	set := &Set{
		Release:  filepath.Base(filepath.Dir(filepath.Clean(dir))),
		Files:    os.DirFS(dir),
		Profiles: map[string]map[string]string{},
	}
	if data, err := os.ReadFile(filepath.Join(dir, ProfilesFile)); err == nil {
		if err := json.Unmarshal(data, &set.Profiles); err != nil {
			return nil, fmt.Errorf("stack: %s: %w", filepath.Join(dir, ProfilesFile), err)
		}
	} else if !errors.Is(err, fs.ErrNotExist) {
		return nil, err
	}
	data, err := os.ReadFile(filepath.Join(dir, varsFile))
	switch {
	case err == nil:
		if err := json.Unmarshal(data, &set.Vars); err != nil {
			return nil, fmt.Errorf("stack: %s: %w", filepath.Join(dir, varsFile), err)
		}
	case errors.Is(err, fs.ErrNotExist):
		if set.Vars, err = FilesVars(set.Files); err != nil {
			return nil, err
		}
	default:
		return nil, err
	}
	return set, nil
}

func WriteSet(dir string, s *Set) error {
	if s != nil && s.Catalog != nil {
		return writeCatalogSet(dir, s)
	}
	if s == nil || s.Files == nil {
		return errors.New("stack: no release files to write")
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}
	err := fs.WalkDir(s.Files, ".", func(name string, d fs.DirEntry, err error) error {
		if err != nil || name == "." {
			return err
		}
		if d.IsDir() {
			return os.MkdirAll(filepath.Join(dir, filepath.FromSlash(name)), 0o755)
		}
		if name == ProfilesFile || name == varsFile {
			return nil
		}
		data, err := fs.ReadFile(s.Files, name)
		if err != nil {
			return err
		}
		return writeReadOnly(filepath.Join(dir, filepath.FromSlash(name)), data)
	})
	if err != nil {
		return err
	}
	profiles := s.Profiles
	if profiles == nil {
		profiles = map[string]map[string]string{}
	}
	data, err := json.MarshalIndent(profiles, "", "  ")
	if err != nil {
		return err
	}
	if err := writeReadOnly(filepath.Join(dir, ProfilesFile), append(data, '\n')); err != nil {
		return err
	}
	vars := s.Vars
	if vars == nil {
		vars = []string{}
	}
	if data, err = json.MarshalIndent(vars, "", "  "); err != nil {
		return err
	}
	return writeReadOnly(filepath.Join(dir, varsFile), append(data, '\n'))
}

func writeCatalogSet(dir string, s *Set) error {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}
	data, err := json.MarshalIndent(s.Catalog, "", "  ")
	if err != nil {
		return err
	}
	if err := writeReadOnly(filepath.Join(dir, SettingsFile), append(data, '\n')); err != nil {
		return err
	}
	if data, err = json.MarshalIndent(s.Profiles, "", "  "); err != nil {
		return err
	}
	return writeReadOnly(filepath.Join(dir, ProfilesFile), append(data, '\n'))
}

func writeReadOnly(target string, data []byte) error {
	if err := os.Chmod(target, 0o644); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	if err := envfile.WriteFileAtomic(target, data, false); err != nil {
		return err
	}
	return os.Chmod(target, 0o444)
}

func ComposeFileNames(files fs.FS) ([]string, error) {
	if files == nil {
		return nil, nil
	}
	entries, err := fs.ReadDir(files, ".")
	if err != nil {
		return nil, err
	}
	var out []string
	for _, e := range entries {
		if !e.IsDir() && isComposeName(e.Name()) {
			out = append(out, e.Name())
		}
	}
	return out, nil
}

func isComposeName(name string) bool {
	ext := path.Ext(name)
	return ext == ".yml" || ext == ".yaml"
}

func FilesVars(files fs.FS) ([]string, error) {
	names, err := ComposeFileNames(files)
	if err != nil {
		return nil, err
	}
	seen := map[string]bool{}
	for _, name := range names {
		data, err := fs.ReadFile(files, name)
		if err != nil {
			return nil, err
		}
		fv, err := ParseVars(name, data)
		if err != nil {
			return nil, err
		}
		for _, v := range fv.All {
			seen[v] = true
		}
	}
	return slices.Sorted(maps.Keys(seen)), nil
}

type FileVars struct {
	All      []string
	Services map[string][]string
}

func ParseVars(name string, data []byte) (*FileVars, error) {
	var dict map[string]any
	if err := yaml.Unmarshal(data, &dict); err != nil {
		return nil, fmt.Errorf("stack: reading %s: %w", name, err)
	}
	out := &FileVars{All: varNames(template.ExtractVariables(dict, template.DefaultPattern)), Services: map[string][]string{}}
	services, _ := dict["services"].(map[string]any)
	for svc, body := range services {
		m, ok := body.(map[string]any)
		if !ok {
			continue
		}
		if vars := varNames(template.ExtractVariables(m, template.DefaultPattern)); len(vars) > 0 {
			out.Services[svc] = vars
		}
	}
	return out, nil
}

func varNames(vars map[string]template.Variable) []string {
	out := make([]string, 0, len(vars))
	for name := range vars {
		if name != "" {
			out = append(out, name)
		}
	}
	slices.Sort(out)
	return out
}
