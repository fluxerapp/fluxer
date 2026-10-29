// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"embed"
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
	"go.yaml.in/yaml/v4"
)

const (
	LegacyRelease = "legacy"
	ProfilesFile  = "profiles.json"
	VarsFile      = "vars.json"
)

type Set struct {
	Release  string
	Files    fs.FS
	Profiles map[string]map[string]string
	Vars     []string
}

//go:embed files
var releaseFiles embed.FS

var embeddedSet = sync.OnceValue(func() *Set {
	files, err := fs.Sub(releaseFiles, "files")
	if err != nil {
		panic(err)
	}
	set := &Set{Release: embeddedRelease(), Files: files, Profiles: map[string]map[string]string{}}
	data, err := fs.ReadFile(files, ProfilesFile)
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(data, &set.Profiles); err != nil {
		panic(fmt.Errorf("stack: %s: %w", ProfilesFile, err))
	}
	if set.Vars, err = FilesVars(files); err != nil {
		panic(err)
	}
	return set
})

func Embedded() *Set { return embeddedSet() }

func embeddedRelease() string {
	data := imageSetJSON
	if p := os.Getenv(ImageSetEnv); p != "" {
		if b, err := os.ReadFile(p); err == nil {
			data = b
		}
	}
	var s struct {
		Release string `json:"release"`
	}
	if json.Unmarshal(data, &s) != nil || s.Release == "" {
		return "dev"
	}
	return s.Release
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
	data, err := os.ReadFile(filepath.Join(dir, VarsFile))
	switch {
	case err == nil:
		if err := json.Unmarshal(data, &set.Vars); err != nil {
			return nil, fmt.Errorf("stack: %s: %w", filepath.Join(dir, VarsFile), err)
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
		if name == ProfilesFile || name == VarsFile {
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
	return writeReadOnly(filepath.Join(dir, VarsFile), append(data, '\n'))
}

func writeReadOnly(target string, data []byte) error {
	if err := os.Chmod(target, 0o644); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	if err := os.WriteFile(target, data, 0o444); err != nil {
		return err
	}
	return os.Chmod(target, 0o444)
}

func ComposeFileNames(files fs.FS) ([]string, error) {
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
