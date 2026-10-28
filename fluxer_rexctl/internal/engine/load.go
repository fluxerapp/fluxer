// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"

	"github.com/compose-spec/compose-go/v2/cli"
	"github.com/compose-spec/compose-go/v2/template"
	"github.com/compose-spec/compose-go/v2/types"
	"github.com/docker/compose/v5/pkg/api"
	"github.com/docker/compose/v5/pkg/compose"
	"go.yaml.in/yaml/v4"
)

func (e *Engine) Named(name, dir string) *Project {
	return &Project{Engine: e, Name: name, Dir: dir}
}

func (e *Engine) Load(ctx context.Context, s Spec) (*Project, error) {
	files := make([]string, 0, len(s.Files))
	for _, f := range s.Files {
		if !filepath.IsAbs(f) {
			f = filepath.Join(s.Dir, f)
		}
		files = append(files, f)
	}
	for _, f := range files {
		if err := refuseUnsupported(f); err != nil {
			return nil, err
		}
	}
	env := make([]string, 0, len(s.Env)+1)
	for k, v := range s.Env {
		env = append(env, k+"="+v)
	}
	if _, ok := s.Env["PWD"]; !ok {
		env = append(env, "PWD="+s.Dir)
	}
	sort.Strings(env)
	opts, err := cli.NewProjectOptions(files,
		cli.WithWorkingDirectory(s.Dir),
		cli.WithEnv(env),
		cli.WithName(s.Name),
		cli.WithDefaultProfiles(),
	)
	if err != nil {
		return nil, &Error{Op: "load", Class: ClassInvalidConfig, Err: err}
	}
	model, err := opts.LoadProject(ctx)
	if err != nil {
		return nil, loadError(err)
	}
	for name, svc := range model.Services {
		if svc.Build != nil {
			return nil, &Error{Op: "load", Class: ClassInvalidConfig, Err: fmt.Errorf("service %s has a build section, which rexctl can't run", name)}
		}
		if len(svc.EnvFiles) > 0 {
			return nil, &Error{Op: "load", Class: ClassInvalidConfig, Err: fmt.Errorf("service %s reads an env_file, which rexctl doesn't support", name)}
		}
		svc.CustomLabels = map[string]string{
			api.ProjectLabel:     model.Name,
			api.ServiceLabel:     name,
			api.VersionLabel:     api.ComposeVersion,
			api.WorkingDirLabel:  model.WorkingDir,
			api.ConfigFilesLabel: strings.Join(model.ComposeFiles, ","),
			api.OneoffLabel:      "False",
		}
		model.Services[name] = svc
	}
	model = model.WithoutUnnecessaryResources()
	return &Project{Engine: e, Name: model.Name, Dir: s.Dir, Model: model}, nil
}

func loadError(err error) error {
	var missing *template.MissingRequiredError
	if errors.As(err, &missing) {
		detailf("engine: required variable %s is missing", missing.Variable)
		return &Error{Op: "load", Class: ClassInvalidConfig, Err: errors.New("a setting Fluxer needs is missing")}
	}
	return &Error{Op: "load", Class: ClassInvalidConfig, Err: err}
}

type rawFile struct {
	Include  any `yaml:"include"`
	Services map[string]struct {
		Build   any `yaml:"build"`
		EnvFile any `yaml:"env_file"`
		Extends any `yaml:"extends"`
	} `yaml:"services"`
}

func refuseUnsupported(path string) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return &Error{Op: "load", Class: ClassInvalidConfig, Err: err}
	}
	var raw rawFile
	if err := yaml.Unmarshal(data, &raw); err != nil {
		return &Error{Op: "load", Class: ClassInvalidConfig, Err: fmt.Errorf("%s: %w", filepath.Base(path), err)}
	}
	name := filepath.Base(path)
	if raw.Include != nil {
		return &Error{Op: "load", Class: ClassInvalidConfig, Err: fmt.Errorf("%s uses include, which rexctl doesn't support", name)}
	}
	for svc, s := range raw.Services {
		switch {
		case s.Build != nil:
			return &Error{Op: "load", Class: ClassInvalidConfig, Err: fmt.Errorf("%s: service %s has a build section, which rexctl can't run", name, svc)}
		case s.EnvFile != nil:
			return &Error{Op: "load", Class: ClassInvalidConfig, Err: fmt.Errorf("%s: service %s reads an env_file, which rexctl doesn't support", name, svc)}
		case extendsFile(s.Extends):
			return &Error{Op: "load", Class: ClassInvalidConfig, Err: fmt.Errorf("%s: service %s extends another file, which rexctl doesn't support", name, svc)}
		}
	}
	return nil
}

func extendsFile(v any) bool {
	m, ok := v.(map[string]any)
	if !ok {
		return false
	}
	f, ok := m["file"]
	return ok && f != nil && f != ""
}

func (p *Project) needModel(op string) error {
	if p.Model == nil {
		return &Error{Op: op, Class: ClassInvalidConfig, Err: errors.New("the project was opened by name only")}
	}
	return nil
}

func (p *Project) Services() []string {
	if p.Model == nil {
		return nil
	}
	all := p.Model.ServiceNames()
	names := make([]string, 0, len(all))
	for _, name := range all {
		if svc := p.Model.Services[name]; svc.GetScale() > 0 {
			names = append(names, name)
		}
	}
	slices.Sort(names)
	return names
}

func (p *Project) Images() []string {
	if p.Model == nil {
		return nil
	}
	seen := map[string]bool{}
	var out []string
	for _, name := range p.Services() {
		img := p.Model.Services[name].Image
		if img != "" && !seen[img] {
			seen[img] = true
			out = append(out, img)
		}
	}
	return out
}

func (p *Project) Image(service string) string {
	if p.Model == nil {
		return ""
	}
	return p.Model.Services[service].Image
}

func (p *Project) Hashes() (map[string]string, error) {
	if err := p.needModel("hash"); err != nil {
		return nil, err
	}
	out := make(map[string]string, len(p.Model.Services))
	for name, svc := range p.Model.Services {
		h, err := compose.ServiceHash(svc)
		if err != nil {
			return nil, &Error{Op: "hash", Class: ClassInvalidConfig, Err: err}
		}
		out[name] = h
	}
	return out, nil
}

func copyModel(m *types.Project) *types.Project {
	c := *m
	c.Services = make(types.Services, len(m.Services))
	for k, v := range m.Services {
		v.CustomLabels = maps.Clone(v.CustomLabels)
		c.Services[k] = v
	}
	c.Networks = make(types.Networks, len(m.Networks))
	for k, v := range m.Networks {
		c.Networks[k] = v
	}
	return &c
}
