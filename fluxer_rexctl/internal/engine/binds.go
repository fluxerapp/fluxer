// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"path/filepath"
	"sort"

	"github.com/compose-spec/compose-go/v2/cli"
	"github.com/compose-spec/compose-go/v2/loader"
	"github.com/compose-spec/compose-go/v2/types"
)

func BindSources(ctx context.Context, dir, name string, files []string, env map[string]string) ([]string, error) {
	vars := make([]string, 0, len(env))
	for k, v := range env {
		vars = append(vars, k+"="+v)
	}
	sort.Strings(vars)
	opts, err := cli.NewProjectOptions(files,
		cli.WithWorkingDirectory(dir),
		cli.WithEnv(vars),
		cli.WithName(name),
		cli.WithDefaultProfiles(),
		cli.WithResolvedPaths(true),
		cli.WithLoadOptions(func(o *loader.Options) {
			o.SkipValidation = true
			o.SkipConsistencyCheck = true
		}),
	)
	if err != nil {
		return nil, err
	}
	model, err := opts.LoadProject(ctx)
	if err != nil {
		return nil, err
	}
	var out []string
	for _, svc := range model.AllServices() {
		for _, v := range svc.Volumes {
			if v.Type == types.VolumeTypeBind && v.Source != "" {
				src := v.Source
				if !filepath.IsAbs(src) {
					src = filepath.Join(dir, src)
				}
				out = append(out, filepath.Clean(src))
			}
		}
	}
	return out, nil
}
