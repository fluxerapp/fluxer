// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"path/filepath"

	"github.com/compose-spec/compose-go/v2/cli"
	"github.com/compose-spec/compose-go/v2/types"
)

func BindSources(ctx context.Context, s Spec) ([]string, error) {
	model, err := loadModel(ctx, s, cli.WithResolvedPaths(true))
	if err != nil {
		return nil, err
	}
	var out []string
	for _, svc := range model.AllServices() {
		for _, v := range svc.Volumes {
			if v.Type == types.VolumeTypeBind && v.Source != "" {
				src := v.Source
				if !filepath.IsAbs(src) {
					src = filepath.Join(s.Dir, src)
				}
				out = append(out, filepath.Clean(src))
			}
		}
	}
	return out, nil
}
