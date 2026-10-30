// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"encoding/json"
	"fmt"

	"go.yaml.in/yaml/v4"
)

func ServicesDefinition(set *Set, overlays []Overlay, images map[string]string, services ...string) (string, error) {
	files, _, err := renderFiles(set, overlays, images, defaultProject)
	if err != nil {
		return "", err
	}
	out := map[string][]any{}
	for _, f := range files {
		if f.name == Caddyfile {
			continue
		}
		var doc struct {
			Services map[string]any `yaml:"services"`
		}
		if err := yaml.Unmarshal(f.data, &doc); err != nil {
			return "", fmt.Errorf("stack: %s isn't valid YAML: %w", f.name, err)
		}
		for _, svc := range services {
			if def, ok := doc.Services[svc]; ok {
				out[svc] = append(out[svc], def)
			}
		}
	}
	data, err := json.Marshal(out)
	if err != nil {
		return "", err
	}
	return string(data), nil
}
