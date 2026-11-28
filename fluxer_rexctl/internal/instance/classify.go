// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"go.yaml.in/yaml/v4"
)

type Kind string

const (
	KindNone       Kind = ""
	KindManaged    Kind = "managed"
	KindHandRunOld Kind = "hand-run-old"
	KindHandRunNew Kind = "hand-run-new"
	KindEjectedOld Kind = "ejected-old"
	KindUnclear    Kind = "unclear"
)

const (
	ComposeEnvFile = "fluxer.env"
	ReleaseImage   = "ghcr.io/fluxerapp/fluxer"
	ReleaseData    = "/var/lib/fluxer"
)

type FolderError struct {
	Dir    string
	Kind   Kind
	Failed []string
}

func (e *FolderError) Error() string {
	return fmt.Sprintf("instance: %s in %s", e.Kind, e.Dir)
}

func (e *FolderError) Is(target error) bool { return target == ErrLegacy }

func Classify(dir string) Kind {
	kind, _ := classify(dir)
	return kind
}

func classify(dir string) (Kind, []string) {
	if data, err := os.ReadFile(filepath.Join(dir, StateDirName, StateFileName)); err == nil {
		if ejectedOld(data) {
			return KindEjectedOld, nil
		}
		return KindManaged, nil
	}
	envPath := filepath.Join(dir, EnvFileName)
	env, _, envErr := loadLegacyEnv(envPath)
	var envMap map[string]string
	if env != nil {
		envMap = env.Map()
	}
	files, err := resolveComposeFiles(dir, envMap, os.Environ())
	if err != nil || len(files) == 0 {
		return KindNone, nil
	}
	services, readErr := composeServices(files)
	if exists(filepath.Join(dir, ComposeEnvFile)) && readErr == nil && releaseService(services) {
		return KindHandRunNew, nil
	}
	if !exists(envPath) {
		return KindNone, nil
	}
	var failed []string
	switch {
	case envErr != nil:
		failed = append(failed, fmt.Sprintf("rexctl can't read %s: %v", EnvFileName, envErr))
	case !hasFluxerKey(env):
		failed = append(failed, EnvFileName+" has no FLUXER_ setting.")
	}
	switch {
	case readErr != nil:
		failed = append(failed, readErr.Error())
	case len(services) <= 1:
		failed = append(failed, fmt.Sprintf("The compose files run %d %s, and the older layout runs one per service.", len(services), plural(len(services), "service", "services")))
	}
	if len(failed) == 0 {
		return KindHandRunOld, nil
	}
	return KindUnclear, failed
}

func plural(n int, one, many string) string {
	if n == 1 {
		return one
	}
	return many
}

func ejectedOld(data []byte) bool {
	var head struct {
		Runtime string   `json:"runtime"`
		Ejected *Ejected `json:"ejected"`
	}
	if json.Unmarshal(data, &head) != nil || head.Ejected == nil {
		return false
	}
	return head.Runtime == "" || head.Runtime == RuntimeCompose
}

type composeService struct {
	Image   string `yaml:"image"`
	Volumes []any  `yaml:"volumes"`
}

func composeServices(files []string) (map[string]composeService, error) {
	out := map[string]composeService{}
	for _, f := range files {
		data, err := os.ReadFile(f)
		if err != nil {
			return nil, fmt.Errorf("rexctl can't read %s.", filepath.Base(f))
		}
		var doc struct {
			Services map[string]composeService `yaml:"services"`
		}
		if err := yaml.Unmarshal(data, &doc); err != nil {
			return nil, fmt.Errorf("%s isn't a compose file rexctl can read.", filepath.Base(f))
		}
		for name, svc := range doc.Services {
			cur := out[name]
			if svc.Image != "" {
				cur.Image = svc.Image
			}
			cur.Volumes = append(cur.Volumes, svc.Volumes...)
			out[name] = cur
		}
	}
	return out, nil
}

func releaseService(services map[string]composeService) bool {
	if len(services) != 1 {
		return false
	}
	for _, svc := range services {
		if !strings.HasPrefix(svc.Image, ReleaseImage+":") {
			return false
		}
		return slices.ContainsFunc(svc.Volumes, func(v any) bool { return volumeTarget(v) == ReleaseData })
	}
	return false
}

func volumeTarget(v any) string {
	switch v := v.(type) {
	case string:
		parts := strings.Split(v, ":")
		if len(parts) < 2 {
			return ""
		}
		return filepath.Clean(parts[1])
	case map[string]any:
		t, _ := v["target"].(string)
		if t == "" {
			return ""
		}
		return filepath.Clean(t)
	}
	return ""
}

func folderError(dir string, withNew bool) error {
	kind, failed := classify(dir)
	switch {
	case kind == KindHandRunOld, kind == KindUnclear, withNew && kind == KindHandRunNew:
		return &FolderError{Dir: dir, Kind: kind, Failed: failed}
	}
	return nil
}
