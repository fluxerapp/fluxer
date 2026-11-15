// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"maps"
	"slices"
	"strconv"
	"strings"

	"go.yaml.in/yaml/v4"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envdiff"
)

const (
	RustService      = "rust"
	RustServicesVar  = "FLUXER_SERVER_SERVICES"
	RustAdminPort    = 18083
	rustSvcPortVar   = "FLUXER_SVC_PORT"
	rustAdminPortVar = "FLUXER_ADMIN_PORT"
)

var RustServices = []string{"snowflakes", "users", "messages", "gifs", "unfurl", "push", "app-proxy", "admin"}

var rustSvcPorts = map[string]int{"snowflakes": 18090, "users": 18091, "messages": 18092, "gifs": 18093, "unfurl": 18094}

var rustImageEnv = map[string]map[string]string{
	"app-proxy": {
		"FLUXER_SELF_HOSTED":         "true",
		"FLUXER_STATIC_CDN_ENDPOINT": "",
		"FLUXER_APP_PROXY_HOST":      "0.0.0.0",
		"FLUXER_APP_PROXY_PORT":      "8080",
	},
	"admin": {
		"FLUXER_ADMIN_HOST": "0.0.0.0",
		"FLUXER_ADMIN_PORT": "8080",
	},
}

var rustInherited = []string{
	"HTTP_PROXY", "HTTPS_PROXY", "NO_PROXY", "http_proxy", "https_proxy", "no_proxy",
	"SSL_CERT_FILE", "NODE_EXTRA_CA_CERTS", "RUST_LOG",
}

var RustProcessKeys = append([]string{"PATH", "HOME", "TMPDIR", "TOKIO_WORKER_THREADS", "MALLOC_ARENA_MAX", "LD_PRELOAD", "MALLOC_CONF"}, rustInherited...)

func RustSingle(settings map[string]string) bool {
	return strings.TrimSpace(settings["rust.process"]) == "single"
}

func RunsIn(name string, single bool) string {
	if single && slices.Contains(RustServices, name) {
		return RustService
	}
	return name
}

func RustServiceEnvs(files [][]byte) (map[string]map[string]string, error) {
	out := map[string]map[string]string{}
	for _, svc := range RustServices {
		out[svc] = maps.Clone(rustImageEnv[svc])
		if out[svc] == nil {
			out[svc] = map[string]string{}
		}
	}
	for i, data := range files {
		envs, err := serviceEnvs(data)
		if err != nil {
			return nil, err
		}
		for _, svc := range RustServices {
			env, ok := envs[svc]
			if i == 0 && !ok {
				return nil, fmt.Errorf("stack: %s has no %s service", ComposeFile, svc)
			}
			maps.Copy(out[svc], env)
		}
	}
	return out, nil
}

func singleProcessEnv(envs map[string]map[string]string) map[string]map[string]string {
	out := make(map[string]map[string]string, len(envs))
	for svc, env := range envs {
		env = maps.Clone(env)
		if port, ok := rustSvcPorts[svc]; ok {
			env[rustSvcPortVar] = strconv.Itoa(port)
		}
		if svc == "admin" {
			env[rustAdminPortVar] = strconv.Itoa(RustAdminPort)
		}
		out[svc] = env
	}
	return out
}

func RustEnv(files [][]byte) (map[string]string, error) {
	envs, err := RustServiceEnvs(files)
	if err != nil {
		return nil, err
	}
	merged, err := envdiff.Encode(singleProcessEnv(envs), RustProcessKeys)
	if err != nil {
		return nil, err
	}
	for _, k := range rustInherited {
		for _, svc := range RustServices {
			if v, ok := envs[svc][k]; ok {
				merged[k] = v
				break
			}
		}
	}
	return merged, nil
}

func serviceEnvs(data []byte) (map[string]map[string]string, error) {
	var doc struct {
		Services map[string]struct {
			Environment yaml.Node `yaml:"environment"`
		} `yaml:"services"`
	}
	if err := yaml.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("stack: reading a compose file: %w", err)
	}
	out := map[string]map[string]string{}
	for name, svc := range doc.Services {
		env, err := envOf(&svc.Environment)
		if err != nil {
			return nil, fmt.Errorf("stack: %s: %w", name, err)
		}
		if env != nil {
			out[name] = env
		}
	}
	return out, nil
}

func envOf(n *yaml.Node) (map[string]string, error) {
	switch n.Kind {
	case 0:
		return nil, nil
	case yaml.MappingNode:
		var m map[string]*string
		if err := n.Decode(&m); err != nil {
			return nil, err
		}
		out := make(map[string]string, len(m))
		for k, v := range m {
			if v != nil {
				out[k] = *v
			}
		}
		return out, nil
	case yaml.SequenceNode:
		var list []string
		if err := n.Decode(&list); err != nil {
			return nil, err
		}
		out := make(map[string]string, len(list))
		for _, item := range list {
			if k, v, ok := strings.Cut(item, "="); ok {
				out[k] = v
			}
		}
		return out, nil
	}
	return nil, errors.New("environment is neither a mapping nor a list")
}

func renderRustOverlay(overlay []byte, files [][]byte) ([]byte, error) {
	env, err := RustEnv(files)
	if err != nil {
		return nil, err
	}
	var doc yaml.Node
	if err := yaml.Unmarshal(overlay, &doc); err != nil {
		return nil, fmt.Errorf("stack: %s isn't valid YAML: %w", OverlayFiles[OverlayRustServer], err)
	}
	if doc.Kind != yaml.DocumentNode || len(doc.Content) == 0 {
		return nil, fmt.Errorf("stack: %s is empty", OverlayFiles[OverlayRustServer])
	}
	target := mappingValue(mappingValue(mappingValue(doc.Content[0], "services"), RustService), "environment")
	if target == nil || target.Kind != yaml.MappingNode {
		return nil, fmt.Errorf("stack: %s has no environment for %s", OverlayFiles[OverlayRustServer], RustService)
	}
	static := map[string]bool{}
	for i := 0; i+1 < len(target.Content); i += 2 {
		static[target.Content[i].Value] = true
	}
	for _, k := range slices.Sorted(maps.Keys(env)) {
		if static[k] {
			return nil, fmt.Errorf("stack: %s sets %s, which rexctl generates", OverlayFiles[OverlayRustServer], k)
		}
		target.Content = append(target.Content,
			&yaml.Node{Kind: yaml.ScalarNode, Tag: "!!str", Value: k},
			&yaml.Node{Kind: yaml.ScalarNode, Tag: "!!str", Value: env[k], Style: yaml.DoubleQuotedStyle})
	}
	var buf bytes.Buffer
	enc := yaml.NewEncoder(&buf)
	enc.SetIndent(2)
	if err := enc.Encode(&doc); err != nil {
		return nil, err
	}
	if err := enc.Close(); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func ModeServices(settings map[string]string, services []string) (containers, inRust []string) {
	single := RustSingle(settings)
	for _, svc := range services {
		switch {
		case (svc == "seaweedfs" || svc == "seaweedfs-init") && FilesystemStorage(settings):
			continue
		case svc == ObjectsInitService && !FilesystemStorage(settings):
			continue
		case svc == "valkey" && KVPostgres(settings):
			continue
		}
		c := RunsIn(svc, single)
		if c != svc && !slices.Contains(inRust, svc) {
			inRust = append(inRust, svc)
		}
		if !slices.Contains(containers, c) {
			containers = append(containers, c)
		}
	}
	return containers, inRust
}

func RustLogService(line string) string {
	var entry struct {
		Spans []struct {
			Svc string `json:"svc"`
		} `json:"spans"`
		Target string `json:"target"`
	}
	if json.Unmarshal([]byte(line), &entry) != nil {
		return ""
	}
	if len(entry.Spans) > 0 && slices.Contains(RustServices, entry.Spans[0].Svc) {
		return entry.Spans[0].Svc
	}
	crate, _, _ := strings.Cut(entry.Target, "::")
	name, ok := strings.CutPrefix(crate, "fluxer_")
	if name = strings.ReplaceAll(name, "_", "-"); ok && slices.Contains(RustServices, name) {
		return name
	}
	return ""
}
