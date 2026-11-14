// SPDX-License-Identifier: AGPL-3.0-or-later

package config

import (
	"bufio"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

type Options struct {
	EnvFile  string
	CaddyDir string
	CertsDir string
	Environ  []string
}

type Config struct {
	Layout   spec.Layout
	EnvFile  string
	CaddyDir string
	CertsDir string
	Env      spec.Env
}

var fileRefPrefixes = []string{"FLUXER_", "POSTGRES_", "MEILI_", "LIVEKIT_"}

var notFileRefs = map[string]bool{"FLUXER_LOG_MAX_FILE": true}

func Load(opt Options) (*Config, error) {
	cfg := &Config{EnvFile: opt.EnvFile, CaddyDir: opt.CaddyDir, CertsDir: opt.CertsDir}
	if cfg.EnvFile == "" {
		cfg.EnvFile = spec.DefaultEnvFile
	}
	if cfg.CaddyDir == "" {
		cfg.CaddyDir = spec.DefaultCaddyDir
	}
	if cfg.CertsDir == "" {
		cfg.CertsDir = spec.DefaultCertsDir
	}
	file, err := ReadEnvFile(cfg.EnvFile)
	if errors.Is(err, fs.ErrNotExist) && opt.EnvFile == "" {
		file, err = map[string]string{}, nil
	}
	if err != nil {
		return nil, err
	}
	proc := map[string]string{}
	for _, kv := range opt.Environ {
		if k, v, ok := strings.Cut(kv, "="); ok && k != "" {
			proc[k] = v
		}
	}
	if err := resolveFileRefs(file, cfg.EnvFile); err != nil {
		return nil, err
	}
	if err := resolveFileRefs(proc, "the process environment"); err != nil {
		return nil, err
	}
	cfg.Env = spec.Env{}
	for _, layer := range []map[string]string{file, proc} {
		for k, v := range layer {
			if strings.TrimSpace(v) == "" && cfg.Env[k] != "" {
				continue
			}
			cfg.Env[k] = v
		}
	}
	cfg.Layout = spec.LayoutFromEnv(func(k string) string { return cfg.Env[k] })
	if err := fillModes(cfg); err != nil {
		return nil, err
	}
	return cfg, nil
}

func resolveFileRefs(layer map[string]string, source string) error {
	for key, path := range layer {
		base, ok := strings.CutSuffix(key, "_FILE")
		if !ok || notFileRefs[key] || !hasFileRefPrefix(base) {
			continue
		}
		delete(layer, key)
		if strings.TrimSpace(path) == "" {
			continue
		}
		if strings.TrimSpace(layer[base]) != "" {
			return fmt.Errorf("%s and %s are both set in %s, set only one", base, key, source)
		}
		data, err := os.ReadFile(path)
		if err != nil {
			return fmt.Errorf("%s could not read %s: %w", key, path, err)
		}
		value := strings.TrimSuffix(string(data), "\n")
		layer[base] = strings.TrimSuffix(value, "\r")
	}
	return nil
}

func hasFileRefPrefix(key string) bool {
	for _, p := range fileRefPrefixes {
		if strings.HasPrefix(key, p) {
			return true
		}
	}
	return false
}

func ReadEnvFile(path string) (map[string]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	out := map[string]string{}
	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 64<<10), 1<<20)
	num := 0
	for sc.Scan() {
		num++
		line := strings.TrimSpace(strings.TrimPrefix(sc.Text(), "\uFEFF"))
		if line == "" || line[0] == '#' {
			continue
		}
		line = strings.TrimSpace(strings.TrimPrefix(line, "export "))
		key, value, ok := strings.Cut(line, "=")
		key = strings.TrimSpace(key)
		if !ok || !validKey(key) {
			return nil, fmt.Errorf("%s:%d: expected KEY=VALUE", path, num)
		}
		out[key] = unquote(strings.TrimSpace(value))
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("%s: %w", path, err)
	}
	return out, nil
}

func unquote(v string) string {
	if len(v) < 2 || (v[0] != '"' && v[0] != '\'') || v[len(v)-1] != v[0] {
		return v
	}
	inner := v[1 : len(v)-1]
	if v[0] == '\'' {
		return inner
	}
	return strings.NewReplacer(`\\`, `\`, `\"`, `"`, `\n`, "\n").Replace(inner)
}

func validKey(k string) bool {
	if k == "" {
		return false
	}
	for i, c := range k {
		switch {
		case c == '_', c >= 'A' && c <= 'Z', c >= 'a' && c <= 'z':
		case c >= '0' && c <= '9' && i > 0:
		default:
			return false
		}
	}
	return true
}
