// SPDX-License-Identifier: AGPL-3.0-or-later

package config

import (
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

var modeKeys = []string{spec.RustProcessEnv, spec.StorageBackendEnv, spec.KVModeEnv}

func fillModes(cfg *Config) error {
	file, err := ReadEnvFile(cfg.Layout.ModesFile())
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	for _, k := range modeKeys {
		if v := file[k]; v != "" && strings.TrimSpace(cfg.Env[k]) == "" {
			cfg.Env[k] = v
		}
	}
	return nil
}

func EnsureModes(cfg *Config, fresh bool) (map[string]string, error) {
	if !fresh {
		return nil, nil
	}
	if _, err := os.Stat(cfg.Layout.ModesFile()); !errors.Is(err, fs.ErrNotExist) {
		return nil, err
	}
	modes := spec.FreshModes(cfg.Env)
	if err := WriteModes(cfg.Layout, modes); err != nil {
		return nil, err
	}
	maps.Copy(cfg.Env, modes)
	return modes, nil
}

func WriteModes(l spec.Layout, modes map[string]string) error {
	var b strings.Builder
	for _, k := range slices.Sorted(maps.Keys(modes)) {
		if !slices.Contains(modeKeys, k) {
			return fmt.Errorf("config: %s is not a mode", k)
		}
		b.WriteString(k + "=" + modes[k] + "\n")
	}
	path := l.ModesFile()
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".modes-*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(b.String()); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Chmod(0o600); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Sync(); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	return os.Rename(tmp.Name(), path)
}

func ReadModes(l spec.Layout) (map[string]string, error) {
	file, err := ReadEnvFile(l.ModesFile())
	if errors.Is(err, fs.ErrNotExist) {
		return map[string]string{}, nil
	}
	return file, err
}
