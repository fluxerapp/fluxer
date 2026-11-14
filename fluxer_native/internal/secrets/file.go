// SPDX-License-Identifier: AGPL-3.0-or-later

package secrets

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/config"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

func Fill(l spec.Layout, env spec.Env) error {
	file, err := config.ReadEnvFile(l.SecretsFile())
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	for k, v := range file {
		if IsUnset(env[k]) && !IsUnset(v) {
			env[k] = v
		}
	}
	return nil
}

func Ensure(l spec.Layout, env spec.Env) ([]string, error) {
	if err := Fill(l, env); err != nil {
		return nil, err
	}
	written, err := Mint(env, HasData(l))
	if err != nil || len(written) == 0 {
		return written, err
	}
	file, err := config.ReadEnvFile(l.SecretsFile())
	if errors.Is(err, fs.ErrNotExist) {
		file, err = map[string]string{}, nil
	}
	if err != nil {
		return nil, err
	}
	for _, k := range written {
		file[k] = env[k]
	}
	return written, write(l.SecretsFile(), file)
}

func Missing(env spec.Env) []string {
	var out []string
	for _, s := range append(append([]Spec(nil), Required...), Native...) {
		if IsUnset(env[s.Key]) {
			out = append(out, s.Key)
		}
	}
	return out
}

func HasData(l spec.Layout) bool {
	if _, err := os.Stat(filepath.Join(l.PostgresDir(), "PG_VERSION")); err == nil {
		return true
	}
	for _, dir := range []string{l.ValkeyDir(), l.NATSDir(), l.SeaweedFSDir(), l.MeilisearchDir(), l.ObjectsDir()} {
		if entries, err := os.ReadDir(dir); err == nil && len(entries) > 0 {
			return true
		}
	}
	return false
}

func write(path string, values map[string]string) error {
	keys := make([]string, 0, len(values))
	for k := range values {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	var b strings.Builder
	for _, k := range keys {
		b.WriteString(k + "=" + values[k] + "\n")
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".secrets-*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if err := tmp.Chmod(0o600); err != nil {
		tmp.Close()
		return err
	}
	if _, err := tmp.WriteString(b.String()); err != nil {
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
