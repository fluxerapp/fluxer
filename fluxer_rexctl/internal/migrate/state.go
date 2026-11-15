// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"time"
)

const StateFile = "migrations.json"

type State struct {
	Storage *Switched `json:"storage,omitempty"`
	KV      *Switched `json:"kv,omitempty"`
}

type Switched struct {
	At          time.Time         `json:"at"`
	Runtime     Runtime           `json:"runtime,omitempty"`
	Previous    map[string]string `json:"previous"`
	OpenedAt    *time.Time        `json:"opened_at,omitempty"`
	ValkeyDir   string            `json:"valkey_dir,omitempty"`
	CleanedUpAt *time.Time        `json:"cleaned_up_at,omitempty"`
}

func (s *Switched) Open() bool { return s != nil && s.OpenedAt != nil }

func (s *Switched) Pending() bool { return s != nil && s.CleanedUpAt == nil }

func statePath(dir string) string { return filepath.Join(dir, ".rexctl", StateFile) }

func LoadState(dir string) (State, error) {
	var st State
	data, err := os.ReadFile(statePath(dir))
	if errors.Is(err, fs.ErrNotExist) {
		return st, nil
	}
	if err != nil {
		return st, err
	}
	return st, json.Unmarshal(data, &st)
}

func (st State) Save(dir string) error {
	path := statePath(dir)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	data, err := json.MarshalIndent(st, "", "  ")
	if err != nil {
		return err
	}
	tmp := path + ".tmp"
	if err := os.WriteFile(tmp, append(data, '\n'), 0o600); err != nil {
		return err
	}
	return os.Rename(tmp, path)
}

func (st State) Current(done []Option) State {
	if !slices.Contains(done, OptStorage) {
		st.Storage = nil
	}
	if !slices.Contains(done, OptKV) {
		st.KV = nil
	}
	return st
}

func (st State) Restored(made time.Time) State {
	if st.Storage != nil && made.Before(st.Storage.At) {
		st.Storage = nil
	}
	if st.Storage.Open() && made.Before(*st.Storage.OpenedAt) {
		window := *st.Storage
		window.OpenedAt = nil
		st.Storage = &window
	}
	if st.KV != nil && made.Before(st.KV.At) {
		st.KV = nil
	}
	return st
}

func (st State) Merge(next State) State {
	if next.Storage != nil {
		st.Storage = next.Storage
	}
	if next.KV != nil {
		st.KV = next.KV
	}
	return st
}
