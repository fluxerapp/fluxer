// SPDX-License-Identifier: AGPL-3.0-or-later

package updates

import (
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/stamp"
)

const FileName = "updates.json"

func FilePath(dataDir string) string { return filepath.Join(dataDir, FileName) }

type State struct {
	CheckedAt time.Time `json:"checked_at,omitzero"`
	TriedAt   time.Time `json:"tried_at,omitzero"`
	FailedAt  time.Time `json:"failed_at,omitzero"`
	Failures  int       `json:"failures"`
	Error     string    `json:"error"`
	NextAt    time.Time `json:"next_at,omitzero"`
	Announced string    `json:"announced"`
	Releases  []Release `json:"releases"`
	Outdated  bool      `json:"outdated,omitempty"`
}

type Instance struct {
	Running  string
	Method   string
	Override string
	GOARCH   string
	Enabled  bool
	URL      string
	Backup   string
}

type Report struct {
	State
	Running string    `json:"running"`
	Method  string    `json:"method"`
	Enabled bool      `json:"enabled"`
	URL     string    `json:"url"`
	Latest  string    `json:"latest"`
	Newer   []Release `json:"newer"`
	Command string    `json:"command"`
	Then    string    `json:"then"`
	Backup  string    `json:"backup"`
}

func (s State) Report(in Instance) Report {
	r := Report{State: s, Running: in.Running, Method: in.Method, Enabled: in.Enabled, URL: in.URL, Backup: in.Backup, Newer: []Release{}}
	if r.Releases == nil {
		r.Releases = []Release{}
	}
	if s.Outdated {
		r.Then = "See " + ReleasesURL + " for the newest release and " + DocsURL + " for the steps."
	}
	if len(s.Releases) == 0 {
		return r
	}
	r.Latest = s.Releases[0].Version
	for _, rel := range s.Releases {
		if c, ok := stamp.Compare(rel.Version, in.Running); !ok || c <= 0 {
			break
		}
		r.Newer = append(r.Newer, rel)
	}
	if len(r.Newer) > 0 && !s.Outdated {
		r.Command, r.Then = Command(in.Method, in.Override, r.Newer[0].Version, in.GOARCH)
	}
	return r
}

func Load(path string) (State, error) {
	data, err := os.ReadFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return State{}, nil
	}
	if err != nil {
		return State{}, err
	}
	var s State
	if err := json.Unmarshal(data, &s); err != nil {
		return State{}, nil
	}
	sortNewest(s.Releases)
	return s, nil
}

func LoadReport(path string) (Report, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return Report{}, err
	}
	var r Report
	return r, json.Unmarshal(data, &r)
}

func Save(path string, r Report) error {
	data, err := json.MarshalIndent(r, "", "  ")
	if err != nil {
		return err
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), ".updates-*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.Write(append(data, '\n')); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Chmod(0o640); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	return os.Rename(tmp.Name(), path)
}
