// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"syscall"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

const StateSchema = generatedSchema

var ErrNewerState = errors.New("instance: state was written by a newer rexctl")

type State struct {
	Schema          int               `json:"schema"`
	Project         string            `json:"project"`
	Engine          EngineRef         `json:"engine"`
	Release         string            `json:"release"`
	PreviousRelease string            `json:"previous_release"`
	FailedRelease   string            `json:"failed_release"`
	Upgrade         *Upgrade          `json:"upgrade"`
	Pending         *Pending          `json:"pending"`
	Images          Images            `json:"images"`
	Settings        map[string]string `json:"settings"`
	Secrets         map[string]string `json:"secrets"`
	AdminImport     *AdminImport      `json:"admin_import"`
	SetupDone       bool              `json:"setup_done"`
	Installing      bool              `json:"installing,omitempty"`
	Stopped         bool              `json:"stopped"`
	Rendered        map[string]string `json:"rendered"`
	Mounted         map[string]string `json:"mounted"`
	LeftInRoot      map[string]string `json:"left_in_root,omitempty"`
	Ejected         *Ejected          `json:"ejected"`
	BackupDir       string            `json:"backup_dir,omitempty"`
	KeepBackups     int               `json:"keep_backups,omitempty"`
	NotifyURL       string            `json:"notify_url,omitempty"`
	InstalledAt     time.Time         `json:"installed_at"`
	UpdatedAt       time.Time         `json:"updated_at"`

	Unknown    []string `json:"-"`
	rootLayout bool
}

type EngineRef struct {
	Kind string `json:"kind"`
	Host string `json:"host"`
	ID   string `json:"id"`
}

type Upgrade struct {
	From       string    `json:"from"`
	To         string    `json:"to"`
	Phase      string    `json:"phase"`
	Backup     string    `json:"backup,omitempty"`
	BackFailed bool      `json:"back_failed,omitempty"`
	StartedAt  time.Time `json:"started_at"`
}

type Pending struct {
	Settings  map[string]string `json:"settings"`
	Images    *Images           `json:"images"`
	Keys      []string          `json:"keys"`
	StartedAt time.Time         `json:"started_at"`
}

type Images struct {
	Release string            `json:"release"`
	Refs    map[string]string `json:"refs"`
}

type AdminImport struct {
	Values map[string]string `json:"values"`
	SentAt string            `json:"sent_at"`
}

type Ejected struct {
	At     time.Time `json:"at"`
	Rexctl string    `json:"rexctl"`
}

type SettingError struct {
	Path    string
	Key     string
	Problem string
}

func (e *SettingError) Error() string {
	return fmt.Sprintf("instance: %s: setting %s: %s", e.Path, e.Key, e.Problem)
}

func (e *SettingError) Fix() string {
	return "Run rexctl config set " + e.Key + " <value>, or rexctl config unset " + e.Key + " to go back to the default."
}

func LoadState(path string) (*State, error) {
	return loadState(path, nil)
}

func loadState(path string, repairing []string) (*State, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	dir := filepath.Dir(filepath.Dir(path))
	return parseStateRepairing(data, dir, repairing)
}

func ParseState(data []byte, dir string) (*State, error) {
	return parseStateRepairing(data, dir, nil)
}

func parseStateRepairing(data []byte, dir string, repairing []string) (*State, error) {
	var head struct {
		Schema  int    `json:"schema"`
		Project string `json:"project"`
	}
	if err := json.Unmarshal(data, &head); err != nil {
		return nil, fmt.Errorf("instance: reading state in %s: %w", dir, err)
	}
	if head.Schema > StateSchema {
		return nil, ErrNewerState
	}
	if head.Project == "" {
		return nil, fmt.Errorf("instance: the state in %s names no project", dir)
	}
	s, err := parseState(data, dir, repairing)
	if err != nil {
		return nil, err
	}
	s.rootLayout = head.Schema < generatedSchema
	s.normalize()
	return s, nil
}

var stateFields = map[string]bool{
	"schema": true, "project": true, "engine": true, "release": true, "previous_release": true, "failed_release": true,
	"upgrade": true, "pending": true, "images": true, "settings": true, "secrets": true, "admin_import": true,
	"setup_done": true, "installing": true, "stopped": true, "rendered": true, "mounted": true, "left_in_root": true, "ejected": true,
	"installed_at": true, "updated_at": true, "backup_dir": true, "keep_backups": true, "notify_url": true,
}

func parseState(data []byte, dir string, repairing []string) (*State, error) {
	var raw map[string]json.RawMessage
	if err := json.Unmarshal(data, &raw); err != nil {
		return nil, fmt.Errorf("instance: reading state in %s: %w", dir, err)
	}
	var unknown []string
	for k := range raw {
		if !stateFields[k] {
			unknown = append(unknown, k)
		}
	}
	values, more, err := checkSettings(raw["settings"], dir, repairing)
	if err != nil {
		return nil, err
	}
	unknown = append(unknown, more...)
	delete(raw, "settings")
	rest, err := json.Marshal(raw)
	if err != nil {
		return nil, err
	}
	var s State
	if err := json.Unmarshal(rest, &s); err != nil {
		return nil, fmt.Errorf("instance: reading state in %s: %w", dir, err)
	}
	s.Settings = values
	slices.Sort(unknown)
	s.Unknown = unknown
	return &s, nil
}

func checkSettings(data json.RawMessage, dir string, repairing []string) (map[string]string, []string, error) {
	out := map[string]string{}
	if len(data) == 0 || string(data) == "null" {
		return out, nil, nil
	}
	var raw map[string]json.RawMessage
	if err := json.Unmarshal(data, &raw); err != nil {
		return nil, nil, fmt.Errorf("instance: reading settings in %s: %w", dir, err)
	}
	keys := map[string]settings.Key{}
	for _, k := range settings.Named() {
		keys[k.Name] = k
	}
	path := filepath.Join(dir, StateDirName, StateFileName)
	var unknown []string
	for name, value := range raw {
		k, isNamed := keys[name]
		var v string
		if err := json.Unmarshal(value, &v); err != nil {
			if isNamed {
				return nil, nil, &SettingError{Path: path, Key: name, Problem: "the value is not text"}
			}
			unknown = append(unknown, name)
			continue
		}
		switch {
		case isNamed && slices.Contains(repairing, name):
		case isNamed:
			if k.Validate != nil {
				if err := k.Validate(v); err != nil {
					return nil, nil, &SettingError{Path: path, Key: name, Problem: err.Error()}
				}
			}
		case !isRaw(name):
			unknown = append(unknown, name)
		}
		out[name] = v
	}
	return out, unknown, nil
}

func isRaw(name string) bool {
	return name != "" && strings.ToUpper(name) == name && !strings.ContainsAny(name, ".- ")
}

func (s *State) InstallUnfinished() bool { return s.Installing }

func (s *State) normalize() {
	s.Schema = StateSchema
	if s.rootLayout {
		s.Schema = generatedSchema - 1
	}
	if s.Settings == nil {
		s.Settings = map[string]string{}
	}
	if s.Secrets == nil {
		s.Secrets = map[string]string{}
	}
	if s.Images.Refs == nil {
		s.Images.Refs = map[string]string{}
	}
	if s.Rendered == nil {
		s.Rendered = map[string]string{}
	}
	if s.Mounted == nil {
		s.Mounted = map[string]string{}
	}
}

func (s *State) Save(path string) error {
	c := *s
	c.normalize()
	data, err := json.MarshalIndent(&c, "", "  ")
	if err != nil {
		return err
	}
	if err := writePrivate(path, append(data, '\n')); err != nil {
		return err
	}
	s.Schema = c.Schema
	return nil
}

func writePrivate(path string, data []byte) error {
	deadline := time.Now().Add(2 * time.Second)
	for {
		err := envfile.WriteFileAtomic(path, data, true)
		if err == nil || !sharingViolation(err) || time.Now().After(deadline) {
			return err
		}
		time.Sleep(50 * time.Millisecond)
	}
}

func sharingViolation(err error) bool {
	if runtime.GOOS != "windows" {
		return false
	}
	var errno syscall.Errno
	if !errors.As(err, &errno) {
		return false
	}
	const accessDenied, sharingViolation, lockViolation = 5, 32, 33
	return errno == accessDenied || errno == sharingViolation || errno == lockViolation
}
