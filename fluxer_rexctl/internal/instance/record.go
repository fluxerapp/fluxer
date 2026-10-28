// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
)

const RecordFileName = "instances.json"

var MachineRecordPath = "/etc/rexctl/" + RecordFileName

var (
	asRoot  = func() bool { return runtime.GOOS != "windows" && euid() == 0 }
	rootUID = 0
)

type Scope string

const (
	ScopeUser    Scope = "user"
	ScopeMachine Scope = "machine"
)

func OwnScope() Scope {
	if asRoot() {
		return ScopeMachine
	}
	return ScopeUser
}

type Record struct {
	Default   string   `json:"default,omitempty"`
	Used      bool     `json:"used,omitempty"`
	Instances []string `json:"instances"`
}

type From int

const (
	FromFlag From = iota
	FromWorkdir
	FromRecord
	FromOnly
	FromPicked
	FromFallback
)

type Located struct {
	Dir    string
	From   From
	Scope  Scope
	Stale  string
	Chosen bool
}

func (l Located) Why() string {
	switch l.From {
	case FromFlag:
		return "from --dir"
	case FromWorkdir:
		return "the current folder"
	case FromPicked:
		return "the one you picked"
	case FromOnly:
		return "the only instance rexctl remembers"
	case FromRecord:
		if l.Scope == ScopeMachine {
			return "the default for this machine"
		}
		return "the default for your account"
	}
	return "the standard folder"
}

type SeveralError struct {
	Dirs  []string
	Stale string
}

func (e *SeveralError) Error() string {
	return fmt.Sprintf("instance: %d instances are recorded and none is the default", len(e.Dirs))
}

type UntrustedError struct {
	Path   string
	Folder bool
}

func (e *UntrustedError) Error() string {
	return fmt.Sprintf("instance: accounts other than root can change %s", e.Path)
}

func RecordPath(s Scope) string {
	if s == ScopeMachine {
		if runtime.GOOS == "windows" {
			return ""
		}
		return MachineRecordPath
	}
	dir, err := os.UserConfigDir()
	if err != nil {
		return ""
	}
	return filepath.Join(dir, "rexctl", RecordFileName)
}

func scopes() []Scope {
	if asRoot() {
		return []Scope{ScopeMachine}
	}
	return []Scope{ScopeUser, ScopeMachine}
}

func ReadRecord(s Scope) Record {
	r, _ := loadRecord(s)
	return r
}

func loadRecord(s Scope) (Record, error) {
	var r Record
	path := RecordPath(s)
	if path == "" {
		return r, nil
	}
	if s == ScopeMachine && asRoot() {
		for _, p := range []string{filepath.Dir(path), path} {
			if !rootOnly(p, true) {
				return r, &UntrustedError{Path: p}
			}
		}
	}
	data, err := os.ReadFile(path)
	if err != nil || json.Unmarshal(data, &r) != nil {
		return Record{}, nil
	}
	return r, nil
}

func rootOnly(path string, owned bool) bool {
	info, err := os.Stat(path)
	if errors.Is(err, fs.ErrNotExist) {
		return true
	}
	if err != nil || info.Mode().Perm()&0o022 != 0 {
		return false
	}
	if !owned {
		return true
	}
	uid, ok := ownerOf(info)
	return ok && uid == rootUID
}

func (r Record) Has(dir string) bool {
	return slices.ContainsFunc(r.Instances, func(d string) bool { return same(d, dir) })
}

func (r Record) Empty() bool { return len(r.Instances) == 0 && r.Default == "" }

func (r Record) With(dir string) Record {
	dir = clean(dir)
	out := Record{Default: r.Default, Used: r.Used}
	for _, d := range r.Instances {
		if !same(d, dir) && !gone(d) {
			out.Instances = append(out.Instances, d)
		}
	}
	out.Instances = append(out.Instances, dir)
	if out.Default != "" && !out.Used && gone(out.Default) {
		out.Default = ""
	}
	return out
}

func (r Record) Without(dir string) Record {
	out := Record{Default: r.Default, Used: r.Used, Instances: slices.DeleteFunc(slices.Clone(r.Instances), func(d string) bool { return same(d, dir) })}
	if same(out.Default, dir) {
		out.Default, out.Used = "", false
	}
	return out
}

func (r Record) pruned() Record {
	out := Record{Default: r.Default, Used: r.Used}
	for _, d := range r.Instances {
		if !gone(d) {
			out.Instances = append(out.Instances, d)
		}
	}
	if out.Default != "" && !out.Used && gone(out.Default) {
		out.Default = ""
	}
	return out
}

func (r Record) remember(dir string, makeDefault bool) Record {
	dir = clean(dir)
	if r.Empty() && !makeDefault {
		if fb, err := Fallback(); err == nil && present(fb) && !same(fb, dir) {
			r = Record{Default: clean(fb), Instances: []string{clean(fb)}}
		}
	}
	var others []string
	for _, d := range r.Instances {
		if !same(d, dir) && !gone(d) {
			others = append(others, d)
		}
	}
	out := r.With(dir)
	switch {
	case makeDefault:
		out.Default, out.Used = dir, true
	case out.Default == "" && len(others) == 1:
		out.Default, out.Used = others[0], false
	}
	return out
}

func (r Record) Encode() []byte {
	if r.Instances == nil {
		r.Instances = []string{}
	}
	data, _ := json.MarshalIndent(r, "", "  ")
	return append(data, '\n')
}

func SaveRecord(s Scope, r Record) error {
	path := RecordPath(s)
	if path == "" {
		return errors.New("instance: no place to record instances on this system")
	}
	if r.Empty() {
		if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return err
		}
		return nil
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	return envfile.WriteFileAtomic(path, r.Encode(), false)
}

func updateRecord(s Scope, fn func(Record) Record) error {
	path := RecordPath(s)
	if path == "" {
		return errors.New("instance: no place to record instances on this system")
	}
	if s == ScopeUser && asRoot() {
		return errors.New("instance: root doesn't keep a record per account")
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	f, err := os.OpenFile(path+".lock", os.O_RDWR|os.O_CREATE, 0o644)
	if err != nil {
		return err
	}
	defer f.Close()
	deadline := time.Now().Add(10 * time.Second)
	for {
		err := tryLock(f)
		if err == nil {
			break
		}
		if !errors.Is(err, ErrLocked) || time.Now().After(deadline) {
			return err
		}
		time.Sleep(50 * time.Millisecond)
	}
	defer unlock(f)
	r, err := loadRecord(s)
	if err != nil {
		return err
	}
	next := fn(r)
	if slices.Equal(next.Encode(), r.Encode()) {
		return nil
	}
	return SaveRecord(s, next)
}

func Remember(s Scope, dir string, makeDefault bool) error {
	return updateRecord(s, func(r Record) Record { return r.remember(dir, makeDefault) })
}

func RememberFirst(s Scope, dir string) error {
	return updateRecord(s, func(r Record) Record {
		if !r.Empty() {
			return r
		}
		return r.remember(dir, false)
	})
}

func Forget(s Scope, dir string) error {
	return updateRecord(s, func(r Record) Record { return r.Without(dir) })
}

func pickRecorded(r Record, s Scope) (Located, bool, error) {
	var live []string
	for _, d := range r.Instances {
		if !gone(d) && (s == ScopeUser || asRoot() || readable(d)) {
			live = append(live, clean(d))
		}
	}
	l := Located{Scope: s}
	if r.Default != "" {
		def := clean(r.Default)
		if !gone(def) && (s == ScopeUser || asRoot() || readable(def)) {
			l.Dir, l.From = def, FromRecord
			return l, true, nil
		}
		if gone(def) {
			l.Stale, l.Chosen = def, r.Used
		}
	}
	switch len(live) {
	case 0:
		return l, false, nil
	case 1:
		l.Dir, l.From = live[0], FromOnly
		return l, true, nil
	}
	return l, false, &SeveralError{Dirs: live, Stale: l.Stale}
}

func Resolve(flag string) (Located, error) {
	if flag != "" {
		return Located{Dir: flag, From: FromFlag}, nil
	}
	if wd, err := os.Getwd(); err == nil && present(wd) {
		return Located{Dir: wd, From: FromWorkdir}, nil
	}
	var stale Located
	for _, s := range scopes() {
		if s == ScopeMachine && !asRoot() {
			if dir, err := DefaultDir(); err == nil && present(dir) {
				return Located{Dir: dir, From: FromFallback, Stale: stale.Stale, Chosen: stale.Chosen}, nil
			}
		}
		r, err := loadRecord(s)
		if err != nil {
			return Located{}, err
		}
		l, ok, err := pickRecorded(r, s)
		if err != nil {
			return l, err
		}
		if ok && asRoot() && !rootOnly(l.Dir, false) {
			return l, &UntrustedError{Path: l.Dir, Folder: true}
		}
		if ok {
			if l.Stale == "" {
				l.Stale, l.Chosen = stale.Stale, stale.Chosen
			}
			return l, nil
		}
		if stale.Stale == "" {
			stale = l
		}
		if s == OwnScope() && !r.Empty() {
			_ = updateRecord(s, Record.pruned)
		}
	}
	dir, err := Fallback()
	return Located{Dir: dir, From: FromFallback, Stale: stale.Stale, Chosen: stale.Chosen}, err
}

func gone(dir string) bool {
	_, err := os.Stat(filepath.Join(dir, StateDirName, StateFileName))
	return errors.Is(err, fs.ErrNotExist) && !isLegacy(dir)
}

func readable(dir string) bool {
	f, err := os.Open(filepath.Join(dir, StateDirName, StateFileName))
	if err == nil {
		f.Close()
		return true
	}
	return isLegacy(dir)
}

func clean(dir string) string {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return filepath.Clean(dir)
	}
	if real, err := filepath.EvalSymlinks(abs); err == nil {
		return real
	}
	return abs
}

func same(a, b string) bool {
	if a == "" || b == "" {
		return a == b
	}
	a, b = clean(a), clean(b)
	if runtime.GOOS == "windows" {
		return strings.EqualFold(a, b)
	}
	return a == b
}
