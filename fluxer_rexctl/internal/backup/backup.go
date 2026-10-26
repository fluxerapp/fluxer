// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"time"
)

const (
	FormatVersion = 1
	Extension     = ".tar.zst"
	ManifestName  = "manifest.json"
	SumsName      = "SHA256SUMS"
	PartialSuffix = ".partial"
	DumpName      = "postgres/fluxer.dump"
	FilesPrefix   = "files/"
	VolumesPrefix = "volumes/"
)

const (
	StageFiles       = "files"
	StageDatabase    = "database"
	StageStopping    = "stopping"
	StageVolumes     = "volumes"
	StageStarting    = "starting"
	StageCompressing = "compressing"
	StageVerifying   = "verifying"
	StageClearing    = "clearing"
	StagePulling     = "pulling"
)

const (
	SkippedDatabase = "database"
	SkippedUploads  = "uploads"
)

var (
	ErrNotBackup = errors.New("backup: not a rexctl backup")
	ErrDamaged   = errors.New("backup: damaged")
	ErrNewer     = errors.New("backup: made by a newer rexctl")
)

type Entry struct {
	Path   string `json:"path"`
	Size   int64  `json:"size"`
	SHA256 string `json:"sha256"`
}

type Manifest struct {
	Format    int       `json:"format"`
	Release   string    `json:"release"`
	Project   string    `json:"project"`
	Domain    string    `json:"domain"`
	CreatedAt time.Time `json:"created_at"`
	Reason    string    `json:"reason,omitempty"`
	Database  string    `json:"database"`
	Volumes   []string  `json:"volumes"`
	Skipped   []string  `json:"skipped,omitempty"`
	Entries   []Entry   `json:"entries"`
}

func (m *Manifest) HasSkipped(what string) bool {
	return slices.Contains(m.Skipped, what)
}

type Info struct {
	File      string    `json:"file"`
	Path      string    `json:"path"`
	CreatedAt time.Time `json:"created_at"`
	Release   string    `json:"release"`
	SizeBytes int64     `json:"size_bytes"`
}

func List(dir string) ([]Info, error) {
	ents, err := os.ReadDir(dir)
	if errors.Is(err, os.ErrNotExist) {
		return []Info{}, nil
	}
	if err != nil {
		return nil, err
	}
	out := []Info{}
	for _, e := range ents {
		name := e.Name()
		if e.IsDir() || !strings.HasSuffix(name, Extension) {
			continue
		}
		path := filepath.Join(dir, name)
		m, err := ReadManifest(path)
		if err != nil {
			continue
		}
		fi, err := e.Info()
		if err != nil {
			continue
		}
		out = append(out, Info{File: name, Path: path, CreatedAt: m.CreatedAt, Release: m.Release, SizeBytes: fi.Size()})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].CreatedAt.After(out[j].CreatedAt) })
	return out, nil
}

func NewestOf(list []Info, release string) *Info {
	var best *Info
	for i := range list {
		b := &list[i]
		if b.Release != release {
			continue
		}
		if best == nil || b.CreatedAt.After(best.CreatedAt) {
			best = b
		}
	}
	return best
}

func Prune(dir string, keep int, protect ...string) ([]Info, error) {
	list, err := List(dir)
	if err != nil || keep <= 0 || len(list) <= keep {
		return nil, err
	}
	var removed []Info
	for _, b := range list[keep:] {
		if slices.ContainsFunc(protect, func(p string) bool { return p != "" && samePath(p, b.Path) }) {
			continue
		}
		if err := os.Remove(b.Path); err != nil && !errors.Is(err, os.ErrNotExist) {
			return removed, err
		}
		removed = append(removed, b)
	}
	return removed, nil
}

func fileName(domain string, at time.Time, n int) string {
	var b strings.Builder
	for _, r := range strings.ToLower(domain) {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9', r == '.', r == '-':
			b.WriteRune(r)
		default:
			b.WriteRune('-')
		}
	}
	d := strings.Trim(b.String(), ".-")
	if d == "" {
		d = "instance"
	}
	stamp := at.UTC().Format("20060102-150405")
	if n > 0 {
		return fmt.Sprintf("fluxer-%s-%s-%d%s", d, stamp, n, Extension)
	}
	return fmt.Sprintf("fluxer-%s-%s%s", d, stamp, Extension)
}

func newPath(dir, domain string, at time.Time) (string, error) {
	for n := 0; n < 100; n++ {
		p := filepath.Join(dir, fileName(domain, at, n))
		_, err := os.Lstat(p)
		if errors.Is(err, os.ErrNotExist) {
			_, err = os.Lstat(p + PartialSuffix)
			if errors.Is(err, os.ErrNotExist) {
				return p, nil
			}
		}
		if err != nil && !errors.Is(err, os.ErrNotExist) {
			return "", err
		}
	}
	return "", errors.New("backup: no free file name")
}

func removePartials(dir string) {
	ents, err := os.ReadDir(dir)
	if err != nil {
		return
	}
	for _, e := range ents {
		if strings.HasSuffix(e.Name(), PartialSuffix) {
			os.Remove(filepath.Join(dir, e.Name()))
		}
	}
}
