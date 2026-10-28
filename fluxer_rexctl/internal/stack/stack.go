// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

const defaultProject = "fluxer"

var defaultName = regexp.MustCompile(`(?m)^name: ` + defaultProject + `$`)

const (
	ComposeFile  = "docker-compose.yml"
	Caddyfile    = "Caddyfile"
	GeneratedDir = ".rexctl/generated"
)

type Overlay string

const (
	OverlayProxy               Overlay = "proxy"
	OverlayTunnel              Overlay = "tunnel"
	OverlayExternalObjectStore Overlay = "external-object-store"
	OverlayIPv6                Overlay = "ipv6"
)

var OverlayFiles = map[Overlay]string{
	OverlayProxy:               "docker-compose.proxy.yml",
	OverlayTunnel:              "tunnel.compose.yml",
	OverlayExternalObjectStore: "external-object-store.compose.yml",
	OverlayIPv6:                "ipv6.compose.yml",
}

var overlayOrder = []Overlay{OverlayProxy, OverlayTunnel, OverlayExternalObjectStore, OverlayIPv6}

var MountedFiles = map[string][]string{
	Caddyfile: {"edge"},
}

var MountedDirs = []string{"certs", "caddy.d"}

var (
	ErrUnknownOverlay  = errors.New("stack: unknown overlay")
	ErrOverlayConflict = errors.New("stack: the proxy and tunnel overlays can't be used together")
	ErrMissingFile     = errors.New("stack: the release has no such file")
)

type Rendered struct {
	Dir           string
	Hashes        map[string]string
	ComposeFiles  []string
	Changed       []string
	RestartNeeded []string
}

func NormalizeOverlays(overlays []Overlay) ([]Overlay, error) {
	seen := make(map[Overlay]bool, len(overlays))
	for _, o := range overlays {
		if _, ok := OverlayFiles[o]; !ok {
			return nil, fmt.Errorf("%w: %s", ErrUnknownOverlay, o)
		}
		seen[o] = true
	}
	if seen[OverlayProxy] && seen[OverlayTunnel] {
		return nil, ErrOverlayConflict
	}
	out := make([]Overlay, 0, len(seen))
	for _, o := range overlayOrder {
		if seen[o] {
			out = append(out, o)
		}
	}
	return out, nil
}

func OverlaysFor(settings map[string]string) []Overlay {
	var out []Overlay
	switch strings.TrimSpace(settings["edge.mode"]) {
	case string(OverlayProxy):
		out = append(out, OverlayProxy)
	case string(OverlayTunnel):
		out = append(out, OverlayTunnel)
	}
	if strings.TrimSpace(settings["storage.endpoint"]) != "" {
		out = append(out, OverlayExternalObjectStore)
	}
	if on, err := strconv.ParseBool(strings.TrimSpace(settings["network.ipv6"])); err == nil && on {
		out = append(out, OverlayIPv6)
	}
	return out
}

func FileNames() []string {
	names := []string{ComposeFile, Caddyfile}
	for _, o := range overlayOrder {
		names = append(names, OverlayFiles[o])
	}
	return names
}

func GeneratedPath(dir string, name ...string) string {
	return filepath.Join(append([]string{dir, filepath.FromSlash(GeneratedDir)}, name...)...)
}

func ProjectDir(composeFile string) string {
	dir := filepath.Dir(composeFile)
	if trimmed, ok := strings.CutSuffix(dir, string(filepath.Separator)+filepath.FromSlash(GeneratedDir)); ok {
		return trimmed
	}
	return dir
}

func FilesDir(dir string, root bool) string {
	if abs, err := filepath.Abs(dir); err == nil {
		dir = abs
	}
	if root {
		return dir
	}
	return GeneratedPath(dir)
}

func ComposeFiles(dir string, overlays []Overlay) []string {
	return ComposeFilesAt(dir, overlays, false)
}

func ComposeFilesAt(dir string, overlays []Overlay, root bool) []string {
	files := FilesDir(dir, root)
	out := []string{filepath.Join(files, ComposeFile)}
	for _, o := range overlayOrder {
		if slices.Contains(overlays, o) {
			out = append(out, filepath.Join(files, OverlayFiles[o]))
		}
	}
	return out
}

func header(name string) []byte {
	how := "To change it, run rexctl config edit."
	if name == Caddyfile {
		how = "To add Caddy directives, put a .caddy file in caddy.d in your instance folder."
	}
	return []byte("# rexctl writes this file and puts its own version back when it changes Fluxer.\n# " + how + "\n\n")
}

func mountRefs(name string) (plain, managed []byte) {
	return []byte("./" + name + ":"), []byte("./" + GeneratedDir + "/" + name + ":")
}

func Managed(name string, data []byte) []byte {
	if name != Caddyfile {
		for mounted := range MountedFiles {
			plain, managed := mountRefs(mounted)
			data = bytes.ReplaceAll(data, plain, managed)
		}
	}
	return append(header(name), data...)
}

func Plain(name string, data []byte) []byte {
	data = bytes.TrimPrefix(data, header(name))
	if name != Caddyfile {
		for mounted := range MountedFiles {
			plain, managed := mountRefs(mounted)
			data = bytes.ReplaceAll(data, managed, plain)
		}
	}
	return data
}

func MountsGenerated(compose []byte, name string) bool {
	_, managed := mountRefs(name)
	return bytes.Contains(compose, managed)
}

type renderedFile struct {
	name string
	data []byte
}

func renderFiles(set *Set, selected []Overlay, images map[string]string, project string) ([]renderedFile, []Overlay, error) {
	overlays, err := NormalizeOverlays(selected)
	if err != nil {
		return nil, nil, err
	}
	compose, err := RenderCompose(set, images, project)
	if err != nil {
		return nil, nil, err
	}
	caddy, err := ReadFile(set, Caddyfile)
	if err != nil {
		return nil, nil, err
	}
	out := []renderedFile{{ComposeFile, compose}, {Caddyfile, caddy}}
	for _, o := range overlays {
		data, err := ReadFile(set, OverlayFiles[o])
		if err != nil {
			return nil, nil, err
		}
		out = append(out, renderedFile{OverlayFiles[o], data})
	}
	return out, overlays, nil
}

func RenderCompose(set *Set, images map[string]string, project string) ([]byte, error) {
	compose, err := ReadFile(set, ComposeFile)
	if err != nil {
		return nil, err
	}
	if set.Release != LegacyRelease {
		if compose, err = pinImages(compose, set.Release, images); err != nil {
			return nil, err
		}
	}
	return nameProject(compose, project), nil
}

func Hashes(set *Set, selected []Overlay, images map[string]string, project string) (map[string]string, error) {
	files, _, err := renderFiles(set, selected, images, project)
	if err != nil {
		return nil, err
	}
	out := make(map[string]string, len(files))
	for _, f := range files {
		out[f.name] = Hash(f.data)
	}
	return out, nil
}

func ReadFile(set *Set, name string) ([]byte, error) {
	if set == nil || set.Files == nil {
		return nil, fmt.Errorf("%w: %s", ErrMissingFile, name)
	}
	data, err := fs.ReadFile(set.Files, name)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, fmt.Errorf("%w: %s in %s", ErrMissingFile, name, set.Release)
	}
	if err != nil {
		return nil, err
	}
	return bytes.ReplaceAll(data, []byte("\r\n"), []byte("\n")), nil
}

func Hash(data []byte) string {
	sum := sha256.Sum256(data)
	return "sha256:" + hex.EncodeToString(sum[:])
}

func plan(dir string, files []renderedFile, overlays []Overlay) (changed, stale []string, err error) {
	for _, f := range files {
		current, err := os.ReadFile(filepath.Join(dir, f.name))
		switch {
		case errors.Is(err, fs.ErrNotExist):
			changed = append(changed, f.name)
		case err != nil:
			return nil, nil, err
		case !bytes.Equal(current, f.data):
			changed = append(changed, f.name)
		}
	}
	for _, o := range overlayOrder {
		if slices.Contains(overlays, o) {
			continue
		}
		name := OverlayFiles[o]
		if _, err := os.Lstat(filepath.Join(dir, name)); err == nil {
			stale = append(stale, name)
		} else if !errors.Is(err, fs.ErrNotExist) {
			return nil, nil, err
		}
	}
	return changed, stale, nil
}

func nameProject(compose []byte, project string) []byte {
	if project == "" || project == defaultProject {
		return compose
	}
	compose = defaultName.ReplaceAll(compose, []byte(`name: "`+project+`"`))
	return bytes.ReplaceAll(compose, []byte("compose -p "+defaultProject+" "), []byte("compose -p "+project+" "))
}

func Files(set *Set, selected []Overlay, images map[string]string, project string) (map[string][]byte, error) {
	files, _, err := renderFiles(set, selected, images, project)
	if err != nil {
		return nil, err
	}
	out := make(map[string][]byte, len(files))
	for _, f := range files {
		out[f.name] = Managed(f.name, f.data)
	}
	return out, nil
}

func Render(set *Set, selected []Overlay, images map[string]string, project, dir string) (*Rendered, error) {
	return RenderAt(set, selected, images, project, dir, false)
}

func RenderAt(set *Set, selected []Overlay, images map[string]string, project, dir string, root bool) (*Rendered, error) {
	files, overlays, err := renderFiles(set, selected, images, project)
	if err != nil {
		return nil, err
	}
	for _, d := range MountedDirs {
		if err := os.MkdirAll(filepath.Join(dir, d), 0o755); err != nil {
			return nil, err
		}
	}
	if !root {
		for i, f := range files {
			files[i].data = Managed(f.name, f.data)
		}
	}
	gen := FilesDir(dir, root)
	if err := os.MkdirAll(gen, 0o755); err != nil {
		return nil, err
	}
	changed, stale, err := plan(gen, files, overlays)
	if err != nil {
		return nil, err
	}
	var write []renderedFile
	for _, f := range files {
		if slices.Contains(changed, f.name) {
			write = append(write, f)
		}
	}
	if err := writeAll(gen, write); err != nil {
		return nil, err
	}
	for _, name := range stale {
		if err := os.Remove(filepath.Join(gen, name)); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return nil, err
		}
	}
	r := &Rendered{Dir: dir, Hashes: make(map[string]string, len(files)), ComposeFiles: ComposeFilesAt(dir, overlays, root)}
	for _, f := range files {
		r.Hashes[f.name] = Hash(f.data)
	}
	r.Changed = append(changed, stale...)
	r.RestartNeeded = restartNeeded(r.Changed)
	return r, nil
}

func restartNeeded(changed []string) []string {
	var out []string
	for _, name := range changed {
		for _, svc := range MountedFiles[name] {
			if !slices.Contains(out, svc) {
				out = append(out, svc)
			}
		}
	}
	slices.Sort(out)
	return out
}

const tmpSuffix = ".rexctl-tmp"

func writeAll(dir string, files []renderedFile) error {
	var staged []string
	cleanup := func() {
		for _, p := range staged {
			os.Remove(p)
		}
	}
	for _, f := range files {
		tmp := filepath.Join(dir, f.name+tmpSuffix)
		if err := writeSynced(tmp, f.data); err != nil {
			os.Remove(tmp)
			cleanup()
			return err
		}
		staged = append(staged, tmp)
	}
	type previous struct {
		path   string
		data   []byte
		exists bool
	}
	var done []previous
	restore := func() {
		for _, p := range done {
			if p.exists {
				writeSynced(p.path, p.data)
			} else {
				os.Remove(p.path)
			}
		}
	}
	for i, f := range files {
		target := filepath.Join(dir, f.name)
		old, err := os.ReadFile(target)
		exists := err == nil
		if err != nil && !errors.Is(err, fs.ErrNotExist) {
			restore()
			cleanup()
			return err
		}
		if err := os.Rename(staged[i], target); err != nil {
			restore()
			cleanup()
			return err
		}
		done = append(done, previous{path: target, data: old, exists: exists})
	}
	return nil
}

func writeSynced(path string, data []byte) error {
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0o644)
	if err != nil {
		return err
	}
	if _, err := f.Write(data); err != nil {
		f.Close()
		return err
	}
	if err := f.Sync(); err != nil {
		f.Close()
		return err
	}
	return f.Close()
}
