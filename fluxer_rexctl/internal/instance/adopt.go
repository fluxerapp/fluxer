// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"bufio"
	"bytes"
	"context"
	"crypto/sha256"
	_ "embed"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"sync"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

var ErrAlreadyManaged = errors.New("instance: already managed by rexctl")

const LegacyRelease = "legacy"

var (
	defaultComposeNames  = []string{"compose.yaml", "compose.yml", "docker-compose.yml", "docker-compose.yaml"}
	defaultOverrideNames = []string{"compose.override.yml", "compose.override.yaml", "docker-compose.override.yml", "docker-compose.override.yaml"}
	oldInstallerNames    = []string{"install.sh", "install.ps1"}
	composeNameLine      = regexp.MustCompile(`^name:\s*["']?([^"'\s#]+)["']?\s*(?:#.*)?$`)
	interpolatedDefault  = regexp.MustCompile(`^\$\{[A-Za-z_][A-Za-z0-9_]*:?-([^}]*)\}$`)
	projectNameInvalid   = regexp.MustCompile(`[^a-z0-9_-]+`)
)

type AdoptError struct {
	Msg, Fix string
}

func (e *AdoptError) Error() string { return e.Msg }

type Legacy struct {
	Dir          string
	ComposeFiles []string
	Project      string
	ImageTag     string

	env      *envfile.File
	snapshot string
	deduped  bool
}

type AdoptPlan struct {
	Dir         string
	Project     string
	Overlays    []string
	CustomFiles []string
	KeepAsMine  []string
	DeleteFiles []string
	Mine        map[string]string
	DedupedEnv  bool

	legacy     *Legacy
	customFrom string
}

func DetectLegacy(dir string) (*Legacy, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return nil, err
	}
	if _, err := os.Stat(filepath.Join(abs, StateDirName, StateFileName)); err == nil {
		return nil, ErrAlreadyManaged
	}
	l := &Legacy{Dir: abs}
	if snap := filepath.Join(abs, StateDirName, ReleasesDir, LegacyRelease); isDir(snap) {
		l.snapshot = snap
	}
	env, deduped, err := loadLegacyEnv(filepath.Join(abs, EnvFileName))
	if errors.Is(err, fs.ErrNotExist) {
		return nil, ErrNotFound
	}
	if err != nil {
		return nil, err
	}
	if !hasFluxerKey(env) {
		return nil, ErrNotFound
	}
	l.env = env
	l.deduped = deduped
	listEnv := env.Map()
	root := abs
	if l.snapshot != "" {
		root = l.snapshot
		if snapEnv, _, err := loadLegacyEnv(filepath.Join(l.snapshot, EnvFileName)); err == nil {
			listEnv = snapEnv.Map()
		}
	}
	files, err := ResolveComposeFiles(root, listEnv, os.Environ())
	if err != nil {
		return nil, err
	}
	for i, f := range files {
		if root != abs && within(root, f) {
			files[i] = filepath.Join(abs, relPath(root, f))
		}
	}
	l.ComposeFiles = files
	l.ImageTag = env.Value("FLUXER_IMAGE_TAG")
	if l.ImageTag == "" {
		l.ImageTag = "v1"
	}
	base, _ := l.read(files[0])
	l.Project = legacyProjectName(abs, env.Map(), os.Environ(), base)
	return l, nil
}

func splitComposeList(dir, list, sep string) []string {
	var out []string
	for _, entry := range strings.Split(list, sep) {
		entry = strings.TrimSpace(entry)
		if entry == "" {
			continue
		}
		if !filepath.IsAbs(entry) {
			entry = filepath.Join(dir, entry)
		}
		out = append(out, filepath.Clean(entry))
	}
	return out
}

func ResolveComposeFiles(dir string, env map[string]string, environ []string) ([]string, error) {
	lookup := func(key string) string {
		for i := len(environ) - 1; i >= 0; i-- {
			if k, v, ok := strings.Cut(environ[i], "="); ok && k == key && v != "" {
				return v
			}
		}
		return env[key]
	}
	if list := lookup("COMPOSE_FILE"); strings.TrimSpace(list) != "" {
		sep := lookup("COMPOSE_PATH_SEPARATOR")
		explicit := sep != ""
		if !explicit {
			sep = string(os.PathListSeparator)
		}
		out := splitComposeList(dir, list, sep)
		if !explicit && sep == ";" && len(out) == 1 && !isFile(out[0]) {
			if alt := splitComposeList(dir, list, ":"); len(alt) > 1 && slices.ContainsFunc(alt, isFile) {
				out = alt
			}
		}
		if len(out) > 0 {
			return out, nil
		}
	}
	var out []string
	for _, name := range defaultComposeNames {
		if p := filepath.Join(dir, name); isFile(p) {
			out = append(out, p)
			break
		}
	}
	if len(out) == 0 {
		return nil, ErrNotFound
	}
	for _, name := range defaultOverrideNames {
		if p := filepath.Join(dir, name); isFile(p) {
			out = append(out, p)
			break
		}
	}
	return out, nil
}

func PlanAdopt(l *Legacy) (*AdoptPlan, error) {
	p := &AdoptPlan{
		Dir:        l.Dir,
		Project:    l.Project,
		Mine:       map[string]string{},
		DedupedEnv: l.deduped,
		legacy:     l,
	}
	base := l.ComposeFiles[0]
	if filepath.Dir(base) != l.Dir {
		return nil, &AdoptError{
			Msg: fmt.Sprintf("Your instance loads its main compose file from outside %s.", l.Dir),
			Fix: fmt.Sprintf("Move %s into %s, then run rexctl again.", filepath.Base(base), l.Dir),
		}
	}
	loaded := map[string]bool{}
	var custom []string
	for i, f := range l.ComposeFiles {
		loaded[f] = true
		if _, err := l.read(f); err != nil {
			if i > 0 && overlayFor(f, l.Dir) != "" {
				p.Overlays = appendUnique(p.Overlays, overlayFor(f, l.Dir))
				continue
			}
			return nil, &AdoptError{
				Msg: fmt.Sprintf("Your instance loads %s, but that file is missing.", displayPath(l.Dir, f)),
				Fix: "Put the file back, then run rexctl again.",
			}
		}
		if i == 0 {
			p.classify(filepath.Base(f), stack.ComposeFile)
			continue
		}
		if o := overlayFor(f, l.Dir); o != "" {
			p.Overlays = appendUnique(p.Overlays, o)
			p.classify(filepath.Base(f), filepath.Base(f))
			continue
		}
		custom = append(custom, f)
	}
	switch len(custom) {
	case 0:
	case 1:
		p.customFrom = custom[0]
		p.CustomFiles = []string{displayPath(l.Dir, custom[0])}
	default:
		names := make([]string, len(custom))
		for i, f := range custom {
			names[i] = displayPath(l.Dir, f)
		}
		return nil, &AdoptError{
			Msg: fmt.Sprintf("Your instance loads %d compose files of your own: %s.", len(custom), strings.Join(names, ", ")),
			Fix: fmt.Sprintf("Combine them into one file named %s in %s, then run rexctl again.", CustomCompose, l.Dir),
		}
	}
	if path := filepath.Join(l.Dir, stack.ComposeFile); path != base && !loaded[path] && l.existed(stack.ComposeFile) {
		p.classify(stack.ComposeFile, stack.ComposeFile)
	}
	customPath := filepath.Join(l.Dir, CustomCompose)
	if p.customFrom != customPath && l.existed(CustomCompose) && !loaded[customPath] {
		p.keepMine(CustomCompose)
	}
	for name := range stack.OverlayFiles {
		file := stack.OverlayFiles[name]
		if !loaded[filepath.Join(l.Dir, file)] && l.existed(file) {
			p.classify(file, file)
		}
	}
	if l.existed(stack.Caddyfile) {
		p.classify(stack.Caddyfile, stack.Caddyfile)
	}
	if l.existed(".env.example") {
		p.classify(".env.example", ".env.example")
		if !slices.Contains(p.KeepAsMine, ".env.example") {
			p.DeleteFiles = appendUnique(p.DeleteFiles, ".env.example")
		}
	}
	if b := filepath.Base(base); b != stack.ComposeFile && !slices.Contains(p.KeepAsMine, b) {
		p.DeleteFiles = appendUnique(p.DeleteFiles, b)
	}
	for _, name := range oldInstallerNames {
		if l.existed(name) {
			p.DeleteFiles = appendUnique(p.DeleteFiles, name)
		}
	}
	slices.Sort(p.Overlays)
	return p, nil
}

func adoptFiles(ctx context.Context, p *AdoptPlan) error {
	l := p.legacy
	if err := p.snapshot(); err != nil {
		return err
	}
	for _, src := range slices.Sorted(maps.Keys(p.Mine)) {
		if err := ctx.Err(); err != nil {
			return err
		}
		if err := l.moveTo(filepath.Join(p.Dir, src), filepath.Join(p.Dir, p.Mine[src])); err != nil {
			return err
		}
	}
	if p.customFrom != "" {
		dst := filepath.Join(p.Dir, CustomCompose)
		if p.customFrom != dst {
			if within(p.Dir, p.customFrom) {
				if err := l.moveTo(p.customFrom, dst); err != nil {
					return err
				}
			} else if err := l.copyTo(p.customFrom, dst); err != nil {
				return err
			}
		}
	}
	for _, name := range p.DeleteFiles {
		if err := removeIfExists(filepath.Join(p.Dir, name)); err != nil {
			return err
		}
	}
	if err := p.keepRunning(); err != nil {
		return err
	}
	for _, d := range []string{BackupsDir, CertsDir, CaddyDropInDir} {
		mode := os.FileMode(0o755)
		if d == BackupsDir {
			mode = 0o700
		}
		if err := os.MkdirAll(filepath.Join(p.Dir, d), mode); err != nil {
			return err
		}
	}
	return envfile.RestrictToOwner(filepath.Join(p.Dir, StateDirName))
}

func (p *AdoptPlan) keepRunning() error {
	l := p.legacy
	want := map[string]string{stack.ComposeFile: l.ComposeFiles[0]}
	for _, f := range l.ComposeFiles[1:] {
		if overlayFor(f, p.Dir) != "" {
			want[filepath.Base(f)] = f
		}
	}
	if l.existed(stack.Caddyfile) {
		want[stack.Caddyfile] = filepath.Join(p.Dir, stack.Caddyfile)
	}
	for _, name := range slices.Sorted(maps.Keys(want)) {
		dst := filepath.Join(p.Dir, name)
		if fi, err := os.Stat(dst); err == nil && fi.Mode().IsRegular() {
			if perm := fi.Mode().Perm(); perm&0o022 != 0 {
				if err := os.Chmod(dst, perm&^0o022); err != nil {
					return err
				}
			}
			continue
		}
		data, err := l.read(want[name])
		if err != nil {
			continue
		}
		if err := envfile.WriteFileAtomic(dst, normalizeLF(data), false); err != nil {
			return err
		}
	}
	return nil
}

func (p *AdoptPlan) classify(name, shipped string) {
	data, err := p.legacy.read(filepath.Join(p.Dir, name))
	if err != nil {
		return
	}
	if !isShipped(shipped, data) {
		p.keepMine(name)
	}
}

func (p *AdoptPlan) keepMine(name string) {
	if slices.Contains(p.KeepAsMine, name) {
		return
	}
	src := filepath.Join(p.Dir, name)
	data, _ := p.legacy.read(src)
	dst := name + ".mine"
	for n := 2; ; n++ {
		existing, err := os.ReadFile(filepath.Join(p.Dir, dst))
		if errors.Is(err, fs.ErrNotExist) || (err == nil && bytes.Equal(existing, data)) {
			break
		}
		dst = fmt.Sprintf("%s.mine.%d", name, n)
	}
	p.KeepAsMine = append(p.KeepAsMine, name)
	p.Mine[name] = dst
}

func (p *AdoptPlan) snapshot() error {
	l := p.legacy
	if l.snapshot != "" {
		return nil
	}
	final := filepath.Join(p.Dir, StateDirName, ReleasesDir, LegacyRelease)
	tmp := final + ".partial"
	if err := os.RemoveAll(tmp); err != nil {
		return err
	}
	if err := os.MkdirAll(tmp, 0o700); err != nil {
		return err
	}
	names := []string{EnvFileName, stack.ComposeFile, stack.Caddyfile, ".env.example", CustomCompose}
	for _, f := range l.ComposeFiles {
		if within(p.Dir, f) {
			names = append(names, relPath(p.Dir, f))
		}
	}
	for _, f := range stack.OverlayFiles {
		names = append(names, f)
	}
	names = append(names, p.DeleteFiles...)
	names = append(names, slices.Sorted(maps.Keys(p.Mine))...)
	slices.Sort(names)
	names = slices.Compact(names)
	for _, name := range names {
		src := filepath.Join(p.Dir, name)
		data, err := os.ReadFile(src)
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			return err
		}
		dst := filepath.Join(tmp, name)
		if err := os.MkdirAll(filepath.Dir(dst), 0o700); err != nil {
			return err
		}
		if err := envfile.WriteFileAtomic(dst, data, true); err != nil {
			return err
		}
		if fi, err := os.Stat(src); err == nil {
			_ = os.Chtimes(dst, fi.ModTime(), fi.ModTime())
		}
	}
	if err := os.Rename(tmp, final); err != nil {
		return err
	}
	l.snapshot = final
	return nil
}

func (l *Legacy) snapshotDir() string {
	if l.snapshot != "" {
		return l.snapshot
	}
	return l.Dir
}

func (l *Legacy) existed(name string) bool {
	if l.snapshot != "" {
		return isFile(filepath.Join(l.snapshot, name))
	}
	return isFile(filepath.Join(l.Dir, name))
}

func (l *Legacy) read(path string) ([]byte, error) {
	if l.snapshot != "" && within(l.Dir, path) {
		if data, err := os.ReadFile(filepath.Join(l.snapshot, relPath(l.Dir, path))); err == nil {
			return data, nil
		}
	}
	return os.ReadFile(path)
}

func (l *Legacy) moveTo(src, dst string) error {
	if err := l.copyTo(src, dst); err != nil {
		return err
	}
	if err := os.Remove(src); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return nil
}

func (l *Legacy) copyTo(src, dst string) error {
	data, err := l.read(src)
	if err != nil {
		return err
	}
	if existing, err := os.ReadFile(dst); err == nil && bytes.Equal(existing, data) {
		return nil
	}
	return envfile.WriteFileAtomic(dst, normalizeLF(data), false)
}

func loadLegacyEnv(path string) (*envfile.File, bool, error) {
	env, err := envfile.Load(path)
	if !errors.Is(err, envfile.ErrDuplicateKey) {
		return env, false, err
	}
	last := env.Map()
	for _, d := range env.Duplicates() {
		env.Delete(d.Key)
		env.Set(d.Key, last[d.Key])
	}
	return env, true, nil
}

func hasFluxerKey(env *envfile.File) bool {
	for _, k := range env.Keys() {
		if strings.HasPrefix(k, "FLUXER_") {
			return true
		}
	}
	return false
}

func legacyProjectName(dir string, env map[string]string, environ []string, baseCompose []byte) string {
	for i := len(environ) - 1; i >= 0; i-- {
		if k, v, ok := strings.Cut(environ[i], "="); ok && k == "COMPOSE_PROJECT_NAME" && v != "" {
			return v
		}
	}
	if v := env["COMPOSE_PROJECT_NAME"]; v != "" {
		return v
	}
	sc := bufio.NewScanner(bytes.NewReader(baseCompose))
	for sc.Scan() {
		m := composeNameLine.FindStringSubmatch(strings.TrimRight(sc.Text(), "\r"))
		if m == nil {
			continue
		}
		name := m[1]
		if d := interpolatedDefault.FindStringSubmatch(name); d != nil {
			name = d[1]
		}
		if name != "" && !strings.Contains(name, "$") {
			return name
		}
	}
	return projectNameInvalid.ReplaceAllString(strings.ToLower(filepath.Base(dir)), "")
}

func overlayFor(path, dir string) string {
	if filepath.Dir(path) != dir {
		return ""
	}
	for o, f := range stack.OverlayFiles {
		if filepath.Base(path) == f {
			return string(o)
		}
	}
	return ""
}

//go:embed shipped.json
var shippedJSON []byte

var shippedHashes = sync.OnceValue(func() map[string][]string {
	var known map[string][]string
	if err := json.Unmarshal(shippedJSON, &known); err != nil {
		panic(err)
	}
	return known
})

func ShippedHash(data []byte) string {
	sum := sha256.Sum256(normalizeLF(data))
	return hex.EncodeToString(sum[:])
}

func isShipped(name string, data []byte) bool {
	if name == stack.ComposeFile {
		data = stack.UnpinImages(normalizeLF(data))
	}
	return slices.Contains(shippedHashes()[name], ShippedHash(data))
}

func normalizeLF(data []byte) []byte {
	return bytes.ReplaceAll(data, []byte("\r\n"), []byte("\n"))
}

func appendUnique(list []string, v string) []string {
	if slices.Contains(list, v) {
		return list
	}
	return append(list, v)
}

func within(dir, path string) bool {
	rel, err := filepath.Rel(dir, path)
	return err == nil && rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator)) && !filepath.IsAbs(rel)
}

func relPath(dir, path string) string {
	rel, err := filepath.Rel(dir, path)
	if err != nil {
		return path
	}
	return rel
}

func displayPath(dir, path string) string {
	if within(dir, path) {
		return relPath(dir, path)
	}
	return path
}

func isFile(path string) bool {
	fi, err := os.Stat(path)
	return err == nil && fi.Mode().IsRegular()
}

func isDir(path string) bool {
	fi, err := os.Stat(path)
	return err == nil && fi.IsDir()
}
