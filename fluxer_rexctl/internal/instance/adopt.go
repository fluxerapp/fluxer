// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"bufio"
	"bytes"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
)

var errAlreadyManaged = errors.New("instance: already managed by rexctl")

const LegacyRelease = "legacy"

var (
	defaultComposeNames  = []string{"compose.yaml", "compose.yml", "docker-compose.yml", "docker-compose.yaml"}
	defaultOverrideNames = []string{"compose.override.yml", "compose.override.yaml", "docker-compose.override.yml", "docker-compose.override.yaml"}
	composeNameLine      = regexp.MustCompile(`^name:\s*["']?([^"'\s#]+)["']?\s*(?:#.*)?$`)
	interpolatedDefault  = regexp.MustCompile(`^\$\{[A-Za-z_][A-Za-z0-9_]*:?-([^}]*)\}$`)
	projectNameInvalid   = regexp.MustCompile(`[^a-z0-9_-]+`)
)

type Legacy struct {
	Dir          string
	ComposeFiles []string
	Project      string
	ImageTag     string

	env      *envfile.File
	snapshot string
	deduped  bool
}

func DetectLegacy(dir string) (*Legacy, error) {
	abs, err := filepath.Abs(dir)
	if err != nil {
		return nil, err
	}
	if _, err := os.Stat(filepath.Join(abs, StateDirName, StateFileName)); err == nil {
		return nil, errAlreadyManaged
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
	files, err := resolveComposeFiles(root, listEnv, os.Environ())
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

func resolveComposeFiles(dir string, env map[string]string, environ []string) ([]string, error) {
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

func (l *Legacy) read(path string) ([]byte, error) {
	if l.snapshot != "" && within(l.Dir, path) {
		if data, err := os.ReadFile(filepath.Join(l.snapshot, relPath(l.Dir, path))); err == nil {
			return data, nil
		}
	}
	return os.ReadFile(path)
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

func isFile(path string) bool {
	fi, err := os.Stat(path)
	return err == nil && fi.Mode().IsRegular()
}

func isDir(path string) bool {
	fi, err := os.Stat(path)
	return err == nil && fi.IsDir()
}
