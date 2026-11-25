// SPDX-License-Identifier: AGPL-3.0-or-later

package lint

import (
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
)

var envServiceRoots = []string{
	"fluxer_api/src", "fluxer_api/pkgs", "fluxer_api/bin", "packages",
	"fluxer_gateway/src", "fluxer_gateway/bouncer", "fluxer_gateway/rust", "fluxer_gateway/config", "fluxer_gateway/scripts",
	"fluxer_media_proxy/src", "fluxer_push/src", "fluxer_app_proxy/src", "fluxer_admin/src",
	"fluxer_common/src", "fluxer_svc/src", "fluxer_gifs/src", "fluxer_messages/src",
	"fluxer_snowflakes/src", "fluxer_users/src", "fluxer_unfurl/src",
	"fluxer_docs/src/server", "fluxer_docs/src/pages", "fluxer_docs/src/installer", "fluxer_docs/scripts",
	"fluxer_static",
}

var envRexctlRoots = []string{"fluxer_rexctl"}

var envStackRoots = []string{"deploy/helm"}

const envCatalog = "fluxer_rexctl/internal/settings/catalog.json"

var envToolDirs = []string{"lint", "schemagen"}

var envNotNames = []string{"FLUXER_MESSAGE", "FLUXER_CACHE_PURGE_HTTP", "FLUXER_CACHE_PURGE_CONTROLLER"}

var envConventions = []string{
	"CARGO_CFG_TARGET_OS", "CARGO_TARGET_DIR", "CLICOLOR_FORCE", "COLORFGBG", "DISPLAY", "DOCKER_DRIVER", "GITHUB_ACTIONS", "FORCE_COLOR", "HOME", "LANG", "LOCALAPPDATA", "OS", "PATH", "PROCESSOR_ARCHITECTURE",
	"PROCESSOR_ARCHITEW6432", "PWD", "Path", "ProgramFiles", "SSH_CONNECTION", "SUDO_UID", "SUDO_USER", "TERM", "TERM_PROGRAM", "USER",
	"USERNAME", "WAYLAND_DISPLAY", "WT_SESSION", "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_RUNTIME_DIR",
}

var envLibraryNames = []string{"ALL_PROXY", "DOCKER_CONFIG", "DOCKER_CONTEXT", "NODE_TLS_REJECT_UNAUTHORIZED", "PGSSLMODE", "SSL_CERT_DIR", "SSL_CERT_FILE"}

var (
	envQuoted   = regexp.MustCompile("[\"'`](FLUXER_[A-Z0-9_]*[A-Z0-9])[\"'`]")
	envKey      = regexp.MustCompile(`(?m)^\s*(FLUXER_[A-Z0-9_]*[A-Z0-9]): \{`)
	envRead     = regexp.MustCompile("(?:process\\.env|Bun\\.env|import\\.meta\\.env)(?:\\??\\.|\\??\\.?\\[[\"'`])([A-Z_][A-Z0-9_]*)|env::var(?:_os)?\\(\\s*\"([A-Z][A-Z0-9_]*)\"|os:getenv\\(\"([A-Z][A-Z0-9_]*)\"|\\b[Gg]etenv\\(\\s*\"([A-Za-z_][A-Za-z0-9_]*)\"|os\\.LookupEnv\\(\\s*\"([A-Za-z_][A-Za-z0-9_]*)\"")
	envCompose  = regexp.MustCompile(`(?:^|[^$])\$\{([A-Z][A-Z0-9_]*)`)
	envSuffix   = regexp.MustCompile(`format!\("\{prefix\}_([A-Z][A-Z0-9_]*)"\)`)
	envWord     = regexp.MustCompile(`[A-Za-z_][A-Za-z0-9_]*`)
	rustTestMod = regexp.MustCompile(`#\[cfg\(test\)\]\s*(?:pub\s+)?mod\s+\w+\s*\{`)
)

type envScan struct {
	reads    map[string][]string
	words    map[string]bool
	quoted   map[string]bool
	suffixes map[string]bool
}

func EnvCoverage(repo string, documented []string) ([]Finding, error) {
	scan := envScan{reads: map[string][]string{}, words: map[string]bool{}, quoted: map[string]bool{}, suffixes: map[string]bool{}}
	for _, root := range envServiceRoots {
		if err := scan.walk(repo, root, scan.service); err != nil {
			return nil, err
		}
	}
	for _, root := range envRexctlRoots {
		if err := scan.walk(repo, root, scan.rexctl); err != nil {
			return nil, err
		}
	}
	for _, root := range envStackRoots {
		if err := scan.walk(repo, root, scan.stack); err != nil {
			return nil, err
		}
	}
	if err := scan.catalog(repo); err != nil {
		return nil, err
	}
	dockerfiles, err := filepath.Glob(filepath.Join(repo, "fluxer_*", "Dockerfile"))
	if err != nil {
		return nil, err
	}
	for _, p := range dockerfiles {
		data, err := os.ReadFile(p)
		if err != nil {
			return nil, err
		}
		scan.addWords(string(data))
	}
	return scan.findings(documented), nil
}

func (s *envScan) findings(documented []string) []Finding {
	known := map[string]bool{}
	for _, name := range documented {
		known[name] = true
	}
	var out []Finding
	for _, name := range slices.Sorted(maps.Keys(s.reads)) {
		if known[name] || slices.Contains(envNotNames, name) || slices.Contains(envConventions, name) || s.prefixOf(name, known) {
			continue
		}
		out = append(out, Finding{File: s.reads[name][0], Rule: "undocumented environment variable", Text: name})
	}
	for _, name := range slices.Sorted(maps.Keys(known)) {
		if s.words[name] || s.words[strings.ToUpper(name)] || slices.Contains(envLibraryNames, name) || s.built(name) {
			continue
		}
		out = append(out, Finding{File: "fluxer_rexctl/internal/instance/envvars.go", Rule: "documented environment variable nothing reads", Text: name})
	}
	return out
}

func (s *envScan) prefixOf(name string, known map[string]bool) bool {
	for suffix := range s.suffixes {
		if known[name+"_"+suffix] {
			return true
		}
	}
	return false
}

func (s *envScan) built(name string) bool {
	for suffix := range s.suffixes {
		if prefix, ok := strings.CutSuffix(name, "_"+suffix); ok && s.quoted[prefix] {
			return true
		}
	}
	return false
}

func (s *envScan) walk(repo, root string, fn func(rel, data string)) error {
	dir := filepath.Join(repo, root)
	return filepath.WalkDir(dir, func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			switch d.Name() {
			case "node_modules", "target", "dist", "test", "tests", "__tests__", "fuzz", "testdata":
				return filepath.SkipDir
			}
			if p != dir && slices.Contains(envToolDirs, d.Name()) {
				return filepath.SkipDir
			}
			return nil
		}
		if envTestFile(d.Name()) {
			return nil
		}
		rel, err := filepath.Rel(repo, p)
		if err != nil {
			return err
		}
		rel = filepath.ToSlash(rel)
		if rel == "fluxer_rexctl/internal/instance/envvars.go" {
			return nil
		}
		data, err := os.ReadFile(p)
		if err != nil {
			return err
		}
		text := string(data)
		if filepath.Ext(p) == ".rs" {
			text = stripRustTests(text)
		}
		fn(rel, text)
		return nil
	})
}

func envTestFile(name string) bool {
	return strings.HasSuffix(name, "_test.go") || strings.Contains(name, ".test.") || strings.HasSuffix(name, "_test.rs") ||
		strings.HasSuffix(name, "_tests.rs") || name == "tests.rs" || strings.HasPrefix(name, "test_") || strings.HasSuffix(name, "_SUITE.erl") || strings.HasSuffix(name, "_test.gleam")
}

func envCodeFile(name string) bool {
	switch filepath.Ext(name) {
	case ".ts", ".mts", ".tsx", ".mjs", ".js", ".cjs", ".astro", ".rs", ".erl", ".hrl", ".c", ".h", ".sh", ".src", ".template", ".ps1", ".go", ".gleam":
		return true
	}
	return false
}

func (s *envScan) service(rel, data string) {
	if !envCodeFile(rel) {
		s.addWords(data)
		return
	}
	s.addReads(rel, data)
	if ext := filepath.Ext(rel); ext != ".c" && ext != ".h" {
		for _, m := range envQuoted.FindAllStringSubmatch(data, -1) {
			s.addRead(m[1], rel)
			s.quoted[m[1]] = true
		}
		for _, m := range envKey.FindAllStringSubmatch(data, -1) {
			s.addRead(m[1], rel)
		}
	}
	for _, m := range envSuffix.FindAllStringSubmatch(data, -1) {
		s.suffixes[m[1]] = true
	}
	s.addWords(data)
}

func (s *envScan) rexctl(rel, data string) {
	if filepath.Ext(rel) != ".go" {
		return
	}
	s.addReads(rel, data)
	s.addWords(data)
}

func (s *envScan) stack(rel, data string) {
	for _, m := range envCompose.FindAllStringSubmatch(data, -1) {
		s.addRead(m[1], rel)
	}
	s.addWords(data)
}

func (s *envScan) catalog(repo string) error {
	data, err := os.ReadFile(filepath.Join(repo, envCatalog))
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	var settings []struct {
		Name    string `json:"name"`
		Default string `json:"default"`
	}
	if err := json.Unmarshal(data, &settings); err != nil {
		return fmt.Errorf("%s: %w", envCatalog, err)
	}
	for _, setting := range settings {
		s.addRead(setting.Name, envCatalog)
		s.addWords(setting.Name + " " + setting.Default)
	}
	return nil
}

func (s *envScan) addReads(rel, data string) {
	for _, m := range envRead.FindAllStringSubmatch(data, -1) {
		for _, name := range m[1:] {
			if name != "" {
				s.addRead(name, rel)
			}
		}
	}
}

func (s *envScan) addRead(name, rel string) {
	if !slices.Contains(s.reads[name], rel) {
		s.reads[name] = append(s.reads[name], rel)
	}
}

func (s *envScan) addWords(data string) {
	for _, w := range envWord.FindAllString(data, -1) {
		s.words[w] = true
	}
}

func stripRustTests(text string) string {
	for {
		loc := rustTestMod.FindStringIndex(text)
		if loc == nil {
			return text
		}
		depth, end := 1, len(text)
		for i := loc[1]; i < len(text); i++ {
			switch text[i] {
			case '{':
				depth++
			case '}':
				depth--
			}
			if depth == 0 {
				end = i + 1
				break
			}
		}
		text = text[:loc[0]] + text[end:]
	}
}
