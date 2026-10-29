// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"encoding/json"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"testing"
	"unicode/utf8"

	"go.yaml.in/yaml/v4"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const repoRoot = "../../.."

var codeRoots = []string{
	"fluxer_api/src", "fluxer_api/pkgs", "fluxer_api/bin", "packages",
	"fluxer_gateway/src", "fluxer_gateway/include", "fluxer_gateway/native", "fluxer_gateway/config", "fluxer_gateway/scripts",
	"fluxer_media_proxy/src", "fluxer_push/src", "fluxer_app_proxy/src", "fluxer_admin/src",
	"fluxer_common/src", "fluxer_svc/src", "fluxer_gifs/src", "fluxer_messages/src",
	"fluxer_snowflakes/src", "fluxer_users/src", "fluxer_unfurl/src", "fluxer_docs/src/server", "fluxer_docs/src/pages",
	"fluxer_docs/src/installer", "fluxer_docs/scripts",
	"fluxer_static",
}

var devScripts = "packages/config/scripts/"

var caddyfiles = map[string]string{"fluxer_static/Caddyfile": "static-proxy"}

var dockerOSVars = []string{"HOME", "PATH", "XDG_CONFIG_HOME", "XDG_DATA_HOME"}

var libraryEnvVars = []string{"ALL_PROXY", "NODE_TLS_REJECT_UNAUTHORIZED", "PGSSLMODE", "SSL_CERT_DIR", "SSL_CERT_FILE"}

var libraryEnvAnchors = map[string][][2]string{
	"ALL_PROXY":                    {{"fluxer_push/Cargo.toml", `reqwest = [^\n]*"rustls"`}},
	"NODE_TLS_REJECT_UNAUTHORIZED": {{"fluxer_api/Dockerfile", `(?m)^CMD \["node"`}},
	"PGSSLMODE":                    {{"fluxer_api/pkgs/postgres/src/Client.ts", `\bssl: this\.config\.ssl \?[^\n]*: undefined,`}},
	"SSL_CERT_DIR":                 {{"fluxer_svc/src/postgres.rs", `MakeRustlsConnect::with_native_certs\(\)`}, {"fluxer_push/Cargo.toml", `reqwest = [^\n]*"rustls"`}},
	"SSL_CERT_FILE":                {{"fluxer_svc/src/postgres.rs", `MakeRustlsConnect::with_native_certs\(\)`}, {"fluxer_push/Cargo.toml", `reqwest = [^\n]*"rustls"`}},
}

var noProxyFiles = []string{"fluxer_common/src/healthcheck.rs"}

var chartInputVars = []string{"FLUXER_VALKEY_PASSWORD"}

var codeServices = map[string][]string{
	"fluxer_api/":         {"api", "worker"},
	"packages/":           {"api", "worker"},
	"fluxer_gateway/":     {"gateway"},
	"fluxer_media_proxy/": {"media-proxy"},
	"fluxer_push/":        {"push"},
	"fluxer_app_proxy/":   {"app-proxy"},
	"fluxer_admin/":       {"admin"},
	"fluxer_common/":      {"admin", "app-proxy", "gifs", "media-proxy", "messages", "push", "snowflakes", "unfurl", "users"},
	"fluxer_svc/":         {"gifs", "messages", "push", "snowflakes", "unfurl", "users"},
	"fluxer_gifs/":        {"gifs"},
	"fluxer_messages/":    {"messages"},
	"fluxer_snowflakes/":  {"snowflakes"},
	"fluxer_users/":       {"users"},
	"fluxer_unfurl/":      {"unfurl"},
	"fluxer_static/":      {"static-proxy"},
}

const helmChart = "deploy/helm/fluxer"

var rexctlRoots = []string{"fluxer_rexctl", "fluxer_docs/src/installer"}

var rexctlOSVars = []string{
	"HOME", "LOCALAPPDATA", "OS", "PATH", "PROCESSOR_ARCHITECTURE", "PROCESSOR_ARCHITEW6432", "PWD", "Path",
	"ProgramFiles", "SUDO_USER", "TERM", "USER", "USERNAME", "XDG_CONFIG_HOME", "XDG_RUNTIME_DIR",
}

var rexctlLibraryVars = []string{"DOCKER_CONFIG", "DOCKER_CONTEXT"}

var rexctlProxyVars = []string{"HTTPS_PROXY", "HTTP_PROXY", "NO_PROXY"}

var runtimeEnvVars = []string{"MALLOC_ARENA_MAX", "NODE_EXTRA_CA_CERTS", "NODE_OPTIONS", "TOKIO_WORKER_THREADS"}

var (
	quotedEnv    = regexp.MustCompile("[\"'`](FLUXER_[A-Z0-9_]*[A-Z0-9])[\"'`]")
	keyEnv       = regexp.MustCompile(`(?m)^\s*(FLUXER_[A-Z0-9_]*[A-Z0-9]): \{`)
	envCall      = regexp.MustCompile(`(?:\b[a-z_]*(?:env|getenv|var|from)[a-z_]*|\b[a-z]*[A-Z][A-Za-z]*Env[A-Za-z]*)\s*\(\s*(?:&?get,\s*|&?env,\s*|process\.env,\s*)?["']([A-Z][A-Z0-9_]*)["']`)
	envListCall  = regexp.MustCompile(`(?:\b[a-z_]*(?:env|getenv|var|from)[a-z_]*|\b[a-z]*[A-Z][A-Za-z]*Env[A-Za-z]*)\s*\(\s*(?:&?get,\s*|&?env,\s*|process\.env,\s*)?&?\[([^\]]*)\]`)
	listName     = regexp.MustCompile("[\"'`]([A-Z][A-Z0-9_]*)[\"'`]")
	processEnv   = regexp.MustCompile("(?:process\\.env|Bun\\.env|import\\.meta\\.env|\\benv)(?:\\??\\.|\\??\\.?\\[[\"'`])([A-Z_][A-Z0-9_]*)")
	destructEnv  = regexp.MustCompile(`\{([^{}]*)\}\s*=\s*(?:process|Bun|import\.meta)\.env\b`)
	envPrefix    = regexp.MustCompile(`\b[a-z_]+_config\(\s*&?env,\s*"([A-Z][A-Z0-9_]*)"`)
	envSuffix    = regexp.MustCompile(`format!\("\{prefix\}_([A-Z][A-Z0-9_]*)"\)`)
	builtEnv     = regexp.MustCompile(`env::var(?:_os)?\(\s*&?(?:format|concat)!\(\s*"([A-Z][A-Z0-9_]*)`)
	mapEnv       = regexp.MustCompile(`\.(?:get|require)\(\s*"([A-Z][A-Z0-9_]*)"`)
	shellEnv     = regexp.MustCompile(`\$\{?([A-Z][A-Z0-9_]*)`)
	psEnv        = regexp.MustCompile(`\$env:([A-Za-z_][A-Za-z0-9_]*)`)
	directEnv    = regexp.MustCompile("(?:process\\.env|Bun\\.env|import\\.meta\\.env)(?:\\??\\.|\\??\\.?\\[[\"'`])([A-Z_][A-Z0-9_]*)|os:getenv\\(\"([A-Z][A-Z0-9_]*)\"|env::var(?:_os)?\\(\"([A-Z][A-Z0-9_]*)\"|\\b(?:g_|secure_)?getenv\\(\\s*\"([A-Z_][A-Z0-9_]*)\"")
	goEnv        = regexp.MustCompile(`os\.(?:Getenv|LookupEnv)\(\s*"([A-Za-z_][A-Za-z0-9_]*)"|(?s)range \[\]string\{([^}]*)\}\s*\{[^}]*?os\.(?:Getenv|LookupEnv)\(|Env\s*=\s*"([A-Z][A-Z0-9_]*)"`)
	dockerEnv    = regexp.MustCompile(`(?m)^ENV\s+((?:[^\n]*\\\n)*[^\n]*)`)
	envAssign    = regexp.MustCompile(`\b([A-Z_][A-Z0-9_]*)=`)
	caddyEnv     = regexp.MustCompile(`\{(?:\$|env\.)([A-Z][A-Z0-9_]*)`)
	composeSh    = regexp.MustCompile(`\$\$\{?([A-Z][A-Z0-9_]*)`)
	constEnv     = regexp.MustCompile("(?m)(?:\\bconst|\\bstatic|\\blet|\\bvar)\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*(?::[^=\\n]+)?=\\s*[\"'`]([A-Z][A-Z0-9_]*)[\"'`]|-define\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*\"([A-Z][A-Z0-9_]*)\"")
	identEnv     = regexp.MustCompile(`(?:\b[a-z_]*(?:env|getenv|var|from)[a-z_]*|\b[a-z]*[A-Z][A-Za-z]*Env[A-Za-z]*)\s*\(\s*(?:&?get,\s*|&?env,\s*|process\.env,\s*|&\[\s*)?&?\??([A-Z][A-Z0-9_]*)\s*[,)\]]|(?:process\.env|Bun\.env|import\.meta\.env|\benv)\??\.?\[\s*([A-Z][A-Z0-9_]*)\s*\]`)
	cEnv         = regexp.MustCompile(`\b(?:g_|secure_)?getenv\(\s*"([A-Z_][A-Z0-9_]*)"`)
	tsImport     = regexp.MustCompile(`(?:from|import)\s*\(?\s*'([^']+)'`)
	goConst      = regexp.MustCompile(`(?m)^\s*(?:const\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*(?:string\s*)?(?:=|:=)\s*"([A-Z][A-Z0-9_]*)"`)
	goIdentEnv   = regexp.MustCompile(`os\.(?:Getenv|LookupEnv)\(\s*(?:[a-z][A-Za-z0-9_]*\.)?([A-Za-z_][A-Za-z0-9_]*)\s*\)`)
	goEnviron    = regexp.MustCompile(`\bk == "([A-Z][A-Z0-9_]*)"|\blookup\("([A-Z][A-Z0-9_]*)"\)`)
	dockerRun    = regexp.MustCompile(`(?m)^(?:HEALTHCHECK|CMD|ENTRYPOINT)\b((?:[^\n]*\\\n)*[^\n]*)`)
	tsFunc       = regexp.MustCompile(`\bfunction\s+([A-Za-z_$][\w$]*)\s*(?:<[^>]*>)?\s*\(([^)]*)\)|\b(?:const|let)\s+([A-Za-z_$][\w$]*)\s*=\s*(?:async\s*)?\(([^)]*)\)[^=;]*=>`)
	rsFunc       = regexp.MustCompile(`\bfn\s+([a-z_][a-z0-9_]*)\s*(?:<[^>]*>)?\s*\(([^)]*)\)`)
	erlFunc      = regexp.MustCompile(`(?m)^([a-z][A-Za-z0-9_]*)\(((?:[^()\n]|\([^()\n]*\))*)\)\s*(?:when\b[^\n]*?)?->`)
	erlForm      = regexp.MustCompile(`(?m)^[a-z-]`)
	erlEnvCallee = regexp.MustCompile(`^(?:getenv|env_[a-z_]+|[a-z_]+_env)$`)
	callStart    = regexp.MustCompile(`\b([A-Za-z_$][\w$]*)\s*\(`)
	quotedName   = regexp.MustCompile("^[\"'`]([A-Z][A-Z0-9_]*)[\"'`]$")
	identName    = regexp.MustCompile(`^&?([A-Z][A-Z0-9_]*)$`)
	paramEnv     = regexp.MustCompile(`(?:process\.env|Bun\.env|import\.meta\.env|\benv)\??\.?\[\s*([a-z_$][\w$]*)\s*\]|env::var(?:_os)?\(\s*&?([a-z_][a-z0-9_]*)\s*\)|\b(?:env|vars)\.get\(\s*&?([a-z_][a-z0-9_]*)\b`)
	envCallee    = regexp.MustCompile(`^(?:[a-z_]*(?:env|getenv|var|from)[a-z_]*|[a-z]*[A-Z][A-Za-z]*Env[A-Za-z]*)$`)
	localEnv     = regexp.MustCompile(`(?:\b[a-z_]*(?:env|getenv|var|from)[a-z_]*|\b[a-z]*[A-Z][A-Za-z]*Env[A-Za-z]*)\s*\(\s*(?:&?get,\s*|&?env,\s*|process\.env,\s*)?&?\??([A-Za-z_][A-Za-z0-9_]*)\s*[,)]|(?:process\.env|Bun\.env|import\.meta\.env|\benv)\??\.?\[\s*([A-Za-z_$][\w$]*)\s*\]`)
	erlBind      = regexp.MustCompile(`\b([A-Z][A-Za-z0-9_]*)\s*=\s*"([A-Z][A-Z0-9_]*)"`)
	rustForList  = regexp.MustCompile(`\bfor\s+([a-z_][a-z0-9_]*)\s+in\s+&?\[([^\]]*)\]\s*\{`)
	fieldRead    = regexp.MustCompile(`\|\(\s*_\s*,\s*([a-z_][a-z0-9_]*)\s*\)\|\s*([a-z_][a-z0-9_]*)\(\s*&?([a-z_][a-z0-9_]*)\s*\)`)
	fieldPair    = regexp.MustCompile(`\(\s*"[a-z_]+"\s*,\s*"([A-Z][A-Z0-9_]*)"\s*\)`)
)

var notEnvNames = []string{"FLUXER_MESSAGE", "FLUXER_CACHE_PURGE_HTTP", "FLUXER_CACHE_PURGE_CONTROLLER"}

func testPath(p string) bool {
	for _, part := range strings.Split(filepath.ToSlash(p), "/") {
		switch part {
		case "test", "tests", "__tests__", "fuzz", "node_modules", "target", "dist":
			return true
		}
	}
	base := filepath.Base(p)
	return strings.Contains(base, ".test.") || strings.HasSuffix(base, "_test.rs") || strings.HasSuffix(base, "_tests.rs") ||
		base == "tests.rs" || base == "test_fixtures.rs" || strings.HasPrefix(base, "test_") || strings.HasSuffix(base, "_SUITE.erl")
}

func scriptLang(p string) string {
	switch filepath.Ext(p) {
	case ".ts", ".mts", ".tsx", ".mjs", ".js", ".cjs", ".astro":
		return "ts"
	case ".rs", ".erl", ".hrl":
		return "native"
	case ".c", ".h":
		return "c"
	case ".sh", ".src", ".template":
		return "shell"
	case ".ps1":
		return "ps"
	case ".go":
		if !strings.HasSuffix(p, "_test.go") {
			return "go"
		}
	}
	return ""
}

func addFound(found map[string][]string, name, rel string) {
	if name != "" && !slices.Contains(notEnvNames, name) && !slices.Contains(found[name], rel) {
		found[name] = append(found[name], rel)
	}
}

type codeFile struct {
	rel, lang, data string
	test            bool
}

func walkCode(t *testing.T, roots []string, fn func(rel, lang, data string)) {
	t.Helper()
	var files []codeFile
	byRel := map[string]bool{}
	for _, root := range roots {
		dir := filepath.Join(repoRoot, root)
		err := filepath.WalkDir(dir, func(p string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() {
				if base := d.Name(); base == "node_modules" || base == "target" || base == "dist" {
					return filepath.SkipDir
				}
				return nil
			}
			lang := scriptLang(p)
			if lang == "" {
				return nil
			}
			data, err := os.ReadFile(p)
			if err != nil {
				return err
			}
			inner, _ := filepath.Rel(dir, p)
			rel, _ := filepath.Rel(repoRoot, p)
			rel = filepath.ToSlash(rel)
			text := string(data)
			if filepath.Ext(p) == ".rs" {
				text = stripRustTests(text)
			}
			files = append(files, codeFile{rel: rel, lang: lang, data: text, test: testPath(inner)})
			byRel[rel] = true
			return nil
		})
		if err != nil {
			t.Fatal(err)
		}
	}
	imported := map[string]bool{}
	for _, f := range files {
		if f.test || f.lang != "ts" {
			continue
		}
		for _, m := range tsImport.FindAllStringSubmatch(f.data, -1) {
			if rel := resolveImport(f.rel, m[1], byRel); rel != "" {
				imported[rel] = true
			}
		}
	}
	for _, f := range files {
		if !f.test || imported[f.rel] {
			fn(f.rel, f.lang, f.data)
		}
	}
}

func resolveImport(from, spec string, files map[string]bool) string {
	var base string
	switch {
	case strings.HasPrefix(spec, "@app/"):
		base = "fluxer_api/src/" + strings.TrimPrefix(spec, "@app/")
	case strings.HasPrefix(spec, "@fluxer/"):
		pkg, rest, _ := strings.Cut(strings.TrimPrefix(spec, "@fluxer/"), "/")
		base = "packages/" + pkg + "/" + rest
		if _, err := os.Stat(filepath.Join(repoRoot, "packages", pkg)); err != nil {
			base = "fluxer_api/pkgs/" + pkg + "/" + rest
		}
	case strings.HasPrefix(spec, "."):
		base = path.Join(path.Dir(from), spec)
	default:
		return ""
	}
	base = strings.TrimSuffix(base, ".js")
	for _, c := range []string{base, base + ".ts", base + ".tsx", base + "/index.ts"} {
		if files[c] {
			return c
		}
	}
	return ""
}

func codeEnvNames(t *testing.T) (reads, mentions map[string][]string) {
	t.Helper()
	var files []codeFile
	walkCode(t, codeRoots, func(rel, lang, data string) {
		files = append(files, codeFile{rel: rel, lang: lang, data: data})
	})
	return scanEnvNames(files)
}

func runtimeReads(code map[string][]string) map[string][]string {
	out := map[string][]string{}
	for name, rels := range code {
		for _, rel := range rels {
			if !strings.HasPrefix(rel, devScripts) {
				out[name] = append(out[name], rel)
			}
		}
	}
	return out
}

func addListNames(found map[string][]string, list, rel string) {
	for _, m := range listName.FindAllStringSubmatch(list, -1) {
		addFound(found, m[1], rel)
	}
}

func scanEnvNames(files []codeFile) (reads, mentions map[string][]string) {
	found := map[string][]string{}
	mentions = map[string][]string{}
	consts := map[string][]string{}
	type pending struct{ rel, ident string }
	var idents []pending
	sources := map[string]string{}
	for _, f := range files {
		rel, lang, data := f.rel, f.lang, f.data
		if lang == "ts" || lang == "native" {
			sources[rel] = data
			for _, m := range envListCall.FindAllStringSubmatch(data, -1) {
				addListNames(found, m[1], rel)
			}
		}
		if (lang == "shell" || lang == "ps") && slices.ContainsFunc(rexctlRoots, func(root string) bool { return strings.HasPrefix(rel, root+"/") }) {
			continue
		}
		if lang == "ts" || lang == "native" {
			local := map[string][]string{}
			for _, m := range constEnv.FindAllStringSubmatch(data, -1) {
				ident, value := m[1]+m[3], m[2]+m[4]
				local[ident] = append(local[ident], value)
				consts[ident] = append(consts[ident], value)
			}
			if erlangFile(rel) {
				for _, m := range erlBind.FindAllStringSubmatch(data, -1) {
					local[m[1]] = append(local[m[1]], m[2])
				}
			}
			for _, m := range localEnv.FindAllStringSubmatch(data, -1) {
				for _, v := range local[m[1]+m[2]] {
					addFound(found, v, rel)
				}
			}
			for _, loc := range rustForList.FindAllStringSubmatchIndex(data, -1) {
				ident, open := data[loc[2]:loc[3]], loc[1]-1
				for _, m := range localEnv.FindAllStringSubmatch(data[open:rustBlockEnd(data, open)], -1) {
					if m[1]+m[2] == ident {
						addListNames(found, data[loc[4]:loc[5]], rel)
					}
				}
			}
			for _, m := range fieldRead.FindAllStringSubmatch(data, -1) {
				if m[1] == m[3] && envCallee.MatchString(m[2]) {
					for _, p := range fieldPair.FindAllStringSubmatch(data, -1) {
						addFound(found, p[1], rel)
					}
				}
			}
			for _, m := range identEnv.FindAllStringSubmatch(data, -1) {
				ident := m[1] + m[2]
				if values, ok := local[ident]; ok {
					for _, v := range values {
						addFound(found, v, rel)
					}
				} else {
					idents = append(idents, pending{rel, ident})
				}
			}
		}
		var patterns, named []*regexp.Regexp
		prefixes := map[string]bool{}
		switch lang {
		case "ts":
			patterns, named = []*regexp.Regexp{envCall, processEnv}, []*regexp.Regexp{quotedEnv, keyEnv}
			if rel == configOverrides {
				patterns = append(patterns, keyEnv)
				if block := namedAliases.FindStringSubmatch(data); block != nil {
					for _, m := range aliasPair.FindAllStringSubmatch(block[1], -1) {
						addFound(found, m[2], rel)
					}
				}
			}
			for _, m := range destructEnv.FindAllStringSubmatch(data, -1) {
				for _, field := range strings.Split(m[1], ",") {
					name, _, _ := strings.Cut(strings.TrimSpace(field), ":")
					name, _, _ = strings.Cut(name, "=")
					addFound(found, strings.TrimSpace(name), rel)
				}
			}
		case "native":
			patterns, named = []*regexp.Regexp{envCall, mapEnv, builtEnv}, []*regexp.Regexp{quotedEnv}
			for _, m := range envPrefix.FindAllStringSubmatch(data, -1) {
				prefixes[m[1]] = true
				for _, s := range envSuffix.FindAllStringSubmatch(data, -1) {
					addFound(found, m[1]+"_"+s[1], rel)
				}
			}
		case "c":
			patterns = []*regexp.Regexp{cEnv}
		case "shell":
			patterns = []*regexp.Regexp{shellEnv}
		case "ps":
			patterns = []*regexp.Regexp{psEnv}
		}
		for _, re := range patterns {
			for _, m := range re.FindAllStringSubmatch(data, -1) {
				if !prefixes[m[1]] {
					addFound(found, m[1], rel)
				}
			}
		}
		for _, re := range named {
			for _, m := range re.FindAllStringSubmatch(data, -1) {
				if !prefixes[m[1]] {
					addFound(mentions, m[1], rel)
				}
			}
		}
	}
	helpers := envHelpers(sources)
	for rel, data := range sources {
		for _, h := range helpers {
			if h.erl != erlangFile(rel) || !strings.Contains(data, h.name) {
				continue
			}
			for _, c := range regexp.MustCompile(`\b`+regexp.QuoteMeta(h.name)+`\s*\(`).FindAllStringIndex(data, -1) {
				args := splitArgs(data, c[1]-1)
				if h.arg >= len(args) {
					continue
				}
				arg := args[h.arg]
				if strings.HasPrefix(strings.TrimPrefix(arg, "&"), "[") {
					addListNames(found, arg, rel)
				} else if m := quotedName.FindStringSubmatch(arg); m != nil {
					addFound(found, m[1], rel)
				} else if m := identName.FindStringSubmatch(arg); m != nil {
					idents = append(idents, pending{rel, m[1]})
				}
			}
		}
	}
	for _, p := range idents {
		for _, v := range consts[p.ident] {
			addFound(found, v, p.rel)
		}
	}
	for name, rels := range found {
		for _, rel := range rels {
			addFound(mentions, name, rel)
		}
	}
	return found, mentions
}

type envHelper struct {
	name string
	arg  int
	erl  bool
}

func erlangFile(rel string) bool {
	ext := filepath.Ext(rel)
	return ext == ".erl" || ext == ".hrl"
}

func splitArgs(data string, open int) []string {
	var args []string
	depth, start := 0, open+1
	var quote byte
	for i := open; i < len(data); i++ {
		c := data[i]
		switch {
		case quote != 0:
			if c == '\\' {
				i++
			} else if c == quote {
				quote = 0
			}
		case c == '"' || c == '\'' || c == '`':
			quote = c
		case c == '(' || c == '[' || c == '{':
			depth++
		case c == ')' || c == ']' || c == '}':
			depth--
			if depth == 0 {
				return append(args, strings.TrimSpace(data[start:i]))
			}
		case c == ',' && depth == 1:
			args = append(args, strings.TrimSpace(data[start:i]))
			start = i + 1
		}
	}
	return args
}

func funcBody(data string, from int) string {
	end := min(len(data), from+4000)
	i := strings.IndexAny(data[from:end], "{\n")
	if i < 0 {
		return data[from:end]
	}
	if data[from+i] == '\n' {
		return data[from : from+i]
	}
	depth := 0
	for j := from + i; j < end; j++ {
		switch data[j] {
		case '{':
			depth++
		case '}':
			depth--
			if depth == 0 {
				return data[from : j+1]
			}
		}
	}
	return data[from:end]
}

func splitParams(params string) []string {
	var out []string
	depth, start := 0, 0
	for i, c := range params {
		switch c {
		case '<', '(', '[', '{':
			depth++
		case '>', ')', ']', '}':
			depth--
		case ',':
			if depth == 0 {
				out = append(out, params[start:i])
				start = i + 1
			}
		}
	}
	return append(out, params[start:])
}

func paramName(p string) string {
	p = strings.TrimSpace(p)
	p = strings.TrimPrefix(strings.TrimPrefix(p, "mut "), "...")
	if i := strings.IndexAny(p, ":=?"); i >= 0 {
		p = p[:i]
	}
	return strings.TrimSpace(p)
}

func erlParam(p string) string {
	p = strings.TrimPrefix(strings.TrimSpace(p), "[")
	p, _, _ = strings.Cut(p, "|")
	return strings.TrimSpace(p)
}

func stripRustTests(data string) string {
	const attr = "#[cfg(test)]"
	var b strings.Builder
	for {
		i := strings.Index(data, attr)
		if i < 0 {
			b.WriteString(data)
			return b.String()
		}
		b.WriteString(data[:i])
		data = data[rustItemEnd(data, i+len(attr)):]
	}
}

func rustSkip(data string, i int) int {
	switch {
	case strings.HasPrefix(data[i:], "//"):
		if j := strings.IndexByte(data[i:], '\n'); j >= 0 {
			return i + j
		}
		return len(data)
	case strings.HasPrefix(data[i:], "/*"):
		if j := strings.Index(data[i+2:], "*/"); j >= 0 {
			return i + 2 + j + 1
		}
		return len(data)
	case data[i] == 'r' && (strings.HasPrefix(data[i+1:], "\"") || strings.HasPrefix(data[i+1:], "#")) && (i == 0 || !isIdent(data[i-1])):
		hashes := 0
		for i+1+hashes < len(data) && data[i+1+hashes] == '#' {
			hashes++
		}
		if i+1+hashes >= len(data) || data[i+1+hashes] != '"' {
			return i
		}
		if j := strings.Index(data[i+2+hashes:], "\""+strings.Repeat("#", hashes)); j >= 0 {
			return i + 2 + hashes + j + hashes
		}
		return len(data)
	case data[i] == '"':
		for j := i + 1; j < len(data); j++ {
			switch data[j] {
			case '\\':
				j++
			case '"':
				return j
			}
		}
		return len(data)
	case data[i] == '\'':
		if i+1 < len(data) && data[i+1] == '\\' {
			if j := strings.IndexByte(data[i+2:], '\''); j >= 0 {
				return i + 2 + j
			}
		}
		_, size := utf8.DecodeRuneInString(data[i+1:])
		if i+1+size < len(data) && data[i+1+size] == '\'' {
			return i + 1 + size
		}
	}
	return i
}

func isIdent(c byte) bool {
	return c == '_' || c >= '0' && c <= '9' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
}

func rustItemEnd(data string, from int) int {
	depth, angle := 0, 0
	for i := from; i < len(data); i++ {
		if j := rustSkip(data, i); j != i {
			i = j
			continue
		}
		switch c := data[i]; {
		case (c == '-' || c == '=') && i+1 < len(data) && data[i+1] == '>':
			i++
		case c == '(' || c == '[':
			depth++
		case c == ')' || c == ']':
			depth--
		case c == '<' && depth == 0:
			angle++
		case c == '>' && depth == 0 && angle > 0:
			angle--
		case c == '{' && depth == 0 && angle == 0:
			return rustBlockEnd(data, i)
		case c == '{':
			depth++
		case c == '}' && depth == 0:
			return i
		case c == '}':
			depth--
		case (c == ';' || c == ',') && depth == 0 && angle == 0:
			return i + 1
		}
	}
	return len(data)
}

func rustBlockEnd(data string, open int) int {
	depth := 0
	for i := open; i < len(data); i++ {
		if j := rustSkip(data, i); j != i {
			i = j
			continue
		}
		switch data[i] {
		case '{':
			depth++
		case '}':
			depth--
			if depth == 0 {
				return i + 1
			}
		}
	}
	return len(data)
}

func envHelpers(files map[string]string) []envHelper {
	type fn struct {
		name   string
		params []string
		body   string
	}
	var fns, erlFns []fn
	for rel, data := range files {
		if erlangFile(rel) {
			for _, m := range erlFunc.FindAllStringSubmatchIndex(data, -1) {
				var names []string
				for _, p := range splitParams(data[m[4]:m[5]]) {
					names = append(names, erlParam(p))
				}
				end := len(data)
				if next := erlForm.FindStringIndex(data[m[1]:]); next != nil {
					end = m[1] + next[0]
				}
				erlFns = append(erlFns, fn{data[m[2]:m[3]], names, data[m[1]:end]})
			}
			continue
		}
		re := tsFunc
		if scriptLang(rel) == "native" {
			re = rsFunc
		}
		for _, m := range re.FindAllStringSubmatchIndex(data, -1) {
			name, params := "", ""
			if m[2] >= 0 {
				name, params = data[m[2]:m[3]], data[m[4]:m[5]]
			}
			if len(m) > 6 && m[6] >= 0 {
				name, params = data[m[6]:m[7]], data[m[8]:m[9]]
			}
			var names []string
			for _, p := range splitParams(params) {
				names = append(names, paramName(p))
			}
			fns = append(fns, fn{name, names, funcBody(data, m[1])})
		}
	}
	known := map[string]bool{}
	var out []envHelper
	add := func(f fn, i int) bool {
		key := f.name + "/" + strconv.Itoa(i)
		if known[key] || envCallee.MatchString(f.name) {
			return false
		}
		known[key] = true
		out = append(out, envHelper{name: f.name, arg: i})
		return true
	}
	for _, f := range fns {
		if !strings.Contains(f.body, "env") && !strings.Contains(f.body, "vars") {
			continue
		}
		for _, m := range paramEnv.FindAllStringSubmatch(f.body, -1) {
			if i := slices.Index(f.params, m[1]+m[2]+m[3]); i >= 0 {
				add(f, i)
			}
		}
	}
	for changed := len(out) > 0; changed; {
		changed = false
		for _, f := range fns {
			for _, c := range callStart.FindAllStringSubmatchIndex(f.body, -1) {
				callee := f.body[c[2]:c[3]]
				if callee == f.name || !envCallee.MatchString(callee) && !slices.ContainsFunc(out, func(h envHelper) bool { return h.name == callee }) {
					continue
				}
				for j, arg := range splitArgs(f.body, c[1]-1) {
					i := slices.Index(f.params, strings.TrimPrefix(arg, "&"))
					if i >= 0 && (envCallee.MatchString(callee) || known[callee+"/"+strconv.Itoa(j)]) && add(f, i) {
						changed = true
					}
				}
			}
		}
	}
	for changed := true; changed; {
		changed = false
		for _, f := range erlFns {
			for _, c := range callStart.FindAllStringSubmatchIndex(f.body, -1) {
				callee := f.body[c[2]:c[3]]
				for j, arg := range splitArgs(f.body, c[1]-1) {
					i := slices.Index(f.params, arg)
					key := f.name + "/" + strconv.Itoa(i)
					if i < 0 || callee == f.name || known[key] || envCallee.MatchString(f.name) ||
						!erlEnvCallee.MatchString(callee) && !known[callee+"/"+strconv.Itoa(j)] {
						continue
					}
					known[key] = true
					out = append(out, envHelper{name: f.name, arg: i, erl: true})
					changed = true
				}
			}
		}
	}
	return out
}

func dockerfileEnvNames(t *testing.T) map[string][]string {
	t.Helper()
	files, err := filepath.Glob(filepath.Join(repoRoot, "fluxer_*", "Dockerfile"))
	if err != nil {
		t.Fatal(err)
	}
	found := map[string][]string{}
	for _, p := range files {
		data, err := os.ReadFile(p)
		if err != nil {
			t.Fatal(err)
		}
		rel, _ := filepath.Rel(repoRoot, p)
		final := string(data)
		if i := strings.LastIndex(final, "\nFROM "); i >= 0 {
			final = final[i:]
		}
		for _, m := range dockerEnv.FindAllStringSubmatch(final, -1) {
			for _, a := range envAssign.FindAllStringSubmatch(m[1], -1) {
				addFound(found, a[1], filepath.ToSlash(rel))
			}
		}
		for _, m := range dockerRun.FindAllStringSubmatch(final, -1) {
			for _, a := range shellEnv.FindAllStringSubmatch(m[1], -1) {
				addFound(found, a[1], filepath.ToSlash(rel))
			}
		}
	}
	return found
}

type composeService struct {
	Image       string         `yaml:"image"`
	Environment map[string]any `yaml:"environment"`
}

func composeEnv(t *testing.T, set *stack.Set) (fluxer, other map[string][]string, feeds map[[2]string][]string) {
	t.Helper()
	fluxer, other, feeds = map[string][]string{}, map[string][]string{}, map[[2]string][]string{}
	names, err := stack.ComposeFileNames(set.Files)
	if err != nil {
		t.Fatal(err)
	}
	images := map[string]string{}
	files := map[string]map[string]composeService{}
	for _, name := range names {
		data, err := fs.ReadFile(set.Files, name)
		if err != nil {
			t.Fatal(err)
		}
		var file struct {
			Services map[string]composeService `yaml:"services"`
		}
		if err := yaml.Unmarshal(data, &file); err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		files[name] = file.Services
		for svc, body := range file.Services {
			if body.Image != "" {
				images[svc] = body.Image
			}
		}
	}
	for _, services := range files {
		for svc, body := range services {
			to := other
			if strings.Contains(images[svc], "/fluxer-") {
				to = fluxer
			}
			for k, value := range body.Environment {
				if !slices.Contains(to[k], svc) {
					to[k] = append(to[k], svc)
				}
				for _, m := range composeRef.FindAllStringSubmatch(fmt.Sprint(value), -1) {
					if from := [2]string{m[1], svc}; m[1] != k && !slices.Contains(feeds[from], k) {
						feeds[from] = append(feeds[from], k)
					}
				}
			}
		}
	}
	return fluxer, other, feeds
}

var composeRef = regexp.MustCompile(`\$\{([A-Z][A-Z0-9_]*)`)

type composeScripts struct {
	Command     any `yaml:"command"`
	Entrypoint  any `yaml:"entrypoint"`
	Healthcheck struct {
		Test any `yaml:"test"`
	} `yaml:"healthcheck"`
}

func composeOwnEnv(t *testing.T, set *stack.Set) (own map[string][]string, scripts map[string][]string) {
	t.Helper()
	own, scripts = map[string][]string{}, map[string][]string{}
	names, err := stack.ComposeFileNames(set.Files)
	if err != nil {
		t.Fatal(err)
	}
	for _, name := range names {
		data, err := fs.ReadFile(set.Files, name)
		if err != nil {
			t.Fatal(err)
		}
		var file struct {
			Services map[string]yaml.Node `yaml:"services"`
		}
		if err := yaml.Unmarshal(data, &file); err != nil {
			t.Fatalf("%s: %v", name, err)
		}
		for svc, node := range file.Services {
			var body composeScripts
			if err := node.Decode(&body); err != nil {
				t.Fatalf("%s: %s: %v", name, svc, err)
			}
			for _, part := range []any{body.Command, body.Entrypoint, body.Healthcheck.Test} {
				if part != nil {
					scripts[svc] = append(scripts[svc], fmt.Sprint(part))
				}
			}
			for i := 0; i+1 < len(node.Content); i += 2 {
				if node.Content[i].Value != "environment" || node.Content[i+1].Kind != yaml.MappingNode {
					continue
				}
				env := node.Content[i+1].Content
				for j := 0; j+1 < len(env); j += 2 {
					if k := env[j].Value; k != "<<" && !slices.Contains(own[k], svc) {
						own[k] = append(own[k], svc)
					}
				}
			}
		}
	}
	return own, scripts
}

var (
	emptyClause  = regexp.MustCompile(`\b(?:it's|both are|[Aa]n) empty\b`)
	suchAs       = regexp.MustCompile("such as [^,.]*")
	backticked   = regexp.MustCompile("`([^`]+)`")
	anyEmpty     = regexp.MustCompile(`(?:^|[^-\w])empty\b`)
	otherEmpty   = regexp.MustCompile("(?:\\b[A-Z][A-Z0-9_]+`?|\\bthat) is empty\\b")
	overridePath = regexp.MustCompile(`(?m)^\s*(FLUXER_[A-Z0-9_]*[A-Z0-9]): \{path: \[([^\]]*)\]`)
	defaultLine  = regexp.MustCompile(`^(\t*)(\w+): (.*?),?$`)
	rustRead     = regexp.MustCompile(`"([A-Z][A-Z0-9_]*[A-Z0-9])"`)
	rustFallback = regexp.MustCompile(`\.unwrap_or(?:_else)?\((?:\|\|\s*)?("[^"]*"|-?[\d_.]+|true|false|[A-Z][A-Z0-9_]*)[),.]`)
	rustArgument = regexp.MustCompile(`^\)\s*,\s*("[^"]*"|-?[\d_.]+|true|false|[A-Z][A-Z0-9_]*)\s*,`)
	rustConst    = regexp.MustCompile(`(?m)^\s*(?:pub(?:\([a-z]+\))? )?const ([A-Z][A-Z0-9_]*): [^=]+= ([^;]+);`)
	codeRange    = regexp.MustCompile(`\bfrom (\d+(?:\.\d+)?(?: [KMG]iB)?) to (\d+(?:\.\d+)?(?: [KMG]iB)?)\b`)
	codeBound    = regexp.MustCompile("\\b(?:at least|at most|capped at) `(\\d+)`|\\bValues (?:under|over) (\\d+)(?: [a-z]+)? count as (\\d+)\\b")
	digitGroups  = regexp.MustCompile(`(\d)_(\d)`)
	products     = regexp.MustCompile(`\b\d+(?:\s*\*\s*\d+)+\b`)
)

var configOverrides = "packages/config/src/config_loader/EnvironmentOverrides.ts"

var configDefaults = []string{"packages/config/src/ConfigLoader.ts", "packages/config/src/EndpointDerivation.ts"}

var envDefaultFiles = map[string][]string{
	"FLUXER_GATEWAY_ZSTD_WINDOW_LOG":                 {"fluxer_gateway/src/gateway/gateway_compress.erl"},
	"FLUXER_MEDIA_PROXY_MAX_ENCODE_FRAMES":           {"fluxer_media_proxy/src/constants.rs"},
	"DISCOVERY_REFRESH_INTERVAL_MS":                  {"fluxer_app_proxy/src/branding_images.rs"},
	"FLUXER_EMAIL_SMTP_PORT":                         {"fluxer_api/src/api/instance/EmailDelivery.ts"},
	"FLUXER_GATEWAY_ENDPOINT":                        {"packages/config/src/EndpointDerivation.ts"},
	"FLUXER_CLAMAV_HOST":                             {"packages/config/src/ConfigLoader.ts"},
	"FLUXER_CLAMAV_PORT":                             {"packages/config/src/ConfigLoader.ts"},
	"FLUXER_MEDIA_PROXY_UPLOAD_RELAY_TOKEN_TTL_SECS": {"packages/config/src/ConfigLoader.ts"},
	"FLUXER_CACHE_PURGE_HTTP_TIMEOUT_MS":             {"packages/config/src/ConfigLoader.ts"},
	"FLUXER_CACHE_PURGE_CONTROLLER_TIMEOUT_MS":       {"packages/config/src/ConfigLoader.ts"},
	"FLUXER_DISCOVERY_MIN_MEMBER_COUNT":              {"packages/config/src/ConfigLoader.ts"},
	"FLUXER_DELETION_GRACE_PERIOD_HOURS":             {"packages/config/src/ConfigLoader.ts"},
	"FLUXER_S3_READ_BUCKET_STYLE":                    {"fluxer_media_proxy/src/config/parse.rs"},
	"FLUXER_GATEWAY_API_RPC_ENDPOINT":                {"fluxer_gateway/src/gateway/rpc_client.erl"},
	"RUST_LOG":                                       {"fluxer_admin/src/main.rs", "fluxer_app_proxy/src/main.rs", "fluxer_media_proxy/src/main.rs", "fluxer_svc/src/lib.rs"},
}

var (
	namedAliases = regexp.MustCompile(`(?s)NAMED_FLUXER_ENV_ALIASES\b[^=]*=\s*\{(.*?)\n\}`)
	aliasPair    = regexp.MustCompile(`\b(FLUXER_[A-Z0-9_]*[A-Z0-9])\s*:\s*'(FLUXER_[A-Z0-9_]*[A-Z0-9])'`)
)

func configAliases(t *testing.T, overrides string) map[string]string {
	t.Helper()
	block := namedAliases.FindStringSubmatch(overrides)
	if block == nil {
		t.Fatalf("%s has no NAMED_FLUXER_ENV_ALIASES", configOverrides)
	}
	out := map[string]string{}
	for _, m := range aliasPair.FindAllStringSubmatch(block[1], -1) {
		out[m[2]] = m[1]
	}
	if len(out) == 0 {
		t.Fatalf("NAMED_FLUXER_ENV_ALIASES in %s has no entries", configOverrides)
	}
	return out
}

func helpSentences(help string) []string {
	var sentences []string
	start, quoted := 0, false
	for i, r := range help {
		switch {
		case r == '`':
			quoted = !quoted
		case r == '.' && !quoted && (i+1 == len(help) || help[i+1] == ' '):
			sentences = append(sentences, help[start:i])
			start = i + 1
		}
	}
	return append(sentences, help[start:])
}

func emptyDefaults(help string) []string {
	var out []string
	for _, sentence := range helpSentences(help) {
		if !emptyClause.MatchString(sentence) {
			continue
		}
		for _, m := range backticked.FindAllStringSubmatch(suchAs.ReplaceAllString(sentence, ""), -1) {
			if !slices.Contains(out, m[1]) {
				out = append(out, m[1])
			}
		}
	}
	return out
}

func literal(v string) string {
	v = strings.TrimSpace(digitGroups.ReplaceAllString(v, "$1$2"))
	if len(v) >= 2 && (v[0] == '\'' || v[0] == '"') && v[len(v)-1] == v[0] {
		return v[1 : len(v)-1]
	}
	if _, err := strconv.ParseFloat(v, 64); err == nil || v == "true" || v == "false" {
		return v
	}
	return ""
}

func configDefaultValues(t *testing.T, overrides string) map[string]string {
	t.Helper()
	data, err := os.ReadFile(filepath.Join(repoRoot, configDefaults[0]))
	if err != nil {
		t.Fatal(err)
	}
	_, body, ok := strings.Cut(string(data), "function defaultConfig(): MasterConfig {\n\treturn {\n")
	body, _, _ = strings.Cut(body, "\n\t};\n}")
	if !ok || body == "" {
		t.Fatalf("%s has no defaultConfig()", configDefaults[0])
	}
	byPath := map[string]string{}
	var keys []string
	for _, line := range strings.Split(body, "\n") {
		m := defaultLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		depth := len(m[1]) - 2
		if depth < 0 || depth > len(keys) {
			t.Fatalf("defaultConfig() in %s has an unexpected indent at %q", configDefaults[0], line)
		}
		keys = append(keys[:depth], m[2])
		byPath[strings.Join(keys, ".")] = literal(m[3])
	}
	out := map[string]string{}
	for _, m := range overridePath.FindAllStringSubmatch(overrides, -1) {
		var path []string
		for _, part := range strings.Split(m[2], ",") {
			path = append(path, strings.Trim(strings.TrimSpace(part), "'"))
		}
		if v, ok := byPath[strings.Join(path, ".")]; ok && v != "" {
			out[m[1]] = v
		}
	}
	if len(out) == 0 {
		t.Fatalf("found no defaults for NAMED_FLUXER_ENV_OVERRIDES in %s", configDefaults[0])
	}
	return out
}

func rustDefaultValues(name string, rels []string, sources map[string]string) []string {
	var out []string
	for _, rel := range rels {
		if !strings.HasSuffix(rel, ".rs") {
			continue
		}
		data := sources[rel]
		consts := map[string]string{}
		for _, m := range rustConst.FindAllStringSubmatch(data, -1) {
			consts[m[1]] = m[2]
		}
		for _, loc := range rustRead.FindAllStringSubmatchIndex(data, -1) {
			if data[loc[2]:loc[3]] != name {
				continue
			}
			rest := data[loc[1]:]
			if next := rustRead.FindStringIndex(rest); next != nil {
				rest = rest[:next[0]]
			}
			rest, _, _ = strings.Cut(rest, ";")
			depth := 0
			for i, r := range rest {
				if r == '(' {
					depth++
				} else if r == ')' {
					depth--
				}
				if depth < -1 {
					rest = rest[:i]
					break
				}
			}
			m := rustArgument.FindStringSubmatch(rest)
			if m == nil {
				m = rustFallback.FindStringSubmatch(rest)
			}
			if m != nil {
				raw := m[1]
				if c, ok := consts[raw]; ok {
					raw = c
				}
				if v := literal(raw); v != "" && !slices.Contains(out, v) {
					out = append(out, v)
				}
			}
		}
	}
	return out
}

func codeDefaultValues(name string, rels []string, sources map[string]string) []string {
	out := rustDefaultValues(name, rels, sources)
	q := regexp.QuoteMeta(name)
	reads := map[string]*regexp.Regexp{
		".erl": regexp.MustCompile(`\b(?:env_[a-z_]+|os:getenv)\(\s*"` + q + `"\s*,\s*(<<"[^"]*">>|<<>>|"[^"]*"|-?[\d.]+|true|false)\s*\)`),
		".ts":  regexp.MustCompile(`(?:\(\s*["'` + "`" + `]` + q + `["'` + "`" + `]\s*\)|\benv\??\.` + q + `\b)(?:\??\.[A-Za-z]+\(\))*\s*(?:\?\?|\|\|)\s*('[^']*'|"[^"]*"|-?[\d.]+|true|false)`),
		".sh":  regexp.MustCompile(`\|\|\s*` + q + `=([^\s;]+)|\$\{` + q + `:[-=]([^}]+)\}`),
	}
	for _, rel := range rels {
		ext := path.Ext(rel)
		if ext == ".hrl" {
			ext = ".erl"
		} else if slices.Contains([]string{".mts", ".tsx", ".mjs", ".js", ".cjs"}, ext) {
			ext = ".ts"
		}
		re := reads[ext]
		if re == nil {
			continue
		}
		for _, m := range re.FindAllStringSubmatch(sources[rel], -1) {
			raw := strings.TrimSuffix(strings.TrimPrefix(m[1], "<<"), ">>")
			v := literal(raw)
			if ext == ".sh" {
				v = strings.Trim(m[1]+m[2], `"'`)
			}
			if v != "" && !slices.Contains(out, v) {
				out = append(out, v)
			}
		}
	}
	return out
}

func clampBounds(name string, rels []string, sources map[string]string) []string {
	re := regexp.MustCompile(`(?m)^\s*` + regexp.QuoteMeta(name) + `="\$\(clamp_int\s+"[^"]*"\s+("?[^"\s)]+"?)\s+("?[^"\s)]+"?)\)"`)
	var out []string
	for _, rel := range rels {
		if !strings.HasSuffix(rel, ".sh") {
			continue
		}
		for _, m := range re.FindAllStringSubmatch(sources[rel], -1) {
			for _, b := range m[1:] {
				if _, err := strconv.Atoi(b); err == nil {
					out = append(out, b)
				}
			}
		}
	}
	return out
}

func rangeValue(v string) string {
	n, unit, ok := strings.Cut(v, " ")
	if !ok {
		return v
	}
	x, _ := strconv.ParseInt(n, 10, 64)
	return strconv.FormatInt(x<<map[string]int{"KiB": 10, "MiB": 20, "GiB": 30}[unit], 10)
}

func TestEnvInventory(t *testing.T) {
	var err error
	set := stack.Embedded()
	reads, all := codeEnvNames(t)
	code := runtimeReads(reads)
	fluxer, other, feeds := composeEnv(t, set)
	docker := dockerfileEnvNames(t)
	helm := helmEnvNames(t)
	passed := func(name string) bool { return fluxer[name] != nil || other[name] != nil }
	registry := map[string]envVar{}
	sections := map[string]bool{}
	for _, s := range slices.Concat(envSections, envInternalGroups) {
		sections[s.id] = true
	}
	for _, v := range envVars {
		if _, dup := registry[v.name]; dup {
			t.Errorf("%s is listed twice", v.name)
		}
		registry[v.name] = v
		switch {
		case !sections[v.section]:
			t.Errorf("%s has an unknown section %q", v.name, v.section)
		case v.section == envStackSection:
			if slices.Contains(set.Vars, v.name) || !passed(v.name) {
				t.Errorf("%s is listed as set by the compose stack, but compose doesn't set it for any service, or it's a .env setting", v.name)
			}
		case v.section == envRexctlSection:
			if passed(v.name) {
				t.Errorf("%s is listed in the rexctl section, but compose passes it", v.name)
			}
		case envTopSection(v.section) == envInternalSection:
			used := code[v.name] != nil || slices.Contains(libraryEnvVars, v.name)
			if !strings.HasPrefix(v.name, "FLUXER_") || slices.Contains(chartInputVars, v.name) {
				used = used || docker[v.name] != nil || helm[v.name] != nil
			}
			if !used || passed(v.name) {
				t.Errorf("%s is listed as not passed by the compose stack, but no code reads it, or compose passes it", v.name)
			}
		case !slices.Contains(set.Vars, v.name) && !strings.HasPrefix(v.name, "COMPOSE_"):
			t.Errorf("%s is listed as a .env setting, but no compose file uses it", v.name)
		}
	}

	for _, v := range set.Vars {
		if _, ok := registry[v]; !ok {
			t.Errorf("%s is used by a compose file but has no entry in envVars", v)
		}
	}
	for _, k := range slices.Sorted(maps.Keys(fluxer)) {
		if _, ok := registry[k]; !ok {
			t.Errorf("compose sets %s for %v but it has no entry in envVars", k, fluxer[k])
		}
	}
	for _, k := range slices.Sorted(maps.Keys(other)) {
		if _, ok := registry[k]; !ok {
			t.Errorf("compose sets %s for %v but it has no entry in envVars", k, other[k])
		}
	}
	for _, name := range slices.Sorted(maps.Keys(all)) {
		if _, ok := registry[name]; !ok {
			t.Errorf("%s is looked up in %v but has no entry in envVars", name, all[name])
		}
	}

	runtime := slices.Concat(libraryEnvVars, runtimeEnvVars, rexctlProxyVars)
	for _, k := range slices.Sorted(maps.Keys(fluxer)) {
		if code[k] == nil && !slices.Contains(runtime, strings.ToUpper(k)) {
			t.Errorf("compose passes %s to %v but no Fluxer code looks it up", k, fluxer[k])
		}
	}
	serviceReads := func(k, svc string) bool {
		var prefixes []string
		for prefix, services := range codeServices {
			if slices.Contains(services, svc) {
				prefixes = append(prefixes, prefix)
			}
		}
		return slices.ContainsFunc(code[k], func(rel string) bool {
			return slices.ContainsFunc(prefixes, func(p string) bool { return strings.HasPrefix(rel, p) })
		}) || slices.ContainsFunc(docker[k], func(f string) bool {
			return slices.ContainsFunc(prefixes, func(p string) bool { return f == p+"Dockerfile" })
		})
	}
	own, scripts := composeOwnEnv(t, set)
	for _, k := range slices.Sorted(maps.Keys(own)) {
		if slices.Contains(runtime, strings.ToUpper(k)) {
			continue
		}
		for _, svc := range own[k] {
			if slices.Contains(fluxer[k], svc) && !serviceReads(k, svc) {
				t.Errorf("compose passes %s to %s, but no code that service runs looks it up", k, svc)
			}
		}
	}
	services := map[string]bool{}
	for _, list := range codeServices {
		for _, svc := range list {
			services[svc] = true
		}
	}
	for _, v := range envVars {
		for _, m := range serviceList.FindAllStringSubmatch(v.help, -1) {
			for _, svc := range strings.FieldsFunc(strings.ReplaceAll(m[1], " and ", ","), func(r rune) bool { return r == ',' || r == ' ' }) {
				if services[svc] && !serviceReads(v.name, svc) && !slices.ContainsFunc(feeds[[2]string{v.name, svc}], func(k string) bool { return serviceReads(k, svc) }) && !slices.Contains(runtime, v.name) {
					t.Errorf("the entry for %s says the %s service uses it, but no code that service runs looks it up", v.name, svc)
				}
			}
		}
	}
	for _, svc := range slices.Sorted(maps.Keys(scripts)) {
		seen := map[string]bool{}
		for _, text := range scripts[svc] {
			for _, m := range composeSh.FindAllStringSubmatch(text, -1) {
				if seen[m[1]] {
					continue
				}
				seen[m[1]] = true
				if _, ok := registry[m[1]]; !ok {
					t.Errorf("the %s service's command uses %s but it has no entry in envVars", svc, m[1])
				}
				if !slices.Contains(other[m[1]], svc) && !slices.Contains(fluxer[m[1]], svc) {
					t.Errorf("the %s service's command uses %s but compose doesn't set it for that service", svc, m[1])
				}
			}
		}
	}
	shell := map[string]bool{}
	for _, name := range []string{stack.ComposeFile, stack.Caddyfile} {
		data, err := fs.ReadFile(set.Files, name)
		if err != nil {
			t.Fatal(err)
		}
		for _, re := range []*regexp.Regexp{caddyEnv, composeSh} {
			for _, m := range re.FindAllStringSubmatch(string(data), -1) {
				shell[m[1]] = true
			}
		}
	}
	for _, k := range slices.Sorted(maps.Keys(other)) {
		if strings.HasPrefix(k, "FLUXER_") && !shell[k] {
			t.Errorf("compose passes %s to %v but nothing there uses it", k, other[k])
		}
	}

	caddy := map[string][]byte{}
	if caddy[stack.Caddyfile], err = fs.ReadFile(set.Files, stack.Caddyfile); err != nil {
		t.Fatal(err)
	}
	for name := range caddyfiles {
		if caddy[name], err = os.ReadFile(filepath.Join(repoRoot, name)); err != nil {
			t.Fatal(err)
		}
	}
	for _, name := range slices.Sorted(maps.Keys(caddy)) {
		svc := caddyfiles[name]
		if svc == "" {
			svc = "edge"
		}
		for _, m := range caddyEnv.FindAllStringSubmatch(string(caddy[name]), -1) {
			if _, ok := registry[m[1]]; !ok {
				t.Errorf("%s uses %s but it has no entry in envVars", name, m[1])
			}
			if !slices.Contains(other[m[1]], svc) && !slices.Contains(fluxer[m[1]], svc) {
				t.Errorf("%s uses %s but compose doesn't set it for the %s service", name, m[1], svc)
			}
		}
	}

	for _, name := range slices.Sorted(maps.Keys(docker)) {
		if _, ok := registry[name]; !ok && !slices.Contains(dockerOSVars, name) {
			t.Errorf("%v sets %s but it has no entry in envVars", docker[name], name)
		}
	}

	for _, k := range settings.Named() {
		for _, v := range k.Vars {
			if _, ok := registry[v]; !ok || !slices.Contains(set.Vars, v) {
				t.Errorf("the %s setting writes %s, but compose doesn't use it or it has no entry in envVars", k.Name, v)
			}
		}
	}
	written := slices.Concat(stack.SizingKeys(), stack.ProfileKeys(set), settings.DerivedVars(), slices.Sorted(maps.Keys(settings.Twins)),
		slices.Sorted(maps.Keys(stack.FoldWorker(stack.Hardware{}, nil, map[string]string{stack.EmbeddedWorkerVar: "true"}))))
	for _, v := range written {
		if _, ok := registry[v]; !ok || !slices.Contains(set.Vars, v) {
			t.Errorf("rexctl writes %s, but compose doesn't use it or it has no entry in envVars", v)
		}
	}

	values := composeValues(set)
	literals := composeLiterals(set)
	for _, v := range envVars {
		if v.section != envStackSection {
			continue
		}
		if lit := literals[v.name]; lit != "" && strings.Contains(v.help, lit) {
			t.Errorf("%s repeats the compose value %q in its description", v.name, lit)
		}
		if _, from, ok := strings.Cut(v.help, " from "); ok {
			for _, ref := range envNamePattern.FindAllString(from, -1) {
				if !slices.ContainsFunc(values[v.name], func(s string) bool { return strings.Contains(s, ref) }) {
					t.Errorf("%s says compose sets it from %s, but its compose value is %v", v.name, ref, values[v.name])
				}
			}
		}
	}

	sources := map[string]string{}
	byRel := map[string]bool{}
	walkCode(t, codeRoots, func(rel, lang, data string) {
		data = digitGroups.ReplaceAllString(data, "$1$2")
		for _, m := range products.FindAllString(data, -1) {
			n := 1
			for _, f := range strings.Split(m, "*") {
				x, _ := strconv.Atoi(strings.TrimSpace(f))
				n *= x
			}
			data += " " + strconv.Itoa(n)
		}
		sources[rel] = data
		byRel[rel] = true
	})
	overrides, err := os.ReadFile(filepath.Join(repoRoot, configOverrides))
	if err != nil {
		t.Fatal(err)
	}
	loaderDefaults := configDefaultValues(t, string(overrides))
	for _, v := range envVars {
		if code[v.name] == nil {
			continue
		}
		defaults := emptyDefaults(v.help)
		inCompose := func(w string) bool {
			token := regexp.MustCompile(`(?:^|[^\w.])` + regexp.QuoteMeta(w) + `(?:$|[^\w.]|\.(?:\D|$))`)
			return slices.ContainsFunc(values[v.name], func(s string) bool { return token.MatchString(s) })
		}
		if len(defaults) > 0 && !slices.ContainsFunc(defaults, inCompose) {
			exact := codeDefaultValues(v.name, code[v.name], sources)
			if lit := loaderDefaults[v.name]; lit != "" && slices.Contains(code[v.name], configOverrides) {
				exact = append(exact, lit)
			}
			for _, lit := range exact {
				if !slices.Contains(defaults, lit) {
					t.Errorf("%s says an empty value gives %v, but the code falls back to %q", v.name, defaults, lit)
				}
			}
		}
		for _, b := range clampBounds(v.name, code[v.name], sources) {
			if !regexp.MustCompile(`\b` + b + `\b`).MatchString(v.help) {
				t.Errorf("%s doesn't mention %s, a bound the code keeps an empty value within", v.name, b)
			}
		}
		want := slices.Clone(defaults)
		for _, m := range codeRange.FindAllStringSubmatch(v.help, -1) {
			want = append(want, rangeValue(m[1]), rangeValue(m[2]))
		}
		for _, m := range codeBound.FindAllStringSubmatch(v.help, -1) {
			for _, n := range m[1:] {
				if n != "" && !slices.Contains(want, n) {
					want = append(want, n)
				}
			}
		}
		files := slices.Concat(code[v.name], envDefaultFiles[v.name])
		if envDefaultFiles[v.name] == nil {
			if slices.Contains(code[v.name], configOverrides) {
				files = append(files, configDefaults...)
			}
			for _, rel := range code[v.name] {
				for _, m := range tsImport.FindAllStringSubmatch(sources[rel], -1) {
					if imp := resolveImport(rel, m[1], byRel); imp != "" {
						files = append(files, imp)
					}
				}
			}
		}
		for _, w := range want {
			token := regexp.MustCompile(`(?:^|[^\w.])` + regexp.QuoteMeta(w) + `(?:$|[^\w.]|\.(?:\D|$))`)
			inCode := slices.ContainsFunc(files, func(rel string) bool { return token.MatchString(sources[rel]) })
			if !inCode && !inCompose(w) {
				t.Errorf("%s says %q, but neither compose nor %v has it", v.name, w, slices.Concat(code[v.name], envDefaultFiles[v.name]))
			}
		}
	}

	composeFiles := []string{stack.ComposeFile}
	for _, o := range envOverlays {
		composeFiles = append(composeFiles, stack.OverlayFiles[o.overlay])
	}
	for i, file := range composeFiles {
		data, err := stack.ReadFile(set, file)
		if err != nil {
			t.Fatal(err)
		}
		defs := map[string][]string{}
		scanDefaults(string(data), defs, map[string]bool{})
		for _, k := range slices.Sorted(maps.Keys(defs)) {
			var lits []string
			empty := false
			for _, d := range defs[k] {
				if d == "" {
					empty = true
				} else if !strings.Contains(d, "$") && !slices.Contains(lits, d) {
					lits = append(lits, d)
				}
			}
			if len(lits) > 1 {
				t.Errorf("%s gives %s different defaults %v", file, k, lits)
				continue
			}
			if i > 0 || len(lits) == 0 {
				continue
			}
			if d := emptyDefaults(registry[k].help); len(d) > 0 && !slices.Contains(d, lits[0]) && !strings.Contains(registry[k].help, "Outside Docker Compose") {
				t.Errorf("%s says an empty value gives %v, but compose falls back to %q", k, d, lits[0])
			}
			if !empty {
				continue
			}
			exact := codeDefaultValues(k, code[k], sources)
			if lit := loaderDefaults[k]; lit != "" && slices.Contains(code[k], configOverrides) {
				exact = append(exact, lit)
			}
			slices.Sort(exact)
			for _, lit := range slices.Compact(exact) {
				if lit != lits[0] {
					t.Errorf("compose gives %s the default %q for one service and an empty value for another, where the code falls back to %q", k, lits[0], lit)
				}
			}
		}
	}

	aliases := configAliases(t, string(overrides))
	for _, alias := range slices.Sorted(maps.Keys(aliases)) {
		if v, ok := registry[alias]; !ok || !strings.Contains(v.help, aliases[alias]) {
			t.Errorf("the config loader reads %s when %s is empty, but the entry for %s doesn't mention %s", alias, aliases[alias], alias, aliases[alias])
		}
	}
	for _, m := range keyEnv.FindAllStringSubmatch(string(overrides), -1) {
		if envTopSection(registry[m[1]].section) != envInternalSection && !slices.Contains(fluxer[m[1]], "api") && !slices.Contains(workerOnlyVars, m[1]) {
			t.Errorf("the config loader looks up %s, but compose doesn't pass it to the api", m[1])
		}
	}
	for _, name := range workerOnlyVars {
		if slices.Contains(fluxer[name], "api") || !slices.Contains(fluxer[name], "worker") {
			t.Errorf("%s is listed as passed to the worker only, but compose passes it to %v", name, fluxer[name])
		}
	}
	for _, name := range slices.Sorted(maps.Keys(code)) {
		if envTopSection(registry[name].section) == envInternalSection {
			continue
		}
		for _, rel := range code[name] {
			if rel == configOverrides && aliases[name] != "" {
				continue
			}
			var want []string
			image := ""
			for prefix, services := range codeServices {
				if strings.HasPrefix(rel, prefix) {
					want, image = services, prefix+"Dockerfile"
				}
			}
			if len(want) > 0 && !slices.Contains(docker[name], image) && !slices.ContainsFunc(want, func(svc string) bool {
				return slices.Contains(fluxer[name], svc) || slices.Contains(other[name], svc)
			}) {
				t.Errorf("%s looks up %s, but compose passes it to none of %v", rel, name, want)
			}
		}
	}
	for _, name := range slices.Sorted(maps.Keys(helm)) {
		if _, ok := registry[name]; !ok {
			t.Errorf("%v sets %s but it has no entry in envVars", helm[name], name)
		}
	}

	for _, name := range envSecretVars {
		if v, ok := registry[name]; !ok || slices.Contains(envUnbundledSections, envTopSection(v.section)) {
			t.Errorf("%s is listed as a secret but isn't a .env setting", name)
		}
	}
	for _, s := range secrets.Required {
		if s.Kind != secrets.KindFixed && s.Key != secrets.VAPIDPublicKey && !slices.Contains(envSecretVars, s.Key) {
			t.Errorf("%s is a generated secret but isn't in envSecretVars", s.Key)
		}
	}
}

var serviceList = regexp.MustCompile(`\b((?:[a-z][a-z-]*, )*[a-z][a-z-]* and [a-z][a-z-]*) services\b`)

var workerOnlyVars = []string{"FLUXER_API_WORKER_ENABLE_CRON_SCHEDULER", "FLUXER_API_WORKER_MODE"}

var helmPodEnv = regexp.MustCompile(`(?m)^\s*-\s*name:\s*"?([A-Z][A-Z0-9_]*)"?\s*$|"name"\s+"([A-Z][A-Z0-9_]*)"|\bset \$c "([A-Z][A-Z0-9_]*)"`)

func helmEnvNames(t *testing.T) map[string][]string {
	t.Helper()
	found := map[string][]string{}
	root := filepath.Join(repoRoot, helmChart)
	err := filepath.WalkDir(filepath.Join(root, "templates"), func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "tests" {
				return filepath.SkipDir
			}
			return nil
		}
		data, err := os.ReadFile(p)
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(repoRoot, p)
		for _, m := range helmPodEnv.FindAllStringSubmatch(string(data), -1) {
			addFound(found, m[1]+m[2]+m[3], filepath.ToSlash(rel))
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	data, err := os.ReadFile(filepath.Join(root, "values.yaml"))
	if err != nil {
		t.Fatal(err)
	}
	var values struct {
		Secrets struct {
			Values map[string]any `yaml:"values"`
		} `yaml:"secrets"`
	}
	if err := yaml.Unmarshal(data, &values); err != nil {
		t.Fatal(err)
	}
	if len(values.Secrets.Values) == 0 {
		t.Fatal("the Helm chart values have no secrets.values keys")
	}
	for _, name := range slices.Sorted(maps.Keys(values.Secrets.Values)) {
		addFound(found, name, helmChart+"/values.yaml")
	}
	return found
}

func TestEnvDefaults(t *testing.T) {
	docs := envDocs()
	if d := docs.get("FLUXER_LIVEKIT_DEFAULT_REGION"); len(d.defaults) != 1 || !json.Valid([]byte(d.defaults[0].value)) {
		t.Errorf("the FLUXER_LIVEKIT_DEFAULT_REGION default isn't one JSON value, got %v", d.defaults)
	}
	for _, name := range []string{"FLUXER_API_NODE_HEAP_MB", "FLUXER_WORKER_NODE_HEAP_MB"} {
		if d := docs.get(name); len(d.defaults) != 0 {
			t.Errorf("%s has the default %v, which belongs to another flag", name, d.defaults)
		}
	}
}

func TestEnvFileForms(t *testing.T) {
	direct := map[string]bool{}
	shellNames := map[string]bool{}
	walkCode(t, codeRoots, func(rel, lang, data string) {
		if strings.HasPrefix(rel, "fluxer_docs/") {
			return
		}
		switch lang {
		case "ts", "native", "c":
			for _, m := range directEnv.FindAllStringSubmatch(data, -1) {
				direct[m[1]+m[2]+m[3]+m[4]] = true
			}
		case "shell":
			for _, m := range shellEnv.FindAllStringSubmatch(data, -1) {
				shellNames[m[1]] = true
			}
		}
	})
	for name := range shellNames {
		if !strings.HasSuffix(name, "_FILE") && !shellNames[name+"_FILE"] {
			direct[name] = true
		}
	}
	got := slices.Sorted(maps.Keys(direct))
	want := slices.Sorted(slices.Values(envNoFileVars))
	if !slices.Equal(got, want) {
		t.Errorf("Fluxer code looks up these variables without the _FILE form\n got %v\nwant %v (envNoFileVars)", got, want)
	}
}

func TestRexctlEnv(t *testing.T) {
	found := map[string][]string{}
	local := map[string]map[string][]string{}
	global := map[string][]string{}
	type pending struct{ rel, ident string }
	var idents []pending
	walkCode(t, rexctlRoots, func(rel, lang, data string) {
		switch lang {
		case "go":
			dir := path.Dir(rel)
			if local[dir] == nil {
				local[dir] = map[string][]string{}
			}
			for _, m := range goConst.FindAllStringSubmatch(data, -1) {
				local[dir][m[1]] = append(local[dir][m[1]], m[2])
				global[m[1]] = append(global[m[1]], m[2])
			}
			for _, m := range goIdentEnv.FindAllStringSubmatch(data, -1) {
				idents = append(idents, pending{rel, m[1]})
			}
			if strings.Contains(data, "os.Environ()") || strings.Contains(data, "environ []string") {
				for _, m := range goEnviron.FindAllStringSubmatch(data, -1) {
					addFound(found, m[1]+m[2], rel)
				}
			}
			for _, m := range goEnv.FindAllStringSubmatch(data, -1) {
				addFound(found, m[1]+m[3], rel)
				for _, q := range regexp.MustCompile(`"([A-Za-z_][A-Za-z0-9_]*)"`).FindAllStringSubmatch(m[2], -1) {
					addFound(found, q[1], rel)
				}
			}
		case "shell":
			for _, m := range shellEnv.FindAllStringSubmatch(data, -1) {
				addFound(found, m[1], rel)
			}
		case "ps":
			for _, m := range psEnv.FindAllStringSubmatch(data, -1) {
				addFound(found, m[1], rel)
			}
		}
	})
	for _, p := range idents {
		values, ok := local[path.Dir(p.rel)][p.ident]
		if !ok && p.ident[0] >= 'A' && p.ident[0] <= 'Z' {
			values = global[p.ident]
		}
		for _, v := range values {
			addFound(found, v, p.rel)
		}
	}
	registry := map[string]envVar{}
	for _, v := range envVars {
		registry[v.name] = v
	}
	for _, name := range rexctlLibraryVars {
		addFound(found, name, "the Docker CLI library")
	}
	network := slices.IndexFunc(envSections, func(s envSection) bool { return s.id == envNetworkSection })
	for _, name := range rexctlProxyVars {
		if registry[name].section != envNetworkSection || network < 0 || !strings.Contains(envSections[network].intro, name) {
			t.Errorf("rexctl and install.sh take %s from the shell, but the outgoing proxy section doesn't say so", name)
		}
	}
	for _, name := range slices.Sorted(maps.Keys(found)) {
		if s := registry[name].section; s != envRexctlSection && s != envComposeSection && !slices.Contains(rexctlOSVars, name) {
			t.Errorf("%v looks up %s but envVars doesn't list it in the rexctl or compose section", found[name], name)
		}
	}
	for _, v := range envVars {
		if v.section == envRexctlSection && found[v.name] == nil {
			t.Errorf("%s is listed in the rexctl section but neither rexctl nor its install scripts look it up", v.name)
		}
	}
}

var unbundledImages = []string{"fluxer_docs"}

func TestEnvCodeRoots(t *testing.T) {
	dockerfiles, err := filepath.Glob(filepath.Join(repoRoot, "fluxer_*", "Dockerfile"))
	if err != nil {
		t.Fatal(err)
	}
	for _, p := range dockerfiles {
		dir := filepath.Base(filepath.Dir(p))
		if !slices.ContainsFunc(codeRoots, func(root string) bool { return strings.HasPrefix(root, dir+"/") || root == dir }) {
			t.Errorf("%s has a Dockerfile but no directory of it is in codeRoots", dir)
		}
		if _, ok := codeServices[dir+"/"]; !ok && !slices.Contains(unbundledImages, dir) {
			t.Errorf("%s has a Dockerfile but codeServices doesn't list its compose services", dir)
		}
	}
	fluxer, _, _ := composeEnv(t, stack.Embedded())
	services := map[string]bool{}
	for _, svcs := range fluxer {
		for _, svc := range svcs {
			services[svc] = true
		}
	}
	for _, svc := range slices.Sorted(maps.Keys(services)) {
		if !slices.ContainsFunc(slices.Collect(maps.Values(codeServices)), func(list []string) bool { return slices.Contains(list, svc) }) {
			t.Errorf("compose runs %s from a Fluxer image, but no codeServices entry lists it", svc)
		}
	}
}

func TestEnvWorkflowPaths(t *testing.T) {
	data, err := os.ReadFile(filepath.Join(repoRoot, ".github/workflows/env-reference.yaml"))
	if err != nil {
		t.Fatal(err)
	}
	var wf struct {
		On struct {
			PullRequest struct {
				Paths []string `yaml:"paths"`
			} `yaml:"pull_request"`
			Push struct {
				Paths []string `yaml:"paths"`
			} `yaml:"push"`
		} `yaml:"on"`
	}
	if err := yaml.Unmarshal(data, &wf); err != nil {
		t.Fatal(err)
	}
	if !slices.Equal(wf.On.PullRequest.Paths, wf.On.Push.Paths) {
		t.Errorf("the pull_request and push paths of env-reference.yaml differ")
	}
	dockerfiles, err := filepath.Glob(filepath.Join(repoRoot, "fluxer_*", "Dockerfile"))
	if err != nil {
		t.Fatal(err)
	}
	inputs := slices.Concat(codeRoots, rexctlRoots, slices.Sorted(maps.Keys(caddyfiles)), []string{configOverrides, docsContent, helmChart})
	for _, p := range dockerfiles {
		rel, _ := filepath.Rel(repoRoot, p)
		inputs = append(inputs, filepath.ToSlash(rel))
	}
	for _, in := range inputs {
		if !slices.ContainsFunc(wf.On.PullRequest.Paths, func(glob string) bool {
			if dir, ok := strings.CutSuffix(glob, "**"); ok {
				return strings.HasPrefix(in+"/", dir)
			}
			ok, _ := path.Match(glob, in)
			return ok
		}) {
			t.Errorf("env-reference.yaml doesn't run for changes to %s", in)
		}
	}
}

var envHelpBanned = regexp.MustCompile(`[<>{}|;–—“”‘’]|[a-z)]: [A-Za-z]`)

func envProse(text string) string {
	parts := strings.Split(text, "`")
	for i := 1; i < len(parts); i += 2 {
		parts[i] = "code"
	}
	return strings.Join(parts, "")
}

func TestEnvHelp(t *testing.T) {
	named := map[string]bool{}
	for _, k := range settings.Named() {
		for _, v := range k.Vars {
			named[v] = true
		}
	}
	for _, v := range envVars {
		if v.help == "" && !named[v.name] {
			t.Errorf("%s has no description", v.name)
		}
		for _, sentence := range helpSentences(v.help) {
			rest := suchAs.ReplaceAllString(sentence, "")
			if anyEmpty.MatchString(rest) && backticked.MatchString(rest) && !emptyClause.MatchString(rest) && !otherEmpty.MatchString(rest) {
				t.Errorf("%s states a value for an empty setting in a form the default check can't read: %q", v.name, strings.TrimSpace(sentence))
			}
		}
	}
	check := func(what, text string) {
		if text == "" {
			return
		}
		if !strings.HasSuffix(text, ".") {
			t.Errorf("%s doesn't end with a full stop", what)
		}
		if m := envHelpBanned.FindString(envProse(text)); m != "" {
			t.Errorf("%s has %q", what, m)
		}
	}
	for _, d := range envDocs() {
		if strings.TrimSpace(d.text) == "" {
			t.Errorf("%s renders without a description", d.name)
		}
		check(d.name, d.text)
		check(d.name+" note", d.note)
	}
	for _, s := range slices.Concat(envSections, envInternalGroups) {
		check("the "+s.title+" section intro", s.intro)
		check("the "+s.title+" page intro", s.page)
		if m := envHelpBanned.FindString(s.title); m != "" {
			t.Errorf("the %s section title has %q", s.title, m)
		}
	}
	check("the note on values from files", envNoFileNote)
	check("the reference note", referenceEnvNote)
	for _, g := range []*secretGroup{{kind: secrets.KindFixed}, {kind: secrets.KindHex32, stateful: true}, {kind: secrets.KindVAPID, stateful: true}} {
		text, _ := secretHelp(g)
		check("the secrets help", text)
	}
}

func TestLibraryEnvAnchors(t *testing.T) {
	for _, name := range libraryEnvVars {
		anchors := libraryEnvAnchors[name]
		if len(anchors) == 0 {
			t.Errorf("%s has no code anchor in libraryEnvAnchors", name)
		}
		for _, a := range anchors {
			data, err := os.ReadFile(filepath.Join(repoRoot, a[0]))
			if err != nil {
				t.Fatal(err)
			}
			if !regexp.MustCompile(a[1]).Match(data) {
				t.Errorf("%s no longer matches %s, which the %s entry depends on", a[0], a[1], name)
			}
		}
	}
	walkCode(t, codeRoots, func(rel, lang, data string) {
		if lang == "native" && strings.Contains(data, ".no_proxy()") && !slices.Contains(noProxyFiles, rel) {
			t.Errorf("%s turns off the proxy, which the ALL_PROXY entry doesn't cover", rel)
		}
	})
}

func TestEnvScanner(t *testing.T) {
	files := []codeFile{
		{rel: "fixture/timings.erl", lang: "native", data: "pod() ->\n    first_name([\"FX_POD\", \"FX_HOST\"], x).\n\nfirst_name([], Fallback) ->\n    Fallback;\nfirst_name([Name | Rest], Fallback) ->\n    case config:env_value(Name) of\n        undefined -> first_name(Rest, Fallback);\n        Value -> Value\n    end.\n"},
		{rel: "fixture/version.rs", lang: "native", data: stripRustTests("fn version() -> String {\n    read_first_env(&[\"FX_BUILD\", \"FX_VERSION\"])\n}\n#[cfg(test)]\nmod tests {\n    fn reads() {\n        let _ = (\"}\", '{');\n        std::env::var(\"FX_TEST_ONLY\");\n    }\n}\nfn after() {\n    std::env::var(\"FX_AFTER\");\n}\n")},
		{rel: "fixture/built.rs", lang: "native", data: "fn built(x: &str) {\n    std::env::var(format!(\"FX_BUILT_{}\", x));\n    std::env::var_os(concat!(\"FX_JOINED_\", \"X\"));\n}\n"},
		{rel: "fixture/check.ts", lang: "ts", data: "if (days < 1) {\n\tthrow new Error('FLUXER_FX_MESSAGE must be at least 1');\n}\nassertIntegerInRange(days, 'FLUXER_FX_MESSAGE', 1, 10);\n"},
		{rel: "fixture/mod.rs", lang: "native", data: stripRustTests("#[cfg(test)]\nmod tests;\nfn port(env: &Env) {\n    read(env, \"FX_PORT\");\n}\nfn read(env: &Env, name: &str) {\n    env.get(name);\n}\n")},
		{rel: "fixture/required.rs", lang: "native", data: "fn load(env: &Env) {\n    env.require(\"FX_REQUIRED\");\n}\n"},
		{rel: "fixture/key.ts", lang: "ts", data: "const key = 'FX_KEY';\nexport const value = process.env[key];\n"},
		{rel: "fixture/list.rs", lang: "native", data: "fn first() {\n    for k in [\"FX_FIRST\", \"FX_SECOND\"] {\n        let _ = std::env::var(k);\n    }\n    let n = \"FX_LOCAL\";\n    let _ = std::env::var(n);\n}\n"},
		{rel: "fixture/node.erl", lang: "native", data: "node() ->\n    Name = \"FX_NODE\",\n    os:getenv(Name).\n"},
		{rel: "fixture/branding.rs", lang: "native", data: "const ICON: (&str, &str) = (\"icon_url\", \"FX_ICON\");\nfn snapshot<F: FnMut(&str) -> Option<String>>(mut read_var: F) {\n    fields(|(_, name)| read_var(name));\n}\n"},
		{rel: "fixture/scheme.ts", lang: "ts", data: "function readString(name: string, fallback: string) {\n\treturn process.env[name] ?? fallback;\n}\nfunction readScheme(name: string) {\n\treturn readString(name, 'http');\n}\nexport const scheme = readScheme('FX_SCHEME');\n"},
	}
	got, mentioned := scanEnvNames(files)
	for _, name := range []string{"FX_POD", "FX_HOST", "FX_BUILD", "FX_VERSION", "FX_AFTER", "FX_PORT", "FX_BUILT_", "FX_JOINED_", "FX_REQUIRED", "FX_SCHEME", "FX_KEY", "FX_FIRST", "FX_SECOND", "FX_LOCAL", "FX_NODE", "FX_ICON"} {
		if got[name] == nil {
			t.Errorf("the scanner misses %s", name)
		}
	}
	if got["FX_TEST_ONLY"] != nil {
		t.Errorf("the scanner counts FX_TEST_ONLY, which only test code reads")
	}
	if got["FLUXER_FX_MESSAGE"] != nil || mentioned["FLUXER_FX_MESSAGE"] == nil {
		t.Errorf("the scanner counts FLUXER_FX_MESSAGE as a read, which only an error message has, or misses it as a mention")
	}
}

var envAlias = regexp.MustCompile(`([A-Za-z_$][\w$]*)\??\s*(?::\s*[^=,;()\n]+)?=\s*(?:process|Bun|import\.meta)\.env\s*(?:[,;)\n]|$)|([A-Za-z_$][\w$]*)\??\s*:\s*NodeJS\.ProcessEnv\b`)

func TestEnvAliases(t *testing.T) {
	walkCode(t, codeRoots, func(rel, lang, data string) {
		if lang != "ts" {
			return
		}
		for _, m := range envAlias.FindAllStringSubmatch(data, -1) {
			if name := m[1] + m[2]; name != "env" {
				t.Errorf("%s keeps the environment in %s, and the scanner only follows reads through env", rel, name)
			}
		}
	})
}
