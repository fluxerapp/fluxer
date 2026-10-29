// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const docsContent = "fluxer_docs/src/content/docs"

var (
	docsEnvName    = regexp.MustCompile(`\bFLUXER_[A-Z0-9_]*[A-Z0-9]\b(\*)?`)
	docsConfigCmd  = regexp.MustCompile("rexctl config (?:set|unset|get) ([A-Za-z0-9_.]+)")
	docsDottedName = regexp.MustCompile("`([a-z][a-z0-9_]*(?:\\.[a-z0-9_]+)+)`")
	composeSecret  = regexp.MustCompile(`(?m)^sed -i "s\|\^([A-Z0-9_]+)=\.\*\|[A-Z0-9_]+=\$\((.*)\)\|" \.env$`)
	composeLoop    = regexp.MustCompile(`(?s)for key in (.*?); do\n\s*sed -i "s\|\^\$key=\.\*\|\$key=\$\((.*?)\)\|" \.env\ndone`)
)

var helmPages = []string{docsContent + "/operator/kubernetes.mdx"}

func docsPages(t *testing.T) map[string]string {
	t.Helper()
	pages := map[string]string{}
	root := filepath.Join(repoRoot, docsContent)
	err := filepath.WalkDir(root, func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || (filepath.Ext(p) != ".md" && filepath.Ext(p) != ".mdx") {
			return err
		}
		data, err := os.ReadFile(p)
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(repoRoot, p)
		pages[filepath.ToSlash(rel)] = string(data)
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	return pages
}

func TestDocsEnvNames(t *testing.T) {
	registry := map[string]bool{}
	for _, v := range envVars {
		registry[v.name] = true
	}
	set := stack.Embedded()
	schema, err := adminapi.ConfigSchema()
	if err != nil {
		t.Fatal(err)
	}
	prefixes := map[string]bool{}
	for _, k := range settings.Named() {
		first, _, _ := strings.Cut(k.Name, ".")
		prefixes[first] = true
	}
	for _, k := range schema {
		first, _, _ := strings.Cut(k.Key, ".")
		prefixes[first] = true
	}
	for rel, data := range docsPages(t) {
		for _, m := range docsEnvName.FindAllStringSubmatch(data, -1) {
			name := m[0]
			if m[1] != "" || slices.Contains(notEnvNames, name) {
				continue
			}
			if base, ok := strings.CutSuffix(name, "_FILE"); ok && registry[base] {
				continue
			}
			if !registry[name] {
				t.Errorf("%s mentions %s, but it has no entry in envVars", rel, name)
			}
		}
		keys := map[string]bool{}
		for _, m := range docsConfigCmd.FindAllStringSubmatch(data, -1) {
			keys[strings.TrimSuffix(m[1], ".")] = true
		}
		for _, m := range docsDottedName.FindAllStringSubmatch(data, -1) {
			first, _, _ := strings.Cut(m[1], ".")
			if prefixes[first] && strings.HasPrefix(rel, docsContent+"/operator/") && !slices.Contains(helmPages, rel) {
				keys[m[1]] = true
			}
		}
		for key := range keys {
			if _, err := settings.Lookup(set, nil, key, schema); err != nil {
				t.Errorf("%s mentions the setting %s, but rexctl doesn't take it: %v", rel, key, err)
			}
		}
	}
}

func TestComposePageSecrets(t *testing.T) {
	data, err := os.ReadFile(filepath.Join(repoRoot, docsContent, "operator/docker-compose.mdx"))
	if err != nil {
		t.Fatal(err)
	}
	page := string(data)
	commands := map[string]string{}
	if m := composeLoop.FindStringSubmatch(page); m != nil {
		for _, key := range strings.Fields(strings.ReplaceAll(m[1], "\\", " ")) {
			commands[key] = m[2]
		}
	} else {
		t.Error("docker-compose.mdx has no loop that fills in the hex secrets")
	}
	for _, m := range composeSecret.FindAllStringSubmatch(page, -1) {
		commands[m[1]] = m[2]
	}
	want := map[secrets.Kind]string{
		secrets.KindHex32:     "openssl rand -hex 32",
		secrets.KindBase64_32: "openssl rand -base64 32",
		secrets.KindRSA:       "openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -outform DER | base64",
		secrets.KindVAPID:     "openssl ec -in vapid.pem",
		secrets.KindAlnum64:   "openssl rand -base64 96 | tr -dc A-Za-z0-9 | head -c 64",
	}
	for _, s := range secrets.Required {
		if s.Kind == secrets.KindFixed {
			continue
		}
		cmd, ok := commands[s.Key]
		switch {
		case !ok:
			t.Errorf("docker-compose.mdx doesn't fill in %s", s.Key)
		case !strings.Contains(cmd, want[s.Kind]):
			t.Errorf("docker-compose.mdx fills in %s with %q, but it needs %q", s.Key, cmd, want[s.Kind])
		}
		delete(commands, s.Key)
	}
	if cmd := commands[SetupTokenVar]; !strings.Contains(envVars[slices.IndexFunc(envVars, func(v envVar) bool { return v.name == SetupTokenVar })].help, cmd) {
		t.Errorf("docker-compose.mdx fills in %s with %q, but its entry in envVars suggests another command", SetupTokenVar, cmd)
	}
	delete(commands, SetupTokenVar)
	for key := range commands {
		t.Errorf("docker-compose.mdx fills in %s, but it isn't a generated secret", key)
	}
}

var (
	docsLink    = regexp.MustCompile(`\]\((/[a-z0-9/_-]*?)/?#([a-z0-9_-]+)\)`)
	docsHeading = regexp.MustCompile(`(?m)^#{1,6} (.+)$`)
	slugDrop    = regexp.MustCompile(`[^a-z0-9 _-]`)
)

func docsSlug(heading string) string {
	return strings.ReplaceAll(slugDrop.ReplaceAllString(strings.ToLower(heading), ""), " ", "-")
}

func TestDocsAnchors(t *testing.T) {
	pages := docsPages(t)
	for _, name := range slices.Sorted(maps.Keys(pages)) {
		if !strings.HasPrefix(name, docsContent+"/operator/") {
			continue
		}
		for _, m := range docsLink.FindAllStringSubmatch(pages[name], -1) {
			base := docsContent + m[1]
			target, ok := "", false
			for _, c := range []string{base + ".mdx", base + ".md", base + "/index.mdx", base + "/index.md"} {
				if _, found := pages[c]; found {
					target, ok = c, true
					break
				}
			}
			if !ok {
				t.Errorf("%s links to %s, but there is no such page", name, m[1])
				continue
			}
			if !slices.ContainsFunc(docsHeading.FindAllStringSubmatch(pages[target], -1), func(h []string) bool { return docsSlug(h[1]) == m[2] }) {
				t.Errorf("%s links to %s#%s, but %s has no heading with that anchor", name, m[1], m[2], target)
			}
		}
	}
}
