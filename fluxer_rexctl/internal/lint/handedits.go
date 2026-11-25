// SPDX-License-Identifier: AGPL-3.0-or-later

package lint

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

var (
	rexctlPages = []string{"with-rexctl"}
	handEdited  = regexp.MustCompile(`\.env\b|state\.json|daemon\.json|containerd/config\.toml|\.ya?ml\b|^\s*` + "```" + `ya?ml|/etc/systemd/`)
)

func HandEdits(repo string) ([]Finding, error) {
	var out []Finding
	for _, page := range rexctlPages {
		file := filepath.ToSlash(filepath.Join(docsContent, "operator", page+".mdx"))
		data, err := os.ReadFile(filepath.Join(repo, file))
		if err != nil {
			return nil, err
		}
		for n, line := range strings.Split(string(data), "\n") {
			if m := handEdited.FindString(line); m != "" {
				out = append(out, Finding{File: file, Line: n + 1, Rule: "rexctl page sends people to a file rexctl manages", Text: strings.TrimSpace(m)})
			}
		}
	}
	return out, nil
}
