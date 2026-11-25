// SPDX-License-Identifier: AGPL-3.0-or-later

package lint

import (
	"bufio"
	"bytes"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"unicode"
)

type copyRule struct {
	name string
	re   *regexp.Regexp
}

var (
	copyRules = []copyRule{
		{"dash", regexp.MustCompile(`[—–]`)},
		{"smart quote", regexp.MustCompile(`[‘’“”]`)},
		{"dot separator", regexp.MustCompile(`[\p{L}\p{N})]\s+[·•]\s+\S|^\s+[·•]\s+$`)},
		{"carry", regexp.MustCompile(`(?i)\bcarr(y|ies|ied|ying)\b`)},
		{"matters", regexp.MustCompile(`(?i)\bmatters\b`)},
		{"shape", regexp.MustCompile(`(?i)\bshap(e|es|ed|ing)\b`)},
		{"reach", regexp.MustCompile(`(?i)\breach(es|ed|ing)?\b`)},
		{"live for where", regexp.MustCompile(`(?i)\blives (in|on|at|under|inside)\b|\blive (in|on|under|inside)\b`)},
		{"name as a verb", regexp.MustCompile(`(?i)\b(names|naming)\b|(^|[.,]\s+|\b(to|and|or|will|can|should|must|then|not|never|always)\s+)name the\b`)},
		{"answer as a verb", regexp.MustCompile(`(?i)\b(answers|answered|answering)\b|\b(doesn't|don't|does not|didn't|did not|do not) answer\b`)},
		{"sitting", regexp.MustCompile(`(?i)\bsitting\b`)},
		{"experimental", regexp.MustCompile(`(?i)(^|[^-\w])experimental\b`)},
	}
	proseSemicolon = regexp.MustCompile(`[\p{L})]; \p{L}`)
	testClaim      = regexp.MustCompile(`(?i)\b(un)?tested\b|\bwe (ran|tried|checked|confirmed|measured|saw|found|observed|noticed)\b|\bhaven't (tested|run|tried|confirmed|measured)\b|\bon our (own )?[\w -]*\b(server|machine|box|pi|cluster|nas|laptop|mac|vm)s?\b`)
	copyNouns      = regexp.MustCompile(`(?i)\b(file|bucket|image|host|project|flag|domain|header|volume|machine|server|service|client|product|display|user|setup|flow|saved|the|their|your|its|these|those|no|without|with|of|for) (names|answers)\b|\banswered (prompt|one|question)s?\b`)
	inlineCode     = regexp.MustCompile("`[^`]*`")
	oldRuntimeFlag = regexp.MustCompile(`--experimental\b`)
	copyDocs       = []string{"deploy/docker/fluxer.env", "deploy/docker/compose.yaml", "deploy/helm/fluxer/README.md", "deploy/helm/fluxer/values.yaml", "deploy/helm/fluxer/values.schema.json", "deploy/helm/fluxer/templates/NOTES.txt", "deploy/helm/fluxer/templates/_helpers.tpl", "deploy/helm/fluxer/templates/httproute.yaml"}
	copyGoRoots    = []string{"fluxer_rexctl", "fluxer_native"}
)

func copyHits(s string) []string {
	s = copyNouns.ReplaceAllString(s, "")
	var out []string
	for _, r := range copyRules {
		if r.re.MatchString(s) {
			out = append(out, r.name)
		}
	}
	return out
}

func Copy(repo string) ([]Finding, error) {
	var out []Finding
	add := func(file string, line int, text string, extra ...string) {
		for _, rule := range append(copyHits(text), extra...) {
			out = append(out, Finding{File: file, Line: line, Rule: rule, Text: text})
		}
	}
	for _, root := range copyGoRoots {
		files, err := goFiles(filepath.Join(repo, root))
		if err != nil {
			return nil, err
		}
		for _, path := range files {
			rel, _ := filepath.Rel(repo, path)
			rel = filepath.ToSlash(rel)
			if strings.HasPrefix(rel, "fluxer_rexctl/internal/lint/") {
				continue
			}
			if err := copyGo(path, func(line int, text string) { add(rel, line, text) }); err != nil {
				return nil, err
			}
		}
		err = filepath.WalkDir(filepath.Join(repo, root), func(path string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() || !strings.HasSuffix(path, ".golden") {
				return err
			}
			rel, _ := filepath.Rel(repo, path)
			return copyLines(path, false, func(line int, text string) { add(filepath.ToSlash(rel), line, text) })
		})
		if err != nil {
			return nil, err
		}
	}
	docs := slices.Clone(copyDocs)
	err := filepath.WalkDir(filepath.Join(repo, filepath.FromSlash(docsContent), "operator"), func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".mdx") {
			return err
		}
		rel, _ := filepath.Rel(repo, path)
		docs = append(docs, filepath.ToSlash(rel))
		return nil
	})
	if err != nil {
		return nil, err
	}
	for _, rel := range docs {
		path := filepath.Join(repo, filepath.FromSlash(rel))
		err := copyLines(path, false, func(line int, text string) {
			if oldRuntimeFlag.MatchString(text) {
				out = append(out, Finding{File: rel, Line: line, Rule: "experimental flag", Text: text})
			}
		})
		if err != nil {
			return nil, err
		}
		err = copyLines(path, true, func(line int, text string) {
			prose := inlineCode.ReplaceAllString(text, "")
			var extra []string
			if proseSemicolon.MatchString(prose) {
				extra = append(extra, "semicolon")
			}
			if testClaim.MatchString(prose) {
				extra = append(extra, "test claim")
			}
			add(rel, line, prose, extra...)
		})
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

func copyGo(path string, emit func(line int, text string)) error {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		return err
	}
	tags := map[*ast.BasicLit]bool{}
	for _, imp := range f.Imports {
		tags[imp.Path] = true
	}
	ast.Inspect(f, func(n ast.Node) bool {
		switch n := n.(type) {
		case *ast.Field:
			if n.Tag != nil {
				tags[n.Tag] = true
			}
		case *ast.FuncDecl:
			if name := n.Name.Name; strings.HasPrefix(name, "Test") && n.Recv == nil {
				emit(fset.Position(n.Pos()).Line, splitCamel(strings.TrimPrefix(name, "Test")))
			}
		case *ast.BasicLit:
			if n.Kind != token.STRING || tags[n] {
				return true
			}
			s, err := strconv.Unquote(n.Value)
			if err == nil && strings.ContainsFunc(s, unicode.IsSpace) {
				emit(fset.Position(n.Pos()).Line, s)
			}
		}
		return true
	})
	return nil
}

func copyLines(path string, skipFences bool, emit func(line int, text string)) error {
	data, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	sc := bufio.NewScanner(bytes.NewReader(data))
	sc.Buffer(nil, 1<<20)
	fenced := false
	for n := 1; sc.Scan(); n++ {
		line := sc.Text()
		if skipFences && strings.HasPrefix(strings.TrimSpace(line), "```") {
			fenced = !fenced
			continue
		}
		if !fenced {
			emit(n, line)
		}
	}
	return sc.Err()
}

func splitCamel(s string) string {
	rs := []rune(s)
	var b strings.Builder
	for i, r := range rs {
		if r == '_' {
			b.WriteByte(' ')
			continue
		}
		if i > 0 && unicode.IsUpper(r) && (unicode.IsLower(rs[i-1]) || i+1 < len(rs) && unicode.IsLower(rs[i+1])) {
			b.WriteByte(' ')
		}
		b.WriteRune(unicode.ToLower(r))
	}
	return b.String()
}
