// SPDX-License-Identifier: AGPL-3.0-or-later

package lint

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

type Finding struct {
	File string
	Line int
	Rule string
	Text string
}

func (f Finding) String() string {
	return fmt.Sprintf("%s:%d: %s in %q", f.File, f.Line, f.Rule, f.Text)
}

func goFiles(root string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		name := d.Name()
		if d.IsDir() {
			if path != root && (strings.HasPrefix(name, ".") || strings.HasPrefix(name, "_") || name == "testdata" || name == "vendor") {
				return filepath.SkipDir
			}
			return nil
		}
		if strings.HasSuffix(name, ".go") {
			files = append(files, path)
		}
		return nil
	})
	sort.Strings(files)
	return files, err
}

const SPDXHeader = "// SPDX-License-Identifier: AGPL-3.0-or-later"

func CheckComments(path string, src []byte) ([]Finding, error) {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, path, src, parser.ParseComments)
	if err != nil {
		return nil, err
	}
	if ast.IsGenerated(f) {
		return nil, nil
	}
	var out []Finding
	hasHeader := false
	for _, group := range f.Comments {
		for _, cm := range group.List {
			line := fset.Position(cm.Pos()).Line
			switch {
			case line == 1 && cm.Text == SPDXHeader:
				hasHeader = true
			case strings.HasPrefix(cm.Text, "//go:"), strings.HasPrefix(cm.Text, "//line "):
			default:
				out = append(out, Finding{File: path, Line: line, Rule: "comment", Text: cm.Text})
			}
		}
	}
	if !hasHeader {
		out = append(out, Finding{File: path, Line: 1, Rule: "missing SPDX header", Text: SPDXHeader})
	}
	return out, nil
}

func Comments(root string) ([]Finding, error) {
	files, err := goFiles(root)
	if err != nil {
		return nil, err
	}
	var all []Finding
	for _, path := range files {
		src, err := os.ReadFile(path)
		if err != nil {
			return nil, err
		}
		rel, _ := filepath.Rel(root, path)
		found, err := CheckComments(rel, src)
		if err != nil {
			return nil, err
		}
		all = append(all, found...)
	}
	return all, nil
}
