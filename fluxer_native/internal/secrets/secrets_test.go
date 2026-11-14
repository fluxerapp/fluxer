// SPDX-License-Identifier: AGPL-3.0-or-later

package secrets

import (
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"slices"
	"strconv"
	"testing"
)

const rexctlSecrets = "../../../fluxer_rexctl/internal/secrets"

var kindNames = map[Kind]string{
	KindHex32:     "KindHex32",
	KindBase64_32: "KindBase64_32",
	KindVAPID:     "KindVAPID",
	KindFixed:     "KindFixed",
	KindRSA:       "KindRSA",
	KindHex16:     "KindHex16",
}

type textSpec struct {
	Key, Kind, Fixed string
	Stateful         bool
}

func parseRexctl(t *testing.T) (specs []textSpec) {
	t.Helper()
	fset := token.NewFileSet()
	consts := map[string]string{}
	var required *ast.CompositeLit
	for _, name := range []string{"secrets.go", "vapid.go"} {
		f, err := parser.ParseFile(fset, filepath.Join(rexctlSecrets, name), nil, 0)
		if err != nil {
			t.Fatal(err)
		}
		for _, decl := range f.Decls {
			gen, ok := decl.(*ast.GenDecl)
			if !ok {
				continue
			}
			for _, s := range gen.Specs {
				vs, ok := s.(*ast.ValueSpec)
				if !ok {
					continue
				}
				for i, id := range vs.Names {
					if gen.Tok == token.CONST && i < len(vs.Values) {
						if lit, ok := vs.Values[i].(*ast.BasicLit); ok {
							consts[id.Name], _ = strconv.Unquote(lit.Value)
						}
					}
					if gen.Tok == token.VAR && id.Name == "Required" {
						required = vs.Values[i].(*ast.CompositeLit)
					}
				}
			}
		}
	}
	if required == nil {
		t.Fatal("rexctl secrets.go has no Required list")
	}
	for _, elt := range required.Elts {
		var s textSpec
		for _, field := range elt.(*ast.CompositeLit).Elts {
			kv := field.(*ast.KeyValueExpr)
			name := kv.Key.(*ast.Ident).Name
			switch v := kv.Value.(type) {
			case *ast.BasicLit:
				str, _ := strconv.Unquote(v.Value)
				switch name {
				case "Key":
					s.Key = str
				case "Fixed":
					s.Fixed = str
				}
			case *ast.Ident:
				switch name {
				case "Key":
					s.Key = consts[v.Name]
				case "Kind":
					s.Kind = v.Name
				case "Stateful":
					s.Stateful = v.Name == "true"
				}
			}
		}
		specs = append(specs, s)
	}
	return specs
}

func TestRequiredMatchesRexctl(t *testing.T) {
	want := parseRexctl(t)
	var got []textSpec
	for _, s := range Required {
		got = append(got, textSpec{Key: s.Key, Kind: kindNames[s.Kind], Fixed: s.Fixed, Stateful: s.Stateful})
	}
	if !slices.Equal(got, want) {
		t.Fatalf("Required differs from rexctl\n got %+v\nwant %+v", got, want)
	}
}
