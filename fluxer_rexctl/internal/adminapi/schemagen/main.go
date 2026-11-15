// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"encoding/json"
	"fmt"
	"maps"
	"os"
	"slices"
	"strings"
)

type schema struct {
	Ref         string             `json:"$ref"`
	Nullable    bool               `json:"nullable"`
	Type        string             `json:"type"`
	Description string             `json:"description"`
	Default     any                `json:"default"`
	Enum        []any              `json:"enum"`
	Properties  map[string]*schema `json:"properties"`
	Required    []string           `json:"required"`
	Items       *schema            `json:"items"`
	AllOf       []*schema          `json:"allOf"`
	AnyOf       []*schema          `json:"anyOf"`
	OneOf       []*schema          `json:"oneOf"`
}

type spec struct {
	Components struct {
		Schemas map[string]*schema `json:"schemas"`
	} `json:"components"`
}

type key struct {
	Key       string   `json:"key"`
	Type      string   `json:"type"`
	Help      string   `json:"help"`
	Default   any      `json:"default,omitempty"`
	Options   []string `json:"options,omitempty"`
	SecretSet string   `json:"secret_set,omitempty"`
	Nullable  bool     `json:"nullable,omitempty"`
}

var skip = map[string]bool{
	"app_public.setup.configured": true,
	"sso.migrate_identities":      true,
}

type gen struct {
	schemas map[string]*schema
	resp    *schema
	keys    []key
}

func (g *gen) nullable(s *schema) bool {
	for s != nil {
		if s.Nullable {
			return true
		}
		for _, v := range append(append([]*schema{}, s.AnyOf...), s.OneOf...) {
			if v.Type == "null" || v.Nullable {
				return true
			}
		}
		switch {
		case s.Ref != "":
			s = g.schemas[s.Ref[strings.LastIndex(s.Ref, "/")+1:]]
		case len(s.AllOf) == 1 && s.Type == "" && len(s.Properties) == 0:
			s = s.AllOf[0]
		default:
			return false
		}
	}
	return false
}

func (g *gen) resolve(s *schema) (*schema, string, any) {
	desc, def := "", any(nil)
	for s != nil {
		if desc == "" {
			desc = s.Description
		}
		if def == nil {
			def = s.Default
		}
		switch {
		case s.Ref != "":
			s = g.schemas[s.Ref[strings.LastIndex(s.Ref, "/")+1:]]
		case len(s.AllOf) == 1 && s.Type == "" && len(s.Properties) == 0:
			s = s.AllOf[0]
		case len(s.AnyOf) > 0 && s.Type == "":
			s = pickVariant(s.AnyOf)
		case len(s.OneOf) > 0 && s.Type == "":
			return s, desc, def
		default:
			return s, desc, def
		}
	}
	return s, desc, def
}

func pickVariant(vs []*schema) *schema {
	for _, v := range vs {
		if v.Type != "boolean" && v.Type != "null" {
			return v
		}
	}
	return vs[0]
}

func (g *gen) respAt(path []string) (*schema, string) {
	s := g.resp
	desc := ""
	for _, p := range path {
		r, _, _ := g.resolve(s)
		if r == nil || r.Properties == nil {
			return nil, ""
		}
		s = r.Properties[p]
		if s == nil {
			return nil, ""
		}
	}
	r, d, _ := g.resolve(s)
	if desc == "" {
		desc = d
	}
	return r, desc
}

func (g *gen) walk(s *schema, path []string) {
	r, desc, def := g.resolve(s)
	if r == nil {
		return
	}
	name := strings.Join(path, ".")
	if skip[name] || strings.HasPrefix(desc, "Deprecated") {
		return
	}
	if r.Type == "object" && len(r.Properties) > 0 && len(r.Required) == 0 && len(path) > 0 || r.Type == "object" && len(path) == 0 {
		for _, n := range slices.Sorted(maps.Keys(r.Properties)) {
			g.walk(r.Properties[n], append(append([]string{}, path...), n))
		}
		return
	}
	k := key{Key: name, Default: def, Nullable: g.nullable(s)}
	switch {
	case r.Type == "object" || len(r.OneOf) > 0:
		k.Type = "json"
	case r.Type == "array":
		item, _, _ := g.resolve(r.Items)
		if item != nil && (item.Type == "string" || item.Type == "") && len(item.Properties) == 0 {
			k.Type = "list"
		} else {
			k.Type = "json"
		}
	case r.Type == "integer" || r.Type == "number" || r.Type == "boolean":
		k.Type = r.Type
	default:
		k.Type = "string"
	}
	for _, e := range r.Enum {
		if s, ok := e.(string); ok {
			k.Options = append(k.Options, s)
		}
	}
	if r.Type == "boolean" {
		k.Options = nil
	}
	respSchema, respDesc := g.respAt(path)
	if desc == "" {
		desc = respDesc
	}
	if respSchema == nil && k.Type == "string" {
		parent, _ := g.respAt(path[:len(path)-1])
		last := path[len(path)-1]
		if parent != nil && parent.Properties != nil {
			for _, cand := range []string{last + "_set", "password_set"} {
				if _, ok := parent.Properties[cand]; ok && (cand != "password_set" || last == "password") {
					k.Type = "secret"
					k.SecretSet = strings.Join(append(append([]string{}, path[:len(path)-1]...), cand), ".")
					break
				}
			}
		}
	}
	if desc == "" {
		desc = humanize(path)
	}
	k.Help = strings.TrimSuffix(strings.TrimSpace(strings.SplitN(desc, "\n", 2)[0]), ".")
	g.keys = append(g.keys, k)
}

var acronyms = map[string]string{
	"url": "URL", "urls": "URLs", "uri": "URI", "id": "ID", "ids": "IDs", "mb": "MB", "css": "CSS",
	"sso": "SSO", "smtp": "SMTP", "api": "API", "ip": "IP", "rpc": "RPC", "jwks": "JWKS", "gif": "GIF",
	"e2ee": "E2EE", "p2p": "P2P", "tos": "terms",
}

func humanize(path []string) string {
	segs := path[len(path)-1:]
	if len(path) > 1 {
		segs = path[len(path)-2:]
	}
	var words []string
	for _, seg := range segs {
		for _, w := range strings.Split(seg, "_") {
			if a, ok := acronyms[w]; ok {
				w = a
			}
			words = append(words, w)
		}
	}
	out := strings.Join(words, " ")
	if out == "" {
		return ""
	}
	return strings.ToUpper(out[:1]) + out[1:]
}

func main() {
	if len(os.Args) != 3 {
		fmt.Fprintln(os.Stderr, "usage: schemagen <openapi-admin.json> <out.json>")
		os.Exit(2)
	}
	data, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	var sp spec
	if err := json.Unmarshal(data, &sp); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	g := &gen{schemas: sp.Components.Schemas, resp: sp.Components.Schemas["InstanceConfigResponse"]}
	req := sp.Components.Schemas["InstanceConfigUpdateRequest"]
	if req == nil || g.resp == nil {
		fmt.Fprintln(os.Stderr, "schemagen: the spec has no instance config schemas")
		os.Exit(1)
	}
	g.walk(req, nil)
	out, err := json.MarshalIndent(g.keys, "", "  ")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err := os.WriteFile(os.Args[2], append(out, '\n'), 0o644); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
