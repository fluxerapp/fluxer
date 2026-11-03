// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"context"
	_ "embed"
	"encoding/json"
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

//go:embed configschema.json
var configSchemaJSON []byte

type ConfigKey struct {
	Key       string   `json:"key"`
	Type      string   `json:"type"`
	Value     any      `json:"value"`
	Default   any      `json:"default,omitempty"`
	Help      string   `json:"help"`
	Options   []string `json:"options,omitempty"`
	SecretSet string   `json:"-"`
	Nullable  bool     `json:"-"`
}

func ConfigSchema() ([]ConfigKey, error) {
	var raw []struct {
		Key       string   `json:"key"`
		Type      string   `json:"type"`
		Help      string   `json:"help"`
		Default   any      `json:"default"`
		Options   []string `json:"options"`
		SecretSet string   `json:"secret_set"`
		Nullable  bool     `json:"nullable"`
	}
	if err := json.Unmarshal(configSchemaJSON, &raw); err != nil {
		return nil, fmt.Errorf("adminapi: config schema: %w", err)
	}
	keys := make([]ConfigKey, len(raw))
	for i, r := range raw {
		keys[i] = ConfigKey{Key: r.Key, Type: r.Type, Help: r.Help, Default: r.Default, Options: r.Options, SecretSet: r.SecretSet, Nullable: r.Nullable}
		switch r.Key {
		case RegistrationModeKey:
			keys[i].Options = RegistrationModes
			keys[i].Help = "Who can create an account. It works like rexctl registration set"
		case registrationLinksKey:
			keys[i].Help = "Whether invite links from admins still create accounts. rexctl registration set changes it for you"
		}
	}
	return keys, nil
}

const (
	RegistrationModeKey  = "registration.mode"
	registrationLinksKey = "registration.admin_registration_urls_enabled"
)

type UnknownKeyError struct {
	Key     string
	Similar []string
}

func (e *UnknownKeyError) Error() string { return fmt.Sprintf("adminapi: no setting %q", e.Key) }

func FindConfigKey(keys []ConfigKey, name string) (ConfigKey, error) {
	name = strings.TrimSpace(name)
	for _, k := range keys {
		if k.Key == name {
			return k, nil
		}
	}
	e := &UnknownKeyError{Key: name}
	last := name[strings.LastIndex(name, ".")+1:]
	for _, k := range keys {
		if last != "" && (strings.HasSuffix(k.Key, "."+last) || strings.Contains(k.Key, name)) {
			e.Similar = append(e.Similar, k.Key)
		}
	}
	return ConfigKey{}, e
}

func lookup(m map[string]any, path string) (any, bool) {
	var cur any = m
	for _, p := range strings.Split(path, ".") {
		obj, ok := cur.(map[string]any)
		if !ok {
			return nil, false
		}
		cur, ok = obj[p]
		if !ok {
			return nil, false
		}
	}
	return cur, true
}

func FillValues(keys []ConfigKey, cfg map[string]any) []ConfigKey {
	out := make([]ConfigKey, len(keys))
	for i, k := range keys {
		if k.Type == "secret" {
			v, _ := lookup(cfg, k.SecretSet)
			set, _ := v.(bool)
			k.Value = set
		} else if v, ok := lookup(cfg, k.Key); ok {
			k.Value = v
		}
		if links, _ := lookup(cfg, registrationLinksKey); k.Key == RegistrationModeKey && k.Value == "closed" && links == true {
			k.Value = "invite"
		}
		out[i] = k
	}
	return out
}

func (c *Client) ConfigValues(ctx context.Context) ([]ConfigKey, error) {
	keys, err := ConfigSchema()
	if err != nil {
		return nil, err
	}
	cfg, err := c.RawConfig(ctx)
	if err != nil {
		return nil, err
	}
	return FillValues(keys, cfg), nil
}

type InvalidValueError struct {
	Key  ConfigKey
	Want string
}

func (e *InvalidValueError) Error() string {
	return fmt.Sprintf("adminapi: %s needs %s", e.Key.Key, e.Want)
}

func ParseConfigValue(k ConfigKey, value string) (any, error) {
	switch k.Type {
	case "integer":
		n, err := strconv.ParseInt(strings.TrimSpace(value), 10, 64)
		if err != nil {
			return nil, &InvalidValueError{Key: k, Want: "a whole number"}
		}
		return n, nil
	case "number":
		f, err := strconv.ParseFloat(strings.TrimSpace(value), 64)
		if err != nil {
			return nil, &InvalidValueError{Key: k, Want: "a number"}
		}
		return f, nil
	case "boolean":
		switch strings.ToLower(strings.TrimSpace(value)) {
		case "true", "on", "yes", "1":
			return true, nil
		case "false", "off", "no", "0":
			return false, nil
		}
		return nil, &InvalidValueError{Key: k, Want: "true or false"}
	case "list":
		items := []string{}
		for _, part := range strings.Split(value, ",") {
			if part = strings.TrimSpace(part); part != "" {
				items = append(items, part)
			}
		}
		return items, nil
	case "json":
		var v any
		if err := json.Unmarshal([]byte(value), &v); err != nil {
			return nil, &InvalidValueError{Key: k, Want: "JSON"}
		}
		return v, nil
	}
	if len(k.Options) > 0 && !slices.Contains(k.Options, value) {
		return nil, &InvalidValueError{Key: k, Want: "one of " + ui.JoinOr(k.Options)}
	}
	return value, nil
}

func ConfigPatch(key string, value any) map[string]any {
	parts := strings.Split(key, ".")
	root := map[string]any{}
	cur := root
	for _, p := range parts[:len(parts)-1] {
		next := map[string]any{}
		cur[p] = next
		cur = next
	}
	cur[parts[len(parts)-1]] = value
	return root
}

func MergePatch(dst, src map[string]any) {
	for k, v := range src {
		sub, ok := v.(map[string]any)
		if have, exists := dst[k].(map[string]any); ok && exists {
			MergePatch(have, sub)
			continue
		}
		dst[k] = v
	}
}

func FormatValue(k ConfigKey) string {
	if k.Type == "secret" {
		if set, _ := k.Value.(bool); set {
			return "set"
		}
		return "not set"
	}
	switch v := k.Value.(type) {
	case nil:
		return ""
	case string:
		return v
	case bool:
		return strconv.FormatBool(v)
	case float64:
		return strconv.FormatFloat(v, 'f', -1, 64)
	case []string:
		return strings.Join(v, ", ")
	case []any:
		if all := scalarList(v); all != nil {
			return strings.Join(all, ", ")
		}
	}
	data, err := json.Marshal(k.Value)
	if err != nil {
		return fmt.Sprint(k.Value)
	}
	return string(data)
}

func scalarList(v []any) []string {
	out := make([]string, 0, len(v))
	for _, item := range v {
		s, ok := item.(string)
		if !ok {
			return nil
		}
		out = append(out, s)
	}
	return out
}
