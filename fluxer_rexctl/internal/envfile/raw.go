// SPDX-License-Identifier: AGPL-3.0-or-later

package envfile

import (
	"bytes"
	"fmt"
	"maps"
	"os"
	"slices"
	"strings"
)

func validRawKey(k string) bool {
	if k == "" {
		return false
	}
	for i, c := range k {
		switch {
		case c == '_', c >= 'A' && c <= 'Z', c >= 'a' && c <= 'z':
		case c >= '0' && c <= '9' && i > 0:
		default:
			return false
		}
	}
	return true
}

func quoteRaw(v string) string {
	if v != "" && plain(v) {
		return v
	}
	return `"` + strings.NewReplacer(`\`, `\\`, `"`, `\"`, "\n", `\n`, "\r", `\r`).Replace(v) + `"`
}

func MarshalRaw(env map[string]string) ([]byte, error) {
	var b bytes.Buffer
	for _, k := range slices.Sorted(maps.Keys(env)) {
		if !validRawKey(k) {
			return nil, fmt.Errorf("envfile: %q can't be a key in a native env file", k)
		}
		b.WriteString(k)
		b.WriteByte('=')
		b.WriteString(quoteRaw(env[k]))
		b.WriteByte('\n')
	}
	return b.Bytes(), nil
}

func LoadRaw(path string) (map[string]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return parseRaw(path, data)
}

func parseRaw(path string, data []byte) (map[string]string, error) {
	out := map[string]string{}
	for num, line := range strings.Split(strings.TrimPrefix(string(data), "\uFEFF"), "\n") {
		line = strings.TrimSpace(line)
		if line == "" || line[0] == '#' {
			continue
		}
		line = strings.TrimSpace(strings.TrimPrefix(line, "export "))
		key, value, ok := strings.Cut(line, "=")
		key = strings.TrimSpace(key)
		if !ok || !validRawKey(key) {
			return nil, fmt.Errorf("%s:%d: expected KEY=VALUE", path, num+1)
		}
		out[key] = unquoteRaw(strings.TrimSpace(value))
	}
	return out, nil
}

func unquoteRaw(v string) string {
	if len(v) < 2 || (v[0] != '"' && v[0] != '\'') || v[len(v)-1] != v[0] {
		return v
	}
	inner := v[1 : len(v)-1]
	if v[0] == '\'' {
		return inner
	}
	return strings.NewReplacer(`\\`, `\`, `\"`, `"`, `\n`, "\n", `\r`, "\r").Replace(inner)
}
