// SPDX-License-Identifier: AGPL-3.0-or-later

package envfile

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"regexp"
	"strings"
)

var ErrDuplicateKey = errors.New("envfile: duplicate key")

type DuplicateKeyError struct {
	Key   string
	Lines []int
}

func (e *DuplicateKeyError) Error() string {
	return fmt.Sprintf("%s is set more than once", e.Key)
}

func (e *DuplicateKeyError) Unwrap() error { return ErrDuplicateKey }

type line struct {
	raw   string
	key   string
	value string
	num   int
}

type File struct {
	path  string
	lines []line
}

func New(path string) *File { return &File{path: path} }

func Load(path string) (*File, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(path, data)
}

func Parse(path string, data []byte) (*File, error) {
	text := strings.ReplaceAll(strings.TrimPrefix(string(data), "\uFEFF"), "\r", "")
	f := &File{path: path}
	num := 1
	for text != "" {
		start := num
		raw, key, value, rest := splitEntry(text)
		num += strings.Count(raw, "\n") + 1
		f.lines = append(f.lines, line{raw: raw, key: key, value: value, num: start})
		text = rest
	}
	if dups := f.Duplicates(); len(dups) > 0 {
		return f, dups[0]
	}
	return f, nil
}

func splitEntry(text string) (raw, key, value, rest string) {
	first, after, _ := strings.Cut(text, "\n")
	body := strings.TrimLeft(first, " \t")
	if body == "" || body[0] == '#' {
		return first, "", "", after
	}
	body = strings.TrimPrefix(body, "export ")
	body = strings.TrimLeft(body, " \t")
	name, val, ok := strings.Cut(body, "=")
	name = strings.TrimRight(name, " \t")
	if !ok || !ValidKey(name) {
		return first, "", "", after
	}
	val = strings.TrimLeft(val, " \t")
	offset := len(first) - len(val)
	if val != "" && (val[0] == '\'' || val[0] == '"') {
		if v, n, closed := readQuoted(text[offset:]); closed {
			end := offset + n
			tail, next, _ := strings.Cut(text[end:], "\n")
			return text[:end+len(tail)], name, v, next
		}
	}
	v, _, _ := strings.Cut(val, " #")
	v = strings.TrimRight(v, " \t")
	return first, name, strings.ReplaceAll(v, "$$", "$"), after
}

func readQuoted(s string) (value string, n int, closed bool) {
	quote := s[0]
	var b strings.Builder
	escaped := false
	for i := 1; i < len(s); i++ {
		c := s[i]
		if c != quote {
			if !escaped && c == '\\' {
				escaped = true
				continue
			}
			if escaped {
				escaped = false
				b.WriteByte('\\')
			}
			b.WriteByte(c)
			continue
		}
		if escaped {
			escaped = false
			b.WriteByte(c)
			continue
		}
		if quote == '"' {
			return unescapeDouble(b.String()), i + 1, true
		}
		return b.String(), i + 1, true
	}
	return "", 0, false
}

func unescapeDouble(s string) string {
	var b strings.Builder
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c == '$' && i+1 < len(s) && s[i+1] == '$' {
			b.WriteByte('$')
			i++
			continue
		}
		if c != '\\' || i+1 >= len(s) {
			b.WriteByte(c)
			continue
		}
		switch s[i+1] {
		case 'n':
			b.WriteByte('\n')
		case 't':
			b.WriteByte('\t')
		case 'r':
			b.WriteByte('\r')
		case '\\', '"', '$':
			b.WriteByte(s[i+1])
		default:
			b.WriteByte(c)
			continue
		}
		i++
	}
	return b.String()
}

func ValidKey(k string) bool {
	if k == "" {
		return false
	}
	for i, c := range k {
		switch {
		case c == '_', c >= 'A' && c <= 'Z', c >= 'a' && c <= 'z':
		case c >= '0' && c <= '9' && i > 0:
		case c == '.' || c == '-':
			if i == 0 {
				return false
			}
		default:
			return false
		}
	}
	return true
}

func (f *File) Get(key string) (string, bool) {
	for i := len(f.lines) - 1; i >= 0; i-- {
		if f.lines[i].key == key {
			return f.lines[i].value, true
		}
	}
	return "", false
}

func (f *File) Value(key string) string {
	v, _ := f.Get(key)
	return v
}

func (f *File) Set(key, value string) {
	set := line{raw: key + "=" + Quote(value), key: key, value: value}
	out := f.lines[:0]
	done := false
	for _, l := range f.lines {
		if l.key != key {
			out = append(out, l)
			continue
		}
		if !done {
			out = append(out, set)
			done = true
		}
	}
	f.lines = out
	if !done {
		f.lines = append(f.lines, set)
	}
}

func (f *File) Delete(key string) {
	out := f.lines[:0]
	for _, l := range f.lines {
		if l.key != key {
			out = append(out, l)
		}
	}
	f.lines = out
}

func (f *File) Keys() []string {
	keys := []string{}
	seen := map[string]bool{}
	for _, l := range f.lines {
		if l.key != "" && !seen[l.key] {
			seen[l.key] = true
			keys = append(keys, l.key)
		}
	}
	return keys
}

func (f *File) Map() map[string]string {
	m := make(map[string]string)
	for _, l := range f.lines {
		if l.key != "" {
			m[l.key] = l.value
		}
	}
	return m
}

func (f *File) Duplicates() []*DuplicateKeyError {
	var out []*DuplicateKeyError
	byKey := map[string]*DuplicateKeyError{}
	for _, l := range f.lines {
		if l.key == "" {
			continue
		}
		d, ok := byKey[l.key]
		if !ok {
			d = &DuplicateKeyError{Key: l.key}
			byKey[l.key] = d
		}
		d.Lines = append(d.Lines, l.num)
		if len(d.Lines) == 2 {
			out = append(out, d)
		}
	}
	return out
}

func (f *File) Bytes() []byte {
	var b bytes.Buffer
	for _, l := range f.lines {
		b.WriteString(l.raw)
		b.WriteByte('\n')
	}
	return b.Bytes()
}

func (f *File) Save() error {
	return WriteFileAtomic(f.path, f.Bytes(), true)
}

func Quote(value string) string {
	if plain(value) {
		return value
	}
	if !strings.ContainsAny(value, "'\n\r") && !strings.HasSuffix(value, `\`) {
		return "'" + value + "'"
	}
	var b strings.Builder
	b.WriteByte('"')
	for i := 0; i < len(value); i++ {
		switch c := value[i]; c {
		case '\\':
			b.WriteString(`\\`)
		case '"':
			b.WriteString(`\"`)
		case '$':
			b.WriteString(`$$`)
		case '\n':
			b.WriteString(`\n`)
		case '\r':
			b.WriteString(`\r`)
		default:
			b.WriteByte(c)
		}
	}
	b.WriteByte('"')
	return b.String()
}

func plain(v string) bool {
	for _, c := range v {
		switch {
		case c >= 'a' && c <= 'z', c >= 'A' && c <= 'Z', c >= '0' && c <= '9':
		case strings.ContainsRune("-_.,:/@+=%~^", c):
		default:
			return false
		}
	}
	return true
}

var refPattern = regexp.MustCompile(`\$\$|\$\{([A-Za-z_][A-Za-z0-9_]*)(?:(:?[-?+])([^}]*))?\}|\$([A-Za-z_][A-Za-z0-9_]*)`)

func Expand(value string, lookup func(string) (string, bool)) (string, bool) {
	ok := !strings.Contains(refPattern.ReplaceAllString(value, ""), "$")
	out := refPattern.ReplaceAllStringFunc(value, func(m string) string {
		if m == "$$" {
			return m
		}
		sub := refPattern.FindStringSubmatch(m)
		name, op, def := sub[1], sub[2], sub[3]
		if name == "" {
			name = sub[4]
		}
		v, found := lookup(name)
		switch {
		case !found && (op == ":-" || op == "-"):
			return def
		case !found:
			ok = false
			return m
		case op == ":-" && v == "":
			return def
		case op == ":+":
			if v != "" {
				return def
			}
			return ""
		case op == "+":
			return def
		}
		return v
	})
	return out, ok
}
