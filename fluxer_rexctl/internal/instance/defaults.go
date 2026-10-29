// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"slices"
	"strings"
)

func composeDefaults(files [][]byte) map[string][]string {
	out := map[string][]string{}
	alt := map[string]bool{}
	for _, data := range files {
		scanDefaults(string(data), out, alt)
	}
	for name := range alt {
		delete(out, name)
	}
	return out
}

func scanDefaults(s string, out map[string][]string, alt map[string]bool) {
	for i := 0; i < len(s); i++ {
		if s[i] != '$' || i+1 >= len(s) {
			continue
		}
		if s[i+1] == '$' {
			i++
			continue
		}
		if s[i+1] != '{' {
			continue
		}
		j := i + 2
		for j < len(s) && isNameByte(s[j], j == i+2) {
			j++
		}
		name := s[i+2 : j]
		if name == "" || j >= len(s) {
			continue
		}
		op := ""
		switch {
		case strings.HasPrefix(s[j:], ":-"):
			op = ":-"
		case s[j] == '-':
			op = "-"
		case strings.HasPrefix(s[j:], ":+"):
			op = ":+"
		case s[j] == '+':
			op = "+"
		}
		if op == "" {
			continue
		}
		start := j + len(op)
		end := closingBrace(s, start)
		if end < 0 {
			continue
		}
		def := s[start:end]
		switch {
		case strings.HasSuffix(op, "+"):
			alt[name] = true
		case !slices.Contains(out[name], def):
			out[name] = append(out[name], def)
		}
		scanDefaults(def, out, alt)
		i = end
	}
}

func closingBrace(s string, from int) int {
	depth := 0
	for k := from; k < len(s); k++ {
		switch {
		case s[k] == '{':
			depth++
		case s[k] == '}':
			if depth == 0 {
				return k
			}
			depth--
		case s[k] == '\n':
			return -1
		}
	}
	return -1
}

func isNameByte(c byte, first bool) bool {
	switch {
	case c == '_', c >= 'A' && c <= 'Z', c >= 'a' && c <= 'z':
		return true
	case c >= '0' && c <= '9':
		return !first
	}
	return false
}
