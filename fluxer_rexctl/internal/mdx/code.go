// SPDX-License-Identifier: AGPL-3.0-or-later

package mdx

import (
	"regexp"
	"slices"
	"strings"
)

type Terms struct {
	Commands   map[string]bool
	Keys       map[string][]string
	ValueFlags map[string]bool
}

const docsHost = "https://docs.fluxer.com"

var (
	flagWord       = regexp.MustCompile(`^--?[a-z][a-z0-9-]*(=\S+)?$`)
	envWord        = regexp.MustCompile(`^\$?[A-Z][A-Z0-9]*(_[A-Z0-9]+)+(=\S+)?$|^\$[A-Z]+$`)
	pathWord       = regexp.MustCompile(`^(~?/|\./)[\w.@/-]*[\w/]$`)
	fileWord       = regexp.MustCompile(`^[\w.-]*\.(ya?ml|json|toml|env|pem|conf|service|timer|sh|ps1|sql|tar|gz|zst)$|^\.env(\.example)?$`)
	urlWord        = regexp.MustCompile(`^https?://\S+$`)
	placeholderURL = regexp.MustCompile(`^https?://([^/]*example\.|[0-9.:\[\]]+[/:]|[^/]*\.\.\.)`)
	queryWord      = regexp.MustCompile(`^\?\S*=\S*$`)
	argWord        = regexp.MustCompile(`^(<[^>]+>|[A-Z][A-Z0-9_]*|[a-z0-9_]+(\.[a-z0-9_]+)+|[0-9][0-9.:]*)$`)
	placeholder    = regexp.MustCompile(`^<[a-z][a-z0-9_-]*>$`)
	timeWord       = regexp.MustCompile(`^[0-2]?[0-9]:[0-5][0-9](,[0-2]?[0-9]:[0-5][0-9])*$|^HH:MM$`)
	valueWord      = regexp.MustCompile(`^[a-z][a-z0-9-]*$`)
	ipWord         = regexp.MustCompile(`^([0-9]{1,3}\.){3}[0-9]{1,3}(/[0-9]{1,2}|:[0-9]+)?$`)
	hostWord       = regexp.MustCompile(`^([a-z0-9-]+\.)*example\.(com|org|net)(:[0-9]+)?$|^localhost:[0-9]+$`)
	schemeWord     = regexp.MustCompile(`^[a-z][a-z0-9+.-]*://\S*$`)
	imageWord      = regexp.MustCompile(`^[a-z0-9][a-z0-9._/-]*:[a-z0-9][a-z0-9._-]*$`)
	trailPunct     = ".,;:!?)"
)

func Code(text string, t Terms) string {
	parts := strings.Split(text, "`")
	for i := 0; i < len(parts); i += 2 {
		parts[i] = codeWords(parts[i], t)
	}
	return strings.Join(parts, "`")
}

type word struct {
	lead, core, trail, space string
}

func splitWords(text string) []word {
	var out []word
	for text != "" {
		end := strings.IndexAny(text, " \t\n")
		tok, rest := text, ""
		if end >= 0 {
			tok, rest = text[:end], text[end:]
		}
		space := rest[:len(rest)-len(strings.TrimLeft(rest, " \t\n"))]
		text = rest[len(space):]
		core := strings.TrimLeft(tok, "(\"'")
		w := word{lead: tok[:len(tok)-len(core)], space: space}
		w.core = strings.TrimRight(core, trailPunct+"\"'")
		if strings.HasPrefix(core[len(w.core):], "...") {
			w.core += "..."
		}
		w.trail = core[len(w.core):]
		out = append(out, w)
	}
	return out
}

func codeWords(text string, t Terms) string {
	words := splitWords(text)
	values := valueWords(words)
	var b strings.Builder
	for i := 0; i < len(words); {
		if values[i] {
			w := words[i]
			b.WriteString(w.lead + "`" + w.core + "`" + w.trail + w.space)
			i++
			continue
		}
		n := 0
		if i > 0 {
			n = valueRun(words[i-1:], t)
		}
		if n == 0 {
			n = codeRun(words[i:], t)
		}
		if n == 0 {
			w := words[i]
			b.WriteString(w.lead + w.core + w.trail + w.space)
			i++
			continue
		}
		run := words[i : i+n]
		if w := run[0]; n == 1 && urlWord.MatchString(w.core) && !placeholderURL.MatchString(w.core) {
			b.WriteString(w.lead + link(w.core) + w.trail + w.space)
			i++
			continue
		}
		b.WriteString(run[0].lead + "`")
		for j, w := range run {
			if j > 0 {
				b.WriteString(w.lead)
			}
			b.WriteString(w.core)
			if j < len(run)-1 {
				b.WriteString(w.trail + w.space)
			}
		}
		last := run[len(run)-1]
		b.WriteString("`" + last.trail + last.space)
		i += n
	}
	return b.String()
}

func codeRun(words []word, t Terms) int {
	w := words[0]
	if w.core == "" {
		return 0
	}
	if w.core == "rexctl" && w.trail == "" {
		n := 1
		path := "rexctl"
		for n < len(words) && joined(words, n) && t.Commands[path+" "+words[n].core] {
			path += " " + words[n].core
			n++
		}
		if n == 1 && (len(words) < 2 || !flagWord.MatchString(words[1].core)) {
			return 0
		}
		if n == 2 && len(words) > 2 && joined(words, 2) && objectWords[words[2].core] {
			return 0
		}
		for n < len(words) && joined(words, n) {
			w := words[n]
			_, key := t.Keys[w.core]
			switch {
			case flagWord.MatchString(w.core), argWord.MatchString(w.core), key:
			case slices.Contains(t.Keys[words[n-1].core], w.core):
			default:
				return n
			}
			n++
		}
		return n
	}
	if w.core == "docker" && len(words) > 1 && w.trail == "" && words[1].core == "compose" {
		n := 2
		if len(words) > 2 && words[1].trail == "" && composeVerbs[words[2].core] {
			n = 3
		}
		return n
	}
	if flagValue(words, t) {
		return 2
	}
	switch {
	case flagWord.MatchString(w.core) && strings.HasPrefix(w.core, "--"),
		envWord.MatchString(w.core),
		strings.Contains(w.core, ".") && keyNamed(t, w.core),
		queryWord.MatchString(w.core),
		placeholder.MatchString(w.core),
		timeWord.MatchString(w.core),
		ipWord.MatchString(w.core),
		hostWord.MatchString(w.core),
		schemeWord.MatchString(w.core),
		imageWord.MatchString(w.core),
		pathWord.MatchString(w.core) && strings.Count(w.core, "/") > 1,
		fileWord.MatchString(w.core),
		urlWord.MatchString(w.core):
		return 1
	}
	return 0
}

func flagValue(words []word, t Terms) bool {
	if len(words) < 3 || !t.ValueFlags[words[0].core] || !joined(words, 1) {
		return false
	}
	v := words[1]
	return v.trail == "" && valueWord.MatchString(v.core) && !objectWords[v.core] && words[2].core == "to"
}

func valueRun(words []word, t Terms) int {
	prev, w := words[0], words[1]
	if w.lead != "" || w.core == "" || objectWords[w.core] {
		return 0
	}
	switch prev.core {
	case "pass", "Pass":
		if prev.trail == "" && w.trail == "" && len(words) > 2 && words[2].core == "to" {
			return 1
		}
	}
	if len(words) > 2 && t.ValueFlags[w.core] && prev.trail == "" && (prev.core == "with" || prev.core == "With") && joined(words, 2) {
		if v := words[2].core; v != "" && strings.ToLower(v) == v && !objectWords[v] {
			return 2
		}
	}
	return 0
}

func valueWords(words []word) map[int]bool {
	out := map[int]bool{}
	for i, w := range words {
		if w.trail != "" {
			continue
		}
		switch w.core {
		case "of":
			if i == 0 || words[i-1].trail != "" || (words[i-1].core != "list" && words[i-1].core != "one") {
				continue
			}
			fallthrough
		case "pass", "Pass":
			for _, j := range listRun(words, i+1) {
				out[j] = true
			}
		case "status":
			if i > 0 && i+1 < len(words) && words[i-1].trail == "" && (words[i-1].core == "with" || words[i-1].core == "With") && plainValue(words[i+1]) && words[i+1].trail != "" {
				out[i+1] = true
			}
		}
	}
	return out
}

func listRun(words []word, from int) []int {
	var items []int
	comma := false
	for j := from; j < len(words); j++ {
		w := words[j]
		if !plainValue(w) || (w.trail != "" && w.trail != ",") {
			return nil
		}
		items = append(items, j)
		comma = comma || w.trail == ","
		if k := j + 1; k+1 < len(words) && words[k].trail == "" && (words[k].core == "or" || words[k].core == "and") {
			if !comma || !plainValue(words[k+1]) {
				return nil
			}
			return append(items, k+1)
		}
		if w.trail != "," {
			return nil
		}
	}
	return nil
}

func plainValue(w word) bool {
	return w.lead == "" && valueWord.MatchString(w.core) && !objectWords[w.core] && w.core != "or" && w.core != "and"
}

func link(url string) string {
	path, ok := strings.CutPrefix(url, docsHost)
	if !ok || path == "" || path == "/" {
		return "[" + url + "](" + url + ")"
	}
	page, anchor, _ := strings.Cut(path, "#")
	text := anchor
	if text == "" {
		parts := strings.Split(strings.Trim(page, "/"), "/")
		text = parts[len(parts)-1]
	}
	text = strings.ReplaceAll(text, "-", " ")
	return "[" + text + "](" + path + ")"
}

func joined(words []word, n int) bool {
	return words[n-1].trail == "" && words[n].lead == ""
}

func keyNamed(t Terms, name string) bool {
	_, ok := t.Keys[name]
	return ok
}

var objectWords = map[string]bool{"a": true, "an": true, "the": true, "it": true, "them": true, "this": true, "that": true, "your": true, "its": true}

var composeVerbs = map[string]bool{"up": true, "down": true, "logs": true, "ps": true, "pull": true, "stop": true, "start": true, "restart": true, "exec": true, "config": true}
