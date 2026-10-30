// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"unicode"
)

type caddyToken struct {
	text   string
	line   int
	quoted bool
}

type caddyEntry struct {
	keys  []caddyToken
	block bool
}

type entryKind int

const (
	entryOther entryKind = iota
	entrySite
	entrySnippet
	entryGlobal
)

var (
	dottedHost  = regexp.MustCompile(`^(?:\*\.)?(?:[A-Za-z0-9_-]+\.)+[A-Za-z0-9_-]+\.?$`)
	plainHost   = regexp.MustCompile(`^[A-Za-z0-9_-]+$`)
	bracketHost = regexp.MustCompile(`^\[[0-9A-Fa-f:.]+\]$`)
	portSuffix  = regexp.MustCompile(`:[0-9]+$`)
	snippetKey  = regexp.MustCompile(`^\([^()\s]+\)$`)
)

func caddyTokens(data []byte) ([]caddyToken, bool) {
	var out []caddyToken
	src := []rune(strings.ReplaceAll(string(data), "\r\n", "\n"))
	line := 1
	for i := 0; i < len(src); {
		ch := src[i]
		switch {
		case ch == '\n':
			line++
			i++
		case unicode.IsSpace(ch):
			i++
		case ch == '#':
			for i < len(src) && src[i] != '\n' {
				i++
			}
		case ch == '"' || ch == '`':
			start := line
			var b strings.Builder
			i++
			closed := false
			for i < len(src) {
				c := src[i]
				if c == '\\' && ch == '"' && i+1 < len(src) && (src[i+1] == '"' || src[i+1] == '\\') {
					b.WriteRune(src[i+1])
					i += 2
					continue
				}
				if c == ch {
					closed = true
					i++
					break
				}
				if c == '\n' {
					line++
				}
				b.WriteRune(c)
				i++
			}
			if !closed {
				return nil, false
			}
			out = append(out, caddyToken{text: b.String(), line: start, quoted: true})
		default:
			start := i
			for i < len(src) && !unicode.IsSpace(src[i]) {
				i++
			}
			text := string(src[start:i])
			if strings.HasPrefix(text, "<<") {
				return nil, false
			}
			out = append(out, caddyToken{text: text, line: line})
		}
	}
	return out, true
}

func caddyEntries(data []byte) ([]caddyEntry, bool) {
	tokens, ok := caddyTokens(data)
	if !ok {
		return nil, false
	}
	var out []caddyEntry
	var keys []caddyToken
	flush := func() {
		if len(keys) > 0 {
			out = append(out, caddyEntry{keys: keys})
			keys = nil
		}
	}
	depth := 0
	for _, t := range tokens {
		opens, closes := !t.quoted && t.text == "{", !t.quoted && t.text == "}"
		if depth > 0 {
			switch {
			case opens:
				depth++
			case closes:
				depth--
			}
			continue
		}
		if len(keys) > 0 {
			last := keys[len(keys)-1]
			if last.line != t.line && !strings.HasSuffix(last.text, ",") {
				flush()
			}
		}
		switch {
		case opens:
			out = append(out, caddyEntry{keys: keys, block: true})
			keys = nil
			depth = 1
		case closes:
			return nil, false
		default:
			keys = append(keys, t)
		}
	}
	if depth != 0 {
		return nil, false
	}
	flush()
	return out, true
}

func (e caddyEntry) kind() entryKind {
	if !e.block {
		return entryOther
	}
	if len(e.keys) == 0 {
		return entryGlobal
	}
	if len(e.keys) == 1 && !e.keys[0].quoted && snippetKey.MatchString(e.keys[0].text) {
		return entrySnippet
	}
	n := 0
	for _, k := range e.keys {
		if k.quoted {
			return entryOther
		}
		for _, addr := range strings.Split(k.text, ",") {
			if addr == "" {
				continue
			}
			if !siteAddress(addr) {
				return entryOther
			}
			n++
		}
	}
	if n == 0 {
		return entryOther
	}
	return entrySite
}

func siteAddress(s string) bool {
	if strings.ContainsAny(s, "{}@") {
		return false
	}
	rest, scheme := strings.CutPrefix(s, "https://")
	if !scheme {
		rest, scheme = strings.CutPrefix(s, "http://")
	}
	host, _, _ := strings.Cut(rest, "/")
	if host == "" {
		return false
	}
	port := portSuffix.FindString(host)
	host = strings.TrimSuffix(host, port)
	switch {
	case host == "":
		return port != ""
	case host == "localhost", bracketHost.MatchString(host), dottedHost.MatchString(host):
		return true
	}
	return (scheme || port != "") && plainHost.MatchString(host)
}

func DefinesSite(data []byte) bool {
	entries, ok := caddyEntries(data)
	if !ok {
		return false
	}
	for _, e := range entries {
		if e.kind() == entrySite {
			return true
		}
	}
	return false
}

func onlySites(data []byte) bool {
	entries, ok := caddyEntries(data)
	if !ok {
		return false
	}
	sites := 0
	for _, e := range entries {
		switch e.kind() {
		case entrySite:
			sites++
		case entrySnippet:
		default:
			return false
		}
	}
	return sites > 0
}

func (i *Instance) MoveSites() ([]string, error) {
	entries, err := os.ReadDir(i.Path(CaddyDropInDir))
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	var moved []string
	for _, e := range entries {
		if !e.Type().IsRegular() || filepath.Ext(e.Name()) != ".caddy" {
			continue
		}
		src := i.Path(CaddyDropInDir, e.Name())
		data, err := os.ReadFile(src)
		if err != nil {
			return moved, err
		}
		if !onlySites(data) {
			continue
		}
		dst := i.Path(CaddySitesDir, e.Name())
		if exists(dst) {
			continue
		}
		if err := os.MkdirAll(i.Path(CaddySitesDir), 0o755); err != nil {
			return moved, err
		}
		if err := os.Rename(src, dst); err != nil {
			return moved, err
		}
		moved = append(moved, e.Name())
	}
	return moved, nil
}

func TopLevelImports(caddyfile []byte) (sites, dropIns bool) {
	entries, ok := caddyEntries(caddyfile)
	if !ok {
		return false, false
	}
	for _, e := range entries {
		if e.block || len(e.keys) < 2 || e.keys[0].text != "import" {
			continue
		}
		switch arg := e.keys[1].text; {
		case strings.HasSuffix(arg, "/sites/*.caddy"):
			sites = true
		case strings.HasSuffix(arg, "/*.caddy"):
			dropIns = true
		}
	}
	return sites, dropIns
}

func (i *Instance) SiteFiles() ([]string, error) {
	entries, err := os.ReadDir(i.Path(CaddySitesDir))
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	var files []string
	for _, e := range entries {
		if e.Type().IsRegular() && filepath.Ext(e.Name()) == ".caddy" {
			files = append(files, e.Name())
		}
	}
	return files, nil
}

func (i *Instance) UnloadSites() ([]string, error) {
	files, err := i.SiteFiles()
	if err != nil {
		return nil, err
	}
	return i.moveCaddyFiles(files, CaddySitesDir, CaddyDropInDir)
}

func (i *Instance) RestoreSites(files []string) ([]string, error) {
	return i.moveCaddyFiles(files, CaddyDropInDir, CaddySitesDir)
}

func (i *Instance) PutBackSites(files []string) error {
	_, err := i.moveCaddyFiles(files, CaddySitesDir, CaddyDropInDir)
	return err
}

func (i *Instance) moveCaddyFiles(files []string, from, to string) ([]string, error) {
	var moved []string
	for _, f := range files {
		src, dst := i.Path(from, f), i.Path(to, f)
		if !isFile(src) || exists(dst) {
			continue
		}
		if err := os.MkdirAll(i.Path(to), 0o755); err != nil {
			return moved, err
		}
		if err := os.Rename(src, dst); err != nil {
			return moved, err
		}
		moved = append(moved, f)
	}
	return moved, nil
}
