// SPDX-License-Identifier: AGPL-3.0-or-later

package lint

import (
	"fmt"
	"io/fs"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"unicode"
)

const (
	docsContent = "fluxer_docs/src/content/docs"
	docsPages   = "fluxer_docs/src/pages"
	docsConfig  = "fluxer_docs/astro.config.ts"
	docsHost    = "https://docs.fluxer.com"
)

var (
	mdLink       = regexp.MustCompile(`\]\(([^)\s]+)(?:\s+"[^"]*")?\)`)
	hrefLink     = regexp.MustCompile(`href="([^"]+)"`)
	hrefExpr     = regexp.MustCompile(`href=\{`)
	codeSpan     = regexp.MustCompile("`[^`]*`")
	urlScheme    = regexp.MustCompile(`^[a-zA-Z][a-zA-Z0-9+.-]*:`)
	mdHeading    = regexp.MustCompile(`^#{1,6}\s+(.*?)\s*#*\s*$`)
	htmlID       = regexp.MustCompile(`\bid="([^"]+)"`)
	htmlTag      = regexp.MustCompile(`<[^>]*>`)
	inlineLink   = regexp.MustCompile(`\[([^\]]*)\]\([^)]*\)`)
	mdEscape     = regexp.MustCompile(`\\([[:punct:]])`)
	redirectPair = regexp.MustCompile(`'(/[^']*)'\s*:\s*'(/[^']*)'`)
	docsURLConst = regexp.MustCompile(`\bdocsURL\s*=\s*"` + regexp.QuoteMeta(docsHost) + `(/[^"]*)"`)
	docsURLJoin  = regexp.MustCompile(`\bdocsURL\s*\+\s*"([^"]*)"`)
	docsURLFmt   = regexp.MustCompile(`"[^"]*%s([a-z0-9][^"\s]*?)[.,]?(?:\s[^"]*)?"[^\n]*\bdocsURL\b`)
	docsURLUse   = regexp.MustCompile(`\bdocsURL\b`)
	docsLiteral  = regexp.MustCompile(regexp.QuoteMeta(docsHost) + `(/[^"\s)\\]*)`)
	bareAside    = regexp.MustCompile(`^\s*:::(note|tip|caution|danger)\s*$`)
	asideTag     = regexp.MustCompile(`<Aside\b[^>]*>`)
	asideTitle   = regexp.MustCompile(`\btitle=`)
)

var requiredLinks = []string{
	"/operator/moving/#moving-from-the-0x-chart",
	"/operator/rexctl/#rexctl-doctor",
	"/operator/settings/",
	"/operator/docker/",
	"/operator/reverse-proxy/",
	"/operator/settings/#fluxer_domain",
	"/operator/settings/#fluxer_edge_trusted_proxies",
	"/operator/settings/#fluxer_livekit_node_ip",
}

type docsSite struct {
	pages     map[string]map[string]bool
	redirects map[string]string
}

func DocLinks(repo string) ([]Finding, error) {
	site, err := loadDocsSite(repo)
	if err != nil {
		return nil, err
	}
	var out []Finding
	content := filepath.Join(repo, docsContent)
	err = filepath.WalkDir(content, func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !isDocsPage(p) {
			return err
		}
		data, err := os.ReadFile(p)
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(content, p)
		if err != nil {
			return err
		}
		route := docsRoute(rel)
		file := filepath.ToSlash(filepath.Join(docsContent, rel))
		if strings.HasPrefix(filepath.ToSlash(rel), "operator/") {
			out = append(out, untitledAsides(file, string(data))...)
		}
		for n, line := range proseLines(string(data)) {
			line = codeSpan.ReplaceAllString(line, "")
			if line == "" {
				continue
			}
			if hrefExpr.MatchString(line) {
				out = append(out, Finding{File: file, Line: n + 1, Rule: "link the check can't read, use a plain href", Text: strings.TrimSpace(line)})
			}
			for _, re := range []*regexp.Regexp{mdLink, hrefLink} {
				for _, m := range re.FindAllStringSubmatch(line, -1) {
					target, ok := resolveLink(route, m[1])
					if !ok {
						continue
					}
					if problem := site.check(route, target); problem != "" {
						out = append(out, Finding{File: file, Line: n + 1, Rule: problem, Text: m[1]})
					}
				}
			}
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	for _, target := range requiredLinks {
		if problem := site.check("/", target); problem != "" {
			out = append(out, Finding{File: docsConfig, Line: 1, Rule: "a link that code or charts use: " + problem, Text: target})
		}
	}
	found, err := site.checkRexctl(repo)
	return append(out, found...), err
}

func untitledAsides(file, text string) []Finding {
	var out []Finding
	for n, line := range strings.Split(text, "\n") {
		bare := bareAside.MatchString(line)
		for _, tag := range asideTag.FindAllString(line, -1) {
			bare = bare || !asideTitle.MatchString(tag)
		}
		if bare {
			out = append(out, Finding{File: file, Line: n + 1, Rule: "callout without a title, give it a specific one", Text: strings.TrimSpace(line)})
		}
	}
	return out
}

func loadDocsSite(repo string) (*docsSite, error) {
	site := &docsSite{pages: map[string]map[string]bool{}, redirects: map[string]string{}}
	content := filepath.Join(repo, docsContent)
	err := filepath.WalkDir(content, func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !isDocsPage(p) {
			return err
		}
		data, err := os.ReadFile(p)
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(content, p)
		if err != nil {
			return err
		}
		site.pages[docsRoute(rel)] = pageAnchors(string(data))
		return nil
	})
	if err != nil {
		return nil, err
	}
	pages := filepath.Join(repo, docsPages)
	err = filepath.WalkDir(pages, func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		rel, err := filepath.Rel(pages, p)
		if err != nil {
			return err
		}
		rel = filepath.ToSlash(strings.TrimSuffix(rel, filepath.Ext(rel)))
		for {
			site.pages["/"+rel] = map[string]bool{}
			site.pages["/"+rel+"/"] = map[string]bool{}
			dir := filepath.ToSlash(filepath.Dir(rel))
			if dir == "." || dir == rel {
				break
			}
			rel = dir
		}
		return nil
	})
	if err != nil {
		return nil, err
	}
	config, err := os.ReadFile(filepath.Join(repo, docsConfig))
	if err != nil {
		return nil, err
	}
	if _, block, ok := strings.Cut(string(config), "redirects: {"); ok {
		block, _, _ = strings.Cut(block, "}")
		for _, m := range redirectPair.FindAllStringSubmatch(block, -1) {
			site.redirects[withSlash(m[1])] = m[2]
		}
	}
	return site, nil
}

func isDocsPage(p string) bool {
	return strings.HasSuffix(p, ".md") || strings.HasSuffix(p, ".mdx")
}

func docsRoute(rel string) string {
	rel = filepath.ToSlash(rel)
	rel = strings.TrimSuffix(strings.TrimSuffix(rel, ".mdx"), ".md")
	rel = strings.TrimSuffix(rel, "index")
	return withSlash("/" + rel)
}

func withSlash(route string) string {
	if !strings.HasSuffix(route, "/") {
		route += "/"
	}
	return strings.ReplaceAll(route, "//", "/")
}

func proseLines(text string) []string {
	lines := strings.Split(text, "\n")
	fence := ""
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		switch {
		case fence != "":
			if strings.HasPrefix(trimmed, fence) {
				fence = ""
			}
			lines[i] = ""
		case strings.HasPrefix(trimmed, "```"), strings.HasPrefix(trimmed, "~~~"):
			fence = trimmed[:3]
			lines[i] = ""
		}
	}
	return lines
}

func pageAnchors(text string) map[string]bool {
	anchors := map[string]bool{}
	seen := map[string]int{}
	for _, line := range proseLines(text) {
		for _, m := range htmlID.FindAllStringSubmatch(line, -1) {
			anchors[m[1]] = true
		}
		m := mdHeading.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		id := slug(m[1])
		if n := seen[id]; n > 0 {
			anchors[fmt.Sprintf("%s-%d", id, n)] = true
		} else {
			anchors[id] = true
		}
		seen[id]++
	}
	return anchors
}

func slug(heading string) string {
	text := htmlTag.ReplaceAllString(heading, "")
	text = inlineLink.ReplaceAllString(text, "$1")
	text = mdEscape.ReplaceAllString(text, "$1")
	text = strings.TrimSpace(strings.ToLower(text))
	var b strings.Builder
	for _, r := range text {
		switch {
		case r == ' ':
			b.WriteRune('-')
		case r == '-', r == '_', unicode.IsLetter(r), unicode.IsNumber(r), unicode.IsMark(r):
			b.WriteRune(r)
		}
	}
	return b.String()
}

func resolveLink(route, target string) (string, bool) {
	if urlScheme.MatchString(target) || strings.HasPrefix(target, "//") {
		return "", false
	}
	if strings.HasPrefix(target, "/") || strings.HasPrefix(target, "#") {
		return target, true
	}
	base, err := url.Parse(route)
	if err != nil {
		return "", false
	}
	ref, err := url.Parse(target)
	if err != nil {
		return "", false
	}
	resolved := base.ResolveReference(ref)
	out := resolved.Path
	if resolved.Fragment != "" {
		out += "#" + resolved.Fragment
	}
	return out, true
}

func (s *docsSite) check(from, target string) string {
	path, anchor, _ := strings.Cut(target, "#")
	path, _, _ = strings.Cut(path, "?")
	if path == "" {
		path = from
	}
	if strings.HasPrefix(path, "/_") || slices.ContainsFunc([]string{".png", ".svg", ".jpg", ".webp", ".ico", ".txt", ".json"}, func(ext string) bool { return strings.HasSuffix(path, ext) }) {
		return ""
	}
	anchors, ok := s.pages[path]
	if !ok {
		anchors, ok = s.pages[withSlash(path)]
	}
	if !ok {
		to, redirected := s.redirects[withSlash(path)]
		if !redirected {
			return "link to a page that doesn't exist"
		}
		if anchors, ok = s.pages[withSlash(to)]; !ok {
			return "link to a redirect whose page doesn't exist"
		}
	}
	if anchor != "" && !anchors[anchor] {
		return "link to an anchor that doesn't exist"
	}
	return ""
}

func (s *docsSite) checkRexctl(repo string) ([]Finding, error) {
	files, err := goFiles(filepath.Join(repo, "fluxer_rexctl"))
	if err != nil {
		return nil, err
	}
	base := ""
	type use struct {
		file, target string
		line         int
	}
	var uses []use
	var unread []Finding
	for _, p := range files {
		rel, err := filepath.Rel(repo, p)
		if err != nil {
			return nil, err
		}
		if strings.HasSuffix(p, "_test.go") || strings.HasPrefix(filepath.ToSlash(rel), "fluxer_rexctl/internal/lint/") {
			continue
		}
		data, err := os.ReadFile(p)
		if err != nil {
			return nil, err
		}
		for n, line := range strings.Split(string(data), "\n") {
			if m := docsURLConst.FindStringSubmatch(line); m != nil {
				base = m[1]
				continue
			}
			matched := false
			for _, re := range []*regexp.Regexp{docsURLJoin, docsURLFmt} {
				for _, m := range re.FindAllStringSubmatch(line, -1) {
					uses = append(uses, use{filepath.ToSlash(rel), m[1], n + 1})
					matched = true
				}
			}
			if !matched && docsURLUse.MatchString(line) {
				unread = append(unread, Finding{File: filepath.ToSlash(rel), Line: n + 1, Rule: "docsURL use the link check can't read, build the link on one line", Text: strings.TrimSpace(line)})
			}
			for _, m := range docsLiteral.FindAllStringSubmatch(line, -1) {
				uses = append(uses, use{filepath.ToSlash(rel), "/" + strings.TrimPrefix(m[1], "/"), n + 1})
			}
		}
	}
	out := unread
	for _, u := range uses {
		target := u.target
		if !strings.HasPrefix(target, "/") {
			if base == "" {
				return nil, fmt.Errorf("found docsURL in %s but not its value", u.file)
			}
			target = base + target
		}
		target = strings.TrimRight(target, ".,")
		if problem := s.check("/", target); problem != "" {
			out = append(out, Finding{File: u.file, Line: u.line, Rule: problem, Text: target})
		}
	}
	return out, nil
}
