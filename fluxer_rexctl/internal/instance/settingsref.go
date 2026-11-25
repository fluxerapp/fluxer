// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"regexp"
	"strings"
	"unicode"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/mdx"
)

type SettingRef struct {
	Section string
	Text    string
	Secret  bool
}

var (
	olderLayoutSentence = regexp.MustCompile(`(?i)docker compose|\bcompose\b|COMPOSE_|docker-compose|\.env\b|\btakeover\b|\beject\b|rexctl (sizes|sets|writes|generates|copies)|\bvalkey container\b|\bthe stack\b|--experimental|WORKER_REPLICAS|\bDocker runs\b`)
	containerWord       = regexp.MustCompile(`\b(containers?)\b`)
	bundledSentence     = regexp.MustCompile(`(?i)bundled (valkey|seaweedfs|object store|nats|meilisearch|postgres)`)
	olderServices       = regexp.MustCompile(`^FLUXER_(SEAWEEDFS|VALKEY)_`)
	settingsCode        = regexp.MustCompile("`[^`]*`")
)

func SettingsReference(t mdx.Terms) map[string]SettingRef {
	out := map[string]SettingRef{}
	for _, d := range envDocs() {
		section := envTopSection(d.section)
		older := olderServices.MatchString(d.name)
		if older {
			section = "bundled"
		}
		text := d.text
		if d.key != nil && d.help != "" && d.key.Help != "" && unicode.IsLower(rune(d.key.Help[0])) {
			text = d.help
		}
		kept := settingSentences(text, older)
		text = strings.Join(kept, " ")
		if section != "backups" {
			text = containerWord.ReplaceAllStringFunc(text, func(w string) string {
				if w == "containers" {
					return "services"
				}
				return "service"
			})
		}
		if text != "" {
			text = envMarkdown(text, t)
		}
		out[d.name] = SettingRef{Section: section, Text: text, Secret: d.secret}
	}
	return out
}

func settingSentences(text string, older bool) []string {
	var kept []string
	for _, s := range splitSentences(text) {
		prose := settingsCode.ReplaceAllString(s, "")
		if !olderLayoutSentence.MatchString(prose) && (older || !bundledSentence.MatchString(prose)) {
			kept = append(kept, s)
		}
	}
	return kept
}

func splitSentences(text string) []string {
	var out []string
	rest := strings.TrimSpace(text)
	for rest != "" {
		i := sentenceEnd(rest)
		if i < 0 {
			out = append(out, rest)
			break
		}
		out = append(out, strings.TrimSpace(rest[:i+1]))
		rest = strings.TrimSpace(rest[i+1:])
	}
	return out
}

func sentenceEnd(s string) int {
	code := false
	for i := 0; i < len(s); i++ {
		switch {
		case s[i] == '`':
			code = !code
		case !code && s[i] == '.' && (i+1 == len(s) || s[i+1] == ' '):
			return i
		}
	}
	return -1
}

func ByteSize(name, value string) string { return envByteSize(name, value) }
