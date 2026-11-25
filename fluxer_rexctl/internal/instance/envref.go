// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"regexp"
	"slices"
	"strconv"
	"strings"
	"sync"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/mdx"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	envRequiredSection    = "required"
	envAdminSection       = "admin"
	envSecretsSection     = "secrets"
	envComposeSection     = "compose"
	envFilesSection       = "files"
	envRexctlSection      = "rexctl"
	envStackSection       = "stack"
	envInternalSection    = "internal"
	envNetworkSection     = "network"
	envVoiceServerSection = "voice-server"
	envVoiceEdgeSection   = "voice-edge"
)

var envInternalGroups = []string{"hosted", "prices", "cluster", "relay", "cassandra", "docs", "dev"}

func envTopSection(id string) string {
	if slices.Contains(envInternalGroups, id) {
		return envInternalSection
	}
	return id
}

func EnvNames() []string {
	names := make([]string, len(envVars))
	for i, v := range envVars {
		names[i] = v.name
	}
	return names
}

func EnvSection(name string) string {
	for _, v := range envVars {
		if v.name == name {
			return v.section
		}
	}
	return ""
}

type envVar struct {
	name    string
	section string
	help    string
}

type envDoc struct {
	envVar
	text     string
	note     string
	key      *settings.Key
	setting  string
	defaults []envDefault
	value    string
	active   bool
	secret   bool
	bundled  bool
}

type envDefault struct {
	value string
}

type envDocList []envDoc

func (l envDocList) get(name string) envDoc {
	for _, d := range l {
		if d.name == name {
			return d
		}
	}
	return envDoc{}
}

func envDocs() envDocList {
	set := settings.NativeSet(stack.EmbeddedRelease())
	defaults := newEnvDefaults(set)
	named := map[string]settings.Key{}
	for _, k := range settings.Named() {
		for _, v := range k.Vars {
			named[v] = k
		}
	}
	required := map[string]secrets.Spec{}
	for _, s := range secrets.Required {
		required[s.Key] = s
	}
	sized := map[string]bool{}
	for _, v := range stack.SizingKeys() {
		sized[v] = true
	}
	for _, p := range set.Profiles {
		for v := range p {
			sized[v] = true
		}
	}
	derived := settings.DerivedVars()

	out := make(envDocList, 0, len(envVars))
	for _, v := range envVars {
		d := envDoc{envVar: v, defaults: defaults.lines(v.name), bundled: !slices.Contains(legacyImageVars, v.name) && !slices.Contains(envUnbundledSections, envTopSection(v.section))}
		d.active = v.section == envRequiredSection || v.section == envAdminSection || v.section == envSecretsSection
		d.secret = slices.Contains(envSecretVars, v.name)
		notes := []string{v.help}
		spec, isRequired := required[v.name]
		switch k, ok := named[v.name]; {
		case ok:
			d.key, d.setting = &k, k.Name
			switch {
			case v.help == "":
				notes[0] = k.Help + "."
			case k.Vars[0] == v.name:
				notes = []string{k.Help + ".", v.help}
			}
		case settings.Twins[v.name] != "" && !slices.Contains(envNoRexctlVars, v.name):
			d.setting = settings.Twins[v.name]
		case settings.Twins[v.name] != "":
		case !isRequired && d.bundled && v.name != SetupTokenVar && v.name != stack.SettingsLoadedVar && !strings.HasPrefix(v.name, "COMPOSE_") && !slices.Contains(envNoRexctlVars, v.name):
			d.setting = v.name
		}
		switch {
		case isRequired && spec.Kind == secrets.KindFixed:
			d.value = spec.Fixed
		case isRequired && spec.Stateful && spec.Kind == secrets.KindVAPID:
			d.note = "Set the pair once before the first start and never change it."
		case isRequired && slices.Contains(envRotatableVars, v.name):
			d.note = "With Docker Compose, a new value applies the next time it starts the services."
		case isRequired && spec.Stateful:
			d.note = "Set it once before the first start and never change it."
		case isRequired:
		case v.name == stack.SettingsLoadedVar:
			d.value = "true"
		case envSizedNotes[v.name] != "":
			d.note = envSizedNotes[v.name]
		case sized[v.name]:
			d.note = "rexctl sizes this."
		case slices.Contains(derived, v.name):
			d.note = "rexctl sets this from your domain and edge settings unless you set it."
		}
		d.text = strings.Join(notes, " ")
		out = append(out, d)
	}
	return out
}

var envUnbundledSections = []string{envRexctlSection, envStackSection, envInternalSection, envVoiceServerSection, envVoiceEdgeSection}

var envRotatableVars = []string{"FLUXER_S3_SECRET_KEY", "LIVEKIT_API_SECRET"}

var envNoRexctlVars = []string{"FLUXER_CADDY_SITE_ADDRESS", "FLUXER_AUTH_BLUESKY_CLIENT_URI", "FLUXER_BACKUP_ON_UPGRADE", "FLUXER_INSTALL_METHOD", "FLUXER_UPGRADE_COMMAND"}

var envSizedNotes = map[string]string{
	"FLUXER_EDGE_PROTOCOLS": "On a machine with little memory, rexctl turns off HTTP/3 unless you set it.",
	stack.LogMaxSizeVar:     "When disk.profile is small, rexctl sets this to 5m unless you set it.",
	stack.LogMaxFileVar:     "When disk.profile is small, rexctl sets this to 2 unless you set it.",
	stack.MaxWALSizeVar:     "When disk.profile is small, rexctl sets this to 512MB unless you set it.",
	stack.SpoolTotalVar:     "rexctl raises this to FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES if you set that higher. When disk.profile is small, rexctl sets this to 536870912, which is 512 MiB, unless you set it.",
}

var (
	envNamePattern = regexp.MustCompile(`\b[A-Z][A-Z0-9]*_[A-Z0-9_]*[A-Z0-9]\b(=\S+)?`)
	envCodePattern = regexp.MustCompile(`\b(?:https?|wss?|s3|postgres)://[^\s,]*[^\s,.)]|(?:^|[\s(])\.env(?:\.example)?\b|\b[\w.-]+\.(?:yml|json|html)\b|(?:^|[\s(])/[\w./:-]*[\w/]`)
)

func envByteSize(name, value string) string {
	n, err := strconv.ParseInt(value, 10, 64)
	if !strings.HasSuffix(name, "_BYTES") || err != nil || n < 1024 {
		return ""
	}
	for _, u := range []struct {
		unit string
		size int64
	}{{"GiB", 1 << 30}, {"MiB", 1 << 20}, {"KiB", 1 << 10}} {
		if n%u.size == 0 {
			return strconv.FormatInt(n/u.size, 10) + " " + u.unit
		}
	}
	return ""
}

func envSecretNote(d envDoc) string {
	if !d.secret || strings.Contains(d.text, envKeepSecret) {
		return ""
	}
	return "Keep it secret."
}

const envKeepSecret = "Keep the value secret"

func envMarkdown(text string, t mdx.Terms) string {
	text = outsideCode(text, envNamePattern, func(name string) string {
		if code, ok := strings.CutSuffix(name, "."); ok {
			return "`" + code + "`."
		}
		return "`" + name + "`"
	})
	text = outsideCode(text, envCodePattern, func(m string) string {
		lead := strings.TrimLeft(m, " \t\n(")
		return m[:len(m)-len(lead)] + "`" + lead + "`"
	})
	return mdx.Prose(mdx.Code(text, t))
}

func outsideCode(text string, re *regexp.Regexp, fn func(string) string) string {
	parts := strings.Split(text, "`")
	for i := 0; i < len(parts); i += 2 {
		parts[i] = re.ReplaceAllStringFunc(parts[i], fn)
	}
	return strings.Join(parts, "`")
}

func envDocText(d envDoc) string {
	return strings.Join(slices.DeleteFunc([]string{d.text, d.note, envSecretNote(d)}, func(t string) bool { return t == "" }), " ")
}

var envDocsOnce = sync.OnceValue(envDocs)

func EnvHelp(name string) string {
	d := envDocsOnce().get(name)
	if d.name == "" {
		return ""
	}
	return strings.ReplaceAll(envDocText(d), "`", "")
}

type envDefaults map[string]string

func newEnvDefaults(set *stack.Set) envDefaults {
	out := envDefaults{}
	for _, c := range set.Catalog {
		if c.Default != "" && !strings.Contains(c.Default, "$") {
			out[c.Name] = c.Default
		}
	}
	return out
}

func (d envDefaults) lines(v string) []envDefault {
	if value := d[v]; value != "" {
		return []envDefault{{value: value}}
	}
	return nil
}
