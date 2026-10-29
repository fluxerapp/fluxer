// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"fmt"
	"regexp"
	"slices"
	"strconv"
	"strings"

	"go.yaml.in/yaml/v4"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/mdx"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	envRequiredSection = "required"
	envAdminSection    = "admin"
	envSecretsSection  = "secrets"
	envComposeSection  = "compose"
	envFilesSection    = "files"
	envRexctlSection   = "rexctl"
	envStackSection    = "stack"
	envInternalSection = "internal"
	envNetworkSection  = "network"
)

type envSection struct {
	id    string
	title string
	intro string
	page  string
}

var envSections = []envSection{
	{envRequiredSection, "Required", "Fill these in before the first start.", ""},
	{envAdminSection, "First admin account", "", ""},
	{envSecretsSection, "Secrets", "Fill these in before the first start, and keep a copy of .env somewhere safe. Only FLUXER_ACTIVITYPUB_SIGNING_KEY_BASE64 may stay empty.",
		"rexctl generates these when it installs Fluxer. With Docker Compose, [these commands](/operator/docker-compose/#fill-in-env) fill them in."},
	{envComposeSection, "Compose files", fmt.Sprintf("The optional compose files change how Fluxer runs. "+
		"To use one, list it after %s in COMPOSE_FILE, such as COMPOSE_FILE=%s:%s.",
		stack.ComposeFile, stack.ComposeFile, stack.OverlayFiles[stack.OverlayProxy]), ""},
	{"address", "Public address", "Fluxer works out every address from FLUXER_DOMAIN. Set these only when a part of Fluxer runs on another address.", ""},
	{"edge", "Edge and HTTPS", "By default Fluxer listens on ports 80 and 443 and gets its own HTTPS certificate for your domain.", ""},
	{"voice", "Voice and video", "Calls connect directly to this machine on FLUXER_LIVEKIT_TCP_PORT and FLUXER_LIVEKIT_UDP_PORT, even when Fluxer is behind your own proxy.", ""},
	{"storage", "Object storage", "By default Fluxer keeps files in the bundled object store. To use your own S3-compatible store, set its address and buckets, add " +
		stack.OverlayFiles[stack.OverlayExternalObjectStore] + " to COMPOSE_FILE and put its keys in FLUXER_S3_ACCESS_KEY and FLUXER_S3_SECRET_KEY.", ""},
	{"database", "Database", "Leave these unset to use the bundled Postgres.", ""},
	{"services", "Cache, queue and search", "Leave these unset to use the bundled Valkey, NATS and Meilisearch.", ""},
	{envNetworkSection, "Outgoing proxy", "These settings are for Fluxer's services. rexctl and the Linux and macOS install script take HTTPS_PROXY, HTTP_PROXY and NO_PROXY from the shell you run them from.", ""},
	{"sso", "Sign-in", "", ""},
	{"email", "Email", "The admin panel can also save the delivery mode, the sender and the SMTP server. A value saved there is used first.", ""},
	{"push", "Push notifications", "", ""},
	{"media", "Media", "", ""},
	{"gateway", "Gateway", "", ""},
	{"app", "Web app", "", ""},
	{"api", "API and worker", "", ""},
	{"instance", "Instance and branding", "Fluxer uses the product name, images, theme colour and status page links set here only until the setup wizard finishes or someone saves branding in the admin panel. After that it uses the saved values and ignores these. Make later changes in the admin panel or with `rexctl config set`.", ""},
	{"billing", "Billing", "The admin panel can also save whether billing is on, the Stripe keys and the prices. A value saved there is used first, and a saved price list replaces all of FLUXER_STRIPE_PRICES.", ""},
	{"integrations", "Integrations", "The admin panel can also save the Bluesky settings and keys. A value saved there is used first.", ""},
	{"tuning", "Memory and performance", "rexctl sets some of these from the machine's memory and CPU on every start. A value you set wins.", ""},
	{"bundled", "Bundled services", "These change how the bundled Valkey, Meilisearch and SeaweedFS run.", ""},
	{"operations", "Logs and health checks", "", ""},
	{settings.ImagesPrefix[:len(settings.ImagesPrefix)-1], "Images",
		"Set one to use your own mirror or another version. Fluxer's own images are pinned to the release in " + stack.ComposeFile + ".", ""},
	{envFilesSection, "Values from files", "The api, worker, gateway and Rust services also take a setting from a file. " +
		"Set `NAME_FILE` to the path of a file that has the value, such as a Docker secret, and leave `NAME` unset. " +
		"A service with both `NAME` and `NAME_FILE` set refuses to start. " + stack.ComposeFile + " passes only the variables it lists. " +
		"To use a `NAME_FILE` variable, add it to the service's environment in a compose file of your own and list that file in COMPOSE_FILE.", ""},
	{envRexctlSection, "rexctl", "rexctl and its install scripts look these up in the shell you run them from. A value in .env has no effect.", ""},
	{envStackSection, "Set by the compose stack", stack.ComposeFile + " sets these for the services from the settings above or to fixed values. A value in .env has no effect.", ""},
	{envInternalSection, "Not passed by the compose stack", "Fluxer's services, their images, some of the libraries they use and the Fluxer Helm chart also use these variables, but " + stack.ComposeFile + " doesn't pass them to any service. A value in .env has no effect.", ""},
}

var envInternalGroups = []envSection{
	{"hosted", "Hosted service only", "Only Fluxer's hosted service uses these.", ""},
	{"prices", "Stripe price IDs", "Set prices with FLUXER_STRIPE_PRICES or in the admin panel.", ""},
	{"cluster", "Gateway clustering", "The stack runs the gateway as one node.", ""},
	{"relay", "Push relay", "Only the push relay Fluxer runs for the mobile apps uses these.", ""},
	{"cassandra", "Cassandra", "The stack runs Postgres only.", ""},
	{"docs", "Docs site", "The docs site isn't part of the compose stack.", ""},
	{"dev", "Tests and development", "Only tests and development use these.", ""},
}

func envTopSection(id string) string {
	if slices.ContainsFunc(envInternalGroups, func(g envSection) bool { return g.id == id }) {
		return envInternalSection
	}
	return id
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
	files []string
}

func (d envDefault) String() string {
	if len(d.files) == 0 {
		return d.value
	}
	return d.value + " with " + orList(d.files)
}

func orList(items []string) string {
	if len(items) < 2 {
		return strings.Join(items, "")
	}
	return strings.Join(items[:len(items)-1], ", ") + " or " + items[len(items)-1]
}

type envDocList []envDoc

func (l envDocList) in(section string) []envDoc {
	var out []envDoc
	for _, d := range l {
		if d.section == section && d.bundled {
			out = append(out, d)
		}
	}
	return out
}

func (l envDocList) get(name string) envDoc {
	for _, d := range l {
		if d.name == name {
			return d
		}
	}
	return envDoc{}
}

func envDocs() envDocList {
	set := stack.Embedded()
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
	literals := composeLiterals(set)

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
		case v.section == envStackSection && literals[v.name] != "":
			d.note = "Docker Compose sets it to `" + literals[v.name] + "`."
		}
		d.text = strings.Join(notes, " ")
		out = append(out, d)
	}
	return out
}

var envUnbundledSections = []string{envRexctlSection, envStackSection, envInternalSection}

var envRotatableVars = []string{"FLUXER_S3_SECRET_KEY", "LIVEKIT_API_SECRET"}

var envNoRexctlVars = []string{"FLUXER_CADDY_SITE_ADDRESS", "FLUXER_AUTH_BLUESKY_CLIENT_URI"}

var envSizedNotes = map[string]string{
	"FLUXER_EDGE_PROTOCOLS": "On a machine with little memory, rexctl turns off HTTP/3 unless you set it.",
}

func composeValues(set *stack.Set) map[string][]string {
	data, err := stack.ReadFile(set, stack.ComposeFile)
	if err != nil {
		return nil
	}
	var file struct {
		Services map[string]struct {
			Environment map[string]any `yaml:"environment"`
		} `yaml:"services"`
	}
	if err := yaml.Unmarshal(data, &file); err != nil {
		return nil
	}
	out := map[string][]string{}
	for _, svc := range file.Services {
		for k, v := range svc.Environment {
			if s := fmt.Sprint(v); !slices.Contains(out[k], s) {
				out[k] = append(out[k], s)
			}
		}
	}
	return out
}

func composeLiterals(set *stack.Set) map[string]string {
	out := map[string]string{}
	for k, vs := range composeValues(set) {
		if len(vs) == 1 && vs[0] != "" && !strings.ContainsAny(vs[0], "$\n") {
			out[k] = vs[0]
		}
	}
	return out
}

const envPageHeader = `---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Environment variables
description: Every environment variable Fluxer and the Docker Compose bundle use.
sidebar:
  label: Environment variables
---

rexctl generates this page and the ` + "`.env.example`" + ` in the compose bundle of each release. Use the ` + "`.env.example`" + ` that came with your release. An entry for a setting rexctl manages ends with the ` + "`rexctl config set`" + ` command that changes it.
`

var (
	envNamePattern = regexp.MustCompile(`\b[A-Z][A-Z0-9]*_[A-Z0-9_]*[A-Z0-9]\b(=\S+)?`)
	envCodePattern = regexp.MustCompile(`\b(?:https?|wss?|s3|postgres)://[^\s,]*[^\s,.)]|(?:^|[\s(])\.env(?:\.example)?\b|\b[\w.-]+\.(?:yml|json|html)\b|(?:^|[\s(])/[\w./:-]*[\w/]`)
)

func EnvPage() string {
	docs := envDocs()
	var b strings.Builder
	b.WriteString(envPageHeader)
	for _, s := range envSections {
		var entries []envDoc
		for _, d := range docs {
			if d.section == s.id {
				entries = append(entries, d)
			}
		}
		if s.id == envSecretsSection {
			entries = envSecretOrder(entries)
		}
		if len(entries) == 0 && s.id != envFilesSection {
			continue
		}
		fmt.Fprintf(&b, "\n## %s\n", s.title)
		if intro := strings.TrimSpace(envMarkdown(s.intro) + " " + s.page); intro != "" {
			b.WriteString("\n" + intro + "\n")
		}
		switch s.id {
		case envComposeSection:
			b.WriteString("\n")
			for _, o := range envOverlays {
				fmt.Fprintf(&b, "- `%s` %s\n", stack.OverlayFiles[o.overlay], o.use)
			}
		case envFilesSection:
			b.WriteString("\n" + envMarkdown("These variables don't take the file form. Set them as plain values.") + "\n\n")
			for _, name := range envNoFileVars {
				fmt.Fprintf(&b, "- `%s`\n", name)
			}
			b.WriteString("\n" + envMarkdown(envNoFileNote) + "\n")
		}
		envPageEntries(&b, docs, entries, "###")
		if s.id != envInternalSection {
			continue
		}
		for _, g := range envInternalGroups {
			fmt.Fprintf(&b, "\n### %s\n\n%s\n", g.title, envMarkdown(g.intro))
			var group []envDoc
			for _, d := range docs {
				if d.section == g.id {
					group = append(group, d)
				}
			}
			envPageEntries(&b, docs, group, "####")
		}
	}
	return b.String()
}

func envPageEntries(b *strings.Builder, docs envDocList, entries []envDoc, heading string) {
	for _, d := range entries {
		if envLowerTwin(docs, d.name) {
			continue
		}
		if lower := docs.get(strings.ToLower(d.name)); lower.name != "" && lower.name != d.name {
			d.text = strings.TrimSpace(d.text + " " + lower.text + " " + lower.note)
		}
		fmt.Fprintf(b, "\n%s `%s`\n\n%s\n", heading, d.name, envPageText(d))
	}
}

const envNoFileNote = "The file form doesn't work for the docs site variables either, or for variables that system libraries and runtimes look up, such as NODE_OPTIONS, MALLOC_ARENA_MAX, TOKIO_WORKER_THREADS, HTTP_PROXY, SSL_CERT_FILE and VIPS_CONCURRENCY. The bundled third-party images follow their own rules."

func envLowerTwin(docs envDocList, name string) bool {
	upper := strings.ToUpper(name)
	return upper != name && docs.get(upper).name != ""
}

func envPageDefault(d envDoc) string {
	switch {
	case d.value != "" && d.setting != "":
		return "The default is `" + d.value + "`."
	case d.value != "" && !strings.Contains(d.text, "`"+d.value+"`"):
		return "The value is `" + d.value + "`."
	case d.value != "":
		return ""
	}
	if len(d.defaults) == 0 {
		return ""
	}
	lines := make([]string, len(d.defaults))
	for i, def := range d.defaults {
		lines[i] = "`" + def.value + "`"
		if len(def.files) > 0 {
			files := make([]string, len(def.files))
			for j, f := range def.files {
				files[j] = "`" + f + "`"
			}
			lines[i] += " with " + orList(files)
		}
		if size := envByteSize(d.name, def.value); size != "" {
			lines[i] += ", which is " + size
		}
	}
	return "The default is " + strings.Join(lines, ", or ") + "."
}

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

func envSecretOrder(entries []envDoc) []envDoc {
	var out []envDoc
	for _, g := range secretGroups() {
		for _, s := range g.keys {
			if i := slices.IndexFunc(entries, func(d envDoc) bool { return d.name == s.Key }); i >= 0 {
				out = append(out, entries[i])
			}
		}
	}
	for _, d := range entries {
		if !slices.ContainsFunc(out, func(o envDoc) bool { return o.name == d.name }) {
			out = append(out, d)
		}
	}
	return out
}

func envSecretNote(d envDoc) string {
	if !d.secret || strings.Contains(d.text, envKeepSecret) {
		return ""
	}
	return "Keep it secret."
}

const envKeepSecret = "Keep the value secret"

func envPageText(d envDoc) string {
	text := envMarkdown(d.text)
	if d.key != nil && envExamples[d.key.Name] != "" {
		first, rest, _ := strings.Cut(text, ". ")
		text = strings.TrimSpace(strings.TrimSuffix(first, ".") + ", such as `" + envExamples[d.key.Name] + "`. " + rest)
	}
	if def := envPageDefault(d); def != "" {
		text += " " + def
	}
	if d.note != "" {
		text += " " + envMarkdown(d.note)
	}
	if note := envSecretNote(d); note != "" {
		text += " " + note
	}
	if d.setting != "" {
		text += " With rexctl, use `rexctl config set " + d.setting + "`."
	}
	return text
}

func envMarkdown(text string) string {
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
	return mdx.Prose(text)
}

func outsideCode(text string, re *regexp.Regexp, fn func(string) string) string {
	parts := strings.Split(text, "`")
	for i := 0; i < len(parts); i += 2 {
		parts[i] = re.ReplaceAllStringFunc(parts[i], fn)
	}
	return strings.Join(parts, "`")
}
