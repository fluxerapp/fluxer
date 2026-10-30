// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"fmt"
	"regexp"
	"slices"
	"strings"
	"sync"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const envWidth = 78

var envRule = "# " + strings.Repeat("=", envWidth-2)

const referenceEnvNote = "This is a reference copy. Use the .env.example in the compose bundle of your Fluxer release."

var envOverlays = []struct {
	overlay stack.Overlay
	use     string
}{
	{stack.OverlayProxy, "for your own reverse proxy"},
	{stack.OverlayTunnel, "for a tunnel"},
	{stack.OverlayExternalObjectStore, "for your own object store"},
	{stack.OverlayIPv6, "for IPv6 on Fluxer's network"},
	{stack.OverlayExternalVoice, "for voice servers of your own without the bundled LiveKit"},
}

var envExamples = map[string]string{
	settings.Domain: "chat.example.com",
}

func BundleEnv() string {
	docs := envDocs()
	var b strings.Builder
	envBanner(&b)
	for _, s := range envSections {
		entries := docs.in(s.id)
		if len(entries) == 0 {
			continue
		}
		var list []string
		if s.id == envComposeSection {
			list = envOverlayList()
		}
		envFrame(&b, strings.ToUpper(s.title), s.intro, list)
		if s.id == envSecretsSection {
			envSecrets(&b, docs)
			continue
		}
		for i, d := range entries {
			if i > 0 {
				b.WriteString("\n")
			}
			envEntry(&b, d)
		}
	}
	return b.String()
}

func ReferenceEnv() string {
	var b strings.Builder
	b.WriteString(envRule + "\n")
	envComment(&b, "#  ", referenceEnvNote)
	b.WriteString(envRule + "\n\n")
	b.WriteString(BundleEnv())
	return b.String()
}

func envBanner(b *strings.Builder) {
	b.WriteString(envRule + "\n")
	b.WriteString("#  Fluxer settings for the Docker Compose bundle\n#\n")
	envComment(b, "#  ", "Copy this file to .env, fill in the required values and run `docker compose up -d`. "+
		"Settings that start with # are optional. To change one, remove the # and set a value.")
	b.WriteString("#\n")
	envComment(b, "#  ", "If you move this instance to rexctl later with rexctl takeover, "+
		"the rexctl line under each setting shows the command that changes it.")
	b.WriteString(envRule + "\n")
}

func envOverlayList() []string {
	width := 0
	for _, o := range envOverlays {
		width = max(width, len(stack.OverlayFiles[o.overlay]))
	}
	var out []string
	for _, o := range envOverlays {
		out = append(out, fmt.Sprintf("%-*s  %s", width, stack.OverlayFiles[o.overlay], o.use))
	}
	return out
}

func envFrame(b *strings.Builder, title, intro string, list []string) {
	b.WriteString("\n\n" + envRule + "\n#  " + title + "\n")
	if intro != "" {
		b.WriteString("#\n")
		envComment(b, "#  ", intro)
	}
	if len(list) > 0 {
		b.WriteString("#\n")
		for _, line := range list {
			b.WriteString("#    " + line + "\n")
		}
	}
	b.WriteString(envRule + "\n\n")
}

func envEntry(b *strings.Builder, d envDoc) {
	envComment(b, "# ", envDocText(d))
	if d.key != nil {
		if len(d.key.Options) > 0 {
			envField(b, "options", strings.Join(d.key.Options, ", "))
		}
		if ex := envExamples[d.key.Name]; ex != "" {
			envField(b, "example", ex)
		}
	}
	for i, line := range d.defaults {
		label := ""
		if i == 0 {
			label = "default"
		}
		envField(b, label, line.String())
	}
	if d.setting != "" {
		envField(b, "rexctl", "rexctl config set "+d.setting)
	}
	if d.active {
		fmt.Fprintf(b, "%s=%s\n", d.name, d.value)
	} else {
		fmt.Fprintf(b, "# %s=%s\n", d.name, d.value)
	}
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

func envField(b *strings.Builder, label, value string) {
	if label != "" {
		label += ":"
	}
	fmt.Fprintf(b, "#   %-10s%s\n", label, value)
}

func envComment(b *strings.Builder, prefix, text string) {
	parts := strings.Split(text, "`")
	for i := 1; i < len(parts); i += 2 {
		parts[i] = strings.ReplaceAll(parts[i], " ", "\x00")
	}
	line := prefix
	for _, word := range strings.Fields(strings.Join(parts, "")) {
		word = strings.ReplaceAll(word, "\x00", " ")
		if line != prefix && len(line)+1+len(word) > envWidth {
			b.WriteString(line + "\n")
			line = prefix
		}
		if line != prefix {
			line += " "
		}
		line += word
	}
	b.WriteString(line + "\n")
}

type secretGroup struct {
	kind     secrets.Kind
	stateful bool
	keys     []secrets.Spec
}

func secretGroups() []*secretGroup {
	var groups []*secretGroup
	for _, s := range secrets.Required {
		i := slices.IndexFunc(groups, func(g *secretGroup) bool { return g.kind == s.Kind && g.stateful == s.Stateful })
		if i < 0 {
			groups = append(groups, &secretGroup{kind: s.Kind, stateful: s.Stateful})
			i = len(groups) - 1
		}
		groups[i].keys = append(groups[i].keys, s)
	}
	return groups
}

func envSecrets(b *strings.Builder, docs envDocList) {
	for i, g := range secretGroups() {
		if i > 0 {
			b.WriteString("\n\n")
		}
		text, commands := secretHelp(g)
		envComment(b, "# ", text)
		for _, c := range commands {
			b.WriteString("#   " + c + "\n")
		}
		for _, s := range g.keys {
			b.WriteString("\n")
			d := docs.get(s.Key)
			envComment(b, "# ", d.text)
			if d.setting != "" {
				envField(b, "rexctl", "rexctl config set "+d.setting)
			}
			fmt.Fprintf(b, "%s=%s\n", s.Key, d.value)
		}
	}
}

func secretHelp(g *secretGroup) (string, []string) {
	each, keep, on := "each value", "them", "them"
	if len(g.keys) == 1 {
		each, keep, on = "the value", "it", "it"
	}
	text := ""
	var commands []string
	switch g.kind {
	case secrets.KindFixed:
		return "These have fixed values for the bundled services. Keep them as they are unless the setting tells you otherwise.", nil
	case secrets.KindHex32:
		commands = []string{"openssl rand -hex 32"}
	case secrets.KindBase64_32:
		commands = []string{"openssl rand -base64 32"}
	case secrets.KindRSA:
		commands = []string{
			"openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -outform DER |",
			"  base64 | tr -d '\\n'",
		}
	case secrets.KindAlnum64:
		commands = []string{"openssl rand -base64 96 | tr -dc A-Za-z0-9 | head -c 64"}
	case secrets.KindVAPID:
		text = "These commands print the public and then the private key of the VAPID key pair for browser push notifications."
		keep, on = "the pair", "it"
		commands = []string{
			"openssl ecparam -name prime256v1 -genkey -noout -out vapid.pem",
			"openssl ec -in vapid.pem -pubout -outform DER | tail -c 65 | base64 |",
			"  tr '/+' '_-' | tr -d '=\\n'",
			"openssl ec -in vapid.pem -outform DER | tail -c +8 | head -c 32 |",
			"  base64 | tr '/+' '_-' | tr -d '=\\n'",
			"rm vapid.pem",
		}
	}
	if text == "" {
		text = fmt.Sprintf("Generate %s with this command.", each)
	}
	if g.stateful {
		text += fmt.Sprintf(" Set %s once before the first start and never change %s.", keep, on)
	}
	return text, commands
}

type envDefaults []struct {
	file string
	defs map[string][]string
}

func newEnvDefaults(set *stack.Set) envDefaults {
	files := []string{stack.ComposeFile}
	for _, o := range envOverlays {
		files = append(files, stack.OverlayFiles[o.overlay])
	}
	var out envDefaults
	for _, name := range files {
		data, err := stack.ReadFile(set, name)
		if err != nil {
			continue
		}
		out = append(out, struct {
			file string
			defs map[string][]string
		}{name, composeDefaults([][]byte{data})})
	}
	return out
}

func (d envDefaults) lines(v string) []envDefault {
	var out []envDefault
	base := ""
	for i, f := range d {
		value := literalDefault(f.defs[v])
		switch {
		case i == 0:
			base = value
			if value != "" {
				out = append(out, envDefault{value: value})
			}
		case value != "" && value != base:
			j := slices.IndexFunc(out, func(e envDefault) bool { return e.value == value && len(e.files) > 0 })
			if j < 0 {
				out = append(out, envDefault{value: value})
				j = len(out) - 1
			}
			out[j].files = append(out[j].files, f.file)
		}
	}
	return out
}

func literalDefault(defs []string) string {
	if len(defs) != 1 || strings.Contains(defs[0], "$") {
		return ""
	}
	return defs[0]
}

var voiceSharedVars = []string{"FLUXER_LIVEKIT_IMAGE", "FLUXER_LIVEKIT_LOG_LEVEL", "FLUXER_LIVEKIT_STUN_PRIMARY", "FLUXER_LIVEKIT_STUN_SECONDARY", "FLUXER_LIVEKIT_TURN_SERVERS"}

var voiceRequiredVars = []string{"FLUXER_VOICE_ID", "FLUXER_VOICE_API_KEY", "FLUXER_VOICE_API_SECRET", "FLUXER_VOICE_WEBHOOK_URL"}

var composeDefaultPattern = regexp.MustCompile(`\$\{([A-Z][A-Z0-9_]*):-([^}$]*)\}`)

func voiceDefaults() map[string]string {
	out := map[string]string{}
	set := stack.Embedded()
	for _, name := range []string{"voice/voice.compose.yml", "voice/voice-http.compose.yml", "voice/voice-edge.compose.yml"} {
		data, err := stack.ReadFile(set, name)
		if err != nil {
			continue
		}
		for _, m := range composeDefaultPattern.FindAllStringSubmatch(string(data), -1) {
			if _, ok := out[m[1]]; !ok {
				out[m[1]] = m[2]
			}
		}
	}
	return out
}

func VoiceBundleEnv() string {
	return voiceEnvFile("Settings for one standalone voice server",
		"Copy this file to <id>.env, fill in the required values and start the server with `docker compose -p fluxer-voice-<id> --env-file <id>.env -f voice.compose.yml up -d`. "+
			"Each server needs its own settings file, project name, key and ports. Settings that start with # are optional.",
		voiceRequiredVars, envVoiceServerSection, voiceSharedVars)
}

func VoiceEdgeBundleEnv() string {
	return voiceEnvFile("Settings for the voice edge",
		"Copy this file to edge.env and start the edge with `docker compose -p fluxer-voice-edge --env-file edge.env -f voice-edge.compose.yml up -d`. "+
			"Every setting is optional.",
		nil, envVoiceEdgeSection, nil)
}

func voiceEnvFile(title, intro string, required []string, section string, shared []string) string {
	docs := envDocs()
	defaults := voiceDefaults()
	var b strings.Builder
	b.WriteString(envRule + "\n")
	b.WriteString("#  " + title + "\n#\n")
	envComment(&b, "#  ", intro)
	b.WriteString(envRule + "\n")
	entry := func(name string, active bool) {
		d := docs.get(name)
		if d.name == "" {
			return
		}
		d.setting, d.active, d.defaults = "", active, nil
		if v, ok := defaults[name]; ok && v != "" {
			d.defaults = []envDefault{{value: v}}
		}
		envEntry(&b, d)
	}
	if len(required) > 0 {
		envFrame(&b, "REQUIRED", "", nil)
		for i, name := range required {
			if i > 0 {
				b.WriteString("\n")
			}
			entry(name, true)
		}
	}
	envFrame(&b, "OPTIONAL", "", nil)
	first := true
	for _, d := range docs.inAll(section) {
		if slices.Contains(required, d.name) {
			continue
		}
		if !first {
			b.WriteString("\n")
		}
		first = false
		entry(d.name, false)
	}
	for _, name := range shared {
		b.WriteString("\n")
		entry(name, false)
	}
	return b.String()
}
