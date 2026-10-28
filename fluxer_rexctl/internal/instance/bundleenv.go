// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"fmt"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const envWidth = 78

var envRule = "# " + strings.Repeat("=", envWidth-2)

type envSection struct {
	prefix string
	title  string
	intro  string
}

var envSections = []envSection{
	{"edge.", "EDGE AND HTTPS",
		"By default Fluxer listens on ports 80 and 443 and gets its own HTTPS certificate for your domain."},
	{"voice.", "VOICE AND VIDEO",
		"Calls connect directly to this machine on these ports, even when Fluxer is behind your own proxy."},
	{"storage.", "OBJECT STORAGE",
		"Leave these unset to use the bundled object store. To use your own S3-compatible store, set them and add " +
			stack.OverlayFiles[stack.OverlayExternalObjectStore] + " to COMPOSE_FILE."},
	{"network.", "OUTGOING PROXY", ""},
	{"sso.", "SINGLE SIGN-ON", ""},
	{settings.ImagesPrefix, "IMAGES",
		"Set one to use your own mirror or another version. Fluxer's own images are pinned to the release in " +
			stack.ComposeFile + "."},
}

var envOverlays = []struct {
	overlay stack.Overlay
	use     string
}{
	{stack.OverlayProxy, "for your own reverse proxy"},
	{stack.OverlayTunnel, "for a tunnel"},
	{stack.OverlayExternalObjectStore, "for your own object store"},
	{stack.OverlayIPv6, "for IPv6 on Fluxer's network"},
}

var envNotes = map[string]string{
	settings.EdgeHTTPSPort: "If people connect to this port directly, set FLUXER_PUBLIC_PORT to the same port.",
	settings.EdgeTrusted:   "Separate entries with spaces.",
	settings.VoicePublicIP: "Set FLUXER_LIVEKIT_NODE_IP to the address and FLUXER_LIVEKIT_USE_EXTERNAL_IP to false.",
	settings.NoProxy:       "Separate hosts with commas.",
}

var envExamples = map[string]string{
	settings.Domain: "chat.example.com",
}

func BundleEnv() string {
	set := stack.Embedded()
	defaults := newEnvDefaults(set)
	required := map[string]bool{}
	for _, s := range secrets.Required {
		required[s.Key] = true
	}
	var domain settings.Key
	bySection := make([][]settings.Key, len(envSections))
	for _, k := range settings.Named() {
		if len(k.Vars) == 0 || !releaseUses(set, k) {
			continue
		}
		if k.Name == settings.Domain {
			domain = k
			continue
		}
		for i, s := range envSections {
			if strings.HasPrefix(k.Name, s.prefix) {
				bySection[i] = append(bySection[i], k)
				break
			}
		}
	}

	var b strings.Builder
	envBanner(&b)

	envFrame(&b, "REQUIRED", "Fill these in before the first start.")
	envComment(&b, "# ", fmt.Sprintf("%s refuses to start without this line. Keep it as it is.", stack.ComposeFile))
	fmt.Fprintf(&b, "%s=true\n\n", stack.SettingsLoadedVar)
	envSetting(&b, domain, defaults, required, true)

	envFrame(&b, "FIRST ADMIN ACCOUNT", "")
	b.WriteString("# setup token\n")
	envComment(&b, "#   ", "The account that signs up with this token becomes Fluxer's first admin. "+
		"Use 32 to 256 characters, such as the output of openssl rand -hex 32. "+
		"If you leave it empty, the api service makes a token and writes it to its log once, on its first start. "+
		"docker compose logs api shows it.")
	envField(&b, "type", "secret")
	fmt.Fprintf(&b, "%s=\n", SetupTokenVar)

	envFrame(&b, "SECRETS", "Fill in each empty value before the first start, and keep a copy of .env somewhere safe.")
	envSecrets(&b)

	for i, s := range envSections {
		if len(bySection[i]) == 0 {
			continue
		}
		envFrame(&b, s.title, s.intro)
		for j, k := range bySection[i] {
			if j > 0 {
				b.WriteString("\n")
			}
			envSetting(&b, k, defaults, required, false)
		}
	}
	return b.String()
}

func releaseUses(set *stack.Set, k settings.Key) bool {
	for _, v := range k.Vars {
		if !slices.Contains(set.Vars, v) {
			return false
		}
	}
	return true
}

func envBanner(b *strings.Builder) {
	b.WriteString(envRule + "\n")
	b.WriteString("#  Fluxer settings for the Docker Compose bundle\n#\n")
	envComment(b, "#  ", "Copy this file to .env, fill in the required values and run docker compose up -d. "+
		"Settings that start with # are optional and use their default until you remove the #.")
	b.WriteString("#\n")
	envComment(b, "#  ", fmt.Sprintf("The optional compose files change how Fluxer runs. To use one, list it after %s in COMPOSE_FILE, "+
		"such as COMPOSE_FILE=%s:%s.", stack.ComposeFile, stack.ComposeFile, stack.OverlayFiles[stack.OverlayProxy]))
	b.WriteString("#\n")
	width := 0
	for _, o := range envOverlays {
		width = max(width, len(stack.OverlayFiles[o.overlay]))
	}
	for _, o := range envOverlays {
		fmt.Fprintf(b, "#    %-*s  %s\n", width, stack.OverlayFiles[o.overlay], o.use)
	}
	b.WriteString("#\n")
	envComment(b, "#  ", "If you move this instance to rexctl later with rexctl takeover, "+
		"the rexctl line under each setting shows the command that changes it.")
	b.WriteString(envRule + "\n")
}

func envFrame(b *strings.Builder, title, intro string) {
	b.WriteString("\n\n" + envRule + "\n#  " + title + "\n")
	if intro != "" {
		b.WriteString("#\n")
		envComment(b, "#  ", intro)
	}
	b.WriteString(envRule + "\n\n")
}

func envSetting(b *strings.Builder, k settings.Key, defaults envDefaults, required map[string]bool, active bool) {
	help := k.Help + "."
	if note := envNotes[k.Name]; note != "" {
		help += " " + note
	}
	elsewhere := len(k.Vars) == 1 && required[k.Vars[0]] && !active
	if elsewhere {
		help += fmt.Sprintf(" For your own store, change %s under SECRETS above.", k.Vars[0])
	}
	b.WriteString("# " + k.Name + "\n")
	envComment(b, "#   ", help)
	envField(b, "type", k.Type)
	if len(k.Options) > 0 {
		envField(b, "options", strings.Join(k.Options, ", "))
	}
	if ex := envExamples[k.Name]; ex != "" {
		envField(b, "example", ex)
	}
	if len(k.Vars) == 1 {
		for i, d := range defaults.lines(k.Vars[0]) {
			if i == 0 {
				envField(b, "default", d)
			} else {
				envField(b, "", d)
			}
		}
	}
	envField(b, "rexctl", "rexctl config set "+k.Name)
	if elsewhere {
		return
	}
	for _, v := range k.Vars {
		if active {
			fmt.Fprintf(b, "%s=\n", v)
		} else {
			fmt.Fprintf(b, "# %s=\n", v)
		}
	}
}

func envField(b *strings.Builder, label, value string) {
	if label != "" {
		label += ":"
	}
	fmt.Fprintf(b, "#   %-10s%s\n", label, value)
}

func envComment(b *strings.Builder, prefix, text string) {
	line := prefix
	for _, word := range strings.Fields(text) {
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

func envSecrets(b *strings.Builder) {
	var groups []*secretGroup
	for _, s := range secrets.Required {
		i := slices.IndexFunc(groups, func(g *secretGroup) bool { return g.kind == s.Kind && g.stateful == s.Stateful })
		if i < 0 {
			groups = append(groups, &secretGroup{kind: s.Kind, stateful: s.Stateful})
			i = len(groups) - 1
		}
		groups[i].keys = append(groups[i].keys, s)
	}
	for i, g := range groups {
		if i > 0 {
			b.WriteString("\n")
		}
		text, commands := secretHelp(g)
		envComment(b, "# ", text)
		for _, c := range commands {
			b.WriteString("#   " + c + "\n")
		}
		for _, s := range g.keys {
			value := ""
			if s.Kind == secrets.KindFixed {
				value = s.Fixed
			}
			fmt.Fprintf(b, "%s=%s\n", s.Key, value)
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
		return "These have fixed values. Keep them as they are unless a setting below tells you to change one.", nil
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
		text = "This is the key pair for push notifications. These commands print the public key and then the private key."
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
		text += fmt.Sprintf(" Do this once before the first start and keep %s the same after that, because your stored data depends on %s.", keep, on)
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

func (d envDefaults) lines(v string) []string {
	var out []string
	base := ""
	for i, f := range d {
		value := literalDefault(f.defs[v])
		switch {
		case i == 0:
			base = value
			if value != "" {
				out = append(out, value)
			}
		case value != "" && value != base:
			out = append(out, value+" with "+f.file)
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
