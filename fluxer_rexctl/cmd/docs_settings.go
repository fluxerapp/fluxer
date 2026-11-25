// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"cmp"
	"fmt"
	"regexp"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/mdx"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const settingsHeader = `---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Settings reference
description: Lists every Fluxer setting, each spelled the same on every way of running it.
---

Every setting has one name, whichever way you run Fluxer. A setting without a default is off, or Fluxer works out its value from the other settings.

## Where settings go

| How you run Fluxer | Where settings go |
| --- | --- |
| Docker without rexctl | ` + "`fluxer.env`" + ` |
| systemd without rexctl | ` + "`/etc/fluxer/fluxer.env`" + ` |
| Kubernetes | Under ` + "`config`" + ` in the Helm chart |
| rexctl | [` + "`rexctl config set`" + `](/operator/configuration/) |
`

type settingsSection struct {
	id, title, intro string
}

var settingsSections = []settingsSection{
	{"required", "Required", "Only this setting is required. Fluxer makes its own passwords and keys when it first starts."},
	{"admin", "First admin account", "Fluxer creates the setup token on its first start. `fluxerd setup-token` prints it."},
	{"secrets", "Passwords and keys", "Fluxer creates these on its first start and keeps them in its data folder, so backups have them too. Set one only to replace the value Fluxer made, and treat every backup as secret."},
	{"address", "Public address", "Every address comes from `FLUXER_DOMAIN`. Set these only for a part on another address."},
	{"edge", "Edge and HTTPS", "By default Fluxer listens on ports 80 and 443 and gets its own certificate for your domain. Behind your own proxy or a tunnel, [turn on proxy mode](/operator/reverse-proxy/#turn-on-proxy-mode)."},
	{"voice", "Voice and video", "Calls connect to the machine on `FLUXER_LIVEKIT_TCP_PORT` and `FLUXER_LIVEKIT_UDP_PORT`, also when Fluxer is behind your own proxy."},
	{"backups", "Backups", "Fluxer backs up its database before every upgrade. These add regular backups, which Fluxer keeps in the `backups` folder of its data folder. [Copy them off the machine](/operator/backups/#copy-backups-off-the-machine), because they are on the same disk as the instance."},
	{"updates", "Updates", "Fluxer doesn't look for new releases by itself until you turn the check on. Then it checks once a day and shows a newer release in the admin panel, the log and `fluxerd version`. The request [holds no instance ID, domain, version, counts or cookies](/operator/upgrades/#turning-the-check-on)."},
	{"storage", "File storage", "A new instance keeps uploads as plain files in its data folder. To use an S3-compatible store, set its address, region and buckets, and put its access key and secret key in `FLUXER_S3_ACCESS_KEY` and `FLUXER_S3_SECRET_KEY`."},
	{"database", "Database", "Fluxer runs its own PostgreSQL unless you set `FLUXER_POSTGRES_URL` for a database and role that already exist, on PostgreSQL 14 or later."},
	{"services", "Cache, queue and search", ""},
	{"network", "Outgoing proxy", ""},
	{"sso", "Sign-in", "The admin panel can also save the single sign-on settings, and a value saved there wins over the ones here. [Set up single sign-on](/operator/sign-in/#single-sign-on)."},
	{"email", "Email", "The admin panel can also save the delivery mode, the sender and the SMTP server. A value saved there wins. [Set up email with a mail provider](/operator/email/)."},
	{"push", "Push notifications", ""},
	{"media", "Media", ""},
	{"gateway", "Gateway", ""},
	{"app", "Web app", ""},
	{"api", "API and worker", ""},
	{"instance", "Instance and branding", "Fluxer uses the product name, images, theme colour and status page links set here only until the setup wizard finishes or someone saves branding in the admin panel. After that it uses the values saved in the admin panel."},
	{"billing", "Billing", "The admin panel can also save whether billing is on, the Stripe keys and the prices. A value saved there is used first, and a saved price list replaces all of `FLUXER_STRIPE_PRICES`."},
	{"integrations", "Integrations", "The admin panel can also save the Bluesky settings and keys, and its values win."},
	{"tuning", "Memory and performance", "rexctl sizes most of these from the memory and CPUs of the machine. Without rexctl, Fluxer sizes its threads from the CPUs it can use each time it starts and uses the defaults below for the rest. A value you set here replaces the size they work out."},
	{"bundled", "Older storage and cache services", "Only installs with Valkey or SeaweedFS read these."},
	{"internal", "Internal ports", "Every part inside the instance listens on [fixed ports](/operator/how-it-works/#ports-inside-the-instance) on one loopback address."},
	{"operations", "Logs and health checks", ""},
}

type fluxerdSetting struct {
	name, section, def, text string
}

var fluxerdSettings = []fluxerdSetting{
	{"FLUXER_EDGE_MODE", "edge", "bundled", "How web traffic gets to Fluxer. `bundled` serves HTTPS on ports 80 and 443 with its own certificate. `proxy` and `tunnel` serve plain HTTP on port 8080 for your own reverse proxy or tunnel."},
	{"FLUXER_LOOPBACK_ADDRESS", "internal", "127.0.0.1", "The loopback address the internal parts listen on, from `127.0.0.1` to `127.255.255.254`. rexctl gives each extra instance on a machine its own."},
}

var (
	settingsNoDefault = []string{"://", "${"}
	settingsSkip      = regexp.MustCompile(`_HEALTHCHECK_|^FLUXER_RESTART_POLICY$|^FLUXER_API_EMBEDDED_WORKER$|^FLUXER_(NATS|NATS_JETSTREAM|SVC_NATS)_URL$|^FLUXER_NATS_GOMEMLIMIT$`)
)

func renderSettings(t mdx.Terms) ([]byte, error) {
	refs := instance.SettingsReference(t)
	catalog := settings.Catalog()
	short := map[string]string{}
	for _, k := range settings.Named() {
		if len(k.Vars) > 0 && !k.Hidden && !settings.DockerOnly(k.Name) {
			short[k.Vars[0]] = k.Name
		}
	}
	rows := map[string][]string{}
	var missing []string
	for _, s := range catalog {
		if settingsSkip.MatchString(s.Name) {
			continue
		}
		ref, ok := refs[s.Name]
		if !ok || ref.Text == "" {
			missing = append(missing, s.Name)
			continue
		}
		if !slices.ContainsFunc(settingsSections, func(sec settingsSection) bool { return sec.id == ref.Section }) {
			continue
		}
		text := ref.Text
		if (ref.Secret || s.Secret) && !strings.Contains(text, "secret.") {
			text += " Keep it secret."
		}
		if s.PerCPU {
			text += " Fluxer sizes it from the CPUs it can use."
		}
		if name := short[s.Name]; name != "" {
			text += " rexctl also calls it `" + name + "`."
		}
		rows[ref.Section] = append(rows[ref.Section], settingsRow(t, s.Name, settingsDefault(s), text))
	}
	if len(missing) > 0 {
		return nil, fmt.Errorf("%s have no description for the settings page in internal/instance/envvars.go", strings.Join(missing, ", "))
	}
	for _, f := range fluxerdSettings {
		if slices.ContainsFunc(catalog, func(s stack.Setting) bool { return s.Name == f.name }) {
			continue
		}
		rows[f.section] = append(rows[f.section], settingsRow(t, f.name, "`"+f.def+"`", f.text))
	}
	var b bytes.Buffer
	b.WriteString(settingsHeader)
	for _, s := range settingsSections {
		if len(rows[s.id]) == 0 {
			continue
		}
		fmt.Fprintf(&b, "\n## %s\n\n", s.title)
		if s.intro != "" {
			b.WriteString(mdx.Prose(mdx.Code(s.intro, t)) + "\n\n")
		}
		b.WriteString("<dl class=\"flx-settings\">\n")
		slices.Sort(rows[s.id])
		for _, row := range rows[s.id] {
			b.WriteString(row)
		}
		b.WriteString("</dl>\n")
	}
	b.WriteString("\n## Values from files\n\n")
	b.WriteString(mdx.Prose(mdx.Code("Every setting also takes its value from a file, such as a Docker or Kubernetes secret.\n\n1. Set NAME_FILE to the path of the file.\n2. Leave `NAME` unset. Fluxer refuses to start when both have a value.\n3. Let the account Fluxer runs as read the file.", t)) + "\n")
	return b.Bytes(), nil
}

func settingsRow(t mdx.Terms, name, def, text string) string {
	var b strings.Builder
	fmt.Fprintf(&b, "<div class=\"flx-setting\" id=\"%s\">\n<dt>`%s`</dt>\n", strings.ToLower(name), name)
	if def != "" {
		fmt.Fprintf(&b, "<dd class=\"flx-setting-default\">Default %s</dd>\n", def)
	}
	fmt.Fprintf(&b, "<dd>%s</dd>\n</div>\n", mdx.Prose(mdx.Code(text, t)))
	return b.String()
}

func settingsDefault(s stack.Setting) string {
	s.Default = cmp.Or(s.Sized, s.Default)
	if s.PerCPU || s.Default == "" || slices.ContainsFunc(settingsNoDefault, func(m string) bool { return strings.Contains(s.Default, m) }) {
		return ""
	}
	out := "`" + s.Default + "`"
	if size := instance.ByteSize(s.Name, s.Default); size != "" {
		out += ", " + size
	}
	return out
}
