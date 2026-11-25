// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"maps"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

const SetupTokenVar = "FLUXER_SETUP_TOKEN"

var legacyImageVars = []string{"FLUXER_IMAGE_TAG", "FLUXER_REGISTRY", "FLUXER_REGISTRY_OWNER"}

func equalsDefault(defs []string, v string, env map[string]string) bool {
	if len(defs) == 0 {
		return false
	}
	for _, d := range defs {
		got, ok := interpolate(d, env)
		if !ok || got != v {
			return false
		}
	}
	return true
}

func isGeneratedSecret(name string) bool {
	for _, s := range secrets.Required {
		if s.Key == name {
			return true
		}
	}
	return name == SetupTokenVar
}

func twinsToAdmin(env map[string]string, translate bool) map[string]string {
	admin := map[string]string{}
	const (
		delivery = "integrations.email.delivery"
		tlsMode  = "integrations.email.smtp.tls_mode"
	)
	enabled, hasEnabled := env["FLUXER_EMAIL_ENABLED"]
	provider, hasProvider := env["FLUXER_EMAIL_PROVIDER"]
	if v, ok := env["FLUXER_EMAIL_DELIVERY"]; ok {
		admin[delivery] = strings.ToLower(strings.TrimSpace(v))
	} else if translate && (hasEnabled || hasProvider) {
		on, _ := strconv.ParseBool(strings.TrimSpace(enabled))
		if on && strings.EqualFold(strings.TrimSpace(provider), "smtp") {
			admin[delivery] = "smtp"
		} else {
			admin[delivery] = "off"
		}
	}
	if v, ok := env["FLUXER_EMAIL_SMTP_TLS_MODE"]; ok {
		admin[tlsMode] = strings.ToLower(strings.TrimSpace(v))
	} else if v, ok := env["FLUXER_EMAIL_SMTP_SECURE"]; ok && translate {
		if mode := tlsFromSecure(v, env["FLUXER_EMAIL_SMTP_PORT"]); mode != "" {
			admin[tlsMode] = mode
		}
	}
	for name, key := range settings.Twins {
		v, ok := env[name]
		if !ok || slices.Contains(legacyEmailVars, name) {
			continue
		}
		delete(env, name)
		if key == delivery || key == tlsMode {
			continue
		}
		admin[key] = v
	}
	return admin
}

var legacyEmailVars = []string{"FLUXER_EMAIL_ENABLED", "FLUXER_EMAIL_PROVIDER", "FLUXER_EMAIL_SMTP_SECURE"}

func tlsFromSecure(secure, port string) string {
	on, err := strconv.ParseBool(strings.TrimSpace(secure))
	if err != nil {
		return ""
	}
	if !on {
		return "opportunistic"
	}
	if p := strings.TrimSpace(port); p == "" || p == "465" {
		return "implicit"
	}
	return "starttls"
}

func parseEnv(data []byte) (map[string]string, bool, error) {
	f, err := envfile.Parse("", data)
	deduped := false
	if err != nil {
		if f == nil || len(f.Duplicates()) == 0 {
			return nil, false, err
		}
		deduped = true
	}
	env := f.Map()
	lookup := func(key string) (string, bool) {
		v, ok := env[key]
		return v, ok
	}
	for _, k := range slices.Sorted(maps.Keys(env)) {
		v := env[k]
		if !strings.Contains(v, "${") {
			continue
		}
		if expanded, ok := envfile.Expand(v, lookup); ok && !strings.Contains(expanded, "$") {
			env[k] = expanded
		}
	}
	return env, deduped, nil
}
