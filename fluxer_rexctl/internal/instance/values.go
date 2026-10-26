// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"maps"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const SetupTokenVar = "FLUXER_SETUP_TOKEN"

var legacyImageVars = []string{"FLUXER_IMAGE_TAG", "FLUXER_REGISTRY", "FLUXER_REGISTRY_OWNER"}

type valueInput struct {
	env      map[string]string
	set      *stack.Set
	custom   []byte
	overlays []stack.Overlay
	examples map[string]string
	proxies  engine.Proxies
}

type valueResult struct {
	settings map[string]string
	secrets  map[string]string
	admin    map[string]string

	dropped       []string
	defaults      []string
	placeholders  []string
	engineProxies bool
}

func importValues(in valueInput) (*valueResult, error) {
	env := maps.Clone(in.env)
	if env == nil {
		env = map[string]string{}
	}
	r := &valueResult{
		settings: map[string]string{},
		secrets:  map[string]string{},
		admin:    map[string]string{},
	}
	for k := range env {
		if strings.HasPrefix(k, "COMPOSE_") || k == stack.SettingsLoadedVar {
			delete(env, k)
		}
	}

	read, err := settings.Vars(in.set, in.custom)
	if err != nil {
		return nil, err
	}
	original := maps.Clone(env)
	defaults := composeDefaults(setFiles(in.set, in.custom))
	for _, k := range slices.Sorted(maps.Keys(env)) {
		v := env[k]
		if strings.TrimSpace(v) == "" {
			delete(env, k)
			continue
		}
		if isImageVar(k) || isMemoryVar(in.set, k) {
			continue
		}
		if equalsDefault(defaults[k], v, original) {
			delete(env, k)
			r.defaults = append(r.defaults, k)
			continue
		}
		if ex, ok := in.examples[k]; ok && ex == v && !isGeneratedSecret(k) && isPlaceholderValue(v) {
			r.placeholders = append(r.placeholders, k)
		}
	}

	for _, s := range secrets.Required {
		if v, ok := env[s.Key]; ok {
			if !secrets.IsUnset(v) {
				r.secrets[s.Key] = v
			}
			delete(env, s.Key)
		}
	}
	if v, ok := env[SetupTokenVar]; ok {
		r.secrets[SetupTokenVar] = v
		delete(env, SetupTokenVar)
	}

	r.admin = twinsToAdmin(env, false)

	r.engineProxies = importProxies(env, in.proxies, r.settings)
	if r.engineProxies {
		for k, v := range map[string]string{"HTTP_PROXY": in.proxies.HTTP, "HTTPS_PROXY": in.proxies.HTTPS, "NO_PROXY": in.proxies.No} {
			original[k] = v
		}
	}

	named := map[string]settings.Key{}
	for _, k := range settings.Named() {
		for _, v := range k.Vars {
			named[v] = k
		}
	}
	external := slices.Contains(in.overlays, stack.OverlayExternalObjectStore)
	tunnel := slices.Contains(in.overlays, stack.OverlayTunnel)
	for _, k := range slices.Sorted(maps.Keys(env)) {
		v := env[k]
		key, isNamed := named[k]
		switch {
		case isImageVar(k):
			r.settings[k] = v
		case tunnel && k == tunnelBindVar && slices.Contains(in.set.Vars, edgeBindVar):
			r.settings[settings.EdgeBind] = v
		case tunnel && k == tunnelBindVar:
			r.settings[k] = v
		case isNamed && key.Vars[0] == k && (key.Name != settings.StorageEndpoint || external) &&
			(key.Name != settings.VoicePublicIP || beforeValue(original, defaults, "FLUXER_LIVEKIT_USE_EXTERNAL_IP") == "false"):
			r.settings[key.Name] = v
		case slices.Contains(read, k):
			r.settings[k] = v
		default:
			r.dropped = append(r.dropped, k)
		}
	}

	for _, o := range in.overlays {
		switch o {
		case stack.OverlayProxy, stack.OverlayTunnel:
			r.settings[settings.EdgeMode] = string(o)
		case stack.OverlayIPv6:
			r.settings[settings.IPv6] = "true"
		}
	}
	if err := dropRedundant(in.set, r); err != nil {
		return nil, err
	}
	if err := preserve(in.set, r, original, read, defaults); err != nil {
		return nil, err
	}
	return r, nil
}

func preserve(set *stack.Set, r *valueResult, before map[string]string, read []string, defaults map[string][]string) error {
	named := map[string]bool{}
	var publicPort settings.Key
	for _, k := range settings.Named() {
		for _, v := range k.Vars {
			named[v] = true
		}
		if k.Name == settings.EdgePublicPort {
			publicPort = k
			named[k.Vars[0]] = false
		}
	}
	for range 3 {
		after, err := settings.Effective(set, r.settings, r.secrets, r.admin)
		if err != nil {
			return err
		}
		changed := false
		for _, v := range read {
			if named[v] || v == stack.SettingsLoadedVar || isGeneratedSecret(v) || settings.Twins[v] != "" || isProxyVar(v) {
				continue
			}
			b, a := before[v], after[v]
			if equivalent(b, a, defaults[v], after) {
				continue
			}
			changed = true
			if v == publicPort.Vars[0] {
				if port := beforeValue(before, defaults, v); publicPort.Validate(port) == nil {
					r.settings[publicPort.Name] = port
					continue
				}
			}
			r.settings[v] = b
		}
		if !changed {
			return nil
		}
	}
	return nil
}

func equivalent(before, after string, defs []string, env map[string]string) bool {
	switch {
	case before == after:
		return true
	case before == "":
		return equalsDefault(defs, after, env)
	case after == "":
		return equalsDefault(defs, before, env)
	}
	return false
}

func isProxyVar(v string) bool {
	switch strings.ToUpper(v) {
	case "HTTP_PROXY", "HTTPS_PROXY", "NO_PROXY":
		return true
	}
	return false
}

func beforeValue(env map[string]string, defaults map[string][]string, name string) string {
	if v := env[name]; v != "" {
		return v
	}
	if defs := defaults[name]; len(defs) > 0 {
		if v, ok := interpolate(defs[0], env); ok {
			return v
		}
	}
	return ""
}

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

func isPlaceholderValue(v string) bool {
	lower := strings.ToLower(strings.TrimSpace(v))
	return strings.Contains(lower, "example.") || lower == "change_me" || lower == "changeme"
}

func isGeneratedSecret(name string) bool {
	for _, s := range secrets.Required {
		if s.Key == name {
			return true
		}
	}
	return name == SetupTokenVar
}

func isImageVar(name string) bool {
	if slices.Contains(legacyImageVars, name) {
		return true
	}
	return strings.HasPrefix(name, "FLUXER_") && strings.HasSuffix(name, "_IMAGE")
}

func isMemoryVar(set *stack.Set, name string) bool {
	return slices.Contains(stack.ProfileKeys(set), name) || slices.Contains(stack.ProfileKeys(stack.Embedded()), name)
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

func importProxies(env map[string]string, engineProxies engine.Proxies, out map[string]string) bool {
	keys := []struct{ key, upper string }{
		{settings.HTTPProxy, "HTTP_PROXY"},
		{settings.HTTPSProxy, "HTTPS_PROXY"},
		{settings.NoProxy, "NO_PROXY"},
	}
	found := false
	for _, k := range keys {
		lower := strings.ToLower(k.upper)
		v := env[k.upper]
		if v == "" {
			v = env[lower]
		}
		delete(env, k.upper)
		delete(env, lower)
		if v != "" {
			out[k.key] = v
			found = true
		}
	}
	if found {
		return false
	}
	fromEngine := map[string]string{settings.HTTPProxy: engineProxies.HTTP, settings.HTTPSProxy: engineProxies.HTTPS, settings.NoProxy: engineProxies.No}
	used := false
	for k, v := range fromEngine {
		if v != "" {
			out[k] = v
			used = true
		}
	}
	return used
}

func dropRedundant(set *stack.Set, r *valueResult) error {
	full, err := settings.Effective(set, r.settings, r.secrets, r.admin)
	if err != nil {
		return err
	}
	for _, k := range slices.Sorted(maps.Keys(r.settings)) {
		envVar := k
		if k == settings.EdgePublicPort {
			envVar = "FLUXER_PUBLIC_PORT"
		} else if !isRawVar(k) || isImageVar(k) || isMemoryVar(set, k) {
			continue
		}
		v := r.settings[k]
		delete(r.settings, k)
		without, err := settings.Effective(set, r.settings, r.secrets, r.admin)
		if err != nil {
			return err
		}
		if got, ok := without[envVar]; !ok || got != v || full[envVar] != v {
			r.settings[k] = v
		}
	}
	return nil
}

func isRawVar(name string) bool {
	return name != "" && strings.ToUpper(name) == name && !strings.ContainsAny(name, ".- ")
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
