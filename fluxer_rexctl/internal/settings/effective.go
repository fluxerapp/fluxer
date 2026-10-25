// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"fmt"
	"net"
	"slices"
	"strconv"
	"strings"
	"sync"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func Effective(set *stack.Set, chosen, secrets, adminImport map[string]string) (map[string]string, error) {
	env := map[string]string{}
	reads := func(v string) bool { return slices.Contains(set.Vars, v) }
	put := func(v, value string) {
		if reads(v) {
			env[v] = value
		}
	}

	fixed := map[string]string{}
	for name, value := range chosen {
		if isRawName(name) {
			fixed[name] = value
		}
	}
	if len(set.Profiles) > 0 {
		for v, value := range stack.Sizing(Hardware(), chosen[MemoryProfile] == "", fixed) {
			put(v, value)
		}
	}

	if name := chosen[MemoryProfile]; name != "" {
		values, ok := set.Profiles[name]
		if !ok {
			return nil, &Error{Key: MemoryProfile, Problem: fmt.Sprintf("Fluxer %s has no memory profile called %s.", set.Release, name),
				Fix: "Run rexctl config unset memory.profile."}
		}
		for v, value := range values {
			put(v, value)
		}
	}

	if reads(stack.EmbeddedWorkerVar) {
		for v, value := range stack.FoldWorker(Hardware(), env, fixed) {
			put(v, value)
		}
	}

	for v, value := range derived(chosen) {
		put(v, value)
	}

	operator := map[string]bool{}
	for name, value := range chosen {
		k, ok := namedKey(name)
		if !ok {
			continue
		}
		for v, value := range namedVars(k, value) {
			if reads(v) {
				env[v] = value
				operator[v] = true
			}
		}
	}
	for name, value := range chosen {
		if isRawName(name) {
			env[name] = value
			operator[name] = true
		}
	}

	for adminKey, value := range adminImport {
		if v, ok := TwinVar(adminKey); ok {
			put(v, value)
		}
	}

	for v, value := range secrets {
		if !operator[v] {
			put(v, value)
		}
	}

	put(stack.SettingsLoadedVar, "true")

	if registry := chosen[ImagesRegistry]; registry != "" {
		for _, v := range ImageVars {
			if ref, ok := env[v]; ok {
				env[v] = RewriteRegistry(ref, registry)
			}
		}
	}
	return env, nil
}

var Hardware = sync.OnceValue(func() stack.Hardware {
	memory, cpus := host.Hardware()
	return stack.Hardware{MemoryBytes: memory, CPUs: cpus}
})

func namedVars(k Key, value string) map[string]string {
	switch k.Name {
	case VoicePublicIP:
		if value == "" {
			return nil
		}
		return map[string]string{"FLUXER_LIVEKIT_NODE_IP": value, "FLUXER_LIVEKIT_USE_EXTERNAL_IP": "false"}
	case EdgeTrusted:
		value = strings.Join(trustedProxyList(value), " ")
	case StoragePath:
		if b, err := strconv.ParseBool(value); err == nil {
			value = strconv.FormatBool(b)
		}
	}
	if len(k.Vars) == 0 {
		return nil
	}
	return map[string]string{k.Vars[0]: value}
}

func derived(chosen map[string]string) map[string]string {
	out := map[string]string{}
	mode := chosen[EdgeMode]
	if mode == "" {
		mode = EdgeBundled
	}
	port := "443"
	if mode == EdgeBundled && chosen[EdgeHTTPSPort] != "" {
		port = chosen[EdgeHTTPSPort]
	}
	out["FLUXER_PUBLIC_SCHEME"] = "https"
	out["FLUXER_PUBLIC_PORT"] = port
	scheme := "https"
	if raw := strings.ToLower(strings.TrimSpace(chosen["FLUXER_PUBLIC_SCHEME"])); raw != "" {
		scheme = raw
	}
	if raw := strings.TrimSpace(chosen[EdgePublicPort]); raw != "" {
		port = raw
	}
	if domain := strings.ToLower(strings.TrimSpace(chosen[Domain])); domain != "" {
		origin := scheme + "://" + domain
		if (scheme == "https" && port != "443") || (scheme == "http" && port != "80") {
			origin = scheme + "://" + net.JoinHostPort(domain, port)
		}
		out["FLUXER_PUBLIC_ORIGIN"] = origin
		if mode == EdgeBundled {
			out["FLUXER_EDGE_SITE_ADDRESS"] = "https://" + domain
		}
	}
	return out
}

func RewriteRegistry(ref, registry string) string {
	name, digest, ok := strings.Cut(ref, "@")
	if !ok || !strings.HasPrefix(digest, "sha256:") || registry == "" {
		return ref
	}
	registry = strings.TrimSuffix(registry, "/")
	first, rest, hasSlash := strings.Cut(name, "/")
	switch {
	case hasSlash && (strings.ContainsAny(first, ".:") || first == "localhost"):
		name = rest
	case !hasSlash:
		name = "library/" + name
	}
	return registry + "/" + name + "@" + digest
}
