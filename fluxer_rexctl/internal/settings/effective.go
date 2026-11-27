// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"cmp"
	"fmt"
	"maps"
	"net"
	"slices"
	"strconv"
	"strings"
	"sync"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func Effective(set *stack.Set, chosen, secrets, adminImport map[string]string) (map[string]string, error) {
	if err := checkCombination(chosen); err != nil {
		return nil, err
	}
	if set.Catalog != nil && chosen[stack.EmbeddedWorkerVar] == "" {
		chosen = maps.Clone(chosen)
		chosen[stack.EmbeddedWorkerVar] = "true"
	}
	env := map[string]string{}
	reads := func(v string) bool { return slices.Contains(set.Vars, v) }
	put := func(v, value string) {
		if reads(v) {
			env[v] = value
		}
	}
	size := put
	if set.Catalog != nil {
		size = func(v, value string) { env[v] = value }
	}

	fixed := map[string]string{}
	for name, value := range chosen {
		if isRawName(name) {
			fixed[name] = value
		}
	}
	if mode := chosen[KVMode]; mode != "" {
		fixed[stack.KVModeVar] = mode
	}
	if len(set.Profiles) > 0 {
		for v, value := range stack.Sizing(Hardware(), chosen[MemoryProfile] == "", fixed) {
			size(v, value)
		}
	}

	for v, value := range stack.DiskSizing(chosen[DiskProfile], fixed) {
		size(v, value)
	}

	if name := chosen[MemoryProfile]; name != "" {
		values, ok := set.Profiles[name]
		if !ok {
			return nil, &Error{Key: MemoryProfile, Problem: fmt.Sprintf("Fluxer %s has no memory profile called %s.", set.Release, name),
				Fix: "Run rexctl config unset memory.profile."}
		}
		for v, value := range values {
			size(v, value)
		}
	}

	if reads(stack.EmbeddedWorkerVar) {
		for v, value := range stack.FoldWorker(Hardware(), env, fixed) {
			size(v, value)
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
	if listen := chosen[EdgeListen]; listen != "" && !reads(EdgeListenVar) && cmp.Or(chosen[EdgeMode], EdgeBundled) == EdgeBundled {
		for v, port := range edgePortVars {
			if reads(v) {
				env[v] = net.JoinHostPort(listen, cmp.Or(env[v], port))
			}
		}
	}

	for adminKey, value := range adminImport {
		if v, ok := twinVar(adminKey); ok {
			put(v, value)
		}
	}

	for v, value := range secrets {
		if !operator[v] {
			put(v, value)
		}
	}

	put(stack.SettingsLoadedVar, "true")

	if set.Catalog != nil {
		maps.Copy(env, stack.NativeSizing(env))
		maps.DeleteFunc(env, func(v, _ string) bool { return !reads(v) && !operator[v] })
	}

	if registry := chosen[ImagesRegistry]; registry != "" {
		for _, v := range ImageVars {
			if ref, ok := env[v]; ok {
				env[v] = rewriteRegistry(ref, registry)
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
	case Domain:
		value = NormalDomain(value)
	case EdgeTrusted:
		value = strings.Join(TrustedProxyList(value), " ")
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
	scheme := "https"
	if raw := strings.ToLower(strings.TrimSpace(chosen["FLUXER_PUBLIC_SCHEME"])); raw != "" {
		scheme = raw
	}
	port, listenPort := "443", EdgeHTTPSPort
	if scheme == "http" {
		port, listenPort = "80", EdgeHTTPPort
	}
	if mode == EdgeBundled && chosen[listenPort] != "" {
		port = chosen[listenPort]
	}
	out["FLUXER_PUBLIC_SCHEME"] = scheme
	out["FLUXER_PUBLIC_PORT"] = port
	if raw := strings.TrimSpace(chosen[EdgePublicPort]); raw != "" {
		port = raw
	}
	if domain := NormalDomain(strings.ToLower(strings.TrimSpace(chosen[Domain]))); domain != "" {
		origin := scheme + "://" + domain
		if (scheme == "https" && port != "443") || (scheme == "http" && port != "80") {
			origin = scheme + "://" + net.JoinHostPort(BareHost(domain), port)
		}
		out["FLUXER_PUBLIC_ORIGIN"] = origin
		if mode == EdgeBundled {
			out["FLUXER_EDGE_SITE_ADDRESS"] = scheme + "://" + domain
		}
	}
	return out
}

func DerivedVars() []string {
	return slices.Sorted(maps.Keys(derived(map[string]string{Domain: "example.com"})))
}

func rewriteRegistry(ref, registry string) string {
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
