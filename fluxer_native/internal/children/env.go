// SPDX-License-Identifier: AGPL-3.0-or-later

package children

import (
	"fmt"
	"maps"
	"net/url"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/sizing"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const overridePrefix = "FLUXER_NATIVE_"

const systemPath = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

const nativeNoProxy = "localhost,127.0.0.1,::1${NO_PROXY:+,${NO_PROXY:-}}"

type Config struct {
	Layout   spec.Layout
	Root     string
	Self     string
	Version  string
	Vars     map[string]string
	Host     sizing.Host
	CaddyDir string
	CertsDir string
	CABundle string
}

var imageEnv = map[string]map[string]string{
	spec.API: {
		"HOME":                "/usr/src/app",
		"NODE_ENV":            "production",
		"NODE_EXTRA_CA_CERTS": "/etc/ssl/certs/ca-certificates.crt",
		"BUILD_VERSION":       "${BUILD_VERSION}",
	},
	spec.MediaProxy: {
		"LD_PRELOAD":              "libjemalloc.so.2",
		"MALLOC_CONF":             "background_thread:true,metadata_thp:auto,dirty_decay_ms:5000,muzzy_decay_ms:5000",
		"VIPS_CONCURRENCY":        "1",
		"VIPS_DISC_THRESHOLD":     "1g",
		"G_MESSAGES_DEBUG":        "",
		"FLUXER_MEDIA_PROXY_HOST": "0.0.0.0",
		"FLUXER_MEDIA_PROXY_PORT": "8080",
		"BUILD_VERSION":           "${BUILD_VERSION}",
	},
	spec.AppProxy: {
		"BUILD_VERSION":              "${BUILD_VERSION}",
		"FLUXER_SELF_HOSTED":         "${FLUXER_SELF_HOSTED}",
		"FLUXER_STATIC_CDN_ENDPOINT": "${PUBLIC_ASSET_BASE_URL}",
		"FLUXER_APP_PROXY_HOST":      "0.0.0.0",
		"FLUXER_APP_PROXY_PORT":      "8080",
		"LD_PRELOAD":                 "libjemalloc.so.2",
		"MALLOC_CONF":                "background_thread:true,metadata_thp:auto,dirty_decay_ms:5000,muzzy_decay_ms:5000",
	},
	spec.Admin: {
		"BUILD_VERSION":     "${BUILD_VERSION}",
		"FLUXER_ADMIN_HOST": "0.0.0.0",
		"FLUXER_ADMIN_PORT": "8080",
	},
	spec.Push:       {"BUILD_VERSION": "${BUILD_VERSION}"},
	spec.Gateway:    {"BUILD_VERSION": "${BUILD_VERSION}"},
	spec.Snowflakes: {"BUILD_VERSION": "${BUILD_VERSION}"},
	spec.Users:      {"BUILD_VERSION": "${BUILD_VERSION}"},
	spec.Messages:   {"BUILD_VERSION": "${BUILD_VERSION}"},
	spec.Gifs:       {"BUILD_VERSION": "${BUILD_VERSION}"},
	spec.Unfurl:     {"BUILD_VERSION": "${BUILD_VERSION}"},
}

var svcPorts = map[string]int{
	spec.Snowflakes: spec.PortSnowflakes,
	spec.Users:      spec.PortUsers,
	spec.Messages:   spec.PortMessages,
	spec.Gifs:       spec.PortGifs,
	spec.Unfurl:     spec.PortUnfurl,
	spec.Push:       spec.PortPush,
	spec.Admin:      spec.PortAdmin,
	spec.AppProxy:   spec.PortAppProxy,
}

func with(base, extra map[string]string) map[string]string {
	out := maps.Clone(base)
	maps.Copy(out, extra)
	return out
}

func (c Config) env(svc string) (spec.Env, error) {
	name := svc
	if svc == spec.SeaweedFSFiler {
		name = spec.SeaweedFS
	}
	block, ok := composeEnv[name]
	if !ok && svc != spec.Valkey {
		return nil, fmt.Errorf("unknown service %q", svc)
	}
	env := spec.Env{}
	image, err := resolve(imageEnv[svc], c.buildArgs())
	if err != nil {
		return nil, fmt.Errorf("%s image env: %w", svc, err)
	}
	maps.Copy(env, image)
	compose, err := resolve(block, c.vars())
	if err != nil {
		return nil, fmt.Errorf("%s env: %w", svc, err)
	}
	maps.Copy(env, compose)
	prefix := overridePrefix + strings.ToUpper(strings.ReplaceAll(svc, "-", "_")) + "__"
	for k, v := range c.Vars {
		if key, ok := strings.CutPrefix(k, prefix); ok && key != "" {
			env[key] = v
		}
	}
	m, err := spec.ResolveModes(c.Vars, c.Layout.Data)
	if err != nil {
		return nil, err
	}
	if err := c.native(svc, env, m); err != nil {
		return nil, fmt.Errorf("%s env: %w", svc, err)
	}
	return env, nil
}

func (c Config) mode(key string) string {
	return strings.TrimSpace(c.Vars[key])
}

func (c Config) buildArgs() map[string]string {
	return map[string]string{"BUILD_VERSION": c.Version, "FLUXER_SELF_HOSTED": "true", "PUBLIC_ASSET_BASE_URL": ""}
}

func (c Config) vars() map[string]string {
	v := sizing.Defaults(c.Host)
	v["FLUXER_LIVEKIT_ENABLED"] = "false"
	maps.Copy(v, c.Vars)
	if strings.TrimSpace(v["FLUXER_LIVEKIT_NODE_IP"]) != "" && strings.TrimSpace(v["FLUXER_LIVEKIT_USE_EXTERNAL_IP"]) == "" {
		v["FLUXER_LIVEKIT_USE_EXTERNAL_IP"] = "false"
	}
	maps.Copy(v, c.inputs())
	return v
}

func (c Config) inputs() map[string]string {
	nats := "nats://" + spec.Addr(spec.PortNATS)
	in := map[string]string{
		"FLUXER_NATS_URL":                                 nats,
		"FLUXER_NATS_JETSTREAM_URL":                       nats,
		"FLUXER_SVC_NATS_URL":                             nats,
		"FLUXER_TRUST_CLIENT_IP_HEADER":                   "true",
		"FLUXER_API_EMBEDDED_WORKER":                      "true",
		"FLUXER_API_PRESIGNED_ATTACHMENT_UPLOADS_ENABLED": "true",
	}
	if c.local(spec.Postgres) {
		in["FLUXER_POSTGRES_HOST"] = c.Layout.PGSocketDir()
		in["FLUXER_POSTGRES_PORT"] = strconv.Itoa(spec.PortPostgres)
	}
	if c.local(spec.Valkey) {
		u := url.URL{Scheme: "redis", User: url.UserPassword("", c.Vars["FLUXER_VALKEY_PASSWORD"]), Host: spec.Addr(spec.PortValkey), Path: "/0"}
		in["FLUXER_KV_URL"] = u.String()
	}
	if c.local(spec.SeaweedFS) {
		in["FLUXER_S3_ENDPOINT"] = spec.URL(spec.PortSeaweedS3)
	}
	if c.local(spec.Meilisearch) {
		in["FLUXER_SEARCH_URL"] = spec.URL(spec.PortMeilisearch)
	}
	if c.local(spec.LiveKit) {
		in["FLUXER_LIVEKIT_INTERNAL_URL"] = spec.URL(spec.PortLiveKit)
	}
	return in
}

func (c Config) local(svc string) bool {
	switch svc {
	case spec.Postgres:
		return c.Vars["FLUXER_POSTGRES_URL"] == ""
	case spec.Valkey:
		return c.Vars["FLUXER_KV_URL"] == "" && c.mode(spec.KVModeEnv) != string(spec.KVPostgres)
	case spec.SeaweedFS, spec.SeaweedFSInit:
		return c.Vars["FLUXER_S3_ENDPOINT"] == "" && c.mode(spec.StorageBackendEnv) != string(spec.StorageFilesystem)
	case spec.Meilisearch:
		engine := c.Vars["FLUXER_SEARCH_ENGINE"]
		return engine == "" || engine == "meilisearch"
	case spec.LiveKit:
		return c.Vars["FLUXER_LIVEKIT_ENABLED"] == "true" && c.Vars["FLUXER_LIVEKIT_BUNDLED"] != "false"
	}
	return true
}

func (c Config) native(svc string, env spec.Env, m spec.Modes) error {
	env["PATH"] = filepath.Join(c.Root, "bin") + ":" + systemPath
	env["HOME"] = c.Layout.Dir(svc)
	env["TMPDIR"] = c.Layout.Tmp(svc)
	if c.CABundle != "" {
		env["NODE_EXTRA_CA_CERTS"] = c.CABundle
		env["SSL_CERT_FILE"] = c.CABundle
	}
	noProxy, err := interpolate(nativeNoProxy, c.Vars)
	if err != nil {
		return err
	}
	only := func(k, v string) {
		if _, ok := env[k]; ok {
			env[k] = v
		}
	}
	only("NO_PROXY", noProxy)
	only("no_proxy", noProxy)
	only("FLUXER_INTERNAL_API_ENDPOINT", spec.URL(spec.PortAPI))
	only("FLUXER_INTERNAL_MEDIA_PROXY_ENDPOINT", spec.URL(spec.PortMediaProxy))
	if m.Storage == spec.StorageFilesystem {
		only("FLUXER_S3_ENDPOINT", "")
		only("FLUXER_S3_PUBLIC_ENDPOINT", "")
		switch svc {
		case spec.API:
			env[spec.StorageBackendEnv] = string(m.Storage)
			env[spec.StorageRootEnv] = m.StorageRoot
			env[spec.StorageUploadsTTLEnv] = m.UploadsTTL
			env[spec.UVThreadpoolSizeEnv] = spec.FilesystemUVThreadpool
			if m.StorageReadOnly {
				env[spec.StorageReadOnlyEnv] = "true"
			}
		case spec.Admin:
			env[spec.StorageBackendEnv] = string(m.Storage)
		case spec.MediaProxy:
			env[spec.MediaProxyStorageEnv] = spec.MediaProxyStorageLocal
			env[spec.MediaProxyStorageRootEnv] = m.StorageRoot
		}
	}
	if m.KV == spec.KVPostgres {
		only(spec.KVURLEnv, "")
	}
	listen := func(hostKey, portKey string, port int) {
		env[hostKey] = spec.Loopback
		env[portKey] = strconv.Itoa(port)
	}
	switch svc {
	case spec.API:
		listen("FLUXER_API_HOST", "FLUXER_API_PORT", spec.PortAPI)
		env["FLUXER_OPERATOR_SOCKET"] = c.Layout.OperatorSocket()
	case spec.Gateway:
		listen("FLUXER_GATEWAY_BIND_HOST", "FLUXER_GATEWAY_PORT", spec.PortGateway)
		schedulers, dirtyCPU, dirtyIO := sizing.Schedulers(c.Host, env)
		env["FLUXER_ERLANG_SCHEDULERS"] = strconv.Itoa(schedulers)
		env["FLUXER_ERLANG_DIRTY_CPU_SCHEDULERS"] = strconv.Itoa(dirtyCPU)
		env["FLUXER_ERLANG_DIRTY_IO_SCHEDULERS"] = strconv.Itoa(dirtyIO)
		env["RELX_REPLACE_OS_VARS"] = "true"
		env["VMARGS_PATH"] = filepath.Join(c.Root, "gateway", "releases", "0.0.0", "vm.args.native")
		env["RELX_CONFIG_PATH"] = filepath.Join(c.Root, "gateway", "releases", "0.0.0", "sys.native.config")
		env["RELX_OUT_FILE_PATH"] = c.Layout.GatewayRuntimeDir()
		env["ERL_CRASH_DUMP"] = filepath.Join(c.Layout.GatewayHome(), "erl_crash.dump")
		env["LANG"] = "C.UTF-8"
	case spec.MediaProxy:
		listen("FLUXER_MEDIA_PROXY_HOST", "FLUXER_MEDIA_PROXY_PORT", spec.PortMediaProxy)
		env["FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_DIR"] = c.Layout.Tmp(svc)
		env["LD_LIBRARY_PATH"] = filepath.Join(c.Root, "lib")
	case spec.Admin:
		listen("FLUXER_ADMIN_HOST", "FLUXER_ADMIN_PORT", spec.PortAdmin)
		env["FLUXER_API_ENDPOINT"] = spec.URL(spec.PortAPI)
	case spec.AppProxy:
		listen("FLUXER_APP_PROXY_HOST", "FLUXER_APP_PROXY_PORT", spec.PortAppProxy)
		env["DISCOVERY_UPSTREAM_URL"] = spec.URL(spec.PortAPI) + "/.well-known/fluxer"
		env["FLUXER_STATIC_DIR"] = filepath.Join(c.Root, "app", "static")
	case spec.Push:
		listen("FLUXER_PUSH_SERVICE_HOST", "FLUXER_PUSH_SERVICE_PORT", spec.PortPush)
	case spec.Snowflakes, spec.Users, spec.Messages, spec.Gifs, spec.Unfurl:
		listen("FLUXER_SVC_LISTEN_HOST", "FLUXER_SVC_PORT", svcPorts[svc])
		if svc == spec.Unfurl {
			env["FLUXER_MEDIA_PROXY_ENDPOINT"] = spec.URL(spec.PortMediaProxy)
		}
	case spec.Meilisearch:
		env["MEILI_HTTP_ADDR"] = spec.Addr(spec.PortMeilisearch)
	case spec.LiveKit:
		cfg, err := replaceOnce(env["LIVEKIT_CONFIG"], "port: 7880\n", fmt.Sprintf("port: %d\nbind_addresses:\n  - %s\n", spec.PortLiveKit, spec.Loopback))
		if err != nil {
			return err
		}
		if cfg, err = replaceOnce(cfg, "http://api:8080/webhooks/livekit", spec.URL(spec.PortAPI)+"/webhooks/livekit"); err != nil {
			return err
		}
		env["LIVEKIT_CONFIG"] = cfg
	}
	return nil
}

func replaceOnce(s, old, repl string) (string, error) {
	if n := strings.Count(s, old); n != 1 {
		return "", fmt.Errorf("expected %q once, found it %d times", old, n)
	}
	return strings.Replace(s, old, repl, 1), nil
}

func resolve(block map[string]string, vars map[string]string) (map[string]string, error) {
	out := make(map[string]string, len(block))
	for k, expr := range block {
		v, err := interpolate(expr, vars)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", k, err)
		}
		out[k] = v
	}
	return out, nil
}

func interpolate(s string, vars map[string]string) (string, error) {
	var b strings.Builder
	for i := 0; i < len(s); i++ {
		if s[i] != '$' || i+1 == len(s) {
			b.WriteByte(s[i])
			continue
		}
		switch next := s[i+1]; {
		case next == '$':
			b.WriteByte('$')
			i++
		case next == '{':
			end := closingBrace(s, i+2)
			if end < 0 {
				return "", fmt.Errorf("unclosed ${ in %q", s)
			}
			v, err := expand(s[i+2:end], vars)
			if err != nil {
				return "", err
			}
			b.WriteString(v)
			i = end
		case isNameByte(next, true):
			j := i + 1
			for j < len(s) && isNameByte(s[j], false) {
				j++
			}
			b.WriteString(vars[s[i+1:j]])
			i = j - 1
		default:
			b.WriteByte('$')
		}
	}
	return b.String(), nil
}

func expand(inner string, vars map[string]string) (string, error) {
	n := 0
	for n < len(inner) && isNameByte(inner[n], n == 0) {
		n++
	}
	name, op := inner[:n], inner[n:]
	if name == "" {
		return "", fmt.Errorf("bad substitution ${%s}", inner)
	}
	value, set := vars[name]
	nonEmpty := set && value != ""
	if op == "" {
		return value, nil
	}
	cond := set
	if rest, ok := strings.CutPrefix(op, ":"); ok {
		cond, op = nonEmpty, rest
	}
	if op == "" || !strings.ContainsRune("-+?", rune(op[0])) {
		return "", fmt.Errorf("bad substitution ${%s}", inner)
	}
	if op[0] == '+' {
		cond, value = !cond, ""
	}
	if cond {
		return value, nil
	}
	arg, err := interpolate(op[1:], vars)
	if err != nil || op[0] != '?' {
		return arg, err
	}
	return "", fmt.Errorf("%s is required: %s", name, arg)
}

func closingBrace(s string, from int) int {
	depth := 1
	for i := from; i < len(s); i++ {
		switch {
		case s[i] == '$' && i+1 < len(s) && s[i+1] == '$':
			i++
		case s[i] == '$' && i+1 < len(s) && s[i+1] == '{':
			depth++
			i++
		case s[i] == '}':
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

func isNameByte(c byte, first bool) bool {
	return c == '_' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || !first && c >= '0' && c <= '9'
}
