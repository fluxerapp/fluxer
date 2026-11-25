// SPDX-License-Identifier: AGPL-3.0-or-later

package children

import (
	"bytes"
	"cmp"
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/edge"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/envdiff"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/natsembed"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/pg"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/probe"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/seaweed"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const serverBinary = "fluxer-server"

func ReleaseRoot(override string) (string, error) {
	if override != "" {
		return filepath.Abs(override)
	}
	exe, err := os.Executable()
	if err != nil {
		return "", err
	}
	if exe, err = filepath.EvalSymlinks(exe); err != nil {
		return "", err
	}
	return filepath.Dir(filepath.Dir(exe)), nil
}

func (c Config) bin(name string) string { return filepath.Join(c.Root, "bin", name) }

func (c Config) child(svc string, argv []string, deps []string, probes ...spec.Probe) (spec.Child, error) {
	env, err := c.env(svc)
	if err != nil {
		return spec.Child{}, err
	}
	return spec.Child{
		Name:        svc,
		Argv:        argv,
		Env:         env,
		Cwd:         c.Layout.Dir(svc),
		Deps:        deps,
		Probes:      probes,
		StopSignal:  syscall.SIGTERM,
		StopTimeout: 10 * time.Second,
	}, nil
}

func Specs(c Config) ([]spec.Child, error) {
	if c.VoiceOnly() {
		return voiceOnlySpecs(c)
	}
	m, err := spec.ResolveModes(c.Vars, c.Layout.Data)
	if err != nil {
		return nil, err
	}
	var out []spec.Child
	var errs []error
	has := map[string]bool{}
	add := func(ch spec.Child, err error) *spec.Child {
		errs = append(errs, err)
		out = append(out, ch)
		has[ch.Name] = true
		return &out[len(out)-1]
	}
	on := func(names ...string) []string {
		var deps []string
		for _, n := range names {
			if has[n] {
				deps = append(deps, n)
			}
		}
		return deps
	}

	if c.local(spec.Postgres) {
		o, err := c.postgres()
		errs = append(errs, err)
		ch := add(c.child(spec.Postgres, pg.Argv(o), nil, pg.ReadyProbe(o)))
		ch.Cwd = o.DataDir
		ch.StopSignal = syscall.SIGINT
		ch.StopTimeout = 30 * time.Second
		ch.Core = true
	}
	if c.local(spec.Valkey) {
		argv, err := c.valkeyArgv()
		errs = append(errs, err)
		ch := add(c.child(spec.Valkey, argv, nil, spec.FuncProbe(probe.Valkey(spec.Addr(spec.PortValkey), c.Vars["FLUXER_VALKEY_PASSWORD"]))))
		ch.Core = true
	}
	if c.local(spec.SeaweedFS) {
		args, err := resolveList(seaweedArgs, c.vars())
		errs = append(errs, err)
		weed := c.bin(spec.WeedBinary)
		tls := seaweed.ConfigDir(c.Layout)
		serverArgv, filerArgv := seaweed.Argv(weed, c.Layout.SeaweedFSDir(), tls, args)
		server := add(c.child(spec.SeaweedFS, serverArgv, nil, seaweed.VolumeProbe(tls), seaweed.LeaderProbe(tls)))
		server.StopTimeout = 30 * time.Second
		server.Core = true
		filer := add(c.child(spec.SeaweedFSFiler, filerArgv, on(spec.SeaweedFS), spec.HTTPProbe(spec.PortSeaweedS3, "/healthz", 200)))
		filer.StopTimeout = 30 * time.Second
		filer.Core = true
		buckets := add(c.child(spec.SeaweedFSInit, seaweed.InitArgv(c.Self, weed, tls), on(spec.SeaweedFS, spec.SeaweedFSFiler)))
		buckets.Cwd = c.Layout.Tmp(spec.SeaweedFSInit)
		buckets.Oneshot = true
		buckets.Rearm = true
	}
	if c.local(spec.Meilisearch) {
		meili := add(c.child(spec.Meilisearch, []string{c.bin("meilisearch")}, nil, spec.HTTPProbe(spec.PortMeilisearch, "/health", 200)))
		meili.Core = true
	}
	if c.local(spec.LiveKit) {
		add(c.child(spec.LiveKit, []string{c.bin("livekit-server")}, nil, spec.HTTPProbe(spec.PortLiveKit, "/", 200)))
	}
	rust := []string{spec.RustServer}
	if m.Rust == spec.RustSingle {
		add(c.rust(on(spec.Postgres)))
	} else {
		rust = rustGating
		for _, svc := range []string{spec.Snowflakes, spec.Gifs, spec.Unfurl} {
			add(c.server(svc, nil))
		}
		for _, svc := range []string{spec.Users, spec.Messages} {
			add(c.server(svc, on(spec.Postgres)))
		}
	}

	api := add(c.child(spec.API, []string{c.bin("node"), filepath.Join(c.Root, "api", "dist", "AppEntrypoint.js")},
		on(append([]string{spec.Postgres, spec.Valkey, spec.Meilisearch, spec.SeaweedFSInit}, rust...)...),
		spec.HTTPProbe(spec.PortAPI, "/_health", 200),
		spec.FileFreshProbe(c.Layout.WorkerHeartbeat(), spec.WorkerHeartbeatMaxAge)))
	api.StopTimeout = 20 * time.Second
	api.Core = true

	gateway := add(c.child(spec.Gateway, []string{filepath.Join(c.Root, "gateway", "bin", "fluxer_gateway"), "foreground"}, on(spec.Valkey),
		spec.HTTPProbe(spec.PortGateway, "/_health/ready", 200)))
	gateway.StopTimeout = 15 * time.Second
	gateway.Core = true

	add(c.child(spec.MediaProxy, []string{c.bin("fluxer-media-proxy")}, on(spec.SeaweedFSInit), spec.HTTPProbe(spec.PortMediaProxy, "/_health", 200)))
	if m.Rust != spec.RustSingle {
		for _, svc := range []string{spec.Push, spec.Admin, spec.AppProxy} {
			add(c.server(svc, on(spec.API)))
		}
	}
	voices, _ := VoiceServers(c)
	out = append(out, voices...)
	errs = append(errs, c.supervision(out))
	_, err = Edge(c)
	errs = append(errs, err)
	_, err = NATS(c)
	errs = append(errs, err)
	return out, errors.Join(errs...)
}

var rustGating = []string{spec.Snowflakes, spec.Users, spec.Messages, spec.Gifs}

var rustCore = append(slices.Clone(rustGating), spec.AppProxy)

var rustSharedKeys = []string{
	"HTTP_PROXY", "HTTPS_PROXY", "NO_PROXY", "http_proxy", "https_proxy", "no_proxy",
	"SSL_CERT_FILE", "NODE_EXTRA_CA_CERTS", "RUST_LOG",
}

var rustProcessKeys = append([]string{"PATH", "HOME", "TMPDIR", "TOKIO_WORKER_THREADS", "MALLOC_ARENA_MAX", "LD_PRELOAD", "MALLOC_CONF"}, rustSharedKeys...)

func rustProbes(svc string) []spec.Probe {
	probes := []spec.Probe{spec.HTTPProbe(svcPorts[svc], "/_health", 200)}
	if svc == spec.AppProxy {
		probes = append(probes, spec.HTTPProbe(svcPorts[svc], "/_ready", 200))
	}
	for i := range probes {
		probes[i].Name = svc
	}
	return probes
}

func (c Config) server(svc string, deps []string) (spec.Child, error) {
	ch, err := c.child(svc, []string{c.bin(serverBinary)}, deps, rustProbes(svc)...)
	if err != nil {
		return ch, err
	}
	ch.Env[spec.ServerServicesEnv] = svc
	ch.Core = slices.Contains(rustCore, svc)
	return ch, nil
}

func (c Config) rust(deps []string) (spec.Child, error) {
	envs := map[string]map[string]string{}
	for _, svc := range spec.RustServices {
		prefix := overridePrefix + strings.ToUpper(strings.ReplaceAll(svc, "-", "_")) + "__"
		for _, k := range rustProcessKeys {
			if _, set := c.Vars[prefix+k]; set {
				return spec.Child{}, fmt.Errorf("%s%s can't be used with %s=%s, because the Rust services share one process. Set %s for all of them, or %s for the worker count", prefix, k, spec.RustProcessEnv, spec.RustSingle, k, spec.RustWorkerThreadsEnv)
			}
		}
		env, err := c.env(svc)
		if err != nil {
			return spec.Child{}, err
		}
		envs[svc] = env
	}
	encoded, err := envdiff.Encode(envs, rustProcessKeys)
	if err != nil {
		return spec.Child{}, err
	}
	env := spec.Env(encoded)
	for _, k := range rustSharedKeys {
		for _, svc := range spec.RustServices {
			v := envs[svc][k]
			if v == "" {
				continue
			}
			if cur, ok := env[k]; ok && cur != v {
				return spec.Child{}, fmt.Errorf("%s is %q for %s and %q for another Rust service, and one process can only have one value", k, v, svc, cur)
			}
			env[k] = v
		}
	}
	env["LD_PRELOAD"] = imageEnv[spec.AppProxy]["LD_PRELOAD"]
	env["MALLOC_CONF"] = imageEnv[spec.AppProxy]["MALLOC_CONF"]
	env["PATH"] = filepath.Join(c.Root, "bin") + ":" + systemPath
	env["HOME"] = c.Layout.Dir(spec.RustServer)
	env["TMPDIR"] = c.Layout.Tmp(spec.RustServer)
	env["TOKIO_WORKER_THREADS"] = c.vars()[spec.RustWorkerThreadsEnv]
	env[spec.ServerServicesEnv] = strings.Join(spec.RustServices, ",")

	ch := spec.Child{
		Name:        spec.RustServer,
		Argv:        []string{c.bin(serverBinary)},
		Env:         env,
		Cwd:         c.Layout.Dir(spec.RustServer),
		Deps:        deps,
		SvcLogs:     true,
		StopSignal:  syscall.SIGTERM,
		StopTimeout: 30 * time.Second,
		Core:        true,
	}
	for _, svc := range spec.RustServices {
		probes := rustProbes(svc)
		switch {
		case slices.Contains(rustGating, svc):
			ch.Probes = append(ch.Probes, probes...)
			continue
		case slices.Contains(rustCore, svc):
			for i := range probes {
				probes[i].Core = true
			}
		}
		ch.Watch = append(ch.Watch, probes...)
	}
	return ch, nil
}

func Prepare(ctx context.Context, c Config, specs []spec.Child) error {
	for svc, key := range map[string]string{spec.Postgres: "POSTGRES_PASSWORD", spec.Valkey: "FLUXER_VALKEY_PASSWORD"} {
		if c.local(svc) && c.Vars[key] == "" {
			return fmt.Errorf("%s is empty, so the bundled %s cannot start", key, svc)
		}
	}
	if err := os.RemoveAll(c.Layout.TmpRoot()); err != nil {
		return err
	}
	dirs := []string{c.Layout.NATSDir(), c.Layout.EdgeDir(), c.Layout.PGSocketDir()}
	if c.VoiceOnly() {
		dirs = []string{c.Layout.EdgeDir()}
	}
	for _, ch := range specs {
		dirs = append(dirs, ch.Cwd, c.Layout.Tmp(ch.Name))
	}
	for _, d := range dirs {
		if err := os.MkdirAll(d, 0o700); err != nil {
			return err
		}
	}
	if c.VoiceOnly() {
		return nil
	}
	if err := c.writeValkeyConf(); err != nil {
		return err
	}
	if c.local(spec.SeaweedFS) {
		if err := seaweed.WriteTLS(seaweed.ConfigDir(c.Layout)); err != nil {
			return err
		}
	}
	if c.mode(spec.StorageBackendEnv) == string(spec.StorageFilesystem) {
		if err := PrepareObjects(c.Layout); err != nil {
			return err
		}
	}
	return PreparePostgres(ctx, c)
}

func Refresh(c Config, specs []spec.Child) ([]string, error) {
	for _, ch := range specs {
		for _, d := range []string{ch.Cwd, c.Layout.Tmp(ch.Name)} {
			if err := os.MkdirAll(d, 0o700); err != nil {
				return nil, err
			}
		}
	}
	if !c.local(spec.Valkey) {
		return nil, nil
	}
	before, _ := os.ReadFile(c.valkeyConf())
	if err := c.writeValkeyConf(); err != nil {
		return nil, err
	}
	after, err := os.ReadFile(c.valkeyConf())
	if err != nil {
		return nil, err
	}
	if bytes.Equal(before, after) {
		return nil, nil
	}
	return []string{spec.Valkey}, nil
}

func PreparePostgres(ctx context.Context, c Config) error {
	if !c.local(spec.Postgres) {
		return nil
	}
	o, err := c.postgres()
	if err != nil {
		return err
	}
	if o.Resolve != nil {
		return o.Resolve
	}
	return pg.Prepare(ctx, o)
}

func PreparePostgresIn(ctx context.Context, c Config, dir string) error {
	o, err := c.postgres()
	if err != nil {
		return err
	}
	choice, err := pg.Resolve(dir, c.Vars["FLUXER_POSTGRES_BIN_DIR"])
	if err != nil {
		return err
	}
	o.BinDir, o.Major, o.DataDir = choice.BinDir, choice.Major, dir
	return pg.Prepare(ctx, o)
}

func PostgresHint(c Config) string {
	if !c.local(spec.Postgres) {
		return ""
	}
	choice, err := pg.Resolve(c.Layout.PostgresDir(), c.Vars["FLUXER_POSTGRES_BIN_DIR"])
	if err != nil {
		return ""
	}
	return choice.Hint()
}

const walCompressionEnv = "FLUXER_POSTGRES_WAL_COMPRESSION"

func walCompression(setting string, major int, vars map[string]string) (string, error) {
	_, v, _ := strings.Cut(setting, "=")
	if major == 0 || major >= 15 {
		return setting, nil
	}
	switch strings.ToLower(strings.TrimSpace(v)) {
	case "on", "off", "true", "false", "yes", "no", "1", "0":
		return setting, nil
	}
	if _, set := vars[walCompressionEnv]; set {
		return "", fmt.Errorf("%s is %s, and PostgreSQL %d only takes on or off. Set it to on, or unset it", walCompressionEnv, v, major)
	}
	return "wal_compression=on", nil
}

func (c Config) postgres() (pg.Options, error) {
	env, err := c.env(spec.Postgres)
	if err != nil {
		return pg.Options{}, err
	}
	choice, resolveErr := pg.Resolve(c.Layout.PostgresDir(), c.Vars["FLUXER_POSTGRES_BIN_DIR"])
	if resolveErr != nil {
		choice.BinDir = cmp.Or(c.Vars["FLUXER_POSTGRES_BIN_DIR"], pg.BinDir(pg.MinMajor))
	}
	o := pg.Options{
		BinDir:    choice.BinDir,
		Major:     choice.Major,
		Newer:     choice.Newer,
		Resolve:   resolveErr,
		DataDir:   c.Layout.PostgresDir(),
		SocketDir: c.Layout.PGSocketDir(),
		Port:      spec.PortPostgres,
		User:      env["POSTGRES_USER"],
		Password:  env["POSTGRES_PASSWORD"],
		Database:  env["POSTGRES_DB"],
	}
	settings, err := resolveList(postgresSettings, c.vars())
	if err != nil {
		return o, err
	}
	pools := 0
	for _, svc := range []string{spec.API, spec.Users, spec.Messages} {
		env, err := c.env(svc)
		if err != nil {
			return o, err
		}
		n, err := strconv.Atoi(env["FLUXER_POSTGRES_MAX_CONNECTIONS"])
		if err != nil {
			return o, fmt.Errorf("%s FLUXER_POSTGRES_MAX_CONNECTIONS: %w", svc, err)
		}
		pools += n
	}
	if c.mode(spec.KVModeEnv) == string(spec.KVPostgres) {
		n, err := strconv.Atoi(c.vars()[spec.KVPostgresMaxConnsEnv])
		if err != nil {
			return o, fmt.Errorf("api %s: %w", spec.KVPostgresMaxConnsEnv, err)
		}
		pools += n + 1
	}
	for i, s := range settings {
		switch name, _, _ := strings.Cut(s, "="); name {
		case "max_connections":
			_, v, _ := strings.Cut(s, "=")
			n, err := strconv.Atoi(strings.TrimSpace(v))
			if err != nil {
				return o, fmt.Errorf("FLUXER_POSTGRES_SERVER_MAX_CONNECTIONS is %q, not a number", v)
			}
			settings[i] = "max_connections=" + strconv.Itoa(max(n, pools+10))
		case "wal_compression":
			if settings[i], err = walCompression(s, o.Major, c.Vars); err != nil {
				return o, err
			}
		case "shared_preload_libraries":
			if _, set := c.Vars["FLUXER_POSTGRES_SHARED_PRELOAD_LIBRARIES"]; !set && !pg.HasLibrary(o.BinDir, "pg_stat_statements") {
				settings[i] = "shared_preload_libraries="
			}
		}
	}
	o.Settings = settings
	return o, nil
}

func (c Config) valkeyConf() string { return filepath.Join(c.Layout.Runtime, "valkey.conf") }

func (c Config) writeValkeyConf() error {
	if !c.local(spec.Valkey) {
		return nil
	}
	pw := strings.NewReplacer(`\`, `\\`, `"`, `\"`).Replace(c.Vars["FLUXER_VALKEY_PASSWORD"])
	return os.WriteFile(c.valkeyConf(), []byte("requirepass \""+pw+"\"\n"), 0o600)
}

func (c Config) valkeyArgv() ([]string, error) {
	args, err := resolveList(valkeyArgs, c.vars())
	if err != nil {
		return nil, err
	}
	i := slices.Index(args, "--dir")
	if i < 0 || i+1 >= len(args) {
		return nil, errors.New("valkey: compose command has no --dir")
	}
	args[i+1] = c.Layout.ValkeyDir()
	argv := append([]string{c.bin(spec.ValkeyBinary), c.valkeyConf()}, args...)
	return append(argv, "--bind", spec.Loopback, "--port", strconv.Itoa(spec.PortValkey)), nil
}

func resolveList(exprs []string, vars map[string]string) ([]string, error) {
	out := make([]string, len(exprs))
	for i, e := range exprs {
		v, err := interpolate(e, vars)
		if err != nil {
			return nil, err
		}
		out[i] = v
	}
	return out, nil
}

func NATS(c Config) (natsembed.Options, error) {
	env, err := c.env(natsService)
	if err != nil {
		return natsembed.Options{}, err
	}
	return natsembed.Options{
		StoreDir:       c.Layout.NATSDir(),
		Token:          c.Vars["FLUXER_NATS_AUTH_TOKEN"],
		Port:           spec.PortNATS,
		MaxMemoryStore: env["FLUXER_NATS_MAX_MEMORY_STORE"],
		MaxFileStore:   env["FLUXER_NATS_MAX_FILE_STORE"],
	}, nil
}

func Edge(c Config) (edge.Config, error) {
	env, err := c.env(edgeService)
	if err != nil {
		return edge.Config{}, err
	}
	e := edge.Config{
		Site:           env["FLUXER_EDGE_SITE_ADDRESS"],
		Bind:           c.Vars["FLUXER_EDGE_LISTEN_ADDRESS"],
		TrustedProxies: env["FLUXER_EDGE_TRUSTED_PROXIES"],
		Encode:         env["FLUXER_EDGE_ENCODE"],
		Protocols:      env["FLUXER_EDGE_PROTOCOLS"],
		GlobalOptions:  globalOptions(env),
		AdminBasePath:  env["FLUXER_ADMIN_BASE_PATH"],
		Dir:            c.Layout.EdgeDir(),
		CaddyDir:       c.CaddyDir,
		Static:         filepath.Join(c.Root, "app", "static"),
		LiveKit:        c.local(spec.LiveKit),
		Container:      c.Vars[spec.ContainerEnv] == "1",
	}
	if e.HTTPPort, err = port(c.Vars["FLUXER_HTTP_PORT"], 80); err != nil {
		return e, fmt.Errorf("FLUXER_HTTP_PORT: %w", err)
	}
	if e.HTTPSPort, err = port(c.Vars["FLUXER_HTTPS_PORT"], 443); err != nil {
		return e, fmt.Errorf("FLUXER_HTTPS_PORT: %w", err)
	}
	if c.VoiceOnly() {
		return c.voiceEdge(e)
	}
	switch mode := c.Vars["FLUXER_EDGE_MODE"]; mode {
	case "", "bundled":
		c.bundledTLS(&e)
	case "proxy", "tunnel":
		if err := c.proxyEdge(&e); err != nil {
			return e, err
		}
	default:
		return e, fmt.Errorf("FLUXER_EDGE_MODE must be bundled, proxy or tunnel, not %q", mode)
	}
	return e, nil
}

func (c Config) proxyEdge(e *edge.Config) error {
	trusted, err := interpolate("${FLUXER_EDGE_TRUSTED_PROXIES:-private_ranges}", c.Vars)
	if err != nil {
		return err
	}
	host, p, err := proxyBind(c.Vars["FLUXER_EDGE_BIND"], c.Vars["FLUXER_EDGE_LISTEN_ADDRESS"], e.Container)
	if err != nil {
		return fmt.Errorf("FLUXER_EDGE_BIND: %w", err)
	}
	e.Site = ":" + strconv.Itoa(p)
	e.Bind = host
	e.TrustedProxies = trusted
	e.Protocols = plainProtocols(e.Protocols)
	return nil
}

func plainProtocols(protocols string) string {
	kept := slices.DeleteFunc(strings.Fields(protocols), func(p string) bool { return p == "h2" || p == "h3" })
	if len(kept) == 0 {
		return "h1"
	}
	return strings.Join(kept, " ")
}

func globalOptions(env map[string]string) string {
	var lines []string
	for _, k := range []string{"FLUXER_EDGE_GLOBAL_OPTIONS", "FLUXER_EDGE_ACME_CA_DIRECTIVE", "FLUXER_EDGE_ACME_CA_ROOT_DIRECTIVE"} {
		if v := strings.TrimSpace(env[k]); v != "" {
			lines = append(lines, v)
		}
	}
	return strings.Join(lines, "\n\t")
}

func proxyBind(bind, listen string, container bool) (string, int, error) {
	fallback := spec.Loopback
	if container {
		fallback = spec.AnyAddress
	}
	bind = strings.TrimSpace(bind)
	if bind == "" {
		return cmp.Or(strings.Trim(strings.TrimSpace(listen), "[]"), fallback), spec.PortEdgeProxy, nil
	}
	if _, err := strconv.Atoi(bind); err == nil {
		bind = ":" + bind
	}
	host, ps, err := net.SplitHostPort(bind)
	if err != nil {
		return "", 0, fmt.Errorf("%q is not a host:port address", bind)
	}
	p, err := port(ps, 0)
	if err != nil || p == 0 {
		return "", 0, fmt.Errorf("%q has no valid port", bind)
	}
	if host == "" {
		host = fallback
	}
	if net.ParseIP(host) == nil {
		return "", 0, fmt.Errorf("%q is not an IP address", host)
	}
	return host, p, nil
}

func (c Config) tlsDirective() string {
	value := strings.TrimSpace(c.Vars["FLUXER_EDGE_TLS"])
	if value == "" && !publicName(c.Vars["FLUXER_DOMAIN"]) {
		value = "internal"
	}
	if value == "" || value == "acme" {
		return ""
	}
	if f := strings.Fields(value); len(f) == 2 && !strings.Contains(value, "@") {
		for i, p := range f {
			if !filepath.IsAbs(p) {
				f[i] = filepath.Join(c.CertsDir, p)
			}
		}
		value = edge.Quote(f[0]) + " " + edge.Quote(f[1])
	}
	return "tls " + value
}

func (c Config) bundledTLS(e *edge.Config) {
	if strings.EqualFold(strings.TrimSpace(c.Vars["FLUXER_PUBLIC_SCHEME"]), "http") {
		e.TLS = ""
		return
	}
	e.TLS = c.tlsDirective()
	if ip, ok := domainIP(strings.TrimSpace(c.Vars["FLUXER_DOMAIN"])); ok {
		if publicAddress(ip) && strings.TrimSpace(c.Vars["FLUXER_EDGE_TLS"]) == "" {
			e.TLS = "tls {\n\t\tissuer acme {\n\t\t\tprofile shortlived\n\t\t}\n\t\tissuer fluxer_local {\n\t\t\tlifetime 2h\n\t\t}\n\t}"
			e.ACMEFirst = ip.String()
		}
		e.GlobalOptions = strings.TrimSpace(e.GlobalOptions + "\n\tdefault_sni " + ip.String())
	}
	domain := NormalDomain(c.Vars["FLUXER_DOMAIN"])
	if e.TLS == "tls internal" {
		e.RootSite = "http://" + domain
	}
	e.RedirectTo = "https://" + domain
	if p, err := port(c.Vars["FLUXER_PUBLIC_PORT"], e.HTTPSPort); err == nil && p != 443 {
		e.RedirectTo += ":" + strconv.Itoa(p)
	}
}

func publicName(domain string) bool {
	host := strings.ToLower(strings.TrimSuffix(domain, "."))
	if h, _, err := net.SplitHostPort(host); err == nil {
		host = h
	}
	if host == "" || net.ParseIP(strings.Trim(host, "[]")) != nil || !strings.Contains(host, ".") {
		return false
	}
	for _, suffix := range []string{".localhost", ".local", ".test", ".internal", ".lan", ".home.arpa", ".example", ".invalid"} {
		if strings.HasSuffix(host, suffix) {
			return false
		}
	}
	return true
}

func port(s string, fallback int) (int, error) {
	if s == "" {
		return fallback, nil
	}
	n, err := strconv.Atoi(s)
	if err != nil || n <= 0 || n > 65535 {
		return 0, fmt.Errorf("%q is not a port", s)
	}
	return n, nil
}
