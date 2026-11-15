// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"net"
	"os"
	"os/user"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	ServiceUser     = "fluxer"
	EnvFileName     = "fluxer.env"
	SecretsFileName = "secrets.env"
	DataDirName     = "data"
	ReleasesDirName = "releases"
	CurrentLink     = "current"
	PreviousLink    = "previous"
	FluxerdPath     = "bin/fluxerd"

	ControlSocketName  = "fluxerd.sock"
	OperatorSocketName = "operator.sock"
	BackupsDirName     = "backups"

	APIAddr = "127.0.0.1:18080"

	dataDirEnv    = "FLUXER_DATA_DIR"
	runtimeDirEnv = "RUNTIME_DIRECTORY"
	edgeModeEnv   = "FLUXER_EDGE_MODE"
	edgeListenEnv = "FLUXER_EDGE_LISTEN_ADDRESS"
	ProxyPort     = "8080"
)

var Warn = func(msg, fix string) { fmt.Fprintf(os.Stderr, "! %s\n  %s\n", msg, fix) }

var (
	ErrNotRoot   = errors.New("native: the native runtime is managed as root")
	ErrProxyPort = errors.New("native: behind a proxy, the native runtime listens on port " + ProxyPort)
)

type Stack struct {
	Inst    *instance.Instance
	Runtime string

	replaced string
}

func New(inst *instance.Instance) *Stack {
	return &Stack{Inst: inst}
}

func (s *Stack) Unit() string { return s.Inst.Project() + ".service" }

func (s *Stack) Root() string { return s.Inst.Path(DataDirName) }

func (s *Stack) RuntimeDir() string {
	if s.Runtime != "" {
		return s.Runtime
	}
	if dir := runningRuntimeDir(context.Background(), s.Unit()); dir != "" {
		return dir
	}
	return host.NativeRuntimeDir(s.Inst.Project())
}

func runningRuntimeDir(ctx context.Context, unit string) string {
	pid, err := strconv.Atoi(unitProperty(ctx, unit, "MainPID"))
	if err != nil || pid <= 0 {
		return ""
	}
	environ, err := os.ReadFile(filepath.Join("/proc", strconv.Itoa(pid), "environ"))
	if err != nil {
		return ""
	}
	for _, kv := range bytes.Split(environ, []byte{0}) {
		if dir, ok := bytes.CutPrefix(kv, []byte(runtimeDirEnv+"=")); ok {
			return string(dir)
		}
	}
	return ""
}

func (s *Stack) ControlSocket() string { return filepath.Join(s.RuntimeDir(), ControlSocketName) }

func (s *Stack) OperatorSocket() string { return filepath.Join(s.RuntimeDir(), OperatorSocketName) }

func (s *Stack) EnvFile() string { return filepath.Join(s.Root(), EnvFileName) }

func (s *Stack) SecretsFile() string { return filepath.Join(s.Root(), SecretsFileName) }

func (s *Stack) ReleasesDir() string { return s.Inst.Path(ReleasesDirName) }

func (s *Stack) ReleaseDir(version string) string { return filepath.Join(s.ReleasesDir(), version) }

func (s *Stack) Current() string { return s.Inst.Path(CurrentLink) }

func (s *Stack) Fluxerd() string { return filepath.Join(s.Current(), FluxerdPath) }

func (s *Stack) BootUnit() host.BootUnit {
	return host.BootUnit{
		Project:  s.Inst.Project(),
		Dir:      s.Inst.Dir,
		Native:   true,
		User:     ServiceUser,
		Fluxerd:  s.Fluxerd(),
		EnvFile:  s.EnvFile(),
		DataDir:  s.Root(),
		CaddyDir: s.Inst.Path(instance.CaddyDropInDir),
		CertsDir: s.Inst.Path(instance.CertsDir),
	}
}

var volumeDirs = map[string]string{
	"postgres-data":    "postgres",
	"valkey-data":      "valkey",
	"nats-data":        "nats",
	"meilisearch-data": "meilisearch",
	"seaweedfs-data":   "seaweedfs",
	"objects-data":     "objects",
	"edge-data":        "edge",
}

func (s *Stack) DataDir(volume string) (string, bool) {
	name, ok := volumeDirs[volume]
	if !ok {
		return "", false
	}
	return filepath.Join(s.Root(), name), true
}

var pathKeys = []string{
	"FLUXER_NODE_EXTRA_CA_CERTS",
	"FLUXER_GEOIP_DB_PATH",
	"FLUXER_PUSH_APNS_PRIVATE_KEY_PATH",
	"FLUXER_PUSH_FCM_PRIVATE_KEY_PATH",
	"FLUXER_PUSH_FCM_SERVICE_ACCOUNT_JSON_PATH",
	"FLUXER_EDGE_TLS",
}

func TranslatePaths(env map[string]string, dir string) {
	prefixes := [][2]string{
		{stack.NodeCADir + "/", filepath.Join(dir, instance.CADir) + "/"},
		{"/certs/", filepath.Join(dir, instance.CertsDir) + "/"},
	}
	for _, k := range pathKeys {
		v, ok := env[k]
		if !ok {
			continue
		}
		words := strings.Fields(v)
		for i, w := range words {
			for _, p := range prefixes {
				if rest, ok := strings.CutPrefix(w, p[0]); ok {
					words[i] = p[1] + rest
					break
				}
			}
		}
		env[k] = strings.Join(words, " ")
	}
}

func (s *Stack) Environment(set *stack.Set) (env, secrets map[string]string, err error) {
	st := s.Inst.State
	env, err = deploy.Environment(set, st, st.Settings)
	if err != nil {
		return nil, nil, err
	}
	secrets = maps.Clone(st.Secrets)
	for k, v := range secrets {
		if env[k] == v {
			delete(env, k)
		}
	}
	delete(env, stack.SettingsLoadedVar)
	mode := st.Settings[settings.EdgeMode]
	if mode == "" {
		mode = settings.EdgeBundled
	}
	env[edgeModeEnv] = mode
	if mode != settings.EdgeBundled {
		addr, err := ProxyListen(st.Settings[settings.EdgeBind])
		if err != nil {
			return nil, nil, err
		}
		env[edgeListenEnv] = addr
	}
	TranslatePaths(env, s.Inst.Dir)
	return env, secrets, nil
}

func ProxyListen(bind string) (string, error) {
	if bind == "" {
		return "127.0.0.1", nil
	}
	h, port, err := net.SplitHostPort(bind)
	if err != nil {
		return "", err
	}
	if port != ProxyPort {
		return "", ErrProxyPort
	}
	if h == "" {
		h = "0.0.0.0"
	}
	return h, nil
}

func envBytes(env map[string]string) []byte {
	var b bytes.Buffer
	for _, k := range slices.Sorted(maps.Keys(env)) {
		b.WriteString(k)
		b.WriteByte('=')
		b.WriteString(quote(env[k]))
		b.WriteByte('\n')
	}
	return b.Bytes()
}

func quote(v string) string {
	if v != "" && !strings.ContainsFunc(v, func(r rune) bool {
		return !(r >= 'a' && r <= 'z' || r >= 'A' && r <= 'Z' || r >= '0' && r <= '9' || strings.ContainsRune("-_.,:/@+=%~^", r))
	}) {
		return v
	}
	r := strings.NewReplacer(`\`, `\\`, `"`, `\"`, "\n", `\n`, "\r", "")
	return `"` + r.Replace(v) + `"`
}

type Rendered struct {
	Env     bool
	Secrets bool
}

func (r Rendered) Changed() bool { return r.Env || r.Secrets }

func (s *Stack) Render(set *stack.Set) (Rendered, error) {
	var r Rendered
	env, secrets, err := s.Environment(set)
	if err != nil {
		return r, err
	}
	if err := os.MkdirAll(s.Root(), 0o750); err != nil {
		return r, err
	}
	if r.Env, err = writeIfChanged(s.EnvFile(), envBytes(env)); err != nil {
		return r, err
	}
	merged := map[string]string{}
	if old, err := envfile.Load(s.SecretsFile()); err == nil {
		maps.Copy(merged, old.Map())
	} else if !errors.Is(err, fs.ErrNotExist) {
		return r, err
	}
	maps.Copy(merged, secrets)
	if r.Secrets, err = writeIfChanged(s.SecretsFile(), envBytes(merged)); err != nil {
		return r, err
	}
	return r, s.chownData(s.EnvFile(), s.SecretsFile())
}

func (s *Stack) ReadSecrets() (map[string]string, error) {
	f, err := envfile.Load(s.SecretsFile())
	if err != nil {
		return nil, err
	}
	return f.Map(), nil
}

func writeIfChanged(path string, data []byte) (bool, error) {
	if old, err := os.ReadFile(path); err == nil && bytes.Equal(old, data) {
		return false, nil
	}
	return true, envfile.WriteFileAtomic(path, data, true)
}

var lookupUser = user.Lookup

func serviceIDs() (int, int, error) {
	u, err := lookupUser(ServiceUser)
	if err != nil {
		return 0, 0, err
	}
	uid, err := strconv.Atoi(u.Uid)
	if err != nil {
		return 0, 0, err
	}
	gid, err := strconv.Atoi(u.Gid)
	if err != nil {
		return 0, 0, err
	}
	return uid, gid, nil
}

func (s *Stack) chownData(paths ...string) error {
	if os.Geteuid() != 0 {
		return nil
	}
	uid, gid, err := serviceIDs()
	if err != nil {
		return fmt.Errorf("native: the account %s: %w", ServiceUser, err)
	}
	for _, p := range append([]string{s.Root()}, paths...) {
		if err := os.Lchown(p, uid, gid); err != nil {
			return err
		}
	}
	return nil
}

func (s *Stack) MountedHash() (string, error) {
	h := sha256.New()
	for _, dir := range []string{instance.CaddyDropInDir, instance.CertsDir} {
		root := s.Inst.Path(dir)
		err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
			if errors.Is(err, fs.ErrNotExist) {
				return nil
			}
			if err != nil || d.IsDir() {
				return err
			}
			data, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			rel, _ := filepath.Rel(s.Inst.Dir, path)
			fmt.Fprintf(h, "%s\x00%d\x00", rel, len(data))
			h.Write(data)
			return nil
		})
		if err != nil {
			return "", err
		}
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}

const mountedKey = "native"

type Applied struct {
	Restarted bool
	Reloaded  bool
	Started   bool
}

func (s *Stack) Apply(ctx context.Context, set *stack.Set) (Applied, error) {
	var a Applied
	if os.Geteuid() != 0 {
		return a, ErrNotRoot
	}
	if err := EnsureUser(ctx); err != nil {
		return a, err
	}
	r, err := s.Render(set)
	if err != nil {
		return a, err
	}
	u := s.BootUnit()
	path, err := u.Path()
	if err != nil {
		return a, err
	}
	before, _ := os.ReadFile(path)
	relaxedBefore, _ := os.ReadFile(u.NoNamespacesPath())
	active := unitActive(ctx, s.Unit())
	if !active {
		_, _ = systemctlCmd(ctx, "reset-failed", s.Unit())
	}
	steps, err := u.InstallSteps(ctx)
	if err != nil {
		return a, err
	}
	if err := host.RunSteps(ctx, steps); err != nil {
		return a, err
	}
	after, _ := os.ReadFile(path)
	relaxed, _ := os.ReadFile(u.NoNamespacesPath())
	if relaxed != nil && relaxedBefore == nil {
		Warn(fmt.Sprintf("Fluxer's service runs with %s turned off, because this machine can't give it its own mount namespace.", strings.Join(host.NoNamespacesOff, " and ")),
			fmt.Sprintf("In an LXC container, turn on nesting, then run rexctl restart to turn them back on. The setting is in %s.", u.NoNamespacesPath()))
	}
	mounted, err := s.MountedHash()
	if err != nil {
		return a, err
	}
	mountedChanged := s.Inst.State.Mounted[mountedKey] != mounted
	s.Inst.State.Mounted = map[string]string{mountedKey: mounted}
	switch {
	case !active:
		a.Started = true
	case r.Changed() || !bytes.Equal(before, after) || !bytes.Equal(relaxedBefore, relaxed):
		if err := s.Restart(ctx); err != nil {
			return a, err
		}
		a.Restarted = true
	case mountedChanged:
		if err := systemctl(ctx, "reload", s.Unit()); err != nil {
			return a, err
		}
		a.Reloaded = true
	}
	return a, nil
}
