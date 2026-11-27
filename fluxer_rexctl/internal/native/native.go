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
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	ServiceUser     = "fluxer"
	envFileName     = "fluxer.env"
	SecretsFileName = "secrets.env"
	DataDirName     = "data"
	releasesDirName = "releases"
	currentLink     = "current"
	PreviousLink    = "previous"
	fluxerdPath     = "bin/fluxerd"

	controlSocketName  = "fluxerd.sock"
	operatorSocketName = "operator.sock"
	backupsDirName     = "backups"

	APIAddr         = "127.0.0.1:18080"
	apiPort         = "18080"
	defaultLoopback = "127.0.0.1"

	FirstName        = "fluxer"
	VoiceMachineName = "voice"
	voiceChildPrefix = "livekit-"
	etcRoot          = "/etc/fluxer"
	varRoot          = "/var/lib/fluxer"
	loopbackEnv      = "FLUXER_LOOPBACK_ADDRESS"

	dataDirEnv    = "FLUXER_DATA_DIR"
	runtimeDirEnv = "RUNTIME_DIRECTORY"
	edgeModeEnv   = "FLUXER_EDGE_MODE"
	edgeListenEnv = "FLUXER_EDGE_LISTEN_ADDRESS"
	edgeBindEnv   = "FLUXER_EDGE_BIND"

	workerPrefix    = "FLUXER_WORKER_"
	sizingSourceEnv = "FLUXER_SIZING_SOURCE"
	sizingByRexctl  = "rexctl"

	backupOnUpgradeEnv = "FLUXER_BACKUP_ON_UPGRADE"
	installMethodEnv   = "FLUXER_INSTALL_METHOD"
	upgradeCommandEnv  = "FLUXER_UPGRADE_COMMAND"
	installedByRexctl  = "rexctl"

	ContainerProxyBind = "0.0.0.0:8080"
)

var ErrNotRoot = errors.New("native: the native runtime is managed as root")

type Stack struct {
	Inst    *instance.Instance
	Runtime string
	Dial    func(ctx context.Context, target string) (net.Conn, error)
	Host    func(ctx context.Context) (state string, restarts int)

	replaced string
}

const (
	DialControl  = "control"
	DialOperator = "operator"
	DialAPI      = "api"
)

func New(inst *instance.Instance) *Stack {
	return &Stack{Inst: inst}
}

func (s *Stack) fhs() bool { return s.Inst.State != nil && s.Inst.State.Layout == instance.LayoutFHS }

func (s *Stack) Unit() string {
	if s.fhs() {
		return UnitName(s.Inst.Project())
	}
	return s.Inst.Project() + ".service"
}

func (s *Stack) Root() string {
	if s.fhs() {
		return DataPath(s.Inst.Project())
	}
	return s.Inst.Path(DataDirName)
}

func (s *Stack) SettingsDir() string {
	if s.fhs() {
		return SettingsPath(s.Inst.Project())
	}
	return s.Root()
}

func (s *Stack) Loopback() string {
	if s.Inst.State != nil && s.Inst.State.Loopback != "" {
		return s.Inst.State.Loopback
	}
	return defaultLoopback
}

func (s *Stack) APIAddr() string { return net.JoinHostPort(s.Loopback(), apiPort) }

var InternalPorts = []int{
	14222, 15432, 16379, 17700, 17880,
	18080, 18081, 18082, 18083, 18084, 18085,
	18090, 18091, 18092, 18093, 18094,
	18333, 19333, 19334, 19335,
}

func InternalPortsFree(loopback string) []int {
	var busy []int
	for _, p := range InternalPorts {
		l, err := net.Listen("tcp", net.JoinHostPort(loopback, strconv.Itoa(p)))
		if err != nil {
			busy = append(busy, p)
			continue
		}
		_ = l.Close()
	}
	return busy
}

func FreeLoopback(taken []string) string {
	for n := 2; n < 255; n++ {
		addr := "127.0.0." + strconv.Itoa(n)
		if slices.Contains(taken, addr) || len(InternalPortsFree(addr)) > 0 {
			continue
		}
		return addr
	}
	return ""
}

func UnitName(name string) string {
	if name == FirstName {
		return FirstName + ".service"
	}
	return FirstName + "@" + name + ".service"
}

func DataPath(name string) string {
	switch name {
	case FirstName:
		return varRoot
	case VoiceMachineName:
		return varRoot + "-" + VoiceMachineName
	}
	return filepath.Join(varRoot, name)
}

func SettingsPath(name string) string {
	if name == FirstName {
		return etcRoot
	}
	return filepath.Join(etcRoot, name)
}

var fluxerdEntries = []string{
	"admin", "api", "app-proxy", "backups", "edge", "gateway", "gifs", "host", "livekit", "media-proxy",
	"meilisearch", "messages", "nats", "objects", "postgres", "push", "rust", "seaweedfs", "seaweedfs-filer",
	"seaweedfs-init", "snowflakes", "tmp", "unfurl", "users", "valkey", "voice", "certs", "caddy.d", "ca",
}

func ReservedName(name string) bool {
	return slices.Contains(fluxerdEntries, name) || strings.HasPrefix(name, voiceChildPrefix)
}

func SettingsTaken(name string) bool {
	_, err := os.Lstat(filepath.Join(SettingsPath(name), envFileName))
	return err == nil
}

func NameTaken(name string) bool {
	paths := []string{filepath.Join(SettingsPath(name), envFileName), filepath.Join("/etc/systemd/system", UnitName(name))}
	if name != FirstName {
		paths = append(paths, DataPath(name))
	} else {
		paths = append(paths, filepath.Join(varRoot, SecretsFileName))
	}
	for _, p := range paths {
		if _, err := os.Lstat(p); err == nil {
			return true
		}
	}
	return false
}

func Nested(root string) []string {
	if root != varRoot {
		return nil
	}
	ents, err := os.ReadDir(root)
	if err != nil {
		return nil
	}
	var out []string
	for _, e := range ents {
		if !e.IsDir() || ReservedName(e.Name()) {
			continue
		}
		if _, err := os.Stat(filepath.Join(SettingsPath(e.Name()), envFileName)); err == nil {
			out = append(out, e.Name())
		}
	}
	return out
}

func (s *Stack) RemoveSettings() error {
	if !s.fhs() {
		return nil
	}
	var err error
	if s.Inst.Project() == FirstName {
		err = os.Remove(s.EnvFile())
		if errors.Is(err, fs.ErrNotExist) {
			err = nil
		}
	} else {
		err = os.RemoveAll(s.SettingsDir())
	}
	_ = os.Remove(etcRoot)
	return err
}

func (s *Stack) RemoveData() error {
	if err := RemoveData(s.Root()); err != nil {
		return err
	}
	if s.fhs() {
		_ = os.Remove(varRoot)
	}
	return nil
}

func RemoveData(root string) error {
	keep := Nested(root)
	if len(keep) == 0 {
		return os.RemoveAll(root)
	}
	ents, err := os.ReadDir(root)
	if err != nil {
		return err
	}
	for _, e := range ents {
		if slices.Contains(keep, e.Name()) {
			continue
		}
		if err := os.RemoveAll(filepath.Join(root, e.Name())); err != nil {
			return err
		}
	}
	return nil
}

func (s *Stack) RuntimeDir() string {
	if s.Runtime != "" {
		return s.Runtime
	}
	if dir := runningRuntimeDir(context.Background(), s.Unit()); dir != "" {
		return dir
	}
	return s.DefaultRuntimeDir()
}

func (s *Stack) DefaultRuntimeDir() string { return host.NativeRuntimeDir(s.Inst.Project(), s.fhs()) }

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

func (s *Stack) ControlSocket() string { return filepath.Join(s.RuntimeDir(), controlSocketName) }

func (s *Stack) OperatorSocket() string { return filepath.Join(s.RuntimeDir(), operatorSocketName) }

func (s *Stack) EnvFile() string { return filepath.Join(s.SettingsDir(), envFileName) }

func (s *Stack) SecretsFile() string { return filepath.Join(s.Root(), SecretsFileName) }

func (s *Stack) ReleasesDir() string { return s.Inst.Path(releasesDirName) }

func (s *Stack) ReleaseDir(version string) string { return filepath.Join(s.ReleasesDir(), version) }

func (s *Stack) Current() string { return s.Inst.Path(currentLink) }

func (s *Stack) Fluxerd() string { return filepath.Join(s.Current(), fluxerdPath) }

func (s *Stack) BootUnit() host.BootUnit {
	bin, _ := host.RunningBinary()
	return host.BootUnit{
		Binary:   bin,
		Project:  s.Inst.Project(),
		Unit:     s.Unit(),
		Dir:      s.Inst.Dir,
		Native:   true,
		FHS:      s.fhs(),
		User:     ServiceUser,
		Fluxerd:  s.Fluxerd(),
		EnvFile:  s.EnvFile(),
		DataDir:  s.Root(),
		CaddyDir: s.Inst.Path(instance.CaddyDropInDir),
		CertsDir: s.Inst.Path(instance.CertsDir),
		Stopped:  s.StoppedMarker(),
		Voice:    s.Inst.State.IsVoice(),
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
	"FLUXER_EDGE_ACME_CA_ROOT",
}

const (
	ContainerData  = "/var/lib/fluxer"
	ContainerFiles = ContainerData + "/host"
	certsPrefix    = "/certs/"
)

func PathKey(key string) bool { return slices.Contains(pathKeys, key) }

func MappedPath(path string) bool {
	return strings.HasPrefix(path, certsPrefix) || strings.HasPrefix(path, settings.GeoIPMountPoint) || strings.HasPrefix(path, stack.NodeCADir+"/")
}

func translatePaths(env map[string]string, dir string) {
	prefixes := [][2]string{
		{stack.NodeCADir + "/", filepath.Join(dir, instance.CADir) + "/"},
		{certsPrefix, filepath.Join(dir, instance.CertsDir) + "/"},
		{settings.GeoIPMountPoint, filepath.Join(dir, instance.GeoIPDir) + "/"},
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
	secrets = map[string]string{}
	maps.Copy(secrets, st.Secrets)
	for k, v := range secrets {
		if env[k] == v {
			delete(env, k)
		}
	}
	delete(env, stack.SettingsLoadedVar)
	maps.DeleteFunc(env, func(k, _ string) bool { return strings.HasPrefix(k, workerPrefix) })
	env[sizingSourceEnv] = sizingByRexctl
	env[backupOnUpgradeEnv] = "false"
	env[installMethodEnv] = installedByRexctl
	env[upgradeCommandEnv] = instance.UpgradeCommand(s.Inst.Dir)
	if st.Loopback != "" {
		env[loopbackEnv] = st.Loopback
	}
	mode := st.Settings[settings.EdgeMode]
	if mode == "" {
		mode = settings.EdgeBundled
	}
	env[edgeModeEnv] = mode
	if mode != settings.EdgeBundled {
		bind, err := ProxyListen(st.Settings[settings.EdgeBind])
		if err != nil {
			return nil, nil, err
		}
		host, _, _ := net.SplitHostPort(bind)
		if st.IsContainer() {
			bind, host = ContainerProxyBind, "0.0.0.0"
		}
		env[edgeBindEnv] = bind
		env[edgeListenEnv] = host
	}
	if st.IsContainer() {
		translatePaths(env, ContainerFiles)
	} else {
		translatePaths(env, s.Inst.Dir)
	}
	return env, secrets, nil
}

func (s *Stack) EdgeBind() string {
	st := s.Inst.State
	if st == nil || st.Settings[settings.EdgeMode] == "" || st.Settings[settings.EdgeMode] == settings.EdgeBundled {
		return ""
	}
	bind, err := ProxyListen(st.Settings[settings.EdgeBind])
	if err != nil {
		return ""
	}
	return bind
}

func (s *Stack) PinEdgeBind() {
	st := s.Inst.State
	if mode := st.Settings[settings.EdgeMode]; mode == "" || mode == settings.EdgeBundled || strings.TrimSpace(st.Settings[settings.EdgeBind]) != "" {
		return
	}
	st.Settings[settings.EdgeBind] = settings.DefaultBind
}

func ProxyListen(bind string) (string, error) {
	if strings.TrimSpace(bind) == "" {
		return settings.DefaultBind, nil
	}
	return settings.NormaliseBind(bind)
}

type Rendered struct {
	Env     bool
	Secrets bool
}

func (r Rendered) Changed() bool { return r.Env || r.Secrets }

func (s *Stack) Render(set *stack.Set) (Rendered, error) {
	var r Rendered
	old, err := envfile.LoadRaw(s.SecretsFile())
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return r, err
	}
	envData, secretsData, err := s.Files(set, old)
	if err != nil {
		return r, err
	}
	if s.fhs() && s.Inst.Project() != FirstName {
		for _, dir := range []string{varRoot, etcRoot} {
			if err := os.MkdirAll(dir, 0o755); err != nil {
				return r, err
			}
		}
	}
	if err := os.MkdirAll(s.Root(), 0o750); err != nil {
		return r, err
	}
	if err := os.MkdirAll(s.SettingsDir(), 0o750); err != nil {
		return r, err
	}
	if r.Env, err = writeIfChanged(s.EnvFile(), envData); err != nil {
		return r, err
	}
	if r.Secrets, err = writeIfChanged(s.SecretsFile(), secretsData); err != nil {
		return r, err
	}
	return r, s.chownData(s.EnvFile(), s.SecretsFile())
}

func (s *Stack) Files(set *stack.Set, oldSecrets map[string]string) (envData, secretsData []byte, err error) {
	env, secrets, err := s.Environment(set)
	if err != nil {
		return nil, nil, err
	}
	if envData, err = envfile.MarshalRaw(env); err != nil {
		return nil, nil, err
	}
	for _, k := range fluxerdSecrets() {
		if _, ok := secrets[k]; !ok && oldSecrets[k] != "" {
			secrets[k] = oldSecrets[k]
		}
	}
	secretsData, err = envfile.MarshalRaw(secrets)
	return envData, secretsData, err
}

var mintedByFluxerd = []string{"FLUXER_NATS_AUTH_TOKEN", "FLUXER_VALKEY_PASSWORD", setup.TokenKey}

func fluxerdSecrets() []string {
	keys := slices.Clone(mintedByFluxerd)
	for _, sp := range secrets.Required {
		keys = append(keys, sp.Key)
	}
	return keys
}

func (s *Stack) ReadSecrets() (map[string]string, error) {
	return envfile.LoadRaw(s.SecretsFile())
}

func writeIfChanged(path string, data []byte) (bool, error) {
	if old, err := os.ReadFile(path); err == nil && bytes.Equal(old, data) {
		return false, nil
	}
	return true, envfile.WriteFileAtomic(path, data, true)
}

func serviceIDs() (int, int, error) {
	u, err := user.Lookup(ServiceUser)
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
	if s.fhs() {
		if err := os.Lchown(s.SettingsDir(), 0, gid); err != nil {
			return err
		}
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

func (s *Stack) RunsCurrent(ctx context.Context) bool { return s.runsCurrent(ctx) }

func (s *Stack) runsCurrent(ctx context.Context) bool {
	pid := unitProperty(ctx, s.Unit(), "MainPID")
	if pid == "" || pid == "0" {
		return false
	}
	running, err := os.Readlink(filepath.Join("/proc", pid, "exe"))
	if err != nil {
		return false
	}
	want, err := filepath.EvalSymlinks(s.Fluxerd())
	return err == nil && running == want
}

type Applied struct {
	Restarted bool
	Started   bool
	Reloaded  []string
}

type ApplyMode int

const (
	ApplyChanges ApplyMode = iota
	ApplyRestart
)

func (s *Stack) Prepare(ctx context.Context, set *stack.Set) error {
	if os.Geteuid() != 0 {
		return ErrNotRoot
	}
	if err := EnsureUser(ctx); err != nil {
		return err
	}
	s.PinEdgeBind()
	_, err := s.Render(set)
	return err
}

func (s *Stack) Apply(ctx context.Context, set *stack.Set, warn func(msg, fix string), mode ApplyMode) (Applied, error) {
	return s.apply(ctx, func() (Rendered, error) {
		s.PinEdgeBind()
		return s.Render(set)
	}, warn, mode)
}

func (s *Stack) ApplyVoice(ctx context.Context, env map[string]string, warn func(msg, fix string)) (Applied, error) {
	return s.apply(ctx, func() (Rendered, error) { return s.RenderVoice(env) }, warn, ApplyChanges)
}

func (s *Stack) RenderVoice(env map[string]string) (Rendered, error) {
	var r Rendered
	if err := os.MkdirAll(s.Root(), 0o750); err != nil {
		return r, err
	}
	data, err := envfile.MarshalRaw(env)
	if err != nil {
		return r, err
	}
	if r.Env, err = writeIfChanged(s.EnvFile(), data); err != nil {
		return r, err
	}
	return r, s.chownData(s.EnvFile())
}

func (s *Stack) apply(ctx context.Context, render func() (Rendered, error), warn func(msg, fix string), mode ApplyMode) (Applied, error) {
	var a Applied
	if os.Geteuid() != 0 {
		return a, ErrNotRoot
	}
	if err := EnsureUser(ctx); err != nil {
		return a, err
	}
	r, err := render()
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
	active := unitProperty(ctx, s.Unit(), "ActiveState") == "active"
	if !active {
		if err := s.clearStopped(); err != nil {
			return a, err
		}
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
		warn(fmt.Sprintf("Fluxer's service runs with %s turned off, because this machine can't give it its own mount namespace.", strings.Join(host.NoNamespacesOff, " and ")),
			fmt.Sprintf("In an LXC container, turn on nesting, then run rexctl restart to turn them back on. The setting is in %s.", u.NoNamespacesPath()))
	}
	mounted, err := s.MountedHash()
	if err != nil {
		return a, err
	}
	mountedChanged := s.Inst.State.Mounted[mountedKey] != mounted
	s.Inst.State.Mounted = map[string]string{mountedKey: mounted}
	unitChanged := !bytes.Equal(before, after) || !bytes.Equal(relaxedBefore, relaxed)
	restart := mode == ApplyRestart || unitChanged
	if active && !restart && (r.Changed() || mountedChanged) {
		restart = !s.runsCurrent(ctx)
	}
	switch {
	case !active:
		a.Started = true
	case !restart && r.Changed():
		reloaded, err := s.Reload(ctx)
		switch {
		case err == nil:
			a.Reloaded = reloaded
			return a, nil
		case !errors.Is(err, errRestartNeeded) && !errors.Is(err, errNoSupervisor):
			return a, err
		}
		fallthrough
	case restart:
		if err := s.Restart(ctx); err != nil {
			return a, err
		}
		a.Restarted = true
	case mountedChanged:
		if err := systemctl(ctx, "reload", s.Unit()); err != nil {
			return a, err
		}
	}
	return a, nil
}
