// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"errors"
	"fmt"
	"io/fs"
	"net"
	"os"
	"os/user"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"time"

	"github.com/compose-spec/compose-go/v2/template"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

var (
	ErrNotFound     = errors.New("instance: not found")
	ErrLegacy       = errors.New("instance: made by the old installer")
	errExists       = errors.New("instance: already exists")
	errBadProject   = errors.New("instance: project names use lowercase letters, digits, dashes and underscores")
	ErrNoReleaseSet = errors.New("instance: the files for this release are missing")
)

const (
	EnvFileName    = ".env"
	CustomCompose  = settings.CustomFile
	StateDirName   = ".rexctl"
	StateFileName  = "state.json"
	lockFileName   = "lock"
	ReleasesDir    = "releases"
	stackDir       = "stack"
	BackupsDir     = "backups"
	CertsDir       = "certs"
	GeoIPDir       = "geoip"
	CADir          = "ca"
	CaddyDropInDir = "caddy.d"
	CaddySitesDir  = "caddy.d/sites"

	DefaultKeepBackups = 3
	KeepAllBackups     = -1
)

var projectName = regexp.MustCompile(`^[a-z0-9][a-z0-9_-]*$`)

type Instance struct {
	Dir   string
	State *State
}

var rootHome = func() (string, error) {
	u, err := user.LookupId("0")
	if err != nil {
		return "", err
	}
	return u.HomeDir, nil
}

const systemDir = "/opt/fluxer"

func DefaultDir() (string, error) {
	if asRoot() {
		return systemDir, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(home, "fluxer"), nil
}

func fallback() (string, error) {
	dir, err := DefaultDir()
	if err != nil || present(dir) {
		return dir, err
	}
	if !asRoot() {
		if rootManaged(systemDir) {
			return systemDir, nil
		}
		return dir, nil
	}
	if home, err := rootHome(); err == nil && filepath.IsAbs(home) && present(filepath.Join(home, "fluxer")) {
		return filepath.Join(home, "fluxer"), nil
	}
	return dir, nil
}

func present(dir string) bool {
	return exists(filepath.Join(dir, StateDirName, StateFileName)) || isLegacy(dir)
}

func Find(dir string) (*Instance, error) {
	return FindRepairing(dir, nil)
}

func FindRepairing(dir string, keys []string) (*Instance, error) {
	dir, err := filepath.Abs(dir)
	if err != nil {
		return nil, err
	}
	i := &Instance{Dir: dir}
	state, err := loadState(i.StatePath(), keys)
	if errors.Is(err, fs.ErrNotExist) {
		if isLegacy(dir) {
			return nil, ErrLegacy
		}
		return nil, ErrNotFound
	}
	if err != nil {
		return nil, err
	}
	i.State = state
	return i, nil
}

func isLegacy(dir string) bool {
	if exists(filepath.Join(dir, StateDirName, StateFileName)) || !exists(filepath.Join(dir, EnvFileName)) {
		return false
	}
	for _, name := range defaultComposeNames {
		if exists(filepath.Join(dir, name)) {
			return true
		}
	}
	env, _ := envfile.Load(filepath.Join(dir, EnvFileName))
	return env != nil && env.Value("COMPOSE_FILE") != ""
}

func rootManaged(dir string) bool {
	info, err := os.Stat(filepath.Join(dir, StateDirName))
	if err != nil || !info.IsDir() {
		return false
	}
	uid, ok := ownerOf(info)
	return ok && uid == 0
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func Create(dir, project string) (*Instance, error) {
	if !projectName.MatchString(project) {
		return nil, errBadProject
	}
	dir, err := filepath.Abs(dir)
	if err != nil {
		return nil, err
	}
	i := &Instance{Dir: dir}
	if exists(i.StatePath()) {
		return nil, errExists
	}
	if isLegacy(dir) {
		return nil, ErrLegacy
	}
	if exists(i.Path(EnvFileName)) {
		return nil, errExists
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, err
	}
	for _, d := range []string{StateDirName, BackupsDir} {
		if err := os.MkdirAll(i.Path(d), 0o700); err != nil {
			return nil, err
		}
		if err := envfile.RestrictToOwner(i.Path(d)); err != nil {
			return nil, err
		}
	}
	for _, d := range []string{CertsDir, CADir, CaddyDropInDir, CaddySitesDir} {
		if err := os.MkdirAll(i.Path(d), 0o755); err != nil {
			return nil, err
		}
	}
	now := time.Now().UTC().Truncate(time.Second)
	i.State = &State{Schema: StateSchema, Project: project, InstalledAt: now, UpdatedAt: now}
	i.State.normalize()
	return i, nil
}

func (i *Instance) Path(elem ...string) string {
	return filepath.Join(append([]string{i.Dir}, elem...)...)
}

func (i *Instance) StateDir() string { return i.Path(StateDirName) }

func (i *Instance) StatePath() string { return i.Path(StateDirName, StateFileName) }

func (i *Instance) ReleaseDir(version string) string {
	return i.Path(StateDirName, ReleasesDir, version)
}

func (i *Instance) SetDir(version string) string {
	return filepath.Join(i.ReleaseDir(version), stackDir)
}

func (i *Instance) BackupDir() string {
	if i.State != nil && i.State.BackupDir != "" {
		return i.State.BackupDir
	}
	return i.Path(BackupsDir)
}

func (i *Instance) KeepBackups() int {
	if i.State == nil || i.State.KeepBackups == 0 {
		return DefaultKeepBackups
	}
	return i.State.KeepBackups
}

func (i *Instance) Project() string {
	if i.State != nil {
		return i.State.Project
	}
	return ""
}

func (i *Instance) Set() (*stack.Set, error) {
	release := i.State.Release
	if i.State.RunsFluxerd() {
		return i.nativeSet(release)
	}
	if release == "" {
		return nil, ErrNoReleaseSet
	}
	set, err := stack.LoadSet(i.SetDir(release))
	if errors.Is(err, fs.ErrNotExist) && FetchSet != nil {
		if ferr := FetchSet(release, i.SetDir(release)); ferr != nil {
			return nil, fmt.Errorf("%w: %s: %w", ErrNoReleaseSet, release, ferr)
		}
		set, err = stack.LoadSet(i.SetDir(release))
	}
	if err == nil {
		return set, nil
	}
	if !errors.Is(err, fs.ErrNotExist) {
		return nil, err
	}
	return nil, fmt.Errorf("%w: %s", ErrNoReleaseSet, release)
}

var FetchSet func(release, dir string) error

func (i *Instance) nativeSet(release string) (*stack.Set, error) {
	if release == "" {
		release = stack.EmbeddedRelease()
	}
	set, err := stack.LoadCatalogSet(i.SetDir(release), release)
	if errors.Is(err, fs.ErrNotExist) {
		return settings.NativeSet(release), nil
	}
	return set, err
}

func (i *Instance) Custom() ([]byte, error) {
	data, err := os.ReadFile(i.Path(CustomCompose))
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	return data, err
}

func (i *Instance) Environment() (map[string]string, error) {
	set, err := i.Set()
	if err != nil {
		return nil, err
	}
	return effectiveFor(set, i.State)
}

func effectiveFor(set *stack.Set, s *State) (map[string]string, error) {
	var admin map[string]string
	if s.AdminImport != nil {
		admin = s.AdminImport.Values
	}
	return settings.Effective(set, s.Settings, s.Secrets, admin)
}

func (i *Instance) Setting(name string) string {
	if i.State == nil {
		return ""
	}
	set, err := i.Set()
	if err != nil {
		return i.State.Settings[name]
	}
	env, err := effectiveFor(set, i.State)
	if err != nil {
		return i.State.Settings[name]
	}
	if v, ok := env[name]; ok {
		return v
	}
	custom, _ := i.Custom()
	return releaseDefault(set, custom, env, name)
}

func releaseDefault(set *stack.Set, custom []byte, env map[string]string, name string) string {
	def := ""
	if set.Catalog != nil {
		s, _ := set.Setting(name)
		def = s.Default
	} else if defs := composeDefaults(setFiles(set, custom))[name]; len(defs) > 0 {
		def = defs[0]
	}
	if def == "" {
		return ""
	}
	v, ok := interpolate(def, env)
	if !ok {
		return ""
	}
	return v
}

func interpolate(s string, env map[string]string) (string, bool) {
	out, err := template.Substitute(s, func(k string) (string, bool) {
		v, ok := env[k]
		return v, ok
	})
	return out, err == nil
}

func setFiles(set *stack.Set, custom []byte) [][]byte {
	var out [][]byte
	if names, err := stack.ComposeFileNames(set.Files); err == nil {
		for _, name := range names {
			if data, err := fs.ReadFile(set.Files, name); err == nil {
				out = append(out, data)
			}
		}
	}
	if len(custom) > 0 {
		out = append(out, custom)
	}
	return out
}

func (i *Instance) Domain() string {
	return settings.NormalDomain(i.Setting("FLUXER_DOMAIN"))
}

func (i *Instance) Origin() string {
	if o := strings.TrimSpace(i.Setting("FLUXER_PUBLIC_ORIGIN")); o != "" && !strings.Contains(o, "${") {
		return strings.TrimRight(o, "/")
	}
	domain := i.Domain()
	if domain == "" {
		return ""
	}
	scheme := i.Setting("FLUXER_PUBLIC_SCHEME")
	if scheme == "" {
		scheme = "https"
	}
	port := i.Setting("FLUXER_PUBLIC_PORT")
	if port == "" || (scheme == "https" && port == "443") || (scheme == "http" && port == "80") {
		return scheme + "://" + domain
	}
	return scheme + "://" + net.JoinHostPort(settings.BareHost(domain), port)
}

func (i *Instance) Overlays() []stack.Overlay {
	if i.State == nil {
		return nil
	}
	return stack.OverlaysFor(i.State.Settings)
}

func (i *Instance) ComposeFiles() []string {
	files := i.StackFiles(i.Overlays())
	if exists(i.Path(CustomCompose)) {
		files = append(files, i.Path(CustomCompose))
	}
	return files
}

func (i *Instance) MissingFiles() []string {
	files := i.StackFiles(i.Overlays())
	for name := range stack.MountedFiles {
		files = append(files, i.Generated(name))
	}
	var out []string
	for _, f := range files {
		if exists(f) {
			continue
		}
		if rel, err := filepath.Rel(i.Dir, f); err == nil {
			f = rel
		}
		out = append(out, f)
	}
	slices.Sort(out)
	return out
}

func (i *Instance) Save() error {
	if i.State == nil {
		return fmt.Errorf("instance: nothing to save in %s", i.Dir)
	}
	i.State.UpdatedAt = time.Now().UTC().Truncate(time.Second)
	return i.State.Save(i.StatePath())
}

func removeIfExists(path string) error {
	if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return nil
}

func ReleaseDefault(set *stack.Set, name string) string {
	return releaseDefault(set, nil, nil, name)
}
