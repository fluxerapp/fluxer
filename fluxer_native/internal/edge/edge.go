// SPDX-License-Identifier: AGPL-3.0-or-later

package edge

import (
	"bytes"
	_ "embed"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"text/template"

	"github.com/caddyserver/caddy/v2"
	"github.com/caddyserver/caddy/v2/caddyconfig"
	_ "github.com/caddyserver/caddy/v2/caddyconfig/httpcaddyfile"
	_ "github.com/caddyserver/caddy/v2/modules/standard"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

//go:embed Caddyfile.native
var caddyfile string

//go:embed Caddyfile.voice
var voiceCaddyfile string

var (
	tmpl      = template.Must(template.New("Caddyfile.native").Option("missingkey=error").Funcs(template.FuncMap{"quote": Quote}).Parse(caddyfile))
	voiceTmpl = template.Must(template.New("Caddyfile.voice").Option("missingkey=error").Funcs(template.FuncMap{"quote": Quote}).Parse(voiceCaddyfile))
)

func Quote(s string) string {
	return `"` + strings.NewReplacer(`\`, `\\`, `"`, `\"`).Replace(s) + `"`
}

const SystemBundle = "/etc/ssl/certs/ca-certificates.crt"

type Config struct {
	Site           string
	TLS            string
	Bind           string
	HTTPPort       int
	HTTPSPort      int
	TrustedProxies string
	Encode         string
	Protocols      string
	GlobalOptions  string
	ACMEFirst      string
	RootSite       string
	RedirectTo     string
	AdminBasePath  string
	Dir            string
	CaddyDir       string
	Static         string
	LiveKit        bool
	Container      bool
	VoiceOnly      bool
	Voices         []VoiceRoute
	Off            bool
}

type VoiceRoute struct {
	ID   string
	Addr string
}

func (c Config) Storage() string { return filepath.Join(c.Dir, "caddy") }

func (c Config) RootDir() string {
	return filepath.Join(c.Storage(), "pki", "authorities", "local")
}

func (c Config) RootCert() string {
	return filepath.Join(c.RootDir(), "root.crt")
}

func (c Config) InternalCertDir(name string) string {
	return filepath.Join(c.Storage(), "certificates", "local", strings.ReplaceAll(strings.ToLower(name), ":", "-"))
}

func (c Config) DropInternalCert() error {
	if c.ACMEFirst == "" {
		return nil
	}
	return os.RemoveAll(c.InternalCertDir(c.ACMEFirst))
}

type Mode int

const (
	Starting Mode = iota
	Ready
	Maintenance
)

var Modes = []Mode{Starting, Ready, Maintenance}

type page struct {
	Config
	Ready        bool
	Maintenance  bool
	API          string
	Gateway      string
	Media        string
	Admin        string
	AppProxy     string
	StaticAssets string
	LiveKitAddr  string
	DropIns      bool
	SiteDropIns  bool
}

func Render(c Config, m Mode) ([]byte, error) {
	var b bytes.Buffer
	if c.VoiceOnly {
		err := voiceTmpl.Execute(&b, c)
		return b.Bytes(), err
	}
	err := tmpl.Execute(&b, page{
		Config:       c,
		Ready:        m == Ready,
		Maintenance:  m == Maintenance,
		API:          spec.Addr(spec.PortAPI),
		Gateway:      spec.Addr(spec.PortGateway),
		Media:        spec.Addr(spec.PortMediaProxy),
		Admin:        spec.Addr(spec.PortAdmin),
		AppProxy:     spec.Addr(spec.PortAppProxy),
		StaticAssets: spec.Addr(spec.PortAppProxy),
		LiveKitAddr:  spec.Addr(spec.PortLiveKit),
		DropIns:      matches(filepath.Join(c.CaddyDir, "*.caddy")),
		SiteDropIns:  matches(filepath.Join(c.CaddyDir, "sites", "*.caddy")),
	})
	return b.Bytes(), err
}

func matches(pattern string) bool {
	found, _ := filepath.Glob(pattern)
	return len(found) > 0
}

func Adapt(body []byte) ([]byte, error) {
	cfg, _, err := caddyconfig.GetAdapter("caddyfile").Adapt(body, map[string]any{"filename": "Caddyfile.native"})
	if err != nil {
		return nil, fmt.Errorf("edge: %w", err)
	}
	return cfg, nil
}

func Load(c Config, m Mode) error { return load(c, m, false) }

func Reload(c Config, m Mode) error { return load(c, m, c.CertFiles()) }

func (c Config) CertFiles() bool {
	rest, ok := strings.CutPrefix(c.TLS, "tls ")
	f := strings.Fields(rest)
	return ok && len(f) == 2 && !strings.ContainsAny(rest, "@{")
}

func load(c Config, m Mode, force bool) error {
	if c.Off {
		return Stop()
	}
	if err := os.MkdirAll(c.Storage(), 0o700); err != nil {
		return err
	}
	for k, v := range map[string]string{"XDG_DATA_HOME": c.Dir, "XDG_CONFIG_HOME": filepath.Join(c.Dir, "config")} {
		if err := os.Setenv(k, v); err != nil {
			return err
		}
	}
	body, err := Render(c, m)
	if err != nil {
		return err
	}
	cfg, err := Adapt(body)
	if err != nil {
		return err
	}
	if err := caddy.Load(cfg, force); err != nil {
		if errors.Is(err, fs.ErrPermission) || strings.Contains(err.Error(), "permission denied") {
			if c.Container {
				return fmt.Errorf("edge: %w. Start the container with --sysctl net.ipv4.ip_unprivileged_port_start=0, or set FLUXER_HTTP_PORT=8080 and FLUXER_HTTPS_PORT=8443", err)
			}
			return fmt.Errorf("edge: %w. To listen without that privilege, set FLUXER_HTTP_PORT=8080 and FLUXER_HTTPS_PORT=8443", err)
		}
		return fmt.Errorf("edge: %w", err)
	}
	return nil
}

func Stop() error { return caddy.Stop() }

func TrustBundle(c Config, system, out string) error {
	root, err := os.ReadFile(c.RootCert())
	if err != nil {
		return fmt.Errorf("edge: Caddy's local root certificate: %w", err)
	}
	bundle, err := os.ReadFile(system)
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	if len(bundle) > 0 && !bytes.HasSuffix(bundle, []byte("\n")) {
		bundle = append(bundle, '\n')
	}
	tmp := out + ".tmp"
	if err := os.WriteFile(tmp, append(bundle, root...), 0o644); err != nil {
		return err
	}
	return os.Rename(tmp, out)
}
