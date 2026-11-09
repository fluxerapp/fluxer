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

var tmpl = template.Must(template.New("Caddyfile.native").Option("missingkey=error").Parse(caddyfile))

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
	AdminBasePath  string
	Dir            string
	CaddyDir       string
	Static         string
	LiveKit        bool
}

func (c Config) Storage() string { return filepath.Join(c.Dir, "caddy") }

func (c Config) RootCert() string {
	return filepath.Join(c.Storage(), "pki", "authorities", "local", "root.crt")
}

type page struct {
	Config
	Ready       bool
	API         string
	Gateway     string
	Media       string
	Admin       string
	AppProxy    string
	LiveKitAddr string
}

func Render(c Config, ready bool) ([]byte, error) {
	var b bytes.Buffer
	err := tmpl.Execute(&b, page{
		Config:      c,
		Ready:       ready,
		API:         spec.Addr(spec.PortAPI),
		Gateway:     spec.Addr(spec.PortGateway),
		Media:       spec.Addr(spec.PortMediaProxy),
		Admin:       spec.Addr(spec.PortAdmin),
		AppProxy:    spec.Addr(spec.PortAppProxy),
		LiveKitAddr: spec.Addr(spec.PortLiveKit),
	})
	return b.Bytes(), err
}

func Adapt(body []byte) ([]byte, error) {
	cfg, _, err := caddyconfig.GetAdapter("caddyfile").Adapt(body, map[string]any{"filename": "Caddyfile.native"})
	if err != nil {
		return nil, fmt.Errorf("edge: %w", err)
	}
	return cfg, nil
}

func Load(c Config, ready bool) error {
	if err := os.MkdirAll(c.Storage(), 0o700); err != nil {
		return err
	}
	for k, v := range map[string]string{"XDG_DATA_HOME": c.Dir, "XDG_CONFIG_HOME": filepath.Join(c.Dir, "config")} {
		if err := os.Setenv(k, v); err != nil {
			return err
		}
	}
	body, err := Render(c, ready)
	if err != nil {
		return err
	}
	cfg, err := Adapt(body)
	if err != nil {
		return err
	}
	if err := caddy.Load(cfg, false); err != nil {
		if errors.Is(err, fs.ErrPermission) || strings.Contains(err.Error(), "permission denied") {
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
