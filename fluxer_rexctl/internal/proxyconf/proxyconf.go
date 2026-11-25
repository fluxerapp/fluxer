// SPDX-License-Identifier: AGPL-3.0-or-later

package proxyconf

import (
	"bufio"
	"bytes"
	"embed"
	"fmt"
	"net"
	"net/netip"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"text/template"
)

//go:embed files
var files embed.FS

type Server string

const (
	Nginx       Server = "nginx"
	Caddy       Server = "caddy"
	Apache      Server = "apache"
	Traefik     Server = "traefik"
	Cloudflared Server = "cloudflared"
)

var servers = []Server{Nginx, Caddy, Apache, Traefik, Cloudflared}

const Dir = "proxy"

type Params struct {
	Domain   string
	Upstream string
	Private  bool
}

const ownCertDir = "/etc/ssl/fluxer"

func (p Params) Cert() string {
	if p.Private {
		return ownCertDir + "/" + p.Domain + ".crt"
	}
	return "/etc/letsencrypt/live/" + p.Domain + "/fullchain.pem"
}

func (p Params) Key() string {
	if p.Private {
		return ownCertDir + "/" + p.Domain + ".key"
	}
	return "/etc/letsencrypt/live/" + p.Domain + "/privkey.pem"
}

func (p Params) certCommands(plugin string) []string {
	if !p.Private {
		return []string{"sudo certbot certonly --" + plugin + " -d " + p.Domain}
	}
	san := "DNS:" + p.Domain
	if net.ParseIP(p.Domain) != nil {
		san = "IP:" + p.Domain
	}
	return []string{
		"sudo mkdir -p " + ownCertDir,
		"sudo openssl req -x509 -newkey rsa:2048 -nodes -days 825 -subj /CN=" + p.Domain + " -addext subjectAltName=" + san + " -keyout " + p.Key() + " -out " + p.Cert(),
	}
}

func Parse(name string) (Server, bool) {
	s := Server(strings.ToLower(strings.TrimSpace(name)))
	return s, slices.Contains(servers, s)
}

func (s Server) Name() string {
	switch s {
	case Nginx:
		return "nginx"
	case Caddy:
		return "Caddy"
	case Apache:
		return "Apache"
	case Traefik:
		return "Traefik"
	case Cloudflared:
		return "Cloudflare Tunnel"
	}
	return string(s)
}

func (s Server) File() string {
	switch s {
	case Traefik, Cloudflared:
		return string(s) + ".yml"
	}
	return string(s) + ".conf"
}

func Render(s Server, p Params) (string, error) {
	if !slices.Contains(servers, s) {
		return "", fmt.Errorf("proxyconf: unknown server %q", s)
	}
	raw, err := files.ReadFile("files/" + s.File())
	if err != nil {
		return "", err
	}
	t, err := template.New(string(s)).Option("missingkey=error").Parse(string(raw))
	if err != nil {
		return "", err
	}
	var b bytes.Buffer
	if err := t.Execute(&b, p); err != nil {
		return "", err
	}
	return b.String(), nil
}

func Write(dir string, p Params) (map[Server]string, error) {
	out := map[Server]string{}
	folder := filepath.Join(dir, Dir)
	if err := os.MkdirAll(folder, 0o755); err != nil {
		return nil, err
	}
	for _, s := range servers {
		text, err := Render(s, p)
		if err != nil {
			return nil, err
		}
		path := filepath.Join(folder, s.File())
		if err := writeFile(path, text); err != nil {
			return nil, err
		}
		out[s] = path
	}
	return out, nil
}

func writeFile(path, text string) error {
	tmp := path + ".tmp"
	if err := os.WriteFile(tmp, []byte(text), 0o644); err != nil {
		return err
	}
	return os.Rename(tmp, path)
}

func Path(dir string, s Server) string {
	return filepath.Join(dir, Dir, s.File())
}

const (
	nginxSite  = "/etc/nginx/conf.d/fluxer.conf"
	nginxOld   = "/etc/nginx/sites-enabled/fluxer.conf"
	apacheSite = "/etc/apache2/sites-available/fluxer.conf"
	caddySite  = "/etc/caddy/fluxer.caddy"
)

var EnabledPaths = []string{nginxSite, nginxOld, apacheSite, caddySite}

func Enable(s Server, path string, p Params) []string {
	switch s {
	case Nginx:
		return append(p.certCommands("nginx"),
			"sudo cp "+path+" "+nginxSite,
			"sudo nginx -t && sudo systemctl reload nginx",
		)
	case Apache:
		return append(append([]string{"sudo a2enmod proxy proxy_http headers ssl"}, p.certCommands("apache")...),
			"sudo cp "+path+" "+apacheSite,
			"sudo a2ensite fluxer && sudo systemctl reload apache2",
		)
	case Caddy:
		return []string{
			"sudo cp " + path + " " + caddySite,
			"grep -q fluxer.caddy /etc/caddy/Caddyfile || echo 'import /etc/caddy/fluxer.caddy' | sudo tee -a /etc/caddy/Caddyfile",
			"sudo systemctl reload caddy",
		}
	case Traefik:
		return []string{"Copy " + path + " into the folder Traefik's file provider watches."}
	case Cloudflared:
		return []string{
			"cloudflared tunnel create fluxer",
			"cloudflared tunnel route dns fluxer " + p.Domain,
			"cloudflared tunnel --config " + path + " run fluxer",
		}
	}
	return nil
}

func ServerFor(owner string) (Server, bool) {
	o := strings.ToLower(owner)
	switch {
	case strings.Contains(o, "nginx"), strings.Contains(o, "openresty"), strings.Contains(o, "angie"):
		return Nginx, true
	case strings.Contains(o, "caddy"):
		return Caddy, true
	case strings.Contains(o, "apache"), strings.Contains(o, "httpd"):
		return Apache, true
	case strings.Contains(o, "traefik"):
		return Traefik, true
	case strings.Contains(o, "cloudflared"):
		return Cloudflared, true
	}
	return "", false
}

func cloudflareRanges() []netip.Prefix {
	raw, err := files.ReadFile("files/cloudflare.txt")
	if err != nil {
		return nil
	}
	var out []netip.Prefix
	sc := bufio.NewScanner(bytes.NewReader(raw))
	for sc.Scan() {
		if p, err := netip.ParsePrefix(strings.TrimSpace(sc.Text())); err == nil {
			out = append(out, p)
		}
	}
	return out
}

func IsCloudflare(addr netip.Addr) bool {
	addr = addr.Unmap()
	for _, p := range cloudflareRanges() {
		if p.Contains(addr) {
			return true
		}
	}
	return false
}

func CloudflareTrusted() string {
	parts := []string{"private_ranges"}
	for _, p := range cloudflareRanges() {
		parts = append(parts, p.String())
	}
	return strings.Join(parts, ",")
}
