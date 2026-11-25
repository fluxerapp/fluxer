// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"net"
	"net/url"
	"os"
	"slices"
	"strings"
	"time"

	"go.yaml.in/yaml/v4"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const ssoProbeSeconds = 8

const ssoDiscoveryMaxBytes = 256 << 10

func init() {
	Register(Check{ID: "sso-provider", Phases: Installed, Run: runSSOProvider, Fix: fixSSOProvider})
}

type execFunc func(ctx context.Context, service, stdin string, command ...string) (string, error)

type ssoProbeInput struct {
	Discovery string `json:"discovery"`
	Issuer    string `json:"issuer,omitempty"`
	Token     string `json:"token"`
	Timeout   int    `json:"timeout_ms"`
	MaxBytes  int    `json:"max_bytes"`
}

type probeTarget struct {
	what      string
	issuerFix string
	tokenFix  string
}

var ssoTarget = probeTarget{
	what:      "your SSO provider",
	issuerFix: "Check sso.issuer with rexctl config get sso.issuer.",
	tokenFix:  "Check sso.token_url with rexctl config get sso.token_url.",
}

type ssoProbeOutput struct {
	OK      bool   `json:"ok"`
	Stage   string `json:"stage"`
	Host    string `json:"host"`
	Code    string `json:"code"`
	Message string `json:"message"`
	Status  int    `json:"status"`
	Token   string `json:"token_host"`
}

const ssoProbeScript = `
const net = require("node:net");
const tls = require("node:tls");
const input = JSON.parse(require("node:fs").readFileSync(0, "utf8"));
const done = (r) => { process.stdout.write(JSON.stringify(r) + "\n"); process.exit(0); };
const host = (u) => u.hostname.replace(/^\[|\]$/g, "");
const why = (e) => {
  let c = e;
  const code = (x) => (x && typeof x.code === "string" ? x.code : "");
  while (c && !code(c) && (c.cause || (c.errors && c.errors.length))) c = c.cause || c.errors[0];
  return { code: code(c) || (c && c.name) || (e && e.name) || "", message: String((c && c.message) || (c && c.errors && c.errors[0] && c.errors[0].message) || e) };
};
const connect = (u) => new Promise((resolve, reject) => {
  const h = host(u);
  const https = u.protocol === "https:";
  const port = Number(u.port) || (https ? 443 : 80);
  const opts = { host: h, port, timeout: input.timeout_ms };
  if (https && net.isIP(h) === 0) opts.servername = h;
  const s = https ? tls.connect(opts, () => { s.destroy(); resolve(); }) : net.connect(opts, () => { s.destroy(); resolve(); });
  s.on("timeout", () => { const e = new Error("connection timed out"); e.code = "ETIMEDOUT"; s.destroy(e); });
  s.on("error", reject);
});
(async () => {
  let token = input.token;
  let discoveryHost = "";
  if (input.discovery) {
    const u = new URL(input.discovery);
    discoveryHost = host(u);
    let res;
    try {
      res = await fetch(u, { redirect: "manual", headers: { accept: "application/json" }, signal: AbortSignal.timeout(input.timeout_ms) });
    } catch (e) {
      done({ ok: false, stage: "discovery", host: discoveryHost, ...why(e) });
    }
    if (res.status < 200 || res.status >= 300) done({ ok: false, stage: "discovery", host: discoveryHost, status: res.status });
    let doc;
    try {
      const chunks = [];
      let size = 0;
      for await (const c of res.body) {
        size += c.length;
        if (size > input.max_bytes) { chunks.length = 0; break; }
        chunks.push(c);
      }
      doc = size > input.max_bytes ? null : JSON.parse(Buffer.concat(chunks).toString("utf8"));
    } catch (e) { doc = null; }
    if (!doc || typeof doc !== "object" || typeof doc.issuer !== "string") done({ ok: false, stage: "discovery", host: discoveryHost, code: "NOT_DISCOVERY" });
    if (input.issuer && doc.issuer.replace(/\/+$/, "") !== input.issuer.replace(/\/+$/, "")) done({ ok: false, stage: "discovery", host: discoveryHost, code: "ISSUER_MISMATCH" });
    if (!token && typeof doc.token_endpoint === "string") token = doc.token_endpoint;
  }
  if (!token) done({ ok: true, host: discoveryHost });
  let t;
  try { t = new URL(token); } catch (e) { done({ ok: true, host: discoveryHost }); }
  try {
    await connect(t);
  } catch (e) {
    done({ ok: false, stage: "token", host: host(t), ...why(e) });
  }
  done({ ok: true, host: discoveryHost, token_host: host(t) });
})().catch((e) => done({ ok: false, stage: "probe", ...why(e) }));
`

func runSSOProvider(ctx context.Context, e *Env) Result {
	r, _ := ssoProvider(ctx, e, e.apiExec)
	return r
}

func (e *Env) apiExec(ctx context.Context, service, stdin string, command ...string) (string, error) {
	if !e.Native || e.Instance == nil {
		return e.exec(ctx, service, stdin, command...)
	}
	set, err := deploy.ReleaseSet(e.Instance)
	if err != nil {
		return "", err
	}
	ctx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	var out bytes.Buffer
	opts := engine.ExecOptions{Stdout: &out, Stderr: &out, Stdin: strings.NewReader(stdin)}
	err = e.stack().Exec(ctx, set, service, opts, command...)
	return strings.TrimSpace(out.String()), err
}

func ssoProvider(ctx context.Context, e *Env, run execFunc) (Result, string) {
	if e.Admin == nil || !e.running(ctx, "api") {
		return skip(""), ""
	}
	cfg, err := e.Admin.Config(ctx)
	if err != nil || !cfg.Sso.Enabled {
		return skip(""), ""
	}
	issuer := trimmed(cfg.Sso.Issuer)
	if issuer == "" {
		return skip(""), ""
	}
	in := ssoProbeInput{Token: trimmed(cfg.Sso.TokenUrl), Timeout: ssoProbeSeconds * 1000, MaxBytes: ssoDiscoveryMaxBytes}
	if trimmed(cfg.Sso.AuthorizationUrl) == "" || in.Token == "" || trimmed(cfg.Sso.JwksUrl) == "" || trimmed(cfg.Sso.UserinfoUrl) == "" {
		in.Discovery = discoveryURL(issuer)
		if in.Discovery == "" {
			return skip(""), ""
		}
	}
	p, ok := runProviderProbe(ctx, run, in)
	if !ok {
		return skip(""), ""
	}
	r := probeResult(p, in.Discovery, ssoTarget, e.containerEngine())
	if r.Status != Fail || !networkCode(p.Code) || !e.Native && e.Engine == nil || !e.thisMachine(ctx, p.Host) {
		return r, ""
	}
	r = fixable(r)
	switch {
	case e.Container:
		r.Fix = fmt.Sprintf("%s runs on this machine. Run rexctl doctor --fix to send Fluxer's connections for %s to this machine, and make your reverse proxy accept connections from %s.", p.Host, p.Host, e.containerEngine())
	case e.Native:
		r.Fix = fmt.Sprintf("%s runs on this machine. Run rexctl doctor --fix to make this machine connect to %s on its own loopback address.", p.Host, p.Host)
	default:
		r.Fix = fmt.Sprintf("%s runs on this machine. Run rexctl doctor --fix to send the API's connections for %s to this machine, and make your reverse proxy accept connections from %s.", p.Host, p.Host, e.containerEngine())
	}
	return r, p.Host
}

func networkCode(code string) bool {
	switch code {
	case "ENOTFOUND", "EAI_AGAIN", "ECONNREFUSED", "ECONNRESET", "TimeoutError", "AbortError", "ETIMEDOUT", "UND_ERR_CONNECT_TIMEOUT", "ENETUNREACH", "EHOSTUNREACH":
		return true
	}
	return false
}

func (e *Env) thisMachine(ctx context.Context, name string) bool {
	if strings.EqualFold(name, e.Domain) {
		return true
	}
	lookupCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	addrs, err := net.DefaultResolver.LookupIPAddr(lookupCtx, name)
	if err != nil {
		return false
	}
	own := map[string]bool{}
	if ifaces, err := net.InterfaceAddrs(); err == nil {
		for _, a := range ifaces {
			if n, ok := a.(*net.IPNet); ok {
				own[n.IP.String()] = true
			}
		}
	}
	if public, err := e.publicAddress(ctx); err == nil && public != "" {
		own[net.ParseIP(public).String()] = true
	}
	for _, a := range addrs {
		if own[a.IP.String()] {
			return true
		}
	}
	return false
}

func fixSSOProvider(ctx context.Context, e *Env) error {
	if e.Instance == nil || !e.Native && e.Engine == nil {
		return errNoProject
	}
	_, name := ssoProvider(ctx, e, e.apiExec)
	if name == "" {
		return errNoFix
	}
	allowPrivateSSO(e)
	if e.Container {
		return addContainerHost(ctx, e, name+":host-gateway")
	}
	if e.Native {
		return e.run(ctx, host.Step{
			Do:  fmt.Sprintf("Adds %s %s to %s, so Fluxer connects to %s on this machine.", loopback, name, hostsFile, name),
			Run: func(ctx context.Context) error { return addHostsEntry(hostsFile, name) },
		})
	}
	path := e.Instance.Path(instance.CustomCompose)
	entry := name + ":host-gateway"
	return e.run(ctx, host.Step{
		Do: fmt.Sprintf("Adds %s to extra_hosts for the api service in %s, then restarts the API.", entry, instance.CustomCompose),
		Run: func(ctx context.Context) error {
			before, err := os.ReadFile(path)
			if err != nil && !errors.Is(err, fs.ErrNotExist) {
				return err
			}
			after, changed, err := withExtraHost(before, "api", entry)
			if err != nil || !changed {
				return err
			}
			mode := fileMode(path, 0o644)
			if err := os.WriteFile(path, after, mode); err != nil {
				return err
			}
			if _, err := deploy.Apply(ctx, e.Instance, e.Engine, deploy.ApplyOptions{Wait: true}); err != nil {
				if before == nil {
					_ = os.Remove(path)
				} else {
					_ = os.WriteFile(path, before, mode)
				}
				return err
			}
			return nil
		},
	})
}

const (
	hostsFile = "/etc/hosts"
	loopback  = "127.0.0.1"
)

func fileMode(path string, def fs.FileMode) fs.FileMode {
	if fi, err := os.Stat(path); err == nil {
		return fi.Mode().Perm()
	}
	return def
}

func addHostsEntry(path, name string) error {
	data, err := os.ReadFile(path)
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	for _, line := range strings.Split(string(data), "\n") {
		f := strings.Fields(line)
		if len(f) > 1 && f[0] == loopback && slices.ContainsFunc(f[1:], func(h string) bool { return strings.EqualFold(h, name) }) {
			return nil
		}
	}
	if len(data) > 0 && !bytes.HasSuffix(data, []byte("\n")) {
		data = append(data, '\n')
	}
	data = append(data, loopback+" "+name+"\n"...)
	return os.WriteFile(path, data, fileMode(path, 0o644))
}

func withExtraHost(data []byte, service, entry string) ([]byte, bool, error) {
	var doc yaml.Node
	if err := yaml.Unmarshal(data, &doc); err != nil {
		return nil, false, err
	}
	prefix := []byte(nil)
	if doc.Kind == 0 || len(doc.Content) == 0 {
		prefix = data
		doc = yaml.Node{Kind: yaml.DocumentNode, Content: []*yaml.Node{{Kind: yaml.MappingNode}}}
	}
	root := doc.Content[0]
	if root.Kind != yaml.MappingNode {
		return nil, false, errors.New("checks: the custom compose file isn't a mapping")
	}
	hosts := child(child(child(root, "services", yaml.MappingNode), service, yaml.MappingNode), "extra_hosts", yaml.SequenceNode)
	if hosts == nil {
		return nil, false, errors.New("checks: extra_hosts in the custom compose file isn't a list")
	}
	for _, n := range hosts.Content {
		if n.Value == entry {
			return data, false, nil
		}
	}
	hosts.Content = append(hosts.Content, &yaml.Node{Kind: yaml.ScalarNode, Value: entry, Style: yaml.DoubleQuotedStyle})
	var out bytes.Buffer
	enc := yaml.NewEncoder(&out)
	enc.SetIndent(2)
	if err := enc.Encode(&doc); err != nil {
		return nil, false, err
	}
	if err := enc.Close(); err != nil {
		return nil, false, err
	}
	if len(prefix) > 0 && !bytes.HasSuffix(prefix, []byte("\n")) {
		prefix = append(prefix, '\n')
	}
	return append(prefix, out.Bytes()...), true, nil
}

func child(m *yaml.Node, key string, kind yaml.Kind) *yaml.Node {
	if m == nil || m.Kind != yaml.MappingNode {
		return nil
	}
	for i := 0; i+1 < len(m.Content); i += 2 {
		if m.Content[i].Value == key {
			v := m.Content[i+1]
			if v.Kind == yaml.ScalarNode && v.Tag == "!!null" {
				v.Kind, v.Tag, v.Value = kind, "", ""
			}
			if v.Kind != kind {
				return nil
			}
			return v
		}
	}
	v := &yaml.Node{Kind: kind}
	m.Content = append(m.Content, &yaml.Node{Kind: yaml.ScalarNode, Value: key}, v)
	return v
}

func runProviderProbe(ctx context.Context, run execFunc, in ssoProbeInput) (ssoProbeOutput, bool) {
	var p ssoProbeOutput
	payload, err := json.Marshal(in)
	if err != nil {
		return p, false
	}
	out, err := run(ctx, "api", string(payload), "node", "-e", ssoProbeScript)
	if err != nil {
		return p, false
	}
	start := strings.Index(out, "{")
	if start < 0 {
		return p, false
	}
	return p, json.NewDecoder(strings.NewReader(out[start:])).Decode(&p) == nil
}

func trimmed(s *string) string {
	if s == nil {
		return ""
	}
	return strings.TrimSpace(*s)
}

func discoveryURL(issuer string) string {
	u, err := url.Parse(issuer)
	if err != nil || u.Scheme == "" || u.Host == "" {
		return ""
	}
	return strings.TrimSuffix(u.String(), "/") + "/.well-known/openid-configuration"
}

func (e *Env) containerEngine() string {
	switch {
	case e.Native && !e.Container:
		return ""
	case e.Engine != nil:
		return e.Engine.Kind.Name()
	case e.Instance != nil && e.Instance.State != nil && e.Instance.State.Engine.Kind != "":
		return engine.Kind(e.Instance.State.Engine.Kind).Name()
	}
	return engine.Docker.Name()
}

func probeResult(p ssoProbeOutput, discovery string, target probeTarget, engineName string) Result {
	if p.OK {
		if p.Token != "" && p.Host != "" && !strings.EqualFold(p.Token, p.Host) {
			return pass(fmt.Sprintf("The API connects to %s at %s and its token endpoint at %s.", target.what, p.Host, p.Token))
		}
		h := p.Host
		if h == "" {
			h = p.Token
		}
		return pass(fmt.Sprintf("The API connects to %s at %s.", target.what, h))
	}
	if p.Stage == "probe" || p.Host == "" {
		return skip("")
	}
	what := target.what
	docFix := fmt.Sprintf("%s Your provider has to serve its discovery document at %s.", target.issuerFix, discovery)
	if p.Stage == "token" {
		what = target.what + "'s token endpoint"
		docFix = target.tokenFix
	}
	network := fmt.Sprintf("Make %s resolve to an address this machine can connect to, and make sure a firewall or proxy there accepts connections from it.", p.Host)
	if engineName != "" {
		network = fmt.Sprintf("Make %s resolve to an address the API container can connect to, and make sure a firewall or proxy there accepts connections from %s.", p.Host, engineName)
	}
	var r Result
	switch {
	case p.Status != 0:
		r = fail(fmt.Sprintf("%s at %s returned HTTP %d to the API.", ui.Capitalise(what), p.Host, p.Status),
			docFix)
	case p.Code == "NOT_DISCOVERY":
		r = fail(fmt.Sprintf("%s at %s didn't send the API a discovery document.", ui.Capitalise(what), p.Host),
			docFix)
	case p.Code == "ISSUER_MISMATCH":
		r = fail(fmt.Sprintf("%s at %s sent a discovery document for another issuer.", ui.Capitalise(what), p.Host),
			target.issuerFix+" It has to match the issuer in the discovery document exactly.")
	case p.Code == "ENOTFOUND" || p.Code == "EAI_AGAIN":
		r = fail(fmt.Sprintf("The API can't look up the address of %s at %s (%s).", what, p.Host, p.Code), network)
	case p.Code == "ECONNREFUSED" || p.Code == "ECONNRESET":
		r = fail(fmt.Sprintf("The API can't connect to %s at %s because the connection was refused (%s).", what, p.Host, p.Code), network)
	case p.Code == "TimeoutError" || p.Code == "AbortError" || p.Code == "ETIMEDOUT" || p.Code == "UND_ERR_CONNECT_TIMEOUT":
		r = fail(fmt.Sprintf("The API can't connect to %s at %s because the connection timed out after %d seconds.", what, p.Host, ssoProbeSeconds), network)
	case p.Code == "ENETUNREACH" || p.Code == "EHOSTUNREACH":
		r = fail(fmt.Sprintf("The API can't connect to %s at %s because there is no network route to it (%s).", what, p.Host, p.Code), network)
	case tlsCode(p.Code):
		r = fail(fmt.Sprintf("The API doesn't trust the certificate of %s at %s (%s).", what, p.Host, p.Code),
			fmt.Sprintf("Give %s a certificate for that name from a public certificate authority.", p.Host))
	default:
		code := p.Code
		if code == "" {
			code = "unknown error"
		}
		r = fail(fmt.Sprintf("The API can't connect to %s at %s (%s).", what, p.Host, code), network)
	}
	if p.Message != "" {
		r.Details = []string{p.Message}
	}
	return r
}

func tlsCode(code string) bool {
	if code == "EPROTO" || code == "DEPTH_ZERO_SELF_SIGNED_CERT" || code == "SELF_SIGNED_CERT_IN_CHAIN" {
		return true
	}
	for _, prefix := range []string{"CERT_", "UNABLE_TO_", "ERR_TLS_", "ERR_SSL_"} {
		if strings.HasPrefix(code, prefix) {
			return true
		}
	}
	return false
}

func addContainerHost(ctx context.Context, e *Env, entry string) error {
	st := e.Instance.State
	if slices.Contains(st.ExtraHosts, entry) {
		return nil
	}
	return e.run(ctx, host.Step{
		Do: fmt.Sprintf("Adds %s to Fluxer's container, then recreates it.", entry),
		Run: func(ctx context.Context) error {
			before := st.ExtraHosts
			st.ExtraHosts = append(slices.Clone(before), entry)
			if err := e.Instance.Save(); err != nil {
				st.ExtraHosts = before
				return err
			}
			if e.Restart == nil {
				return nil
			}
			return e.Restart(ctx, nil)
		},
	})
}

const ssoPrivateVar = "FLUXER_SSO_ALLOW_PRIVATE_ADDRESSES"

func allowPrivateSSO(e *Env) {
	if strings.EqualFold(e.value(ssoPrivateVar), "true") {
		return
	}
	on := "true"
	e.stage(ssoPrivateVar, &on)
}
