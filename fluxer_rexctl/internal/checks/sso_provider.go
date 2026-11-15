// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"encoding/json"
	"fmt"
	"net/url"
	"strings"
)

const ssoProbeSeconds = 8

const ssoDiscoveryMaxBytes = 256 << 10

func init() {
	Register(Check{ID: "sso-provider", Phases: Installed, Run: runSSOProvider})
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
	return ssoProvider(ctx, e, e.exec)
}

func ssoProvider(ctx context.Context, e *Env, run execFunc) Result {
	if e.Admin == nil || !e.running(ctx, "api") {
		return skip("")
	}
	cfg, err := e.Admin.Config(ctx)
	if err != nil || !cfg.Sso.Enabled {
		return skip("")
	}
	issuer := trimmed(cfg.Sso.Issuer)
	if issuer == "" {
		return skip("")
	}
	in := ssoProbeInput{Token: trimmed(cfg.Sso.TokenUrl), Timeout: ssoProbeSeconds * 1000, MaxBytes: ssoDiscoveryMaxBytes}
	if trimmed(cfg.Sso.AuthorizationUrl) == "" || in.Token == "" || trimmed(cfg.Sso.JwksUrl) == "" || trimmed(cfg.Sso.UserinfoUrl) == "" {
		in.Discovery = discoveryURL(issuer)
		if in.Discovery == "" {
			return skip("")
		}
	}
	p, ok := runProviderProbe(ctx, run, in)
	if !ok {
		return skip("")
	}
	return probeResult(p, in.Discovery, ssoTarget)
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

func probeResult(p ssoProbeOutput, discovery string, target probeTarget) Result {
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
	network := fmt.Sprintf("Make %s resolve to an address the API container can connect to. Add %q to extra_hosts for the api service with rexctl config edit, and make your reverse proxy accept connections from Docker.", p.Host, p.Host+":host-gateway")
	var r Result
	switch {
	case p.Status != 0:
		r = fail(fmt.Sprintf("%s at %s answered the API with HTTP %d.", capital(what), p.Host, p.Status),
			docFix)
	case p.Code == "NOT_DISCOVERY":
		r = fail(fmt.Sprintf("%s at %s didn't send the API a discovery document.", capital(what), p.Host),
			docFix)
	case p.Code == "ISSUER_MISMATCH":
		r = fail(fmt.Sprintf("%s at %s sent a discovery document for another issuer.", capital(what), p.Host),
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
			fmt.Sprintf("Give %s a certificate for that name from a public certificate authority, or check that extra_hosts for the api service sends %s to the proxy that serves it.", p.Host, p.Host))
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

func capital(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}
