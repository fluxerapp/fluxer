// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"cmp"
	"context"
	"crypto/tls"
	"crypto/x509"
	"errors"
	"fmt"
	"net"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

func init() {
	Register(Check{ID: "certificate", Phases: Installed, Run: runCertificate, Fix: fixCertificate})
	Register(Check{ID: "local-certificate", Phases: Installed, Run: runLocalCertificate})
}

func runCertificate(ctx context.Context, e *Env) Result {
	if e.Instance == nil || e.mode() != modeBundled || privateDomain(e.Domain) || overlayDomain(e.Domain) {
		return skip("")
	}
	if strings.EqualFold(e.value("FLUXER_PUBLIC_SCHEME"), "http") || strings.EqualFold(e.value("FLUXER_EDGE_TLS"), "internal") {
		return skip("")
	}
	d := tls.Dialer{
		NetDialer: &net.Dialer{Timeout: 10 * time.Second},
		Config:    &tls.Config{ServerName: settings.BareHost(e.Domain)},
	}
	dialCtx, cancel := context.WithTimeout(ctx, 15*time.Second)
	defer cancel()
	own := ownCertificate(e.value("FLUXER_EDGE_TLS"))
	conn, err := d.DialContext(dialCtx, "tcp", e.edgeAddr("FLUXER_HTTPS_PORT", "443"))
	if err != nil {
		var opErr *net.OpError
		if errors.As(err, &opErr) && opErr.Op == "dial" || errors.Is(err, context.DeadlineExceeded) || ctx.Err() != nil {
			return skip("")
		}
		if own {
			return fail(fmt.Sprintf("The edge couldn't serve a valid certificate for %s.", e.Domain),
				fmt.Sprintf("Check that the certificate file you set in FLUXER_EDGE_TLS covers %s, hasn't expired and comes from a public certificate authority. Run rexctl logs edge to see whether the edge could load it.", e.Domain))
		}
		if _, ip := settings.DomainIP(e.Domain); ip && e.servesInternal(dialCtx) {
			return fixable(fail(fmt.Sprintf("%s still uses the certificate Fluxer signs itself, so browsers show a warning.", e.Domain), FirewallFix+", then run rexctl restart to ask Let's Encrypt again."))
		}
		problem := fmt.Sprintf("Couldn't get a certificate for %s.", e.Domain)
		if tailnetDomain(e.Domain) && e.Container {
			return fail(problem, fmt.Sprintf("Fluxer in a container can't get a certificate from Tailscale. Run rexctl proxy on, then sudo tailscale serve --bg http://%s, then run rexctl doctor.", cmp.Or(e.setting(settings.EdgeBind), settings.DefaultBind)))
		}
		if tailnetDomain(e.Domain) {
			return fail(problem, TailnetCertFix+", then run rexctl doctor.")
		}
		switch LookupDomain(ctx, e.Domain).Points {
		case PointsElsewhere, PointsNowhere:
			return fail(problem, fmt.Sprintf("Fluxer gets one once %s points to this machine. Run rexctl doctor to see what is wrong with its DNS record.", e.Domain))
		}
		ca := e.acmeCA()
		return fail(problem, classifyACME(e.edgeLogs(ctx), ca).fix(e.Domain, ca))
	}
	defer conn.Close()
	certs := conn.(*tls.Conn).ConnectionState().PeerCertificates
	if len(certs) == 0 {
		return skip("")
	}
	expires := certs[0].NotAfter
	if renewOverdue(certs[0], time.Now()) {
		fix := "Run rexctl logs edge to see why it hasn't renewed."
		if own {
			fix = "Put a renewed certificate in place of the file you set in FLUXER_EDGE_TLS, then run rexctl restart."
		}
		return warn(fmt.Sprintf("The certificate for %s expires on %s.", e.Domain, day(expires)), fix)
	}
	return pass(fmt.Sprintf("The certificate for %s is valid until %s.", e.Domain, day(expires)))
}

func renewOverdue(c *x509.Certificate, now time.Time) bool {
	return c.NotAfter.Sub(now) < min(14*24*time.Hour, c.NotAfter.Sub(c.NotBefore)/4)
}

func ownCertificate(directive string) bool {
	f := strings.Fields(directive)
	return len(f) >= 2 && f[0] != "internal" && f[1] != "{" && !strings.Contains(f[0], "@")
}

const FirewallFix = "Open TCP ports 80 and 443 and UDP port 443 in your hosting provider's firewall, such as a Hetzner Cloud Firewall or an AWS security group, and forward them to this machine if it is behind a router"

func (e *Env) servesInternal(ctx context.Context) bool {
	certs, err := e.edgeChain(ctx)
	return err == nil && len(certs) > 0 && internalIssuer(certs[0].Issuer.CommonName)
}

func (e *Env) edgeChain(ctx context.Context) ([]*x509.Certificate, error) {
	d := tls.Dialer{
		NetDialer: &net.Dialer{Timeout: 10 * time.Second},
		Config:    &tls.Config{ServerName: settings.BareHost(e.Domain), InsecureSkipVerify: true},
	}
	conn, err := d.DialContext(ctx, "tcp", e.edgeAddr("FLUXER_HTTPS_PORT", "443"))
	if err != nil {
		return nil, err
	}
	defer conn.Close()
	return conn.(*tls.Conn).ConnectionState().PeerCertificates, nil
}

func internalIssuer(cn string) bool {
	return ownAuthority(cn, " ECC Intermediate")
}

func ownAuthority(cn, suffix string) bool {
	return strings.HasSuffix(cn, suffix) && (strings.HasPrefix(cn, "Caddy Local Authority") || strings.HasPrefix(cn, "Fluxer"))
}

func runLocalCertificate(ctx context.Context, e *Env) Result {
	if e.Instance == nil || e.mode() != modeBundled || overlayDomain(e.Domain) || tailnetDomain(e.Domain) {
		return skip("")
	}
	edgeTLS := strings.TrimSpace(e.value("FLUXER_EDGE_TLS"))
	if !privateDomain(e.Domain) && !strings.EqualFold(edgeTLS, "internal") {
		return skip("")
	}
	if strings.EqualFold(e.value("FLUXER_PUBLIC_SCHEME"), "http") || ownCertificate(edgeTLS) || strings.TrimSpace(e.value("FLUXER_EDGE_ACME_CA")) != "" {
		return skip("")
	}
	dialCtx, cancel := context.WithTimeout(ctx, 15*time.Second)
	defer cancel()
	chain, err := e.edgeChain(dialCtx)
	if err != nil || len(chain) == 0 {
		return skip("")
	}
	return localChainResult(e.Domain, edgeTLS, chain)
}

func localChainResult(domain, edgeTLS string, chain []*x509.Certificate) Result {
	leaf := chain[0]
	fromOwnCA := internalIssuer(leaf.Issuer.CommonName) && (len(chain) < 2 || ownAuthority(chain[1].Issuer.CommonName, " ECC Root"))
	if !fromOwnCA {
		fix := "Check that no other program on this machine uses port 443, then run rexctl restart."
		if edgeTLS != "" && !strings.EqualFold(edgeTLS, "internal") {
			fix = "Run rexctl config unset FLUXER_EDGE_TLS, then run rexctl restart."
		}
		return fail(fmt.Sprintf("The certificate for %s comes from %s, so devices that trust this instance's Fluxer root certificate still show a warning.", domain, cmp.Or(leaf.Issuer.CommonName, "an unknown issuer")), fix)
	}
	if leaf.VerifyHostname(settings.BareHost(domain)) != nil {
		return fail(fmt.Sprintf("The certificate the edge sends doesn't include %s, so browsers show a warning.", domain), "Run rexctl restart so Fluxer signs a new certificate for "+domain+".")
	}
	return pass(fmt.Sprintf("The certificate for %s comes from this instance's Fluxer root certificate.", domain))
}

func fixCertificate(ctx context.Context, e *Env) error {
	if e.Instance == nil || e.Restart == nil {
		return errNoFix
	}
	return e.run(ctx, host.Step{
		Do:      "Restarts Fluxer, which deletes the certificate it signed itself and asks Let's Encrypt for one.",
		Command: "rexctl restart",
		Run:     func(ctx context.Context) error { return e.Restart(ctx, nil) },
	})
}
