// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"crypto/tls"
	"errors"
	"fmt"
	"net"
	"strings"
	"time"
)

func init() {
	Register(Check{ID: "certificate", Phases: Installed, Run: runCertificate})
}

func runCertificate(ctx context.Context, e *Env) Result {
	if e.Instance == nil || e.mode() != modeBundled || privateDomain(e.Domain) {
		return skip("")
	}
	if strings.EqualFold(e.value("FLUXER_PUBLIC_SCHEME"), "http") || strings.EqualFold(e.value("FLUXER_EDGE_TLS"), "internal") {
		return skip("")
	}
	d := tls.Dialer{
		NetDialer: &net.Dialer{Timeout: 10 * time.Second},
		Config:    &tls.Config{ServerName: e.Domain},
	}
	dialCtx, cancel := context.WithTimeout(ctx, 15*time.Second)
	defer cancel()
	conn, err := d.DialContext(dialCtx, "tcp", e.localAddr("FLUXER_HTTPS_PORT", "443"))
	if err != nil {
		var opErr *net.OpError
		if errors.As(err, &opErr) && opErr.Op == "dial" {
			return skip("")
		}
		return fail(fmt.Sprintf("Couldn't get a certificate for %s.", e.Domain),
			"Open ports 80 and 443 in your hosting provider's firewall, then run rexctl doctor.")
	}
	defer conn.Close()
	certs := conn.(*tls.Conn).ConnectionState().PeerCertificates
	if len(certs) == 0 {
		return skip("")
	}
	expires := certs[0].NotAfter
	if time.Until(expires) < 14*24*time.Hour {
		return warn(fmt.Sprintf("The certificate for %s expires on %s.", e.Domain, day(expires)),
			"Run rexctl logs edge to see why it hasn't renewed.")
	}
	return pass(fmt.Sprintf("The certificate for %s is valid until %s.", e.Domain, day(expires)))
}
