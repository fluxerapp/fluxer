// SPDX-License-Identifier: AGPL-3.0-or-later

package edge

import (
	"context"
	"crypto/x509"

	"github.com/caddyserver/caddy/v2"
	"github.com/caddyserver/caddy/v2/modules/caddytls"
	"github.com/caddyserver/certmagic"
)

func init() {
	caddy.RegisterModule(LocalIssuer{})
}

type LocalIssuer struct {
	caddytls.InternalIssuer
}

func (LocalIssuer) CaddyModule() caddy.ModuleInfo {
	return caddy.ModuleInfo{
		ID:  "tls.issuance.fluxer_local",
		New: func() caddy.Module { return new(LocalIssuer) },
	}
}

func (i *LocalIssuer) Issue(ctx context.Context, csr *x509.CertificateRequest) (*certmagic.IssuedCertificate, error) {
	if err := ctx.Err(); err != nil {
		return nil, err
	}
	return i.InternalIssuer.Issue(ctx, csr)
}

var (
	_ caddy.Provisioner = (*LocalIssuer)(nil)
	_ certmagic.Issuer  = (*LocalIssuer)(nil)
)
