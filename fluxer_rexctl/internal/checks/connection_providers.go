// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
)

func init() {
	Register(Check{ID: "connection-providers", Phases: Installed, Run: runConnectionProviders})
}

func runConnectionProviders(ctx context.Context, e *Env) Result {
	return connectionProviders(ctx, e, e.exec)
}

func connectionProviders(ctx context.Context, e *Env, run execFunc) Result {
	if e.Admin == nil || !e.running(ctx, "api") {
		return skip("")
	}
	cfg, err := e.Admin.Config(ctx)
	if err != nil {
		return skip("")
	}
	var reached []string
	for _, provider := range cfg.Integrations.ConnectionProviders {
		if !provider.Enabled {
			continue
		}
		in, ok := connectionProviderProbe(provider)
		if !ok {
			continue
		}
		p, ok := runProviderProbe(ctx, run, in)
		if !ok {
			continue
		}
		r := probeResult(p, in.Discovery, connectionProviderTarget(string(provider.Id)))
		if r.Status != Pass {
			return r
		}
		reached = append(reached, string(provider.Id))
	}
	if len(reached) == 0 {
		return skip("")
	}
	return pass(fmt.Sprintf("The API connects to the connection providers %s.", strings.Join(reached, ", ")))
}

func connectionProviderProbe(provider adminapi.ConnectionProviderConfigResponse) (ssoProbeInput, bool) {
	in := ssoProbeInput{Timeout: ssoProbeSeconds * 1000, MaxBytes: ssoDiscoveryMaxBytes}
	if provider.Kind == adminapi.ConnectionProviderConfigResponseKindOIDC {
		issuer := trimmed(provider.Issuer)
		in.Discovery = discoveryURL(issuer)
		in.Issuer = issuer
		in.Token = trimmed(provider.TokenUrl)
		return in, in.Discovery != ""
	}
	in.Token = trimmed(provider.TokenUrl)
	return in, in.Token != ""
}

func connectionProviderTarget(id string) probeTarget {
	fix := "Check the provider with rexctl config get integrations.connection_providers."
	return probeTarget{
		what:      fmt.Sprintf("the %s connection provider", id),
		issuerFix: fix,
		tokenFix:  fix,
	}
}
