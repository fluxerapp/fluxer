// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func TestPrivateSSOAddressPointsAtTheSetting(t *testing.T) {
	for _, field := range []string{"issuer", "authorization_url", "token_url", "userinfo_url", "jwks_url"} {
		err := adminapi.Explain(&adminapi.APIError{Status: 400, Fields: []adminapi.FieldError{
			{Path: field, Code: "URL_NOT_PUBLICLY_ROUTABLE", Message: "URL must resolve to a publicly routable address."},
		}})
		var exit *ui.ExitError
		if !errors.As(err, &exit) || exit.Fix != adminapi.SSOPrivateFix {
			t.Errorf("Explain() for %s = %#v, want the private address fix", field, err)
		}
	}
	if !strings.Contains(adminapi.SSOPrivateFix, "config set "+settings.SSOPrivate+" true") {
		t.Errorf("fix %q doesn't name %s", adminapi.SSOPrivateFix, settings.SSOPrivate)
	}
}

func TestUnresolvableSSOHostnamePointsAtTheSpelling(t *testing.T) {
	err := adminapi.Explain(&adminapi.APIError{Status: 400, Fields: []adminapi.FieldError{
		{Path: "issuer", Code: "URL_HOSTNAME_NOT_RESOLVABLE", Message: "Fluxer couldn't look up the hostname in this URL."},
	}})
	var exit *ui.ExitError
	if !errors.As(err, &exit) || exit.Fix != adminapi.SSOHostnameFix {
		t.Fatalf("Explain() = %#v, want the hostname fix", err)
	}
}

func TestUnreachableSSOProviderPointsAtTheIssuer(t *testing.T) {
	err := adminapi.Explain(&adminapi.APIError{Status: 400, Fields: []adminapi.FieldError{
		{Path: "sso", Code: "SSO_DISCOVERY_FAILED", Message: "Couldn't connect to the sign-in provider at its issuer URL."},
	}})
	var exit *ui.ExitError
	if !errors.As(err, &exit) || exit.Fix != adminapi.SSODiscoveryFix {
		t.Fatalf("Explain() = %#v, want the discovery fix", err)
	}
}

func TestMissingSSOIssuerPointsAtTheSetting(t *testing.T) {
	err := adminapi.Explain(&adminapi.APIError{Status: 400, Fields: []adminapi.FieldError{
		{Path: "issuer", Code: "SSO_ISSUER_REQUIRED", Message: "Enter the issuer URL of your sign-in provider."},
	}})
	var exit *ui.ExitError
	if !errors.As(err, &exit) || exit.Fix != adminapi.SSOIssuerRequiredFix {
		t.Fatalf("Explain() = %#v, want the issuer fix", err)
	}
}

func TestSSOIssuerChangeWithLinksPointsAtTheFlags(t *testing.T) {
	err := adminapi.Explain(&adminapi.APIError{Status: 400, Fields: []adminapi.FieldError{
		{Path: "issuer", Code: "SSO_ISSUER_CHANGE_HAS_IDENTITIES", Message: "2 accounts are linked to the old issuer."},
	}})
	var exit *ui.ExitError
	if !errors.As(err, &exit) || exit.Fix != adminapi.SSOIssuerChangeFix {
		t.Fatalf("Explain() = %#v, want the migrate fix", err)
	}
}
