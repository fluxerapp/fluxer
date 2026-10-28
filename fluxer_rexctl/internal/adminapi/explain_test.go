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
	err := adminapi.Explain(&adminapi.APIError{Status: 400, Fields: []adminapi.FieldError{
		{Path: "issuer", Code: "URL_NOT_PUBLICLY_ROUTABLE", Message: "URL must resolve to a publicly routable address."},
	}})
	var exit *ui.ExitError
	if !errors.As(err, &exit) || exit.Fix != adminapi.SSOPrivateFix {
		t.Fatalf("Explain() = %#v, want the private address fix", err)
	}
	if !strings.Contains(adminapi.SSOPrivateFix, "config set "+settings.SSOPrivate+" true") {
		t.Errorf("fix %q doesn't name %s", adminapi.SSOPrivateFix, settings.SSOPrivate)
	}
}
