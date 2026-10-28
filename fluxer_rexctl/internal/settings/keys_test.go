// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"errors"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestSSOPrivateAddressesIsANamedSetting(t *testing.T) {
	set := stack.Embedded()
	if _, err := Lookup(set, nil, SSOPrivate, nil); err != nil {
		t.Fatalf("Lookup(%s) = %v", SSOPrivate, err)
	}
	if _, err := Lookup(set, nil, "FLUXER_SSO_ALLOW_PRIVATE_ADDRESSES", nil); !errors.Is(err, ErrOtherName) {
		t.Errorf("raw name lookup = %v, want it to point at %s", err, SSOPrivate)
	}
	env, err := Effective(set, map[string]string{SSOPrivate: "true"}, nil, nil)
	if err != nil {
		t.Fatal(err)
	}
	if got := env["FLUXER_SSO_ALLOW_PRIVATE_ADDRESSES"]; got != "true" {
		t.Errorf("FLUXER_SSO_ALLOW_PRIVATE_ADDRESSES = %q, want true", got)
	}
}
