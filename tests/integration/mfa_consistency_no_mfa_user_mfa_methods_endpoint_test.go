/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

package integration

import (
	"net/http"
	"testing"
)

// TestMfaConsistencyNoMfaUserMfaMethodsEndpoint verifies that the
// /users/@me/sudo/mfa-methods endpoint correctly reports has_mfa=false
// for users without any MFA.
func TestMfaConsistencyNoMfaUserMfaMethodsEndpoint(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.getWithAuth("/users/@me/sudo/mfa-methods", account.Token)
	if err != nil {
		t.Fatalf("failed to get mfa methods: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("mfa methods returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var mfaMethods struct {
		TOTP     bool `json:"totp"`
		SMS      bool `json:"sms"`
		WebAuthn bool `json:"webauthn"`
		HasMFA   bool `json:"has_mfa"`
	}
	decodeJSONResponse(t, resp, &mfaMethods)

	if mfaMethods.HasMFA {
		t.Fatalf("expected has_mfa=false for non-MFA user, got true")
	}
	if mfaMethods.TOTP {
		t.Fatalf("expected totp=false for non-MFA user, got true")
	}
	if mfaMethods.WebAuthn {
		t.Fatalf("expected webauthn=false for non-MFA user, got true")
	}
	if mfaMethods.SMS {
		t.Fatalf("expected sms=false for non-MFA user, got true")
	}

	t.Logf("correctly reported has_mfa=false for non-MFA user")
}
