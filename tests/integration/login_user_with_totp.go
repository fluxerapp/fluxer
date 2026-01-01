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

// Completes login for TOTP-enabled users by following the MFA ticket flow.
// Falls back to the standard login response if MFA is not required.
func loginTestUserWithTotp(t testing.TB, client *testClient, email, password, totpSecret string) loginResponse {
	t.Helper()

	loginResp := loginTestUser(t, client, email, password)
	if !loginResp.MFA {
		if loginResp.Token == "" {
			t.Fatalf("expected token in non-MFA login response")
		}
		return loginResp
	}

	if loginResp.Ticket == "" {
		t.Fatalf("expected mfa ticket in login response")
	}

	resp, err := client.postJSON("/auth/login/mfa/totp", map[string]string{
		"ticket": loginResp.Ticket,
		"code":   totpCodeNow(t, totpSecret),
	})
	if err != nil {
		t.Fatalf("failed to complete totp login: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("totp login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var mfaResp mfaLoginResponse
	decodeJSONResponse(t, resp, &mfaResp)
	if mfaResp.Token == "" {
		t.Fatalf("expected token in totp login response")
	}

	return loginResponse{
		MFA:    false,
		UserID: loginResp.UserID,
		Token:  mfaResp.Token,
	}
}
