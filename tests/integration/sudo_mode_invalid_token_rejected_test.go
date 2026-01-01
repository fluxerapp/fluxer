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

// TestSudoModeInvalidTokenRejected verifies that an invalid or malformed
// sudo token is rejected and the user is required to verify with MFA/password.
func TestSudoModeInvalidTokenRejected(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	secret := newTotpSecret(t)
	resp, err := client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret": secret,
		"code":   totpCodePrev(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to enable totp: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("enable totp returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	loginResp, err := client.postJSON("/auth/login", loginRequest{
		Email:    account.Email,
		Password: account.Password,
	})
	if err != nil {
		t.Fatalf("failed to login: %v", err)
	}
	var login loginResponse
	decodeJSONResponse(t, loginResp, &login)
	if !login.MFA || login.Ticket == "" {
		t.Fatalf("expected MFA required for login")
	}

	resp, err = client.postJSON("/auth/login/mfa/totp", map[string]string{
		"code":   totpCodeNow(t, secret),
		"ticket": login.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to complete MFA login: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("MFA login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var mfaLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &mfaLogin)
	if mfaLogin.Token == "" {
		t.Fatalf("MFA login did not return a token")
	}
	account.Token = mfaLogin.Token

	invalidTokens := []string{
		"invalid-token",
		"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.invalid.signature",
		"",
		"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0eXBlIjoibm90LXN1ZG8ifQ.fakesignature",
	}

	for _, invalidToken := range invalidTokens {
		var headers map[string]string
		if invalidToken != "" {
			headers = map[string]string{sudoModeHeader: invalidToken}
		} else {
			headers = nil
		}

		resp, err = client.postJSONWithAuthAndHeaders("/users/@me/disable", map[string]any{}, account.Token, headers)
		if err != nil {
			t.Fatalf("failed to make request with invalid token: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 for invalid sudo token %q, got %d: %s", invalidToken, resp.StatusCode, readResponseBody(resp))
		}

		var errResp errorResponse
		decodeJSONResponse(t, resp, &errResp)
		if errResp.Code != "SUDO_MODE_REQUIRED" {
			t.Fatalf("expected SUDO_MODE_REQUIRED error code for invalid token, got: %s", errResp.Code)
		}
	}

	t.Logf("all invalid sudo tokens were correctly rejected")
}
