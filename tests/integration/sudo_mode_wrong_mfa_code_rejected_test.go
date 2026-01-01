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

// TestSudoModeWrongMfaCodeRejected verifies that incorrect MFA codes
// are rejected during sudo verification, preventing brute force attacks.
func TestSudoModeWrongMfaCodeRejected(t *testing.T) {
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

	resp, err = client.postJSON("/auth/login/mfa/totp", map[string]string{
		"code":   totpCodeNow(t, secret),
		"ticket": login.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to complete MFA login: %v", err)
	}
	var mfaLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &mfaLogin)
	account.Token = mfaLogin.Token

	wrongCodes := []string{
		"000000",
		"123456",
		"999999",
		"12345",
		"1234567",
		"abcdef",
	}

	for _, wrongCode := range wrongCodes {
		resp, err := client.postJSONWithAuth("/users/@me/disable", map[string]string{
			"mfa_method": "totp",
			"mfa_code":   wrongCode,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to make request with wrong MFA code: %v", err)
		}
		if resp.StatusCode != http.StatusBadRequest {
			t.Fatalf("expected 400 for wrong MFA code %q, got %d: %s", wrongCode, resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()
	}

	t.Logf("all incorrect MFA codes were correctly rejected")
}
