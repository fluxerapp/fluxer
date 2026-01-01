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

// TestSudoModeTokenAllowsSkippingMfa verifies that a valid sudo token
// allows MFA-enabled users to skip MFA verification for subsequent
// sensitive operations within the token's validity period.
func TestSudoModeTokenAllowsSkippingMfa(t *testing.T) {
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

	resp, err = client.postJSONWithAuth("/users/@me/mfa/backup-codes", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNext(t, secret),
		"regenerate": false,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to request backup codes with MFA: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	sudoToken := resp.Header.Get(sudoModeHeader)
	if sudoToken == "" {
		t.Fatalf("expected sudo token in response")
	}
	resp.Body.Close()

	resp, err = client.postJSONWithAuthAndHeaders("/users/@me/mfa/backup-codes", map[string]any{
		"regenerate": true,
	}, account.Token, map[string]string{
		sudoModeHeader: sudoToken,
	})
	if err != nil {
		t.Fatalf("failed to regenerate backup codes with sudo token: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 with valid sudo token, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	t.Logf("successfully reused sudo token to skip MFA verification")
}
