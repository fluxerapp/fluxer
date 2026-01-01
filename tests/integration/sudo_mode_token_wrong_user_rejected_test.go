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

// TestSudoModeTokenWrongUserRejected verifies that a valid sudo token
// from one user cannot be used by a different user to bypass MFA verification.
// This is a critical security test to prevent token theft attacks.
func TestSudoModeTokenWrongUserRejected(t *testing.T) {
	client := newTestClient(t)

	account1 := createTestAccount(t, client)
	secret1 := newTotpSecret(t)
	resp, err := client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret": secret1,
		"code":   totpCodePrev(t, secret1),
	}, account1.Token)
	if err != nil {
		t.Fatalf("failed to enable totp for user1: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("enable totp for user1 returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	loginResp, err := client.postJSON("/auth/login", loginRequest{
		Email:    account1.Email,
		Password: account1.Password,
	})
	if err != nil {
		t.Fatalf("failed to login user1: %v", err)
	}
	var login loginResponse
	decodeJSONResponse(t, loginResp, &login)

	resp, err = client.postJSON("/auth/login/mfa/totp", map[string]string{
		"code":   totpCodeNow(t, secret1),
		"ticket": login.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to complete MFA login for user1: %v", err)
	}
	var mfaLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &mfaLogin)
	account1.Token = mfaLogin.Token

	resp, err = client.postJSONWithAuth("/users/@me/mfa/backup-codes", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNext(t, secret1),
		"regenerate": false,
	}, account1.Token)
	if err != nil {
		t.Fatalf("failed to request backup codes for user1: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	user1SudoToken := resp.Header.Get(sudoModeHeader)
	if user1SudoToken == "" {
		t.Fatalf("expected sudo token for user1")
	}
	resp.Body.Close()

	account2 := createTestAccount(t, client)
	secret2 := newTotpSecret(t)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret": secret2,
		"code":   totpCodePrev(t, secret2),
	}, account2.Token)
	if err != nil {
		t.Fatalf("failed to enable totp for user2: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("enable totp for user2 returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	loginResp, err = client.postJSON("/auth/login", loginRequest{
		Email:    account2.Email,
		Password: account2.Password,
	})
	if err != nil {
		t.Fatalf("failed to login user2: %v", err)
	}
	decodeJSONResponse(t, loginResp, &login)

	resp, err = client.postJSON("/auth/login/mfa/totp", map[string]string{
		"code":   totpCodeNow(t, secret2),
		"ticket": login.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to complete MFA login for user2: %v", err)
	}
	decodeJSONResponse(t, resp, &mfaLogin)
	account2.Token = mfaLogin.Token

	resp, err = client.postJSONWithAuthAndHeaders("/users/@me/disable", map[string]any{}, account2.Token, map[string]string{
		sudoModeHeader: user1SudoToken,
	})
	if err != nil {
		t.Fatalf("failed to make request: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 when using another user's sudo token, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var errResp errorResponse
	decodeJSONResponse(t, resp, &errResp)
	if errResp.Code != "SUDO_MODE_REQUIRED" {
		t.Fatalf("expected SUDO_MODE_REQUIRED error code, got: %s", errResp.Code)
	}

	t.Logf("successfully rejected cross-user sudo token attack")
}
