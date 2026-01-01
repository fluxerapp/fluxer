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
	"encoding/json"
	"net/http"
	"testing"
)

// TestMfaConsistencyTotpUserCannotUsePassword verifies that users with TOTP MFA
// cannot use password for sudo verification - they must use an MFA method.
func TestMfaConsistencyTotpUserCannotUsePassword(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	secret := newTotpSecret(t)
	resp, err := client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret": secret,
		"code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to enable totp: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("enable totp returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var enableResp backupCodesResponse
	decodeJSONResponse(t, resp, &enableResp)

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
		"code":   enableResp.BackupCodes[0].Code,
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

	resp, err = client.postJSONWithAuth("/users/@me/disable", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for MFA user with password, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var errorResp struct {
		Code string `json:"code"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&errorResp); err != nil {
		t.Fatalf("failed to decode error response: %v", err)
	}

	if errorResp.Code != "SUDO_MODE_REQUIRED" {
		t.Fatalf("expected error code SUDO_MODE_REQUIRED, got %s", errorResp.Code)
	}

	t.Logf("correctly rejected password for TOTP user")
}
