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

func TestAuthSudoTOTPVerification(t *testing.T) {
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
	assertStatus(t, resp, http.StatusOK)
	var enableResp backupCodesResponse
	decodeJSONResponse(t, resp, &enableResp)
	if len(enableResp.BackupCodes) == 0 {
		t.Fatalf("expected backup codes after enabling totp")
	}

	account.loginWithTotp(t, client, secret)

	resp, err = client.getWithAuth("/auth/sessions", account.Token)
	if err != nil {
		t.Fatalf("failed to get sessions: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var sessions []authSessionResponse
	decodeJSONResponse(t, resp, &sessions)
	if len(sessions) == 0 {
		t.Fatalf("expected at least one session")
	}

	resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{sessions[0].ID},
		"mfa_method":        "totp",
		"mfa_code":          totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to logout session with totp: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	sudoToken := resp.Header.Get(sudoModeHeader)
	if sudoToken == "" {
		t.Fatalf("expected sudo token in response header")
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to check token: %v", err)
	}
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected token to be revoked, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	account.loginWithTotp(t, client, secret)

	resp, err = client.getWithAuth("/auth/sessions", account.Token)
	if err != nil {
		t.Fatalf("failed to get sessions: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &sessions)
	if len(sessions) == 0 {
		t.Fatalf("expected at least one session")
	}

	resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{sessions[0].ID},
		"mfa_method":        "totp",
		"mfa_code":          "000000",
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to call logout with wrong totp: %v", err)
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 with wrong totp code, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to check token after failed logout: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{sessions[0].ID},
		"mfa_method":        "totp",
		"mfa_code":          enableResp.BackupCodes[0].Code,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to logout session with backup code: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	sudoToken = resp.Header.Get(sudoModeHeader)
	if sudoToken == "" {
		t.Fatalf("expected sudo token in response header when using backup code")
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to check token: %v", err)
	}
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected token to be revoked after backup code logout, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	account.loginWithTotp(t, client, secret)

	resp, err = client.getWithAuth("/auth/sessions", account.Token)
	if err != nil {
		t.Fatalf("failed to get sessions: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &sessions)
	if len(sessions) == 0 {
		t.Fatalf("expected at least one session")
	}

	resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{sessions[0].ID},
		"password":          account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to call logout with password: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 when using password with MFA enabled, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
		"session_id_hashes": []string{sessions[0].ID},
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to call logout without mfa: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 without mfa method, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()
}
