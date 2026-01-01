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
	"fmt"
	"net/http"
	"testing"
	"time"
)

func TestAuthSudoRequiredOperations(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	t.Run("SessionLogoutRequiresSudo", func(t *testing.T) {
		resp, err := client.getWithAuth("/auth/sessions", account.Token)
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
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to call logout: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 without sudo, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.postJSONWithAuth("/auth/sessions/logout", map[string]any{
			"session_id_hashes": []string{sessions[0].ID},
			"password":          account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to logout with password: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		account.login(t, client)
	})

	t.Run("DeleteApplicationRequiresSudo", func(t *testing.T) {
		appName := fmt.Sprintf("Test App %d", time.Now().UnixNano())
		appID, _, _ := createOAuth2BotApplication(t, client, account, appName, []string{"https://example.com/callback"})

		resp, err := client.deleteJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID),
			map[string]any{}, account.Token)
		if err != nil {
			t.Fatalf("failed to call delete application: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 without sudo for delete app, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.deleteJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID),
			map[string]any{"password": account.Password}, account.Token)
		if err != nil {
			t.Fatalf("failed to delete application with password: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("BotTokenResetRequiresSudo", func(t *testing.T) {
		appName := fmt.Sprintf("Test App %d", time.Now().UnixNano())
		appID, _, _ := createOAuth2BotApplication(t, client, account, appName, []string{"https://example.com/callback"})

		resp, err := client.postJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s/bot/reset-token", appID),
			map[string]any{}, account.Token)
		if err != nil {
			t.Fatalf("failed to call reset bot token: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 without sudo for reset token, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.postJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s/bot/reset-token", appID),
			map[string]any{"password": account.Password}, account.Token)
		if err != nil {
			t.Fatalf("failed to reset bot token with password: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		deleteOAuth2Application(t, client, account, appID)
	})

	t.Run("DisableMFARequiresSudo", func(t *testing.T) {
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
			t.Fatalf("expected backup codes")
		}

		account.login(t, client)

		resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/disable", map[string]any{
			"code": enableResp.BackupCodes[0].Code,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to call disable totp: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 without sudo for disable mfa, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/disable", map[string]any{
			"code":       enableResp.BackupCodes[0].Code,
			"mfa_method": "totp",
			"mfa_code":   totpCodeNow(t, secret),
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to disable totp with mfa: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("DisableAccountRequiresSudo", func(t *testing.T) {
		testUser := createTestAccount(t, client)

		resp, err := client.postJSONWithAuth("/users/@me/disable", map[string]any{}, testUser.Token)
		if err != nil {
			t.Fatalf("failed to call disable account: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 without sudo for disable account, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.postJSONWithAuth("/users/@me/disable",
			map[string]any{"password": testUser.Password}, testUser.Token)
		if err != nil {
			t.Fatalf("failed to disable account with password: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("DeleteAccountRequiresSudo", func(t *testing.T) {
		testUser := createTestAccount(t, client)

		resp, err := client.postJSONWithAuth("/users/@me/delete", map[string]any{}, testUser.Token)
		if err != nil {
			t.Fatalf("failed to call delete account: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 without sudo for delete account, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.postJSONWithAuth("/users/@me/delete",
			map[string]any{"password": testUser.Password}, testUser.Token)
		if err != nil {
			t.Fatalf("failed to delete account with password: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("NonSudoOperationsWorkWithoutSudo", func(t *testing.T) {
		account.login(t, client)

		resp, err := client.getWithAuth("/users/@me", account.Token)
		if err != nil {
			t.Fatalf("failed to get user profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.getWithAuth("/auth/sessions", account.Token)
		if err != nil {
			t.Fatalf("failed to list sessions: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.getWithAuth("/users/@me/sudo/mfa-methods", account.Token)
		if err != nil {
			t.Fatalf("failed to get mfa methods: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})
}
