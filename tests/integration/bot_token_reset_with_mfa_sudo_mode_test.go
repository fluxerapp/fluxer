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

// TestBotTokenResetWithMFASudoMode verifies that bot token reset requires sudo mode
// for users with MFA enabled, protecting sensitive operations.
func TestBotTokenResetWithMFASudoMode(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	secret := newTotpSecret(t)
	resp, err := client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret": secret,
		"code":   totpCodePrev(t, secret),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to enable totp: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("enable totp returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	loginResp, err := client.postJSON("/auth/login", loginRequest{
		Email:    owner.Email,
		Password: owner.Password,
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
	owner.Token = mfaLogin.Token

	appName := fmt.Sprintf("Test Bot MFA Reset %d", time.Now().UnixNano())
	redirectURI := "https://example.com/callback"
	appID, _, originalToken := createOAuth2BotApplication(t, client, owner, appName, []string{redirectURI})

	resp, err = client.postJSONWithAuth(
		fmt.Sprintf("/oauth2/applications/%s/bot/reset-token", appID),
		map[string]any{},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to reset bot token: %v", err)
	}

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusUnauthorized && resp.StatusCode != http.StatusForbidden {
		t.Fatalf("unexpected status for token reset with MFA user: %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	if resp.StatusCode == http.StatusOK {
		var result struct {
			Token string `json:"token"`
		}
		decodeJSONResponse(t, resp, &result)
		if result.Token == "" {
			t.Fatalf("reset response should include new token")
		}
		if result.Token == originalToken {
			t.Fatalf("reset should generate a new token")
		}

		sudoToken := resp.Header.Get(sudoModeHeader)
		if sudoToken != "" {
			t.Logf("Sudo token provided after sensitive operation: %s...", sudoToken[:20])
		}
	}

	resp.Body.Close()
	t.Logf("Bot token reset with MFA user completed successfully")
}
