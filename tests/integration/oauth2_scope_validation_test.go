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
	"strings"
	"testing"
	"time"
)

// TestOAuth2ScopeValidation verifies that only valid OAuth2 scopes are accepted
// during the authorization flow. Invalid or unknown scopes should be rejected
// with an appropriate error, and the server must validate scopes against a known set.
func TestOAuth2ScopeValidation(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/callback"

	t.Run("accept valid platform scopes", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Valid Scopes App %d", time.Now().UnixNano()), []string{redirectURI})

		validScopes := []string{"identify", "email", "guilds", "connections"}
		for _, scope := range validScopes {
			authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{scope}, "", "", "")

			if authCode == "" {
				t.Fatalf("valid scope %s should be accepted", scope)
			}

			token := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "")
			if !strings.Contains(token.Scope, scope) {
				t.Fatalf("token scope %s should contain requested scope %s", token.Scope, scope)
			}
		}
	})

	t.Run("accept multiple valid scopes", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Multi Scope App %d", time.Now().UnixNano()), []string{redirectURI})

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI,
			[]string{"identify", "email", "guilds"}, "", "", "")

		if authCode == "" {
			t.Fatalf("multiple valid scopes should be accepted")
		}

		token := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "")
		if !strings.Contains(token.Scope, "identify") || !strings.Contains(token.Scope, "email") || !strings.Contains(token.Scope, "guilds") {
			t.Fatalf("token scope should contain all requested scopes, got: %s", token.Scope)
		}
	})

	t.Run("reject unknown scope", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Unknown Scope App %d", time.Now().UnixNano()), []string{redirectURI})

		resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", map[string]any{
			"response_type": "code",
			"client_id":     appID,
			"redirect_uri":  redirectURI,
			"scope":         "unknown_scope_invalid",
			"state":         fmt.Sprintf("state-%d", time.Now().UnixNano()),
		}, endUser.Token)
		if err != nil {
			t.Fatalf("authorize request failed: %v", err)
		}
		if resp.Body != nil {
			resp.Body.Close()
		}

		if resp.StatusCode != http.StatusBadRequest {
			t.Fatalf("expected 400 for invalid scope, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("reject mix of valid and invalid scopes", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Mixed Scope App %d", time.Now().UnixNano()), []string{redirectURI})

		resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", map[string]any{
			"response_type": "code",
			"client_id":     appID,
			"redirect_uri":  redirectURI,
			"scope":         "identify invalid_scope_xyz",
			"state":         fmt.Sprintf("state-%d", time.Now().UnixNano()),
		}, endUser.Token)
		if err != nil {
			t.Fatalf("authorize request failed: %v", err)
		}
		if resp.Body != nil {
			resp.Body.Close()
		}

		if resp.StatusCode != http.StatusBadRequest {
			t.Fatalf("expected 400 for invalid scope mix, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("allow bot scope on applications with bots", func(t *testing.T) {
		appID, _, _, _ := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Bot Scope App %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify", "bot"},
		)

		resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", map[string]any{
			"response_type": "code",
			"client_id":     appID,
			"redirect_uri":  redirectURI,
			"scope":         "bot",
			"state":         fmt.Sprintf("state-%d", time.Now().UnixNano()),
		}, endUser.Token)
		if err != nil {
			t.Fatalf("authorize request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("expected 200 for bot scope flow, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}

		var payload struct {
			RedirectTo string `json:"redirect_to"`
		}
		decodeJSONResponse(t, resp, &payload)

		if payload.RedirectTo == "" {
			t.Fatalf("expected redirect_to in response for bot scope flow")
		}
	})
}
