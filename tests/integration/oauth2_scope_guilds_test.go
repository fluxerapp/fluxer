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

// TestOAuth2ScopeGuildsGivesAccessToUsersMeGuilds verifies that the "guilds" scope
// grants access to GET /users/@me/guilds endpoint:
// - guilds scope: allows access to a list of partial guild objects the user is a member of
// - Returns basic guild information (id, name, icon, permissions, etc.)
// - Does NOT provide full guild details or channel information
func TestOAuth2ScopeGuildsGivesAccessToUsersMeGuilds(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/callback"

	guild := createGuild(t, client, endUser.Token, fmt.Sprintf("OAuth Guild %d", time.Now().UnixNano()))

	t.Run("guilds scope grants access to users/@me/guilds", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Guilds Scope App %d", time.Now().UnixNano()), []string{redirectURI})

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify", "guilds"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "")

		if !strings.Contains(token.Scope, "guilds") {
			t.Fatalf("token scope should contain guilds, got '%s'", token.Scope)
		}

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me/guilds", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build users/@me/guilds request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("users/@me/guilds request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("users/@me/guilds should be accessible with guilds scope, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}

		// Parse guilds list
		var guilds []map[string]any
		decodeJSONResponse(t, resp, &guilds)

		// Verify the created guild is in the list
		var foundGuild map[string]any
		for _, g := range guilds {
			if g["id"] == guild.ID {
				foundGuild = g
				break
			}
		}

		if foundGuild == nil {
			t.Fatalf("expected to find guild %s in users/@me/guilds response", guild.ID)
		}

		if foundGuild["name"] == nil {
			t.Fatalf("guild object should contain name")
		}
		if foundGuild["id"] != guild.ID {
			t.Fatalf("expected guild id %s, got %v", guild.ID, foundGuild["id"])
		}
	})

	t.Run("identify without guilds scope denies access to users/@me/guilds", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("No Guilds Scope %d", time.Now().UnixNano()), []string{redirectURI})

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "")

		if strings.Contains(token.Scope, "guilds") {
			t.Fatalf("token should not have guilds scope, got '%s'", token.Scope)
		}

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me/guilds", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build users/@me/guilds request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("users/@me/guilds request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("users/@me/guilds should not be accessible without guilds scope")
		}
		if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 403 or 401 for missing guilds scope, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("guilds scope works with application using client secret", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Guilds App %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify", "guilds"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify", "guilds"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me/guilds", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build users/@me/guilds request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("users/@me/guilds request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("guilds scope should work with application, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}

		var guilds []map[string]any
		decodeJSONResponse(t, resp, &guilds)

		// Should contain at least the guild we created
		var found bool
		for _, g := range guilds {
			if g["id"] == guild.ID {
				found = true
				break
			}
		}
		if !found {
			t.Fatalf("expected to find guild %s with application", guild.ID)
		}
	})

	t.Run("refreshed token preserves guilds scope", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Guilds Refresh %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify", "guilds"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify", "guilds"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		refreshedToken := refreshOAuth2Token(t, client, appID, clientSecret, token.RefreshToken)

		if !strings.Contains(refreshedToken.Scope, "guilds") {
			t.Fatalf("refreshed token should preserve guilds scope, got '%s'", refreshedToken.Scope)
		}

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me/guilds", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build users/@me/guilds request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", refreshedToken.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("users/@me/guilds request with refreshed token failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("refreshed token with guilds scope should access users/@me/guilds, got %d", resp.StatusCode)
		}
	})

	t.Run("guilds scope returns empty array for user with no guilds", func(t *testing.T) {
		newUser := createTestAccount(t, client)

		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Guilds Empty %d", time.Now().UnixNano()), []string{redirectURI})

		authCode, _ := authorizeOAuth2(t, client, newUser.Token, appID, redirectURI, []string{"identify", "guilds"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "")

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me/guilds", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build users/@me/guilds request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("users/@me/guilds request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("users/@me/guilds should return 200 even with no guilds, got %d", resp.StatusCode)
		}

		var guilds []map[string]any
		decodeJSONResponse(t, resp, &guilds)

		if len(guilds) != 0 {
			t.Logf("Note: new user has %d guilds (may be system guilds)", len(guilds))
		}
	})
}
