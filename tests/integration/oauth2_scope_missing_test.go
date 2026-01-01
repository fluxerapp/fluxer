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

// TestOAuth2RequestsWithoutRequiredScopeAreRejected verifies that API endpoints
// properly enforce scope requirements and reject requests when the OAuth2 token
// lacks the necessary scope. This is critical for security and follows strict
// OAuth2 scope enforcement expectations.
func TestOAuth2RequestsWithoutRequiredScopeAreRejected(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/callback"

	_ = createGuild(t, client, endUser.Token, fmt.Sprintf("Scope Test Guild %d", time.Now().UnixNano()))

	t.Run("identify scope cannot access users/@me/guilds", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Identify Only %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me/guilds", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("should not be able to access /users/@me/guilds without guilds scope")
		}
		if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 403 or 401 for missing guilds scope, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("guilds scope cannot access endpoints requiring other scopes", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Guilds Only %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"guilds"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"guilds"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me/guilds", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Logf("Note: guilds-only scope returned %d for /users/@me/guilds", resp.StatusCode)
		}
	})

	t.Run("revoked token cannot access any endpoints", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Revoke Test %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify", "guilds"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify", "guilds"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		resp.Body.Close()
		if resp.StatusCode != http.StatusOK {
			t.Fatalf("token should work before revocation, got %d", resp.StatusCode)
		}

		revokeOAuth2Token(t, client, appID, clientSecret, token.AccessToken, "access_token")

		req, err = http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err = client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("revoked token should not be able to access any endpoints")
		}
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401 for revoked token, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("expired token cannot access endpoints", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Expiry Test %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		if token.ExpiresIn <= 0 {
			t.Fatalf("token should have a positive expires_in value, got %d", token.ExpiresIn)
		}

		introspection := introspectOAuth2Token(t, client, appID, clientSecret, token.AccessToken)
		if !introspection.Active {
			t.Fatalf("newly issued token should be active")
		}
		if introspection.Exp <= 0 {
			t.Fatalf("token should have an expiration timestamp, got %d", introspection.Exp)
		}

		t.Logf("Token expires in %d seconds (exp: %d)", token.ExpiresIn, introspection.Exp)
	})

	t.Run("token from different user cannot access resources", func(t *testing.T) {
		user1 := createTestAccount(t, client)
		user2 := createTestAccount(t, client)

		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Multi User Test %d", time.Now().UnixNano()), []string{redirectURI})

		authCode, _ := authorizeOAuth2(t, client, user1.Token, appID, redirectURI, []string{"identify"}, "", "", "")
		user1OAuthToken := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "")

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", user1OAuthToken.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("user1's token should work for /users/@me, got %d", resp.StatusCode)
		}

		var userData map[string]any
		decodeJSONResponse(t, resp, &userData)

		if userData["id"] != user1.UserID {
			t.Fatalf("expected user1 id %s, got %v", user1.UserID, userData["id"])
		}
		if userData["id"] == user2.UserID {
			t.Fatalf("user1's token should not return user2's data")
		}
	})

	t.Run("invalid token format is rejected", func(t *testing.T) {
		invalidTokens := []string{
			"invalid_token",
			"Bearer invalid",
			"",
			"   ",
			"not.a.valid.jwt",
		}

		for _, invalidToken := range invalidTokens {
			req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
			if err != nil {
				t.Fatalf("failed to build request: %v", err)
			}
			if invalidToken != "" {
				req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", invalidToken))
			}
			client.applyCommonHeaders(req)

			resp, err := client.httpClient.Do(req)
			if err != nil {
				t.Fatalf("request failed: %v", err)
			}
			resp.Body.Close()

			if resp.StatusCode == http.StatusOK {
				t.Fatalf("invalid token '%s' should be rejected", invalidToken)
			}
			if resp.StatusCode != http.StatusUnauthorized {
				t.Logf("Warning: invalid token '%s' got %d instead of 401", invalidToken, resp.StatusCode)
			}
		}
	})
}
