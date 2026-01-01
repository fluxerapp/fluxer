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

// TestOAuth2ScopeIdentifyGivesAccessToUsersMe verifies that the "identify" scope
// grants access to GET /users/@me endpoint. This is the most basic OAuth2 scope
// and allows applications to retrieve basic user information such as id, username,
// discriminator, and avatar.
//
// The identify scope should NOT include email or other sensitive information
// unless additional scopes are granted.
func TestOAuth2ScopeIdentifyGivesAccessToUsersMe(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/callback"

	t.Run("identify scope grants access to users/@me", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Identify App %d", time.Now().UnixNano()), []string{redirectURI})

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "")

		if token.Scope != "identify" {
			t.Fatalf("expected scope 'identify', got '%s'", token.Scope)
		}

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build users/@me request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("users/@me request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("users/@me should be accessible with identify scope, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}

		// Parse user data
		var userData map[string]any
		decodeJSONResponse(t, resp, &userData)

		if userData["id"] == nil {
			t.Fatalf("users/@me should return id with identify scope")
		}
		if userData["username"] == nil {
			t.Fatalf("users/@me should return username with identify scope")
		}
		if userData["discriminator"] == nil {
			t.Fatalf("users/@me should return discriminator with identify scope")
		}

		if userData["id"] != endUser.UserID {
			t.Fatalf("expected user id %s, got %v", endUser.UserID, userData["id"])
		}
	})

	t.Run("identify scope does not include email field", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Identify No Email %d", time.Now().UnixNano()), []string{redirectURI})

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "")

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build users/@me request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("users/@me request failed: %v", err)
		}
		defer resp.Body.Close()

		var userData map[string]any
		decodeJSONResponse(t, resp, &userData)

		if email, ok := userData["email"]; ok && email != nil {
			t.Fatalf("email field should not be returned with only identify scope, got: %v", email)
		}
	})

	t.Run("identify scope works with application using client secret", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Identify App %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build users/@me request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("users/@me request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("identify scope should work with application, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}

		var userData map[string]any
		decodeJSONResponse(t, resp, &userData)

		if userData["id"] != endUser.UserID {
			t.Fatalf("expected user id %s, got %v", endUser.UserID, userData["id"])
		}
	})

	t.Run("refreshed token preserves identify scope", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Identify Refresh %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		refreshedToken := refreshOAuth2Token(t, client, appID, clientSecret, token.RefreshToken)

		if refreshedToken.Scope != "identify" {
			t.Fatalf("refreshed token should preserve identify scope, got '%s'", refreshedToken.Scope)
		}

		req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
		if err != nil {
			t.Fatalf("failed to build users/@me request: %v", err)
		}
		req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", refreshedToken.AccessToken))
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("users/@me request with refreshed token failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			t.Fatalf("refreshed token with identify scope should access users/@me, got %d", resp.StatusCode)
		}
	})
}
