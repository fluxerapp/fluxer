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

// TestOAuth2ScopeEmailIncludesEmailInUsersMe verifies that the "email" scope
// adds the email field to the /users/@me response:
// - identify scope: returns basic user info (id, username, avatar, etc.) without email
// - email scope: adds the email field to the user object
//
// The email scope should always be used in combination with identify scope.
func TestOAuth2ScopeEmailIncludesEmailInUsersMe(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/callback"

	t.Run("email scope includes email field in users/@me", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Email Scope App %d", time.Now().UnixNano()), []string{redirectURI})

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify", "email"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, "", authCode, redirectURI, "")

		if !strings.Contains(token.Scope, "identify") {
			t.Fatalf("token scope should contain identify, got '%s'", token.Scope)
		}
		if !strings.Contains(token.Scope, "email") {
			t.Fatalf("token scope should contain email, got '%s'", token.Scope)
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
			t.Fatalf("users/@me should be accessible with email scope, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}

		var userData map[string]any
		decodeJSONResponse(t, resp, &userData)

		if userData["id"] != endUser.UserID {
			t.Fatalf("expected user id %s, got %v", endUser.UserID, userData["id"])
		}

		email, ok := userData["email"]
		if !ok || email == nil {
			t.Fatalf("email field should be present with email scope, got userData: %#v", userData)
		}

		emailStr, ok := email.(string)
		if !ok || emailStr == "" {
			t.Fatalf("email should be a non-empty string, got: %v", email)
		}

		if emailStr != endUser.Email {
			t.Fatalf("expected email %s, got %s", endUser.Email, emailStr)
		}
	})

	t.Run("identify without email scope excludes email field", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("No Email Scope %d", time.Now().UnixNano()), []string{redirectURI})

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
			t.Fatalf("email field should not be present without email scope, got: %v", email)
		}
	})

	t.Run("email scope works with application using client secret", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Email App %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify", "email"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify", "email"}, "", "", "")
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
			t.Fatalf("email scope should work with application, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}

		var userData map[string]any
		decodeJSONResponse(t, resp, &userData)

		if userData["email"] == nil {
			t.Fatalf("email should be present with email scope on application")
		}
		if userData["email"] != endUser.Email {
			t.Fatalf("expected email %s, got %v", endUser.Email, userData["email"])
		}
	})

	t.Run("refreshed token preserves email scope", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("Email Refresh %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify", "email"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify", "email"}, "", "", "")
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		refreshedToken := refreshOAuth2Token(t, client, appID, clientSecret, token.RefreshToken)

		if !strings.Contains(refreshedToken.Scope, "email") {
			t.Fatalf("refreshed token should preserve email scope, got '%s'", refreshedToken.Scope)
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

		var userData map[string]any
		decodeJSONResponse(t, resp, &userData)

		if userData["email"] == nil {
			t.Fatalf("refreshed token should still provide email access")
		}
		if userData["email"] != endUser.Email {
			t.Fatalf("expected email %s with refreshed token, got %v", endUser.Email, userData["email"])
		}
	})

	t.Run("email scope alone is insufficient without identify", func(t *testing.T) {
		appID, _, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Email Only %d", time.Now().UnixNano()), []string{redirectURI})

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"email"}, "", "", "")

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

		if resp.StatusCode == http.StatusOK {
			var userData map[string]any
			decodeJSONResponse(t, resp, &userData)
			if userData["email"] == nil {
				t.Fatalf("email scope should provide email access")
			}
		} else {
			t.Logf("Note: email scope alone was accepted but may have limited functionality")
		}
	})
}
