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
	"net/url"
	"strings"
	"testing"
	"time"
)

// TestOAuth2ApplicationsMustUseClientSecret verifies that applications
// must authenticate using their client_secret during token exchange using HTTP Basic
// auth with client_id and client_secret.
func TestOAuth2ApplicationsMustUseClientSecret(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/callback"
	appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
		fmt.Sprintf("Confidential App %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")

	t.Run("reject token exchange without client secret", func(t *testing.T) {
		form := url.Values{
			"grant_type":   {"authorization_code"},
			"code":         {authCode},
			"redirect_uri": {redirectURI},
			"client_id":    {appID},
		}

		resp, err := client.postForm("/oauth2/token", form, "")
		if err != nil {
			t.Fatalf("failed to make token request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	authCode, _ = authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")

	t.Run("reject token exchange with invalid client secret", func(t *testing.T) {
		form := url.Values{
			"grant_type":   {"authorization_code"},
			"code":         {authCode},
			"redirect_uri": {redirectURI},
			"client_id":    {appID},
		}

		req, err := http.NewRequest(http.MethodPost, fmt.Sprintf("%s/oauth2/token", client.baseURL), strings.NewReader(form.Encode()))
		if err != nil {
			t.Fatalf("failed to build token request: %v", err)
		}
		req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
		req.SetBasicAuth(appID, "invalid_secret_12345")
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("token request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 401 Unauthorized, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	authCode, _ = authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")

	t.Run("accept token exchange with valid client secret", func(t *testing.T) {
		token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

		if token.AccessToken == "" {
			t.Fatalf("should successfully exchange code with valid client_secret")
		}
		if token.TokenType != "Bearer" {
			t.Fatalf("expected token_type Bearer, got %s", token.TokenType)
		}
		if token.Scope != "identify" {
			t.Fatalf("expected scope identify, got %s", token.Scope)
		}
	})
}
