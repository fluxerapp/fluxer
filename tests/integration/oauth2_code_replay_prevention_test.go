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

// TestOAuth2AuthorizationCodeReplayPrevention verifies that authorization codes
// can only be used once. This is a critical security measure to prevent replay attacks
// where an attacker intercepts an authorization code and attempts to use it multiple times,
// as required by the OAuth 2.0 specification (RFC 6749).
func TestOAuth2AuthorizationCodeReplayPrevention(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/callback"

	t.Run("application authorization code can only be used once with client secret", func(t *testing.T) {
		appID, _, _, clientSecret := createOAuth2Application(t, client, appOwner,
			fmt.Sprintf("App %d", time.Now().UnixNano()),
			[]string{redirectURI},
			[]string{"identify"},
		)

		authCode, _ := authorizeOAuth2(t, client, endUser.Token, appID, redirectURI, []string{"identify"}, "", "", "")

		token1 := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")
		if token1.AccessToken == "" {
			t.Fatalf("first token exchange should succeed")
		}

		form := url.Values{
			"grant_type":   {"authorization_code"},
			"code":         {authCode},
			"redirect_uri": {redirectURI},
			"client_id":    {appID},
		}

		req, err := http.NewRequest(http.MethodPost, fmt.Sprintf("%s/oauth2/token", client.baseURL), strings.NewReader(form.Encode()))
		if err != nil {
			t.Fatalf("failed to build second token request: %v", err)
		}
		req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
		req.SetBasicAuth(appID, clientSecret)
		client.applyCommonHeaders(req)

		resp, err := client.httpClient.Do(req)
		if err != nil {
			t.Fatalf("second token request failed: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("authorization code should not be reusable - second exchange should fail")
		}
		if resp.StatusCode != http.StatusBadRequest && resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("expected 400 or 401 for code replay, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

}
