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

// TestOAuth2TokenRevoke verifies token revocation functionality.
//
// Steps:
// 1. Get access token and refresh token
// 2. Revoke the access token
// 3. Verify the token no longer works
// 4. Verify introspection shows token as inactive
func TestOAuth2TokenRevoke(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/revoke/callback"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Revoke Test %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify"},
	)

	authCode, _ := authorizeOAuth2(
		t,
		client,
		endUser.Token,
		appID,
		redirectURI,
		[]string{"identify"},
		"",
		"",
		"",
	)
	tokens := exchangeOAuth2AuthorizationCode(
		t, client,
		appID,
		clientSecret,
		authCode,
		redirectURI,
		"",
	)

	userInfo := getOAuth2UserInfo(t, client, tokens.AccessToken)
	if userInfo["sub"] == nil {
		t.Fatal("token should work before revocation")
	}

	revokeOAuth2Token(t, client, appID, clientSecret, tokens.AccessToken, "access_token")

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/userinfo", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", tokens.AccessToken))
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	if resp.Body != nil {
		resp.Body.Close()
	}

	if resp.StatusCode == http.StatusOK {
		t.Fatal("revoked token should not work")
	}
	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected 401 Unauthorized for revoked token, got %d", resp.StatusCode)
	}

	introspection := introspectOAuth2Token(t, client, appID, clientSecret, tokens.AccessToken)
	if introspection.Active {
		t.Fatal("introspection should show revoked token as inactive")
	}
}
