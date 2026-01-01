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

// TestOAuth2TokenRefreshRevokedToken verifies that revoked refresh tokens
// cannot be used.
func TestOAuth2TokenRefreshRevokedToken(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/refresh/revoked"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Revoked Refresh %d", time.Now().UnixNano()),
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

	revokeOAuth2Token(t, client, appID, clientSecret, tokens.RefreshToken, "refresh_token")

	form := url.Values{
		"grant_type":    {"refresh_token"},
		"refresh_token": {tokens.RefreshToken},
		"client_id":     {appID},
	}

	req, err := http.NewRequest(http.MethodPost, fmt.Sprintf("%s/oauth2/token", client.baseURL), strings.NewReader(form.Encode()))
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.SetBasicAuth(appID, clientSecret)
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK {
		t.Fatal("revoked refresh token should not work")
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 Bad Request for revoked token, got %d", resp.StatusCode)
	}
}
