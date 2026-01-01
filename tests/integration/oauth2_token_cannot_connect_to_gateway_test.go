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

// TestOAuth2TokenCannotConnectToGateway verifies that OAuth2 access tokens
// cannot connect to the gateway; only bot tokens should be accepted for gateway use.
func TestOAuth2TokenCannotConnectToGateway(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appName := fmt.Sprintf("Gateway OAuth2 Client %d", time.Now().UnixNano())
	redirectURI := "https://example.com/callback"
	appID, _, _, clientSecret := createOAuth2Application(t, client, owner, appName, []string{redirectURI}, []string{"identify"})

	code, _ := obtainAuthCode(t, client, appID, redirectURI, []string{"identify"})
	token := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, code, redirectURI, "")

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/gateway/bot", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build gateway bot request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", token.AccessToken))
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("gateway bot request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected 401 for OAuth2 access token on /gateway/bot, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
}
