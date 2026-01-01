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

// TestOAuth2AuthorizationsDeauthorize verifies that a user can deauthorize
// an application, which revokes all associated tokens.
func TestOAuth2AuthorizationsDeauthorize(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/authz/deauth"
	appID, _, _, clientSecret := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Deauth Test %d", time.Now().UnixNano()),
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
		t.Fatal("token should work before deauthorization")
	}

	resp, err := client.getWithAuth("/oauth2/@me/authorizations", endUser.Token)
	if err != nil {
		t.Fatalf("failed to list authorizations: %v", err)
	}
	var beforeAuthz []oauth2AuthorizationResponse
	decodeJSONResponse(t, resp, &beforeAuthz)
	if len(beforeAuthz) != 1 {
		t.Fatalf("expected 1 authorization before deauth, got %d", len(beforeAuthz))
	}

	resp, err = client.delete(fmt.Sprintf("/oauth2/@me/authorizations/%s", appID), endUser.Token)
	if err != nil {
		t.Fatalf("failed to deauthorize: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("deauthorize failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/userinfo", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", tokens.AccessToken))
	client.applyCommonHeaders(req)

	tokenResp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	if tokenResp.Body != nil {
		tokenResp.Body.Close()
	}

	if tokenResp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("expected 401 for revoked token, got %d", tokenResp.StatusCode)
	}

	resp, err = client.getWithAuth("/oauth2/@me/authorizations", endUser.Token)
	if err != nil {
		t.Fatalf("failed to list authorizations: %v", err)
	}
	var afterAuthz []oauth2AuthorizationResponse
	decodeJSONResponse(t, resp, &afterAuthz)
	if len(afterAuthz) != 0 {
		t.Fatalf("expected 0 authorizations after deauth, got %d", len(afterAuthz))
	}
}
