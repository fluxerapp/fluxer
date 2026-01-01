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

// TestOAuth2MeReturnsApplicationAndUser verifies the /oauth2/@me endpoint returns
// application metadata and user info when the identify scope is granted.
func TestOAuth2MeReturnsApplicationAndUser(t *testing.T) {
	t.Parallel()

	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/me/identify"
	appID, _, _, clientSecret := createOAuth2Application(
		t,
		client,
		appOwner,
		fmt.Sprintf("OAuth2 Me Identify %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"identify", "email"},
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
	tokens := exchangeOAuth2AuthorizationCode(t, client, appID, clientSecret, authCode, redirectURI, "")

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build /oauth2/@me request: %v", err)
	}
	req.Header.Set("Authorization", "Bearer "+tokens.AccessToken)
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("oauth2/@me request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 from oauth2/@me, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var payload struct {
		Application struct {
			ID   string `json:"id"`
			Name string `json:"name"`
		} `json:"application"`
		Scopes  []string               `json:"scopes"`
		Expires string                 `json:"expires"`
		User    map[string]interface{} `json:"user"`
	}
	decodeJSONResponse(t, resp, &payload)

	if payload.Application.ID != appID {
		t.Fatalf("expected application id %s, got %s", appID, payload.Application.ID)
	}
	if payload.Expires == "" {
		t.Fatalf("expires should be set")
	}
	if len(payload.Scopes) == 0 {
		t.Fatalf("scopes should be returned")
	}
	if payload.User == nil {
		t.Fatalf("user should be populated when identify scope is granted")
	}
	if payload.User["id"] != endUser.UserID {
		t.Fatalf("user.id should match %s, got %v", endUser.UserID, payload.User["id"])
	}
}
