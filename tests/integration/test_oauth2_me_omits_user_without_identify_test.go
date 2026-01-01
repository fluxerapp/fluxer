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

// TestOAuth2MeOmitsUserWithoutIdentify verifies the /oauth2/@me endpoint omits the
// user object when identify scope was not granted.
func TestOAuth2MeOmitsUserWithoutIdentify(t *testing.T) {
	t.Parallel()

	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	endUser := createTestAccount(t, client)

	redirectURI := "https://example.com/me/no-identify"
	appID, _, _, clientSecret := createOAuth2Application(
		t,
		client,
		appOwner,
		fmt.Sprintf("OAuth2 Me No Identify %d", time.Now().UnixNano()),
		[]string{redirectURI},
		[]string{"guilds", "email"},
	)

	authCode, _ := authorizeOAuth2(
		t,
		client,
		endUser.Token,
		appID,
		redirectURI,
		[]string{"guilds"},
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
		User    map[string]interface{} `json:"user"`
		Expires string                 `json:"expires"`
	}
	decodeJSONResponse(t, resp, &payload)

	if payload.Application.ID != appID {
		t.Fatalf("expected application id %s, got %s", appID, payload.Application.ID)
	}
	if payload.User != nil {
		t.Fatalf("user should be omitted when identify scope is not granted")
	}
	if len(payload.Scopes) != 1 || payload.Scopes[0] != "guilds" {
		t.Fatalf("expected scopes [guilds], got %v", payload.Scopes)
	}
	if payload.Expires == "" {
		t.Fatalf("expires should be set")
	}
}
