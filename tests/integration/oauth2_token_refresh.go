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
)

// refreshOAuth2Token refreshes an OAuth2 access token using a refresh token.
func refreshOAuth2Token(t testing.TB, client *testClient, clientID string, clientSecret string, refreshToken string) oauth2TokenResponse {
	t.Helper()
	if clientSecret == "" {
		clientSecret = getClientSecret(t, clientID)
	}
	form := url.Values{
		"grant_type":    {"refresh_token"},
		"refresh_token": {refreshToken},
	}

	var resp *http.Response
	var err error

	form.Set("client_id", clientID)
	req, reqErr := http.NewRequest(http.MethodPost, fmt.Sprintf("%s/oauth2/token", client.baseURL), strings.NewReader(form.Encode()))
	if reqErr != nil {
		t.Fatalf("failed to build token refresh request: %v", reqErr)
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.SetBasicAuth(clientID, clientSecret)
	client.applyCommonHeaders(req)
	resp, err = client.httpClient.Do(req)

	if err != nil {
		t.Fatalf("failed to refresh oauth2 token: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("oauth2 token refresh failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var token oauth2TokenResponse
	decodeJSONResponse(t, resp, &token)
	if token.AccessToken == "" {
		t.Fatalf("oauth2 token refresh response missing access_token")
	}
	return token
}
