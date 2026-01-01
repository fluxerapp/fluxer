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

// exchangeOAuth2AuthorizationCode exchanges an authorization code for an access token.
func exchangeOAuth2AuthorizationCode(t testing.TB, client *testClient, clientID string, clientSecret string, code string, redirectURI string, codeVerifier string) oauth2TokenResponse {
	t.Helper()
	_ = codeVerifier
	if clientSecret == "" {
		clientSecret = getClientSecret(t, clientID)
	}
	form := url.Values{
		"grant_type":   {"authorization_code"},
		"code":         {code},
		"redirect_uri": {redirectURI},
	}
	var resp *http.Response
	var err error

	form.Set("client_id", clientID)
	req, reqErr := http.NewRequest(http.MethodPost, fmt.Sprintf("%s/oauth2/token", client.baseURL), strings.NewReader(form.Encode()))
	if reqErr != nil {
		t.Fatalf("failed to build token request: %v", reqErr)
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.SetBasicAuth(clientID, clientSecret)
	client.applyCommonHeaders(req)
	resp, err = client.httpClient.Do(req)

	if err != nil {
		t.Fatalf("failed to exchange oauth2 code: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("oauth2 token exchange failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var token oauth2TokenResponse
	decodeJSONResponse(t, resp, &token)
	if token.AccessToken == "" {
		t.Fatalf("oauth2 token response missing access_token")
	}
	return token
}
