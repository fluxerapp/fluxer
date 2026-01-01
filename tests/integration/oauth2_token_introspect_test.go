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

// introspectOAuth2Token introspects an OAuth2 token to get its metadata.
func introspectOAuth2Token(t testing.TB, client *testClient, clientID string, clientSecret string, token string) oauth2IntrospectionResponse {
	t.Helper()
	if clientSecret == "" {
		clientSecret = getClientSecret(t, clientID)
	}
	form := url.Values{
		"token": {token},
	}

	req, err := http.NewRequest(http.MethodPost, fmt.Sprintf("%s/oauth2/introspect", client.baseURL), strings.NewReader(form.Encode()))
	if err != nil {
		t.Fatalf("failed to build introspect request: %v", err)
	}
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	req.SetBasicAuth(clientID, clientSecret)
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("introspect request failed: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("introspect failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var result oauth2IntrospectionResponse
	decodeJSONResponse(t, resp, &result)
	return result
}
