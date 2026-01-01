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
)

// getOAuth2UserInfo retrieves user information using an OAuth2 access token.
func getOAuth2UserInfo(t testing.TB, client *testClient, accessToken string) map[string]any {
	t.Helper()
	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/userinfo", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build userinfo request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", accessToken))
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("userinfo request failed: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("userinfo request failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var userInfo map[string]any
	decodeJSONResponse(t, resp, &userInfo)
	return userInfo
}
