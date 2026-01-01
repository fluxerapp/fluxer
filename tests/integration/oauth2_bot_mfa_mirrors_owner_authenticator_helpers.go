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

func fetchUserMeWithToken(t testing.TB, client *testClient, token string, isBot bool) userMeResponse {
	t.Helper()
	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build users/@me request: %v", err)
	}
	if isBot {
		req.Header.Set("Authorization", fmt.Sprintf("Bot %s", token))
	} else {
		req.Header.Set("Authorization", token)
	}
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("users/@me request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("users/@me returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var body userMeResponse
	decodeJSONResponse(t, resp, &body)
	return body
}
