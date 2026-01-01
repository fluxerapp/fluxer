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
	"net/http"
	"testing"
)

// listOAuth2Applications retrieves all OAuth2 applications for the authenticated user.
func listOAuth2Applications(t testing.TB, client *testClient, token string) []oauth2ApplicationResponse {
	t.Helper()
	resp, err := client.getWithAuth("/oauth2/applications/@me", token)
	if err != nil {
		t.Fatalf("failed to list applications: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("list applications failed: %s", readResponseBody(resp))
	}
	var apps []oauth2ApplicationResponse
	decodeJSONResponse(t, resp, &apps)
	return apps
}
