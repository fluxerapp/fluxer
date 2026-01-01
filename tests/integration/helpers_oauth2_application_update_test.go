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

// updateOAuth2Application updates an OAuth2 application with the provided changes.
func updateOAuth2Application(t testing.TB, client *testClient, token string, applicationID string, updates map[string]any) oauth2ApplicationResponse {
	t.Helper()
	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s", applicationID), updates, token)
	if err != nil {
		t.Fatalf("failed to update application: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("update application failed: %s", readResponseBody(resp))
	}
	var app oauth2ApplicationResponse
	decodeJSONResponse(t, resp, &app)
	return app
}
