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

// updateBotProfile updates the bot profile for an OAuth2 application.
func updateBotProfile(t testing.TB, client *testClient, token string, applicationID string, updates map[string]any) map[string]any {
	t.Helper()
	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s/bot", applicationID), updates, token)
	if err != nil {
		t.Fatalf("failed to update bot profile: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("update bot profile failed: %s", readResponseBody(resp))
	}
	var result map[string]any
	decodeJSONResponse(t, resp, &result)
	return result
}
