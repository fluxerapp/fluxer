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

// TestOAuth2ApplicationListAlternativeEndpoint validates that the alternative endpoint
// /users/@me/applications also works, matching the dual endpoint support in the API.
func TestOAuth2ApplicationListAlternativeEndpoint(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appName := fmt.Sprintf("Alt Endpoint Test %d", time.Now().UnixNano())
	appID, _, _ := createOAuth2BotApplication(t, client, owner, appName, []string{"https://example.com/callback"})

	resp, err := client.getWithAuth("/users/@me/applications", owner.Token)
	if err != nil {
		t.Fatalf("request to alternative endpoint failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 OK from alternative endpoint, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var apps []oauth2ApplicationResponse
	decodeJSONResponse(t, resp, &apps)

	// Should find the application
	var found bool
	for _, app := range apps {
		if app.ID == appID {
			found = true
			break
		}
	}

	if !found {
		t.Fatalf("application not found in alternative endpoint response")
	}
}
