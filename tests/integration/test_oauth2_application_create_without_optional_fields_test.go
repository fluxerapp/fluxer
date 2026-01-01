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

// TestOAuth2ApplicationCreateWithoutOptionalFields validates that applications can be created
// with minimal required fields.
func TestOAuth2ApplicationCreateWithoutOptionalFields(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	name := fmt.Sprintf("Minimal App %d", time.Now().UnixNano())
	payload := map[string]any{
		"name": name,
	}

	resp, err := client.postJSONWithAuth("/oauth2/applications", payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create application: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 OK, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var app oauth2ApplicationResponse
	decodeJSONResponse(t, resp, &app)

	if app.ID == "" {
		t.Fatalf("application response missing id")
	}
	if app.Name != name {
		t.Fatalf("expected name %q, got %q", name, app.Name)
	}

	if app.RedirectURIs == nil {
		t.Fatalf("redirect_uris should be an empty array, not nil")
	}

	if app.Bot == nil {
		t.Fatalf("application should have a bot user by default")
	}
}
