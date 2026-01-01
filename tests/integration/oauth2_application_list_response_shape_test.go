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

// TestOAuth2ApplicationListResponseShape validates the response structure matches the API contract.
func TestOAuth2ApplicationListResponseShape(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appName := fmt.Sprintf("Shape Test %d", time.Now().UnixNano())
	createOAuth2BotApplication(t, client, owner, appName, []string{"https://example.com/callback"})

	resp, err := client.getWithAuth("/oauth2/applications/@me", owner.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 OK, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	// Parse as array of maps
	var data []map[string]any
	decodeJSONResponse(t, resp, &data)

	if len(data) == 0 {
		t.Fatalf("expected at least one application in list")
	}

	app := data[0]

	requiredFields := []string{"id", "name", "redirect_uris"}
	for _, field := range requiredFields {
		if _, ok := app[field]; !ok {
			t.Fatalf("application missing required field %q: %#v", field, app)
		}
	}

	if _, ok := app["client_secret"]; ok {
		t.Fatalf("client_secret should not be included in list response")
	}

	if bot, ok := app["bot"].(map[string]any); ok {
		botRequiredFields := []string{"id", "username", "discriminator"}
		for _, field := range botRequiredFields {
			if _, ok := bot[field]; !ok {
				t.Fatalf("bot object missing required field %q: %#v", field, bot)
			}
		}
		if _, ok := bot["token"]; ok {
			t.Fatalf("bot token should not be included in list response")
		}
	}
}
