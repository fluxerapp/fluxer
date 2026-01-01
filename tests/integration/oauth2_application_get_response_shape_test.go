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

// TestOAuth2ApplicationGetResponseShape validates the complete response structure
// matches the expected API contract.
func TestOAuth2ApplicationGetResponseShape(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	name := fmt.Sprintf("Shape Test %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback", "https://example.com/callback2"}
	scopes := []string{"identify", "email"}

	appID, _, _, _ := createOAuth2Application(t, client, owner, name, redirectURIs, scopes)

	resp, err := client.getWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID), owner.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 OK, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	// Parse as generic map to inspect structure
	var data map[string]any
	decodeJSONResponse(t, resp, &data)

	requiredFields := []string{"id", "name", "redirect_uris"}
	for _, field := range requiredFields {
		if _, ok := data[field]; !ok {
			t.Fatalf("response missing required field %q: %#v", field, data)
		}
	}

	bot, ok := data["bot"].(map[string]any)
	if !ok {
		t.Fatalf("bot should be an object, got %T", data["bot"])
	}

	botRequiredFields := []string{"id", "username", "discriminator"}
	for _, field := range botRequiredFields {
		if _, ok := bot[field]; !ok {
			t.Fatalf("bot object missing required field %q: %#v", field, bot)
		}
	}

	if _, ok := bot["token"]; ok {
		t.Fatalf("bot token should not be included in GET response")
	}

	if secret, ok := data["client_secret"]; ok && secret != "" {
		t.Fatalf("client_secret should not be included in GET response, got: %v", secret)
	}
}
