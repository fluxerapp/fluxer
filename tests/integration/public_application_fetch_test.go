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

// TestPublicApplicationFetch validates unauthenticated public metadata retrieval.
func TestPublicApplicationFetch(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)

	appID, _, _, _ := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Public App Fetch %d", time.Now().UnixNano()),
		[]string{"https://example.com/redirect"},
		[]string{"bot", "applications.commands"},
	)

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/applications/%s/public", client.baseURL, appID), nil)
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected 200 for public application fetch, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var payload struct {
		ID           string   `json:"id"`
		Name         string   `json:"name"`
		RedirectURIs []string `json:"redirect_uris"`
		Scopes       []string `json:"scopes"`
		BotPublic    bool     `json:"bot_public"`
	}
	decodeJSONResponse(t, resp, &payload)
	if payload.ID != appID {
		t.Fatalf("expected id %s, got %s", appID, payload.ID)
	}
	if len(payload.RedirectURIs) == 0 {
		t.Fatalf("redirect_uris should be populated")
	}
	if len(payload.Scopes) == 0 {
		t.Fatalf("scopes should be populated")
	}
}
