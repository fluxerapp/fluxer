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

// TestOAuth2ApplicationDelete validates deleting an OAuth2 application.
// Deletion should also remove associated resources to avoid leaks.
func TestOAuth2ApplicationDelete(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	name := fmt.Sprintf("Delete Test App %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}
	scopes := []string{"identify"}

	var _ []string = scopes
	appID, botUserID, botToken := createOAuth2BotApplication(t, client, owner, name, redirectURIs)

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build bot auth request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bot %s", botToken))
	client.applyCommonHeaders(req)

	botResp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("bot auth request failed: %v", err)
	}
	botResp.Body.Close()

	if botResp.StatusCode != http.StatusOK {
		t.Fatalf("bot should be authenticated before deletion")
	}

	deleteOAuth2Application(t, client, owner, appID)

	resp, err := client.getWithAuth(fmt.Sprintf("/oauth2/applications/%s", appID), owner.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	resp.Body.Close()

	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("deleted application should return 404, got %d", resp.StatusCode)
	}

	apps := listOAuth2Applications(t, client, owner.Token)
	for _, app := range apps {
		if app.ID == appID {
			t.Fatalf("deleted application should not appear in list")
		}
	}

	req2, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build bot auth request: %v", err)
	}
	req2.Header.Set("Authorization", fmt.Sprintf("Bot %s", botToken))
	client.applyCommonHeaders(req2)

	botResp2, err := client.httpClient.Do(req2)
	if err != nil {
		t.Fatalf("bot auth request failed: %v", err)
	}
	botResp2.Body.Close()

	if botResp2.StatusCode == http.StatusOK {
		t.Fatalf("bot token should be invalidated after application deletion")
	}

	userResp, err := client.getWithAuth(fmt.Sprintf("/users/%s", botUserID), owner.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	userResp.Body.Close()

	if userResp.StatusCode != http.StatusNotFound {
		t.Fatalf("bot user should be deleted when application is deleted, got status %d", userResp.StatusCode)
	}
}
