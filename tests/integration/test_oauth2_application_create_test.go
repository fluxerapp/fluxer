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

// TestOAuth2ApplicationCreate validates creating an OAuth2 application with a bot user.
// Creating an application should also create an associated bot.
func TestOAuth2ApplicationCreate(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	name := fmt.Sprintf("Test App %d", time.Now().UnixNano())
	redirectURIs := []string{"https://example.com/callback"}

	payload := map[string]any{
		"name":          name,
		"redirect_uris": redirectURIs,
		"bot_public":    true,
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
	if len(app.RedirectURIs) != len(redirectURIs) || app.RedirectURIs[0] != redirectURIs[0] {
		t.Fatalf("expected redirect_uris %v, got %v", redirectURIs, app.RedirectURIs)
	}

	if app.Bot == nil {
		t.Fatalf("expected bot object in response, got nil")
	}
	if app.Bot.ID == "" {
		t.Fatalf("bot response missing id")
	}
	if app.Bot.Username == "" {
		t.Fatalf("bot response missing username")
	}
	if app.Bot.Discriminator == "" {
		t.Fatalf("bot response missing discriminator")
	}
	if app.Bot.Token == "" {
		t.Fatalf("bot token should be returned on creation")
	}

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build bot auth request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bot %s", app.Bot.Token))
	client.applyCommonHeaders(req)

	botResp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("bot auth request failed: %v", err)
	}
	defer botResp.Body.Close()

	if botResp.StatusCode != http.StatusOK {
		t.Fatalf("bot authentication failed with status %d: %s", botResp.StatusCode, readResponseBody(botResp))
	}

	var botUser struct {
		ID  string `json:"id"`
		Bot bool   `json:"bot"`
	}
	decodeJSONResponse(t, botResp, &botUser)

	if botUser.ID != app.Bot.ID {
		t.Fatalf("bot user id mismatch: expected %s, got %s", app.Bot.ID, botUser.ID)
	}
	if !botUser.Bot {
		t.Fatalf("user should have bot flag set to true")
	}
}
