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

// createOAuth2BotApplication is a convenience wrapper that returns only the bot-related fields.
// All applications have both bot tokens and client secrets; this just ignores the client secret.
func createOAuth2BotApplication(t testing.TB, client *testClient, owner testAccount, name string, redirectURIs []string) (applicationID string, botUserID string, botToken string) {
	t.Helper()
	payload := map[string]any{
		"name":          name,
		"redirect_uris": redirectURIs,
	}
	resp, err := client.postJSONWithAuth("/oauth2/applications", payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create application: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("application creation failed: %s", readResponseBody(resp))
	}
	var created struct {
		ID           string `json:"id"`
		ClientSecret string `json:"client_secret"`
		Bot          struct {
			ID    string `json:"id"`
			Token string `json:"token"`
		} `json:"bot"`
	}
	decodeJSONResponse(t, resp, &created)
	if created.ID == "" {
		t.Fatalf("application response missing id")
	}
	if created.ClientSecret == "" {
		t.Fatalf("application response missing client_secret")
	}
	if created.Bot.ID == "" {
		t.Fatalf("application response missing bot.id")
	}
	if created.Bot.Token == "" {
		t.Fatalf("application response missing bot.token")
	}
	storeClientSecret(created.ID, created.ClientSecret)
	return created.ID, created.Bot.ID, created.Bot.Token
}
