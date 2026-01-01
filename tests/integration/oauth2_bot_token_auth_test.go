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

// TestBotTokenAuthentication verifies that bot tokens can be used to authenticate API requests.
// Bot tokens should use the "Bot" authorization scheme (e.g., "Authorization: Bot TOKEN")
// rather than the "Bearer" scheme used for OAuth2 access tokens.
func TestBotTokenAuthentication(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appName := fmt.Sprintf("Auth Test Bot %d", time.Now().UnixNano())
	redirectURI := "https://example.com/callback"
	appID, botUserID, botToken := createOAuth2BotApplication(t, client, owner, appName, []string{redirectURI})

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bot %s", botToken))
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("bot authentication request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("bot authentication failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var botUser map[string]any
	decodeJSONResponse(t, resp, &botUser)

	if botUser["id"] != botUserID {
		t.Fatalf("expected bot id %s, got %v", botUserID, botUser["id"])
	}

	if username, ok := botUser["username"].(string); !ok || username == "" {
		t.Fatalf("bot user should have username, got %v", botUser["username"])
	}

	if bot, ok := botUser["bot"].(bool); !ok || !bot {
		t.Fatalf("bot user should have bot=true, got %v", botUser["bot"])
	}

	t.Logf("Bot authenticated successfully - ID: %s, Username: %s", botUserID, botUser["username"])

	req2, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req2.Header.Set("Authorization", fmt.Sprintf("Bearer %s", botToken))
	client.applyCommonHeaders(req2)

	resp2, err := client.httpClient.Do(req2)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp2.Body.Close()

	if resp2.StatusCode == http.StatusOK {
		t.Fatalf("bot token should not work with Bearer prefix, but got status %d", resp2.StatusCode)
	}

	t.Logf("Bearer prefix correctly rejected for bot tokens")

	req3, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/oauth2/applications/%s", client.baseURL, appID), nil)
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req3.Header.Set("Authorization", fmt.Sprintf("Bot %s", botToken))
	client.applyCommonHeaders(req3)

	resp3, err := client.httpClient.Do(req3)
	if err != nil {
		t.Fatalf("application request failed: %v", err)
	}
	defer resp3.Body.Close()

	if resp3.StatusCode != http.StatusOK && resp3.StatusCode != http.StatusForbidden {
		t.Fatalf("unexpected status when bot accesses application: %d: %s", resp3.StatusCode, readResponseBody(resp3))
	}

	if resp3.StatusCode == http.StatusOK {
		t.Logf("Bot can access application details")
	} else {
		t.Logf("Bot has restricted access to application details (expected)")
	}

	req4, err := http.NewRequest(http.MethodPost, fmt.Sprintf("%s/oauth2/applications", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req4.Header.Set("Authorization", fmt.Sprintf("Bot %s", botToken))
	req4.Header.Set("Content-Type", "application/json")
	client.applyCommonHeaders(req4)

	resp4, err := client.httpClient.Do(req4)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp4.Body.Close()

	if resp4.StatusCode == http.StatusOK {
		t.Fatalf("bot should not be able to create applications, got status %d", resp4.StatusCode)
	}

	t.Logf("Bot correctly restricted from creating applications")
}
