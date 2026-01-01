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

// TestBotTokenReset verifies that bot tokens can be reset through the OAuth2 application API.
// This is a sensitive operation that should require sudo mode verification for users with MFA
// to ensure recent authentication is present.
func TestBotTokenReset(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appName := fmt.Sprintf("Test Bot Reset %d", time.Now().UnixNano())
	redirectURI := "https://example.com/callback"
	appID, botUserID, originalToken := createOAuth2BotApplication(t, client, owner, appName, []string{redirectURI})

	if originalToken == "" {
		t.Fatalf("bot creation should return a token")
	}

	t.Logf("Original bot token: %s...", originalToken[:20])

	botAccount := authenticateWithBotToken(t, client, originalToken)
	if botAccount.UserID != botUserID {
		t.Fatalf("expected bot user id %s, got %s", botUserID, botAccount.UserID)
	}
	t.Logf("Original token successfully authenticated as bot %s", botUserID)

	newToken := resetBotToken(t, client, owner, appID)
	if newToken == "" {
		t.Fatalf("reset token should return a new token")
	}

	if newToken == originalToken {
		t.Fatalf("reset should generate a new token, got same token")
	}

	t.Logf("New bot token: %s...", newToken[:20])

	newBotAccount := authenticateWithBotToken(t, client, newToken)
	if newBotAccount.UserID != botUserID {
		t.Fatalf("expected bot user id %s with new token, got %s", botUserID, newBotAccount.UserID)
	}
	t.Logf("New token successfully authenticated as bot %s", botUserID)

	req, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req.Header.Set("Authorization", fmt.Sprintf("Bot %s", originalToken))
	client.applyCommonHeaders(req)

	resp, err := client.httpClient.Do(req)
	if err != nil {
		t.Fatalf("request with old token failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusUnauthorized {
		t.Fatalf("old token should be rejected, got status %d", resp.StatusCode)
	}

	t.Logf("Old token correctly rejected after reset")

	anotherToken := resetBotToken(t, client, owner, appID)
	if anotherToken == "" {
		t.Fatalf("second reset should return a token")
	}

	if anotherToken == newToken || anotherToken == originalToken {
		t.Fatalf("each reset should generate a unique token")
	}

	finalBotAccount := authenticateWithBotToken(t, client, anotherToken)
	if finalBotAccount.UserID != botUserID {
		t.Fatalf("expected bot user id %s with final token, got %s", botUserID, finalBotAccount.UserID)
	}

	req2, err := http.NewRequest(http.MethodGet, fmt.Sprintf("%s/users/@me", client.baseURL), nil)
	if err != nil {
		t.Fatalf("failed to build request: %v", err)
	}
	req2.Header.Set("Authorization", fmt.Sprintf("Bot %s", newToken))
	client.applyCommonHeaders(req2)

	resp2, err := client.httpClient.Do(req2)
	if err != nil {
		t.Fatalf("request with second token failed: %v", err)
	}
	defer resp2.Body.Close()

	if resp2.StatusCode != http.StatusUnauthorized {
		t.Fatalf("second token should be rejected after third reset, got status %d", resp2.StatusCode)
	}

	t.Logf("Token reset working correctly - only latest token is valid")
}
