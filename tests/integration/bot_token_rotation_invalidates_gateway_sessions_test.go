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

func TestBotTokenRotationInvalidatesGatewaySessions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	observer := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Session Invalidator %d", time.Now().UnixNano()))
	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, observer.Token, invite.Code)

	appID, botUserID, botToken := createOAuth2BotApplication(t, client, owner, fmt.Sprintf("Session Invalidator %d", time.Now().UnixNano()), []string{"https://example.com/callback"})

	permissions := fmt.Sprintf("%d", 1<<5)
	payload := map[string]any{
		"client_id":   appID,
		"scope":       "bot",
		"guild_id":    guild.ID,
		"permissions": permissions,
	}

	resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to authorize bot: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var consentResp struct {
		RedirectTo string `json:"redirect_to"`
	}
	decodeJSONResponse(t, resp, &consentResp)
	if consentResp.RedirectTo == "" {
		t.Fatal("expected redirect_to in consent response")
	}

	authenticateWithBotToken(t, client, botToken)

	botGateway := newGatewayClient(t, client, botToken)
	t.Cleanup(botGateway.Close)

	assertUserHasActivePresence(t, client, botUserID, true)

	newToken := resetBotToken(t, client, owner, appID)

	time.Sleep(1 * time.Second)

	assertUserHasActivePresence(t, client, botUserID, false)

	botGateway.Close()
	newBotGateway := newGatewayClient(t, client, newToken)
	t.Cleanup(newBotGateway.Close)

	assertUserHasActivePresence(t, client, botUserID, true)
}

// assertUserHasActivePresence checks if a user has an active presence via the test harness
func assertUserHasActivePresence(t testing.TB, client *testClient, userID string, expected bool) {
	t.Helper()

	resp, err := client.get(fmt.Sprintf("/test/users/%s/presence/has-active", userID))
	if err != nil {
		t.Fatalf("failed to check user presence: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var presenceResp struct {
		UserID    string `json:"user_id"`
		HasActive bool   `json:"has_active"`
		Error     string `json:"error,omitempty"`
	}
	decodeJSONResponse(t, resp, &presenceResp)

	if presenceResp.Error != "" {
		t.Fatalf("presence check returned error: %s", presenceResp.Error)
	}

	if presenceResp.HasActive != expected {
		t.Fatalf("expected user %s to has_active=%v, got %v", userID, expected, presenceResp.HasActive)
	}
}
