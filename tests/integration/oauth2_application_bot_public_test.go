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

// TestOAuth2ApplicationBotPublicToggle verifies bots can be made private and block non-owner invites.
func TestOAuth2ApplicationBotPublicToggle(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	otherUser := createTestAccount(t, client)

	appID, _, _, _ := createOAuth2Application(
		t, client, appOwner,
		fmt.Sprintf("Bot Visibility %d", time.Now().UnixNano()),
		[]string{"https://example.com/callback"},
		[]string{"bot"},
	)

	app := getOAuth2Application(t, client, appOwner.Token, appID)
	if !app.BotPublic {
		t.Fatalf("bot_public should default to true")
	}

	updates := map[string]any{
		"bot_public": false,
	}
	updated := updateOAuth2Application(t, client, appOwner.Token, appID, updates)
	if updated.BotPublic {
		t.Fatalf("bot_public should be false after update")
	}

	payload := map[string]any{
		"response_type": "code",
		"client_id":     appID,
		"scope":         "bot",
	}
	resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", payload, otherUser.Token)
	if err != nil {
		t.Fatalf("authorize request failed: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 when non-owner invites private bot, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	resp, err = client.postJSONWithAuth("/oauth2/authorize/consent", payload, appOwner.Token)
	if err != nil {
		t.Fatalf("authorize request failed: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected owner to authorize private bot, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}
}
