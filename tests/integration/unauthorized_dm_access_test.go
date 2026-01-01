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
)

// TestUnauthorizedDMAccess tests DM privacy
func TestUnauthorizedDMAccess(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	attacker := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)

	dmChannel := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))
	channelID := parseSnowflake(t, dmChannel.ID)

	message := sendChannelMessage(t, client, user1.Token, channelID, "private DM content")

	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=10", channelID), attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt DM read: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized DM read, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	ensureSessionStarted(t, client, attacker.Token)
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), map[string]string{"content": "attacker intrusion"}, attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt DM send: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized DM send, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	messageID := parseSnowflake(t, message.ID)
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), map[string]string{"content": "hacked dm"}, attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt DM edit: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized DM edit, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
