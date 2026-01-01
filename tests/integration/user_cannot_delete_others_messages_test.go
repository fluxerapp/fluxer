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

// TestUserCannotDeleteOthersMessages tests message delete permissions
func TestUserCannotDeleteOthersMessages(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Message Delete Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	ownerMessage := sendChannelMessage(t, client, owner.Token, channelID, "owner's message to delete")
	ownerMessageID := parseSnowflake(t, ownerMessage.ID)

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d", channelID, ownerMessageID), member.Token)
	if err != nil {
		t.Fatalf("failed to attempt delete: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for deleting other's message, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=10", channelID), owner.Token)
	if err != nil {
		t.Fatalf("failed to verify message exists: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var messages []messageResponse
	decodeJSONResponse(t, resp, &messages)
	found := false
	for _, msg := range messages {
		if msg.ID == ownerMessage.ID {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("message was deleted when it shouldn't have been")
	}
}
