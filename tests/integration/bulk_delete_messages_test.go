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

func TestBulkDeleteMessages(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)

	guild := createGuild(t, client, owner.Token, "Bulk Delete Test Guild")
	waitForGuildEvent(t, ownerSocket, "GUILD_CREATE", guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", parseSnowflake(t, guild.ID)), map[string]any{
		"name": "test-channel",
		"type": 0,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel minimalChannelResponse
	decodeJSONResponse(t, resp, &channel)
	waitForChannelEvent(t, ownerSocket, "CHANNEL_CREATE", channel.ID)

	var messageIDs []string
	for i := 0; i < 5; i++ {
		msg := sendChannelMessage(t, client, owner.Token, parseSnowflake(t, channel.ID), fmt.Sprintf("Message %d to delete", i+1))
		waitForMessageEvent(t, ownerSocket, "MESSAGE_CREATE", msg.ID, nil)
		messageIDs = append(messageIDs, msg.ID)
	}

	bulkDeletePayload := map[string]any{
		"message_ids": messageIDs,
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/bulk-delete", parseSnowflake(t, channel.ID)), bulkDeletePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to bulk delete messages: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForMessageDeleteBulk(t, ownerSocket, channel.ID, messageIDs)

	for _, msgID := range messageIDs {
		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d", parseSnowflake(t, channel.ID), parseSnowflake(t, msgID)), owner.Token)
		if err != nil {
			t.Fatalf("failed to get deleted message: %v", err)
		}
		if resp.StatusCode != http.StatusNotFound {
			t.Fatalf("expected deleted message to return 404, got %d", resp.StatusCode)
		}
		resp.Body.Close()
	}
}
