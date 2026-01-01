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

func TestAddRecipientToGroupDM(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)
	user2Socket := newGatewayClient(t, client, user2.Token)
	t.Cleanup(user2Socket.Close)
	user3Socket := newGatewayClient(t, client, user3.Token)
	t.Cleanup(user3Socket.Close)

	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)

	drainRelationshipEvents(t, user1Socket)
	drainRelationshipEvents(t, user2Socket)
	drainRelationshipEvents(t, user3Socket)

	dmChannel := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))
	waitForChannelEvent(t, user1Socket, "CHANNEL_CREATE", dmChannel.ID)

	sendChannelMessage(t, client, user1.Token, parseSnowflake(t, dmChannel.ID), "test message")
	waitForChannelEvent(t, user2Socket, "CHANNEL_CREATE", dmChannel.ID)

	groupDmChannel := createGroupDmChannel(t, client, user1.Token, user2.UserID, user3.UserID)
	waitForChannelEvent(t, user1Socket, "CHANNEL_CREATE", groupDmChannel.ID)
	waitForChannelEvent(t, user2Socket, "CHANNEL_CREATE", groupDmChannel.ID)
	waitForChannelEvent(t, user3Socket, "CHANNEL_CREATE", groupDmChannel.ID)

	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, groupDmChannel.ID)), user3.Token)
	if err != nil {
		t.Fatalf("failed to get channel as user3: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel struct {
		ID         string `json:"id"`
		Type       int    `json:"type"`
		Recipients []any  `json:"recipients"`
	}
	decodeJSONResponse(t, resp, &channel)
	if channel.Type != 3 {
		t.Fatalf("expected channel type to be 3 (GROUP_DM), got %d", channel.Type)
	}
	if len(channel.Recipients) != 2 {
		t.Fatalf("expected 2 recipients in group DM (excluding current user), got %d", len(channel.Recipients))
	}
}
