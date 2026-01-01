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

func TestTypingIndicatorsInDMChannel(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)
	user2Socket := newGatewayClient(t, client, user2.Token)
	t.Cleanup(user2Socket.Close)

	createFriendship(t, client, user1, user2)

	drainRelationshipEvents(t, user1Socket)
	drainRelationshipEvents(t, user2Socket)

	channel := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))
	waitForChannelEvent(t, user1Socket, "CHANNEL_CREATE", channel.ID)

	sendChannelMessage(t, client, user1.Token, parseSnowflake(t, channel.ID), "test")
	waitForChannelEvent(t, user2Socket, "CHANNEL_CREATE", channel.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/typing", parseSnowflake(t, channel.ID)), nil, user1.Token)
	if err != nil {
		t.Fatalf("failed to start typing: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForTypingEvent(t, user2Socket, channel.ID, user1.UserID)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/typing", parseSnowflake(t, channel.ID)), nil, user2.Token)
	if err != nil {
		t.Fatalf("failed to start typing for user2: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForTypingEvent(t, user1Socket, channel.ID, user2.UserID)
}
