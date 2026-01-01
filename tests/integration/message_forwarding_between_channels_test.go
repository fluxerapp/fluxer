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

func TestMessageForwardingBetweenChannels(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)
	ensureSessionStarted(t, client, user1.Token)
	ensureSessionStarted(t, client, user2.Token)
	ensureSessionStarted(t, client, user3.Token)

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)
	user2Socket := newGatewayClient(t, client, user2.Token)
	t.Cleanup(user2Socket.Close)

	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)

	drainRelationshipEvents(t, user1Socket)
	drainRelationshipEvents(t, user2Socket)

	channel1 := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))
	channel2 := createDmChannel(t, client, user1.Token, parseSnowflake(t, user3.UserID))

	waitForChannelEvent(t, user1Socket, "CHANNEL_CREATE", channel1.ID)
	waitForChannelEvent(t, user1Socket, "CHANNEL_CREATE", channel2.ID)

	originalMessage := sendChannelMessage(t, client, user1.Token, parseSnowflake(t, channel1.ID), "Original message to forward")
	waitForMessageEvent(t, user1Socket, "MESSAGE_CREATE", originalMessage.ID, nil)
	waitForMessageEvent(t, user2Socket, "MESSAGE_CREATE", originalMessage.ID, nil)

	forwardPayload := map[string]any{
		"message_reference": map[string]any{
			"message_id": originalMessage.ID,
			"channel_id": channel1.ID,
			"type":       1,
		},
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", parseSnowflake(t, channel2.ID)), forwardPayload, user1.Token)
	if err != nil {
		t.Fatalf("failed to forward message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var forwardedMessage messageResponse
	decodeJSONResponse(t, resp, &forwardedMessage)
	if forwardedMessage.ID == "" {
		t.Fatalf("forwarded message response missing id")
	}

	waitForMessageEvent(t, user1Socket, "MESSAGE_CREATE", forwardedMessage.ID, nil)

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d", parseSnowflake(t, channel2.ID), parseSnowflake(t, forwardedMessage.ID)), user1.Token)
	if err != nil {
		t.Fatalf("failed to get forwarded message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var fetchedMessage struct {
		ID               string `json:"id"`
		Content          string `json:"content"`
		MessageSnapshots []any  `json:"message_snapshots,omitempty"`
		MessageReference any    `json:"message_reference,omitempty"`
	}
	decodeJSONResponse(t, resp, &fetchedMessage)
	if len(fetchedMessage.MessageSnapshots) == 0 {
		t.Fatalf("expected forwarded message to have message_snapshots")
	}
}
