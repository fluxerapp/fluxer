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
	"strings"
	"testing"
)

func TestGatewayDirectMessageEvents(t *testing.T) {
	client := newTestClient(t)
	author := createTestAccount(t, client)
	recipient := createTestAccount(t, client)

	authorSocket := newGatewayClient(t, client, author.Token)
	t.Cleanup(authorSocket.Close)
	recipientSocket := newGatewayClient(t, client, recipient.Token)
	t.Cleanup(recipientSocket.Close)

	createFriendship(t, client, author, recipient)

	drainRelationshipEvents(t, authorSocket)
	drainRelationshipEvents(t, recipientSocket)

	channel := createDmChannel(t, client, author.Token, parseSnowflake(t, recipient.UserID))
	channelID := parseSnowflake(t, channel.ID)

	waitForChannelEvent(t, authorSocket, "CHANNEL_CREATE", channel.ID)

	message := sendChannelMessage(t, client, author.Token, channelID, "hello via gateway dm")
	waitForMessageEvent(t, authorSocket, "MESSAGE_CREATE", message.ID, func(content string) bool { return strings.Contains(content, "gateway dm") })

	waitForChannelEvent(t, recipientSocket, "CHANNEL_CREATE", channel.ID)
	waitForMessageEvent(t, recipientSocket, "MESSAGE_CREATE", message.ID, nil)

	updateURL := fmt.Sprintf("/channels/%d/messages/%d", channelID, parseSnowflake(t, message.ID))
	resp, err := client.patchJSONWithAuth(updateURL, map[string]string{"content": "edited gateway content"}, author.Token)
	if err != nil {
		t.Fatalf("failed to edit dm message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	waitForMessageEvent(t, authorSocket, "MESSAGE_UPDATE", message.ID, func(content string) bool { return content == "edited gateway content" })
	waitForMessageEvent(t, recipientSocket, "MESSAGE_UPDATE", message.ID, func(content string) bool { return content == "edited gateway content" })

	ackURL := fmt.Sprintf("/channels/%d/messages/%d/ack", channelID, parseSnowflake(t, message.ID))
	resp, err = client.postJSONWithAuth(ackURL, map[string]any{"mention_count": 0}, recipient.Token)
	if err != nil {
		t.Fatalf("failed to ack message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForAckEvent(t, recipientSocket, channel.ID, message.ID)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/typing", channelID), nil, recipient.Token)
	if err != nil {
		t.Fatalf("failed to start typing: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForTypingEvent(t, authorSocket, channel.ID, recipient.UserID)

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d", channelID, parseSnowflake(t, message.ID)), author.Token)
	if err != nil {
		t.Fatalf("failed to delete message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForMessageDelete(t, authorSocket, channel.ID, message.ID)
	waitForMessageDelete(t, recipientSocket, channel.ID, message.ID)
}
