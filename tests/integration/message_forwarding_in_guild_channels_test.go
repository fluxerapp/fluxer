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

func TestMessageForwardingInGuildChannels(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)

	guild := createGuild(t, client, owner.Token, "Forward Test Guild")

	waitForGuildEvent(t, ownerSocket, "GUILD_CREATE", guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", parseSnowflake(t, guild.ID)), map[string]any{
		"name": "channel-one",
		"type": 0,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create first channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel1 minimalChannelResponse
	decodeJSONResponse(t, resp, &channel1)

	waitForChannelEvent(t, ownerSocket, "CHANNEL_CREATE", channel1.ID)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", parseSnowflake(t, guild.ID)), map[string]any{
		"name": "channel-two",
		"type": 0,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create second channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel2 minimalChannelResponse
	decodeJSONResponse(t, resp, &channel2)

	waitForChannelEvent(t, ownerSocket, "CHANNEL_CREATE", channel2.ID)

	originalMessage := sendChannelMessage(t, client, owner.Token, parseSnowflake(t, channel1.ID), "Guild message to forward")
	waitForMessageEvent(t, ownerSocket, "MESSAGE_CREATE", originalMessage.ID, nil)

	forwardPayload := map[string]any{
		"message_reference": map[string]any{
			"message_id": originalMessage.ID,
			"channel_id": channel1.ID,
			"guild_id":   guild.ID,
			"type":       1,
		},
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", parseSnowflake(t, channel2.ID)), forwardPayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to forward guild message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var forwardedMessage messageResponse
	decodeJSONResponse(t, resp, &forwardedMessage)

	waitForMessageEvent(t, ownerSocket, "MESSAGE_CREATE", forwardedMessage.ID, nil)

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d", parseSnowflake(t, channel2.ID), parseSnowflake(t, forwardedMessage.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to get forwarded guild message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var fetchedMessage struct {
		ID               string `json:"id"`
		Content          string `json:"content"`
		MessageSnapshots []any  `json:"message_snapshots,omitempty"`
	}
	decodeJSONResponse(t, resp, &fetchedMessage)
	if len(fetchedMessage.MessageSnapshots) == 0 {
		t.Fatalf("expected forwarded guild message to have message_snapshots")
	}
}
