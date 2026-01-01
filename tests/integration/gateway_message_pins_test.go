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
	"encoding/json"
	"fmt"
	"net/http"
	"testing"
	"time"
)

func TestGatewayMessagePins(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)
	memberSocket := newGatewayClient(t, client, member.Token)
	t.Cleanup(memberSocket.Close)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Pins Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)
	invite := createChannelInvite(t, client, owner.Token, channelID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	memberSocket.WaitForEvent(t, "GUILD_CREATE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == guild.ID
	})

	message := sendChannelMessage(t, client, owner.Token, channelID, "pin this message")

	resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/pins/%d", channelID, parseSnowflake(t, message.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to pin message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForPinsUpdate := func(socket *gatewayClient) {
		socket.WaitForEvent(t, "CHANNEL_PINS_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
			var payload struct {
				ChannelID string `json:"channel_id"`
			}
			if err := json.Unmarshal(raw, &payload); err != nil {
				t.Fatalf("failed to decode pins update: %v", err)
			}
			return payload.ChannelID == guild.SystemChannel
		})
	}

	waitForPinsUpdate(ownerSocket)
	waitForPinsUpdate(memberSocket)

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/pins", channelID), owner.Token)
	if err != nil {
		t.Fatalf("failed to get pins: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var pinsResponse struct {
		Items   []channelPinResponse `json:"items"`
		HasMore bool                 `json:"has_more"`
	}
	decodeJSONResponse(t, resp, &pinsResponse)
	if len(pinsResponse.Items) != 1 || pinsResponse.Items[0].Message.ID != message.ID {
		t.Fatalf("expected 1 pinned message")
	}

	resp, err = client.delete(fmt.Sprintf("/channels/%d/pins/%d", channelID, parseSnowflake(t, message.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to unpin message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForPinsUpdate(ownerSocket)
	waitForPinsUpdate(memberSocket)
}
