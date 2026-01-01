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

func TestTypingIndicatorsWithMultipleUsers(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member1 := createTestAccount(t, client)
	member2 := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member1.Token)
	ensureSessionStarted(t, client, member2.Token)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)
	member1Socket := newGatewayClient(t, client, member1.Token)
	t.Cleanup(member1Socket.Close)
	member2Socket := newGatewayClient(t, client, member2.Token)
	t.Cleanup(member2Socket.Close)

	guild := createGuild(t, client, owner.Token, "Multi Typing Test Guild")
	waitForGuildEvent(t, ownerSocket, "GUILD_CREATE", guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", parseSnowflake(t, guild.ID)), map[string]any{
		"name": "multi-user-test",
		"type": 0,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel minimalChannelResponse
	decodeJSONResponse(t, resp, &channel)
	waitForChannelEvent(t, ownerSocket, "CHANNEL_CREATE", channel.ID)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, channel.ID))

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member1.Token)
	if err != nil {
		t.Fatalf("failed to accept invite for member1: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
	waitForGuildEvent(t, member1Socket, "GUILD_CREATE", guild.ID)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member2.Token)
	if err != nil {
		t.Fatalf("failed to accept invite for member2: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
	waitForGuildEvent(t, member2Socket, "GUILD_CREATE", guild.ID)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/typing", parseSnowflake(t, channel.ID)), nil, member1.Token)
	if err != nil {
		t.Fatalf("failed to start typing for member1: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForTypingEvent(t, ownerSocket, channel.ID, member1.UserID)
	waitForTypingEvent(t, member2Socket, channel.ID, member1.UserID)

	time.Sleep(100 * time.Millisecond)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/typing", parseSnowflake(t, channel.ID)), nil, member2.Token)
	if err != nil {
		t.Fatalf("failed to start typing for member2: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForTypingEvent(t, ownerSocket, channel.ID, member2.UserID)
	waitForTypingEvent(t, member1Socket, channel.ID, member2.UserID)
}
