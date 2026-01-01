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

func TestTypingIndicatorsInGuildChannel(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)
	memberSocket := newGatewayClient(t, client, member.Token)
	t.Cleanup(memberSocket.Close)

	guild := createGuild(t, client, owner.Token, "Typing Test Guild")
	waitForGuildEvent(t, ownerSocket, "GUILD_CREATE", guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", parseSnowflake(t, guild.ID)), map[string]any{
		"name": "general",
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
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	waitForGuildEvent(t, memberSocket, "GUILD_CREATE", guild.ID)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/typing", parseSnowflake(t, channel.ID)), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to start typing: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForTypingEvent(t, ownerSocket, channel.ID, member.UserID)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/typing", parseSnowflake(t, channel.ID)), nil, owner.Token)
	if err != nil {
		t.Fatalf("failed to start typing for owner: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForTypingEvent(t, memberSocket, channel.ID, owner.UserID)
}
