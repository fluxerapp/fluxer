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

func TestBulkDeleteMessagesPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Permissions Test Guild")

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

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, channel.ID))
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	var messageIDs []string
	for i := 0; i < 3; i++ {
		msg := sendChannelMessage(t, client, owner.Token, parseSnowflake(t, channel.ID), fmt.Sprintf("Owner message %d", i+1))
		messageIDs = append(messageIDs, msg.ID)
	}

	bulkDeletePayload := map[string]any{
		"message_ids": messageIDs,
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/bulk-delete", parseSnowflake(t, channel.ID)), bulkDeletePayload, member.Token)
	if err != nil {
		t.Fatalf("failed to send bulk delete request: %v", err)
	}
	if resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected bulk delete to fail for member without MANAGE_MESSAGES permission")
	}
	resp.Body.Close()
}
