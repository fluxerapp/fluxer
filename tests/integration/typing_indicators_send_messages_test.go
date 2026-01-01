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

func TestTypingIndicatorsRespectSendMessagesPermission(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Typing Permission Guild")

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", parseSnowflake(t, guild.ID)), map[string]any{
		"name": "no-send",
		"type": 0,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel minimalChannelResponse
	decodeJSONResponse(t, resp, &channel)
	resp.Body.Close()

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, channel.ID))
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	viewChannel := int64(1 << 10)
	sendMessages := int64(1 << 11)
	resp, err = client.requestJSON(http.MethodPut, fmt.Sprintf("/channels/%d/permissions/%s", parseSnowflake(t, channel.ID), member.UserID), map[string]any{
		"type":  1,
		"allow": fmt.Sprintf("%d", viewChannel),
		"deny":  fmt.Sprintf("%d", sendMessages),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to apply permission overwrite: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/typing", parseSnowflake(t, channel.ID)), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to send typing request: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected typing to be forbidden for user without send permissions, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
