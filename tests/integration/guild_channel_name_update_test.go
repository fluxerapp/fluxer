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

func TestGuildChannelNameUpdate(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)

	guild := createGuild(t, client, owner.Token, "Update Test Guild")
	waitForGuildEvent(t, ownerSocket, "GUILD_CREATE", guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", parseSnowflake(t, guild.ID)), map[string]any{
		"name": "original-name",
		"type": 0,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channel minimalChannelResponse
	decodeJSONResponse(t, resp, &channel)
	waitForChannelEvent(t, ownerSocket, "CHANNEL_CREATE", channel.ID)

	updatePayload := map[string]any{
		"name": "updated-channel-name",
		"type": 0,
	}
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, channel.ID)), updatePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to update channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var updatedChannel struct {
		ID   string `json:"id"`
		Name string `json:"name"`
	}
	decodeJSONResponse(t, resp, &updatedChannel)
	if updatedChannel.Name != "updated-channel-name" {
		t.Fatalf("expected channel name to be 'updated-channel-name', got %s", updatedChannel.Name)
	}

	waitForChannelEvent(t, ownerSocket, "CHANNEL_UPDATE", channel.ID)
}
