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

func TestGroupDMNameUpdate(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)

	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)
	drainRelationshipEvents(t, user1Socket)

	groupDmChannel := createGroupDmChannel(t, client, user1.Token, user2.UserID, user3.UserID)
	waitForChannelEvent(t, user1Socket, "CHANNEL_CREATE", groupDmChannel.ID)

	updatePayload := map[string]any{
		"name": "Cool Group Chat",
	}
	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, groupDmChannel.ID)), updatePayload, user1.Token)
	if err != nil {
		t.Fatalf("failed to update group DM name: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var updatedChannel struct {
		ID   string  `json:"id"`
		Name *string `json:"name"`
	}
	decodeJSONResponse(t, resp, &updatedChannel)
	if updatedChannel.Name == nil || *updatedChannel.Name != "Cool Group Chat" {
		t.Fatalf("expected group DM name to be 'Cool Group Chat'")
	}
}
