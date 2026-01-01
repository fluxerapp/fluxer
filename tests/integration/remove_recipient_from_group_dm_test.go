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

func TestRemoveRecipientFromGroupDM(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)

	user1Socket := newGatewayClient(t, client, user1.Token)
	t.Cleanup(user1Socket.Close)
	user3Socket := newGatewayClient(t, client, user3.Token)
	t.Cleanup(user3Socket.Close)

	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)

	drainRelationshipEvents(t, user1Socket)
	drainRelationshipEvents(t, user3Socket)

	groupDmChannel := createGroupDmChannel(t, client, user1.Token, user2.UserID, user3.UserID)
	waitForChannelEvent(t, user3Socket, "CHANNEL_CREATE", groupDmChannel.ID)

	resp, err := client.delete(fmt.Sprintf("/channels/%d/recipients/%s", parseSnowflake(t, groupDmChannel.ID), user3.UserID), user1.Token)
	if err != nil {
		t.Fatalf("failed to remove recipient: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	waitForChannelEvent(t, user3Socket, "CHANNEL_DELETE", groupDmChannel.ID)

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, groupDmChannel.ID)), user3.Token)
	if err != nil {
		t.Fatalf("failed to get channel request: %v", err)
	}
	if resp.StatusCode != http.StatusNotFound && resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected user3 to no longer have access to channel, got status %d", resp.StatusCode)
	}
	resp.Body.Close()
}
