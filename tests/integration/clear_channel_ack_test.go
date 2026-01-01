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

func TestClearChannelAck(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	ensureSessionStarted(t, client, user1.Token)
	ensureSessionStarted(t, client, user2.Token)

	createFriendship(t, client, user1, user2)
	channel := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	message := sendChannelMessage(t, client, user1.Token, parseSnowflake(t, channel.ID), "Test message")

	ackPayload := map[string]any{
		"mention_count": 0,
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d/ack", parseSnowflake(t, channel.ID), parseSnowflake(t, message.ID)), ackPayload, user2.Token)
	if err != nil {
		t.Fatalf("failed to ack message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/ack", parseSnowflake(t, channel.ID)), user2.Token)
	if err != nil {
		t.Fatalf("failed to clear ack: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()
}
