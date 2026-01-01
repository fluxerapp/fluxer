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

func TestMessageForwardingPermissions(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)
	ensureSessionStarted(t, client, user1.Token)
	ensureSessionStarted(t, client, user2.Token)
	ensureSessionStarted(t, client, user3.Token)

	createFriendship(t, client, user1, user2)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)
	joinGuild(t, client, user3.Token, invite.Code)

	channel := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))
	originalMessage := sendChannelMessage(t, client, user1.Token, parseSnowflake(t, channel.ID), "Private message")

	t.Run("can forward message from inaccessible channel", func(t *testing.T) {
		user3Channel := createDmChannel(t, client, user3.Token, parseSnowflake(t, user1.UserID))

		forwardPayload := map[string]any{
			"message_reference": map[string]any{
				"message_id": originalMessage.ID,
				"channel_id": channel.ID,
				"type":       1,
			},
		}
		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", parseSnowflake(t, user3Channel.ID)),
			forwardPayload,
			user3.Token,
		)
		if err != nil {
			t.Fatalf("failed to send forward request: %v", err)
		}
		if resp.StatusCode != http.StatusOK {
			t.Fatalf("expected forward to succeed for user without access to source channel, got status %d", resp.StatusCode)
		}
		resp.Body.Close()
	})
}
