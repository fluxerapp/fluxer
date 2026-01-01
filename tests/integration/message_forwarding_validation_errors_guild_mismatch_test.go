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

func TestMessageForwardingRejectsMismatchedGuildID(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guildA := createGuild(t, client, user.Token, "Guild A")
	guildB := createGuild(t, client, user.Token, "Guild B")

	targetChannel := createGuildChannel(t, client, user.Token, parseSnowflake(t, guildA.ID), "forward-target")

	originalMessage := sendChannelMessage(
		t,
		client,
		user.Token,
		parseSnowflake(t, guildA.SystemChannel),
		"Source message",
	)

	payload := map[string]any{
		"message_reference": map[string]any{
			"channel_id": guildA.SystemChannel,
			"message_id": originalMessage.ID,
			"guild_id":   guildB.ID,
			"type":       1,
		},
	}

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages", parseSnowflake(t, targetChannel.ID)),
		payload,
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to send request: %v", err)
	}
	assertStatus(t, resp, http.StatusBadRequest)
	resp.Body.Close()
}
