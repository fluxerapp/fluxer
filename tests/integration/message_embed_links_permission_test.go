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

func TestMessageEmbedLinksPermission(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Embed Permission Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	denyEmbed := fmt.Sprintf("%d", 1<<14)
	allowSend := fmt.Sprintf("%d", 1<<11)
	resp, err = client.requestJSON(http.MethodPut, fmt.Sprintf("/channels/%d/permissions/%d", channelID, guildID), map[string]any{
		"type":  0,
		"allow": allowSend,
		"deny":  denyEmbed,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to update channel permissions: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	t.Run("send embed without permission", func(t *testing.T) {
		payload := map[string]any{
			"content": "Embed attempt",
			"embeds": []map[string]any{
				{"title": "Denied", "url": "https://example.com"},
			},
		}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to send message: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("edit message to add embed without permission", func(t *testing.T) {
		msg := sendChannelMessage(t, client, member.Token, channelID, "plain text")
		msgID := parseSnowflake(t, msg.ID)

		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, msgID), map[string]any{
			"embeds": []map[string]any{
				{"title": "Edited", "url": "https://example.com"},
			},
		}, member.Token)
		if err != nil {
			t.Fatalf("failed to edit message: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})
}
