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
	"strings"
	"testing"
)

func TestEmoji_MessageSanitization_Guild(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Emoji Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), user.Token)
	if err != nil {
		t.Fatalf("failed to get guild: %v", err)
	}
	var guildData struct {
		SystemChannelID string `json:"system_channel_id"`
	}
	decodeJSONResponse(t, resp, &guildData)
	resp.Body.Close()
	systemChannelID := parseSnowflake(t, guildData.SystemChannelID)

	t.Run("external emoji replaced for non-premium in guild", func(t *testing.T) {
		content := "Guild message <:external:999999999999999997>"

		msg := sendChannelMessage(t, client, user.Token, systemChannelID, content)

		fetchResp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%s", systemChannelID, msg.ID), user.Token)
		if err != nil {
			t.Fatalf("failed to fetch message: %v", err)
		}
		defer fetchResp.Body.Close()
		assertStatus(t, fetchResp, http.StatusOK)

		var result struct {
			Content string `json:"content"`
		}
		decodeJSONResponse(t, fetchResp, &result)

		if strings.Contains(result.Content, "<:external:") {
			t.Error("expected external emoji to be sanitized for non-premium user in guild")
		}
	})

	t.Run("multiple emojis sanitized individually", func(t *testing.T) {
		content := "Start <:one:111> middle <:two:222> end <:three:333>"

		msg := sendChannelMessage(t, client, user.Token, systemChannelID, content)

		fetchResp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%s", systemChannelID, msg.ID), user.Token)
		if err != nil {
			t.Fatalf("failed to fetch message: %v", err)
		}
		defer fetchResp.Body.Close()
		assertStatus(t, fetchResp, http.StatusOK)

		var result struct {
			Content string `json:"content"`
		}
		decodeJSONResponse(t, fetchResp, &result)

		t.Logf("Multi-emoji result: %s", result.Content)

		if strings.Contains(result.Content, "<:one:") ||
			strings.Contains(result.Content, "<:two:") ||
			strings.Contains(result.Content, "<:three:") {
			t.Error("expected all external emojis to be sanitized")
		}
	})
}
