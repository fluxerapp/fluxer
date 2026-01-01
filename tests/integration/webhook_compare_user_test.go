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

func TestWebhook_CompareToRegularUser(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Comparison Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	emoji := createGuildEmoji(t, client, user.Token, guildID, "compare")

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

	webhookPayload := map[string]any{
		"name": "Comparison Webhook",
	}

	webhookResp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/webhooks", systemChannelID),
		webhookPayload,
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to create webhook: %v", err)
	}
	defer webhookResp.Body.Close()
	assertStatus(t, webhookResp, http.StatusOK)

	var webhook struct {
		ID    string `json:"id"`
		Token string `json:"token"`
	}
	decodeJSONResponse(t, webhookResp, &webhook)

	emojiContent := fmt.Sprintf("Test <:compare:%s>", emoji.ID)

	t.Run("regular user emoji is sanitized", func(t *testing.T) {
		msg := sendChannelMessage(t, client, user.Token, systemChannelID, emojiContent)

		fetchResp, err := client.getWithAuth(
			fmt.Sprintf("/channels/%d/messages/%s", systemChannelID, msg.ID),
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to fetch message: %v", err)
		}
		defer fetchResp.Body.Close()

		var result struct {
			Content string `json:"content"`
		}
		decodeJSONResponse(t, fetchResp, &result)

		if strings.Contains(result.Content, "<:compare:") {
			t.Errorf("expected regular user emoji to be sanitized, but got: %s", result.Content)
		}
	})

	t.Run("webhook emoji is NOT sanitized", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": emojiContent,
		}

		executeResp, err := client.postJSON(
			fmt.Sprintf("/webhooks/%s/%s?wait=true", webhook.ID, webhook.Token),
			messagePayload,
		)
		if err != nil {
			t.Fatalf("failed to execute webhook: %v", err)
		}
		defer executeResp.Body.Close()

		assertStatus(t, executeResp, http.StatusOK)

		var result struct {
			Content string `json:"content"`
		}
		decodeJSONResponse(t, executeResp, &result)

		if !strings.Contains(result.Content, "<:compare:") {
			t.Errorf("expected webhook emoji to NOT be sanitized, but got: %s", result.Content)
		}
	})
}
