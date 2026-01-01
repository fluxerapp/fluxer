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

func TestWebhook_StickerBypass(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Webhook Sticker Test Guild")
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

	webhookPayload := map[string]any{
		"name": "Sticker Test Webhook",
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

	t.Run("webhook messages typically do not support stickers", func(t *testing.T) {

		messagePayload := map[string]any{
			"content":     "Webhook sticker test",
			"sticker_ids": []string{"999999999999999999"},
		}

		executeResp, err := client.postJSON(
			fmt.Sprintf("/webhooks/%s/%s", webhook.ID, webhook.Token),
			messagePayload,
		)
		if err != nil {
			t.Fatalf("failed to execute webhook: %v", err)
		}
		defer executeResp.Body.Close()

		if executeResp.StatusCode != http.StatusOK &&
			executeResp.StatusCode != http.StatusNoContent &&
			executeResp.StatusCode != http.StatusBadRequest {
			t.Errorf("expected webhook with sticker_ids to either succeed (ignoring stickers) or return 400, got %d", executeResp.StatusCode)
		}
	})
}
