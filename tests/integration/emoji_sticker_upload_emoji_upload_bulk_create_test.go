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

func TestEmojiUpload_BulkCreate(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Emoji Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	emojis := make([]map[string]any, 5)
	for i := 0; i < 5; i++ {
		emojis[i] = map[string]any{
			"name":  fmt.Sprintf("bulk_emoji_%d", i+1),
			"image": "data:image/png;base64," + getValidPNGBase64(),
		}
	}

	payload := map[string]any{
		"emojis": emojis,
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis/bulk", guildID), payload, user.Token)
	if err != nil {
		t.Fatalf("failed to bulk create emojis: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)
}
