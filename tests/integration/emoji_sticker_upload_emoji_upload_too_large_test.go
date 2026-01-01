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

func TestEmojiUpload_TooLarge(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Emoji Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	payload := map[string]any{
		"name":  "large_emoji",
		"image": "data:image/png;base64," + getLargeBase64(EmojiMaxSize+10000),
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), payload, user.Token)
	if err != nil {
		t.Fatalf("failed to make request: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK {
		t.Error("expected request to fail for emoji over size limit")
	}
}
