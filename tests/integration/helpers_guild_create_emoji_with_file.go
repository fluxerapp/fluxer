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

func createGuildEmojiWithFile(t testing.TB, client *testClient, token string, guildID int64, name, filename, mimeType string) emojiResponse {
	t.Helper()

	image := loadFixtureAsDataURL(t, filename, mimeType)

	payload := map[string]any{
		"name":  name,
		"image": image,
	}

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/guilds/%d/emojis", guildID),
		payload,
		token,
	)
	if err != nil {
		t.Fatalf("failed to create emoji: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusCreated && resp.StatusCode != http.StatusOK {
		t.Fatalf("expected status 200 or 201, got %d", resp.StatusCode)
	}

	var emoji emojiResponse
	decodeJSONResponse(t, resp, &emoji)
	return emoji
}
