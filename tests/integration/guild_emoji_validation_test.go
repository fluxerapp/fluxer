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

func TestGuildEmojiValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	guild := createGuild(t, client, owner.Token, "Emoji Validation Guild")
	guildID := parseSnowflake(t, guild.ID)

	t.Run("reject emoji with missing name", func(t *testing.T) {
		emojiImage := loadFixtureAsDataURL(t, "yeah.png", "image/png")
		payload := map[string]string{
			"image": emojiImage,
		}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("reject emoji with missing image", func(t *testing.T) {
		payload := map[string]string{
			"name": "testmoji",
		}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("reject emoji name too long", func(t *testing.T) {
		emojiImage := loadFixtureAsDataURL(t, "yeah.png", "image/png")
		payload := map[string]string{
			"name":  "verylongemojinamethatexceedsthemaximumlengthallowedbytheapi",
			"image": emojiImage,
		}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("reject invalid image data", func(t *testing.T) {
		payload := map[string]string{
			"name":  "testmoji",
			"image": "invalid-base64-data",
		}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("update nonexistent emoji returns 404", func(t *testing.T) {
		payload := map[string]string{"name": "newname"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis/999999999999999999", guildID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("delete nonexistent emoji returns 404", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/emojis/999999999999999999", guildID), owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})
}
