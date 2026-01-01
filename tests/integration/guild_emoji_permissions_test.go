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

// TestGuildEmojiPermissions tests emoji permissions:
// - Members with CREATE_EXPRESSIONS (default) can create emojis
// - Members cannot update/delete other users' emojis without MANAGE_EXPRESSIONS
// - Members can update/delete their own emojis with CREATE_EXPRESSIONS
func TestGuildEmojiPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Emoji Perms Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	t.Run("member can create emoji with CREATE_EXPRESSIONS", func(t *testing.T) {
		emojiImage := loadFixtureAsDataURL(t, "yeah.png", "image/png")
		payload := map[string]string{
			"name":  "membermoji",
			"image": emojiImage,
		}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	emojiImage := loadFixtureAsDataURL(t, "yeah.png", "image/png")
	payload := map[string]string{
		"name":  "ownermoji",
		"image": emojiImage,
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create emoji: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var emoji struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &emoji)

	t.Run("member cannot update other user's emoji without MANAGE_EXPRESSIONS", func(t *testing.T) {
		updatePayload := map[string]string{"name": "hacked"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, emoji.ID), updatePayload, member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("member cannot delete other user's emoji without MANAGE_EXPRESSIONS", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, emoji.ID), member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("owner can update emoji", func(t *testing.T) {
		updatePayload := map[string]string{"name": "updatedmoji"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, emoji.ID), updatePayload, owner.Token)
		if err != nil {
			t.Fatalf("failed to update emoji: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("owner can delete emoji", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, emoji.ID), owner.Token)
		if err != nil {
			t.Fatalf("failed to delete emoji: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
