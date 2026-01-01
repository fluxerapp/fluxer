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
	"time"
)

// TestEmojiPermissions tests that members with CREATE_EXPRESSIONS can create emoji
// but cannot edit/delete emojis created by other users without MANAGE_EXPRESSIONS
func TestEmojiPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Emoji Perm Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	emojiImage := "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), map[string]string{
		"name":  "membermoji",
		"image": emojiImage,
	}, member.Token)
	if err != nil {
		t.Fatalf("failed to create emoji: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), map[string]string{
		"name":  "ownermoji",
		"image": emojiImage,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create emoji: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var emoji struct {
		ID   string `json:"id"`
		Name string `json:"name"`
	}
	decodeJSONResponse(t, resp, &emoji)

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), member.Token)
	if err != nil {
		t.Fatalf("failed to list emojis: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, emoji.ID), map[string]string{
		"name": "hackedmoji",
	}, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt emoji update: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for emoji update on other user's emoji, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, emoji.ID), member.Token)
	if err != nil {
		t.Fatalf("failed to attempt emoji delete: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for emoji delete on other user's emoji, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
