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

// TestGuildChannelPositionsPermissionsAndLock covers permission gates, lock_permissions, and ordering defaults
func TestGuildChannelPositionsPermissionsAndLock(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Channel Position Perms")
	guildID := parseSnowflake(t, guild.ID)

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, guild.SystemChannel))
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	catA := createGuildCategory(t, client, owner.Token, guildID, "cat-a")
	catB := createGuildCategory(t, client, owner.Token, guildID, "cat-b")
	text1 := createGuildChannel(t, client, owner.Token, guildID, "text-one")
	voice1 := createGuildVoiceChannel(t, client, owner.Token, guildID, "voice-one")

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":        text1.ID,
		"parent_id": catB.ID,
	}}, member.Token)
	if err != nil {
		t.Fatalf("member move attempt failed: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for member move, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":        voice1.ID,
		"parent_id": catA.ID,
	}}, owner.Token)
	if err != nil {
		t.Fatalf("owner move voice failed: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":               text1.ID,
		"parent_id":        catA.ID,
		"lock_permissions": true,
		"position":         0,
	}}, owner.Token)
	if err != nil {
		t.Fatalf("owner move text failed: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), owner.Token)
	if err != nil {
		t.Fatalf("failed to fetch channels: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channels []struct {
		ID       string  `json:"id"`
		ParentID *string `json:"parent_id"`
		Position int     `json:"position"`
		Type     int     `json:"type"`
	}
	decodeJSONResponse(t, resp, &channels)
	resp.Body.Close()

	var textPos, voicePos int
	for _, ch := range channels {
		if ch.ParentID == nil || *ch.ParentID != catA.ID {
			continue
		}
		if ch.ID == text1.ID {
			textPos = ch.Position
		}
		if ch.ID == voice1.ID {
			voicePos = ch.Position
		}
	}
	if textPos == 0 || voicePos == 0 {
		t.Fatalf("did not find both channels under category")
	}
	if !(textPos < voicePos) {
		t.Fatalf("expected text before voice in category (positions %d, %d)", textPos, voicePos)
	}

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":        text1.ID,
		"parent_id": "999999999999999999",
	}}, owner.Token)
	if err != nil {
		t.Fatalf("invalid parent request failed: %v", err)
	}
	if resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected invalid parent move to be rejected")
	}
	resp.Body.Close()
}
