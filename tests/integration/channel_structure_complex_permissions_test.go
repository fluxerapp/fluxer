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

// TestChannelStructureComplexPermissions builds a large structure and exercises moves and access constraints
func TestChannelStructureComplexPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	moderator := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, moderator.Token)

	guild := createGuild(t, client, owner.Token, "Complex Channel Structure")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "Moderator",
		"permissions": fmt.Sprintf("%d", 1<<4),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create moderator role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var modRole struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &modRole)
	resp.Body.Close()

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, guild.SystemChannel))
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, moderator.Token)
	if err != nil {
		t.Fatalf("failed to invite moderator: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.putWithAuth(fmt.Sprintf("/guilds/%d/members/%s/roles/%s", guildID, moderator.UserID, modRole.ID), owner.Token)
	if err != nil {
		t.Fatalf("failed to assign moderator role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	catPublic := createGuildCategory(t, client, owner.Token, guildID, "public-cat")
	catSecret := createGuildCategory(t, client, owner.Token, guildID, "secret-cat")
	textRoot := createGuildChannel(t, client, owner.Token, guildID, "root-text")
	createGuildVoiceChannel(t, client, owner.Token, guildID, "root-voice")
	textPublic := createGuildChannel(t, client, owner.Token, guildID, "public-text")
	voicePublic := createGuildVoiceChannel(t, client, owner.Token, guildID, "public-voice")

	viewChannel := 1 << 10
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/channels/%d", parseSnowflake(t, catSecret.ID)), map[string]any{
		"type": 4,
		"permission_overwrites": []map[string]any{
			{"id": modRole.ID, "type": 0, "deny": fmt.Sprintf("%d", viewChannel)},
		},
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to set secret perms: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":        textRoot.ID,
		"parent_id": catSecret.ID,
	}}, moderator.Token)
	if err != nil {
		t.Fatalf("moderator move attempt failed: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 when moving into inaccessible category, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":        textPublic.ID,
		"parent_id": catPublic.ID,
	}}, moderator.Token)
	if err != nil {
		t.Fatalf("moderator move into public failed: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":        voicePublic.ID,
		"parent_id": catPublic.ID,
		"position":  5,
	}}, moderator.Token)
	if err != nil {
		t.Fatalf("moderator move voice failed: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	secretChild := createGuildChannel(t, client, owner.Token, guildID, "secret-text")
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":        secretChild.ID,
		"parent_id": catSecret.ID,
	}}, owner.Token)
	if err != nil {
		t.Fatalf("owner move into secret failed: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()
}
