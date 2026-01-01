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

// TestMemberCannotManageGuild tests that regular members cannot perform admin actions
func TestMemberCannotManageGuild(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Member Perm Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d", guildID), map[string]string{"name": "Hacked Guild"}, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt guild update: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for member attempting guild update, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{"type": 0, "name": "hacker-channel"}, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt channel create: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for member attempting channel create, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{"name": "Admin", "permissions": "8"}, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt role create: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for member attempting role create, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/guilds/%d/members/%s", guildID, owner.UserID), member.Token)
	if err != nil {
		t.Fatalf("failed to attempt member kick: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for member attempting kick, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/guilds/%d/bans/%s", guildID, owner.UserID), map[string]any{"delete_message_seconds": 0}, member.Token)
	if err != nil {
		t.Fatalf("failed to attempt ban: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for member attempting ban, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
