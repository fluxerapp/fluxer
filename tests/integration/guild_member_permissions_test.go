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

func TestGuildMemberPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	otherMember := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)
	ensureSessionStarted(t, client, otherMember.Token)

	guild := createGuild(t, client, owner.Token, "Member Perms Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, otherMember.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	t.Run("member can get member list", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d/members", guildID), member.Token)
		if err != nil {
			t.Fatalf("failed to get members: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("member can get specific member", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d/members/%s", guildID, owner.UserID), member.Token)
		if err != nil {
			t.Fatalf("failed to get member: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("member can update own nickname", func(t *testing.T) {
		payload := map[string]string{"nick": "Member Nick"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/members/@me", guildID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to update self: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("member cannot update other member without MANAGE_NICKNAMES", func(t *testing.T) {
		payload := map[string]string{"nick": "Hacked"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/members/%s", guildID, otherMember.UserID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("member cannot kick without KICK_MEMBERS", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/members/%s", guildID, otherMember.UserID), member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("owner can update member nickname", func(t *testing.T) {
		payload := map[string]string{"nick": "Owner Set Nick"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to update member: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("owner can kick member", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/members/%s", guildID, member.UserID), owner.Token)
		if err != nil {
			t.Fatalf("failed to kick member: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
