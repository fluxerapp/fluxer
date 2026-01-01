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

const invitePermsEveryoneMask = "137543274048" // keep the defined voice/text bits only so no unknown perms slip in.

func TestInvitePermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Invite Perms Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	everyoneRoleID := guild.ID
	resp, err := client.patchJSONWithAuth(
		fmt.Sprintf("/guilds/%d/roles/%s", guildID, everyoneRoleID),
		map[string]any{
			"permissions": invitePermsEveryoneMask,
		},
		owner.Token,
	)
	if err != nil {
		t.Fatalf("failed to update role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	t.Run("member cannot create invite without CREATE_INSTANT_INVITE", func(t *testing.T) {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/invites", channelID), map[string]any{}, member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	ownerInvite := createChannelInvite(t, client, owner.Token, channelID)

	t.Run("member cannot delete invite without MANAGE_CHANNELS", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/invites/%s", ownerInvite.Code), member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("member cannot get channel invites without MANAGE_CHANNELS", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/invites", channelID), member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("member cannot get guild invites without MANAGE_GUILD", func(t *testing.T) {
		guildID := parseSnowflake(t, guild.ID)
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d/invites", guildID), member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("owner can delete invite", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/invites/%s", ownerInvite.Code), owner.Token)
		if err != nil {
			t.Fatalf("failed to delete invite: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
