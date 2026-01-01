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

func TestGuildCRUDPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	nonmember := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)
	ensureSessionStarted(t, client, nonmember.Token)

	guild := createGuild(t, client, owner.Token, "Perms Test Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	t.Run("member cannot update guild without MANAGE_GUILD", func(t *testing.T) {
		payload := map[string]string{"name": "Hacked Guild"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d", guildID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("nonmember cannot get guild", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), nonmember.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("member can get guild", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), member.Token)
		if err != nil {
			t.Fatalf("failed to get guild: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("member can leave guild", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/users/@me/guilds/%d", guildID), member.Token)
		if err != nil {
			t.Fatalf("failed to leave guild: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("owner cannot leave guild without deleting", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/users/@me/guilds/%d", guildID), owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})
}
