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

func TestGuildMemberValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Member Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	t.Run("reject getting nonexistent member", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d/members/999999999999999999", guildID), owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject updating nonexistent member", func(t *testing.T) {
		payload := map[string]string{"nick": "Test"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/members/999999999999999999", guildID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject removing nonexistent member", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/members/999999999999999999", guildID), owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("owner can get member list", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d/members", guildID), owner.Token)
		if err != nil {
			t.Fatalf("failed to get members: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var members []struct {
			User struct {
				ID string `json:"id"`
			} `json:"user"`
		}
		decodeJSONResponse(t, resp, &members)
		if len(members) == 0 {
			t.Error("expected at least owner in member list")
		}
	})

	t.Run("owner can get self as member", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d/members/@me", guildID), owner.Token)
		if err != nil {
			t.Fatalf("failed to get self: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("owner can update self nickname", func(t *testing.T) {
		payload := map[string]string{"nick": "Owner Nick"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/members/@me", guildID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to update self: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})
}
