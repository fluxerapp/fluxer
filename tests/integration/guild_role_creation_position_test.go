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

func TestGuildRoleCreationPositionAlwaysOne(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	guild := createGuild(t, client, owner.Token, "Role Position One Test")
	guildID := parseSnowflake(t, guild.ID)

	createRole := func(name string) (string, int) {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{"name": name}, owner.Token)
		if err != nil {
			t.Fatalf("failed to create role %s: %v", name, err)
		}
		assertStatus(t, resp, http.StatusOK)
		var role struct {
			ID       string `json:"id"`
			Position int    `json:"position"`
		}
		decodeJSONResponse(t, resp, &role)
		return role.ID, role.Position
	}

	createdRoles := []struct {
		name     string
		id       string
		position int
	}{
		{name: "First Role"},
		{name: "Second Role"},
	}

	for idx := range createdRoles {
		id, position := createRole(createdRoles[idx].name)
		createdRoles[idx].id = id
		createdRoles[idx].position = position
		if position != 1 {
			t.Fatalf("expected new role position to be 1, got %d", position)
		}
	}

	resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), owner.Token)
	if err != nil {
		t.Fatalf("failed to fetch guild: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var guildResp struct {
		Roles []struct {
			ID       string `json:"id"`
			Position int    `json:"position"`
		} `json:"roles"`
	}
	decodeJSONResponse(t, resp, &guildResp)

	for _, created := range createdRoles {
		found := false
		for _, role := range guildResp.Roles {
			if role.ID == created.id {
				found = true
				if role.Position != 1 {
					t.Fatalf("expected guild role %s to have position 1, got %d", created.id, role.Position)
				}
				break
			}
		}
		if !found {
			t.Fatalf("created role %s not found in guild response", created.id)
		}
	}
}
