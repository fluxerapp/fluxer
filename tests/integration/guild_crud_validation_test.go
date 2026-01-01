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

func TestGuildCRUDValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	t.Run("reject getting nonexistent guild", func(t *testing.T) {
		resp, err := client.getWithAuth("/guilds/999999999999999999", owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject updating nonexistent guild", func(t *testing.T) {
		payload := map[string]string{"name": "New Name"}
		resp, err := client.patchJSONWithAuth("/guilds/999999999999999999", payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject leaving nonexistent guild", func(t *testing.T) {
		resp, err := client.delete("/users/@me/guilds/999999999999999999", owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("CRUD Test Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)

	t.Run("owner can get guild", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), owner.Token)
		if err != nil {
			t.Fatalf("failed to get guild: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("owner can update guild", func(t *testing.T) {
		payload := map[string]string{"name": fmt.Sprintf("Updated Guild %d", time.Now().UnixNano())}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d", guildID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to update guild: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("get user guilds returns owned guilds", func(t *testing.T) {
		resp, err := client.getWithAuth("/users/@me/guilds", owner.Token)
		if err != nil {
			t.Fatalf("failed to get user guilds: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var guilds []struct {
			ID string `json:"id"`
		}
		decodeJSONResponse(t, resp, &guilds)
		if len(guilds) == 0 {
			t.Error("expected at least one guild")
		}
	})
}
