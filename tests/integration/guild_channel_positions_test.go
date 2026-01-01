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

func TestGuildChannelPositions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Position Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	ch1Payload := map[string]any{"name": "channel-1", "type": 0}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), ch1Payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel 1: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var ch1 struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &ch1)

	ch2Payload := map[string]any{"name": "channel-2", "type": 0}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), ch2Payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel 2: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var ch2 struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &ch2)

	t.Run("owner can update channel positions", func(t *testing.T) {
		positions := []map[string]any{
			{"id": ch1.ID, "position": 1},
			{"id": ch2.ID, "position": 0},
		}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), positions, owner.Token)
		if err != nil {
			t.Fatalf("failed to update positions: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
