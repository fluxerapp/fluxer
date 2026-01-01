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

// TestGuildChannelPositionsCrossGuild ensures channel position updates cannot reference foreign channels
func TestGuildChannelPositionsCrossGuild(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	g1 := createGuild(t, client, owner.Token, "Position Guard Guild A")
	g1ID := parseSnowflake(t, g1.ID)
	g1Ch := createGuildChannel(t, client, owner.Token, g1ID, "a-one")

	g2 := createGuild(t, client, owner.Token, "Position Guard Guild B")
	g2ID := parseSnowflake(t, g2.ID)
	g2Ch := createGuildChannel(t, client, owner.Token, g2ID, "b-one")

	payload := []map[string]any{
		{"id": g1Ch.ID, "position": 1},
		{"id": g2Ch.ID, "position": 0},
	}

	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", g1ID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to send cross-guild positions payload: %v", err)
	}
	if resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected cross-guild channel id to be rejected")
	}
	resp.Body.Close()

	t.Run("reject positions without required fields", func(t *testing.T) {
		badPayload := []map[string]any{
			{"position": 0},
			{"id": g1Ch.ID, "position": "not-int"},
		}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", g1ID), badPayload, owner.Token)
		if err != nil {
			t.Fatalf("failed to send invalid positions payload: %v", err)
		}
		if resp.StatusCode == http.StatusNoContent {
			t.Fatalf("expected invalid position payload to fail validation")
		}
		resp.Body.Close()
	})
}
