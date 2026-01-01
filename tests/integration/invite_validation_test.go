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

func TestInviteValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Invite Validation Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	t.Run("reject getting nonexistent invite", func(t *testing.T) {
		resp, err := client.getWithAuth("/invites/invalidcode123", owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject accepting nonexistent invite", func(t *testing.T) {
		resp, err := client.postJSONWithAuth("/invites/invalidcode123", nil, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject deleting nonexistent invite", func(t *testing.T) {
		resp, err := client.delete("/invites/invalidcode123", owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject invalid max_uses value", func(t *testing.T) {
		payload := map[string]any{"max_uses": -1}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/invites", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("reject invalid max_age value", func(t *testing.T) {
		payload := map[string]any{"max_age": -1}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/invites", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("accept valid max_uses and max_age", func(t *testing.T) {
		payload := map[string]any{
			"max_uses": 5,
			"max_age":  3600,
		}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/invites", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to create invite: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var invite struct {
			Code string `json:"code"`
		}
		decodeJSONResponse(t, resp, &invite)
		if invite.Code == "" {
			t.Error("expected invite code in response")
		}

		resp, err = client.delete(fmt.Sprintf("/invites/%s", invite.Code), owner.Token)
		if err == nil {
			resp.Body.Close()
		}
	})
}
