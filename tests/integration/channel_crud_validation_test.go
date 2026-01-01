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

func TestChannelCRUDValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Channel CRUD Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	t.Run("reject getting nonexistent channel", func(t *testing.T) {
		resp, err := client.getWithAuth("/channels/999999999999999999", owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject updating nonexistent channel", func(t *testing.T) {
		payload := map[string]string{"name": "new-name"}
		resp, err := client.patchJSONWithAuth("/channels/999999999999999999", payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("reject deleting nonexistent channel", func(t *testing.T) {
		resp, err := client.delete("/channels/999999999999999999", owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("can get channel", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d", channelID), owner.Token)
		if err != nil {
			t.Fatalf("failed to get channel: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("can update channel name", func(t *testing.T) {
		payload := map[string]string{"name": fmt.Sprintf("updated-%d", time.Now().UnixNano())}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to update channel: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("can update channel topic", func(t *testing.T) {
		payload := map[string]string{"topic": "New topic"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to update channel: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	createPayload := map[string]any{
		"name": fmt.Sprintf("to-delete-%d", time.Now().UnixNano()),
		"type": 0,
	}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), createPayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var newChannel struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &newChannel)
	newChannelID := parseSnowflake(t, newChannel.ID)

	t.Run("can delete channel", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/channels/%d", newChannelID), owner.Token)
		if err != nil {
			t.Fatalf("failed to delete channel: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
