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
	"encoding/json"
	"fmt"
	"net/http"
	"testing"
	"time"
)

func TestGatewayUserSettingsNotifications(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	target := createTestAccount(t, client)

	socket := newGatewayClient(t, client, account.Token)
	t.Cleanup(socket.Close)

	resp, err := client.getWithAuth("/users/@me/settings", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch settings: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var current userSettingsResponse
	decodeJSONResponse(t, resp, &current)

	settingsPatch := map[string]any{
		"status":                  "idle",
		"inline_attachment_media": !current.InlineAttachmentMedia,
		"gif_auto_play":           !current.GifAutoPlay,
	}
	resp, err = client.patchJSONWithAuth("/users/@me/settings", settingsPatch, account.Token)
	if err != nil {
		t.Fatalf("failed to patch settings: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	socket.WaitForEvent(t, "USER_SETTINGS_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			Status string `json:"status"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode settings payload: %v", err)
		}
		return payload.Status == "idle"
	})

	note := "Gateway powered notes"
	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/notes/%s", target.UserID), map[string]string{"note": note}, account.Token)
	if err != nil {
		t.Fatalf("failed to set note: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	socket.WaitForEvent(t, "USER_NOTE_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID   string `json:"id"`
			Note string `json:"note"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode note payload: %v", err)
		}
		return payload.ID == target.UserID && payload.Note == note
	})

	guild := createGuild(t, client, account.Token, fmt.Sprintf("Settings Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/users/@me/guilds/%d/settings", guildID), map[string]any{
		"suppress_everyone": true,
		"muted":             false,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to patch guild settings: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	socket.WaitForEvent(t, "USER_GUILD_SETTINGS_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			GuildID *string `json:"guild_id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode guild settings payload: %v", err)
		}
		return payload.GuildID != nil && *payload.GuildID == guild.ID
	})

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", target.UserID), map[string]int{"type": 1}, account.Token)
	if err != nil {
		t.Fatalf("failed to send friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", account.UserID), map[string]int{"type": 1}, target.Token)
	if err != nil {
		t.Fatalf("failed to accept friend request: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	dm := createDmChannel(t, client, account.Token, parseSnowflake(t, target.UserID))
	dmID := parseSnowflake(t, dm.ID)

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/channels/%d/pin", dmID), nil, account.Token)
	if err != nil {
		t.Fatalf("failed to pin dm: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	socket.WaitForEvent(t, "USER_PINNED_DMS_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload []string
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode pinned dm payload: %v", err)
		}
		return len(payload) == 1 && payload[0] == dm.ID
	})

	resp, err = client.delete(fmt.Sprintf("/users/@me/channels/%d/pin", dmID), account.Token)
	if err != nil {
		t.Fatalf("failed to unpin dm: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	socket.WaitForEvent(t, "USER_PINNED_DMS_UPDATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload []string
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode pinned dm payload: %v", err)
		}
		return len(payload) == 0
	})
}
