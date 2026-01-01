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

// TestGuildChannelPositionsOptionalPosition verifies missing position defaults place text above voice when moving into a category
func TestGuildChannelPositionsOptionalPosition(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Channel Position Defaults")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "cat",
		"type": 4,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create category: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var category minimalChannelResponse
	decodeJSONResponse(t, resp, &category)

	text := createGuildChannel(t, client, owner.Token, guildID, "text-chan")
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), map[string]any{
		"name": "voice-chan",
		"type": 2,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create voice channel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var voice minimalChannelResponse
	decodeJSONResponse(t, resp, &voice)

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":        voice.ID,
		"parent_id": category.ID,
		"position":  0,
	}}, owner.Token)
	if err != nil {
		t.Fatalf("failed to move voice into category: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), []map[string]any{{
		"id":        text.ID,
		"parent_id": category.ID,
	}}, owner.Token)
	if err != nil {
		t.Fatalf("failed to move text into category: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/channels", guildID), owner.Token)
	if err != nil {
		t.Fatalf("failed to fetch channels: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var channels []struct {
		ID       string  `json:"id"`
		ParentID *string `json:"parent_id"`
		Position int     `json:"position"`
		Type     int     `json:"type"`
	}
	decodeJSONResponse(t, resp, &channels)

	var textPos, voicePos int
	for _, ch := range channels {
		if ch.ID == text.ID {
			textPos = ch.Position
			if ch.ParentID == nil || *ch.ParentID != category.ID {
				t.Fatalf("text channel parent not updated")
			}
		}
		if ch.ID == voice.ID {
			voicePos = ch.Position
			if ch.ParentID == nil || *ch.ParentID != category.ID {
				t.Fatalf("voice channel parent not updated")
			}
		}
	}
	if textPos == 0 || voicePos == 0 {
		t.Fatalf("failed to locate moved channels in response")
	}
	if !(textPos < voicePos) {
		t.Fatalf("expected text channel to be placed before voice channel when position omitted (got %d vs %d)", textPos, voicePos)
	}
}
