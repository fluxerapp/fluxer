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

func TestUserSettingsNotesAndGuildSettings(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	target := createTestAccount(t, client)

	resp, err := client.getWithAuth("/users/@me/settings", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch user settings: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var settings userSettingsResponse
	decodeJSONResponse(t, resp, &settings)

	patch := map[string]any{
		"status":                  "dnd",
		"inline_attachment_media": !settings.InlineAttachmentMedia,
		"gif_auto_play":           !settings.GifAutoPlay,
	}
	resp, err = client.patchJSONWithAuth("/users/@me/settings", patch, account.Token)
	if err != nil {
		t.Fatalf("failed to update user settings: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var updatedSettings userSettingsResponse
	decodeJSONResponse(t, resp, &updatedSettings)
	if updatedSettings.Status != "dnd" {
		t.Fatalf("expected status to update to dnd, got %s", updatedSettings.Status)
	}
	if updatedSettings.InlineAttachmentMedia != patch["inline_attachment_media"].(bool) {
		t.Fatalf("expected inline_attachment_media to update")
	}

	resp, err = client.getWithAuth("/users/@me/notes", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch notes: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var notes map[string]string
	decodeJSONResponse(t, resp, &notes)
	if len(notes) != 0 {
		t.Fatalf("expected no notes initially, got %d", len(notes))
	}

	noteText := "Target user is very helpful"
	resp, err = client.putJSONWithAuth(fmt.Sprintf("/users/@me/notes/%s", target.UserID), map[string]string{"note": noteText}, account.Token)
	if err != nil {
		t.Fatalf("failed to set user note: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/notes", account.Token)
	if err != nil {
		t.Fatalf("failed to refetch notes: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &notes)
	if notes[target.UserID] != noteText {
		t.Fatalf("expected notes map to contain target note")
	}

	resp, err = client.getWithAuth(fmt.Sprintf("/users/@me/notes/%s", target.UserID), account.Token)
	if err != nil {
		t.Fatalf("failed to fetch single note: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var noteResp userNoteResponse
	decodeJSONResponse(t, resp, &noteResp)
	if noteResp.Note != noteText {
		t.Fatalf("expected single note response to match set value")
	}

	guildName := fmt.Sprintf("Integration Guild %d", time.Now().UnixNano())
	guild := createGuild(t, client, account.Token, guildName)
	guildID := parseSnowflake(t, guild.ID)

	guildPatch := map[string]any{
		"suppress_everyone": true,
		"muted":             false,
	}
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/users/@me/guilds/%d/settings", guildID), guildPatch, account.Token)
	if err != nil {
		t.Fatalf("failed to update guild settings: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var guildSettings userGuildSettingsResponse
	decodeJSONResponse(t, resp, &guildSettings)
	if guildSettings.GuildID == nil || *guildSettings.GuildID != guild.ID {
		t.Fatalf("expected guild settings response for guild %s, got %v", guild.ID, guildSettings.GuildID)
	}
	if !guildSettings.SuppressEveryone {
		t.Fatalf("expected suppress_everyone flag to be true")
	}
}
