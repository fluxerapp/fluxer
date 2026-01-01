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

type savedMessageEntryResponse struct {
	ID      string           `json:"id"`
	Status  string           `json:"status"`
	Message *messageResponse `json:"message"`
}

func TestSavedMessagesFilteredWhenAccessLost(t *testing.T) {
	client := newTestClient(t)

	guildOwner := registerTestUser(t, client, "filter-owner@example.com", "TestUncommonPw1!")
	user := registerTestUser(t, client, "filter-user@example.com", "TestUncommonPw1!")

	guild := createGuild(t, client, guildOwner.Token, "filter-saved")
	channelSnowflake := guild.SystemChannel
	channelID := parseSnowflake(t, channelSnowflake)

	invite := createChannelInvite(t, client, guildOwner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, user.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	message := sendChannelMessage(t, client, guildOwner.Token, channelID, "Filter saved message")

	payload := map[string]string{
		"channel_id": formatSnowflake(channelID),
		"message_id": formatSnowflake(parseSnowflake(t, message.ID)),
	}

	resp, err = client.postJSONWithAuth("/users/@me/saved-messages", payload, user.Token)
	if err != nil {
		t.Fatalf("failed to save message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/guilds/%d/members/%s", parseSnowflake(t, guild.ID), user.UserID), guildOwner.Token)
	if err != nil {
		t.Fatalf("failed to remove user from guild: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/saved-messages?limit=10", user.Token)
	if err != nil {
		t.Fatalf("failed to fetch saved messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var entries []savedMessageEntryResponse
	decodeJSONResponse(t, resp, &entries)
	resp.Body.Close()

	if len(entries) != 1 {
		t.Fatalf("expected one saved entry even after access loss, got %d entries", len(entries))
	}

	if entries[0].Status != "missing_permissions" {
		t.Fatalf("expected saved entry to be missing_permissions, got %q", entries[0].Status)
	}

	if entries[0].Message != nil {
		t.Fatalf("expected missing saved entry to have null message, got %+v", entries[0].Message)
	}
}
