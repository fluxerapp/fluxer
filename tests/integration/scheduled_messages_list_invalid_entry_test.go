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
)

func TestScheduledMessagesListInvalidEntry(t *testing.T) {
	client := newTestClient(t)

	owner := registerTestUser(t, client, "invalid-list-owner@example.com", "TestUncommonPw1!")
	member := registerTestUser(t, client, "invalid-list-member@example.com", "TestUncommonPw1!")
	guild := createGuild(t, client, owner.Token, "scheduled-invalid")
	channel := createGuildChannel(t, client, owner.Token, parseSnowflake(t, guild.ID), "scheduled-invalid")
	channelID := parseSnowflake(t, channel.ID)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	content := "invalid scheduled"
	scheduled := scheduleMessage(t, client, channelID, member.Token, content)

	resp, err = client.delete(fmt.Sprintf("/guilds/%d/members/%s", parseSnowflake(t, guild.ID), member.UserID), owner.Token)
	if err != nil {
		t.Fatalf("failed to remove member: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	triggerScheduledMessageJob(t, client, member.UserID, scheduled.ID)

	resp, err = client.getWithAuth("/users/@me/scheduled-messages", member.Token)
	if err != nil {
		t.Fatalf("failed to fetch scheduled messages for invalid entry: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var list scheduledMessagesListResponse
	if err := json.NewDecoder(resp.Body).Decode(&list); err != nil {
		t.Fatalf("failed to decode scheduled message list: %v", err)
	}
	resp.Body.Close()

	found := false
	for _, entry := range list {
		if entry.ID == scheduled.ID {
			found = true
			if entry.Status != "invalid" {
				t.Fatalf("expected invalid status, got %s", entry.Status)
			}
			if entry.StatusReason == nil || *entry.StatusReason == "" {
				t.Fatalf("invalid entry missing status reason")
			}
			break
		}
	}

	if !found {
		t.Fatalf("expected invalid scheduled message %s in list", scheduled.ID)
	}
}
