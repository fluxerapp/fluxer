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

func TestScheduledMessagesListLifecycle(t *testing.T) {
	client := newTestClient(t)

	owner := registerTestUser(t, client, "list-owner@example.com", "TestUncommonPw1!")
	guild := createGuild(t, client, owner.Token, "scheduled-list")
	channel := createGuildChannel(t, client, owner.Token, parseSnowflake(t, guild.ID), "scheduled-list")
	channelID := parseSnowflake(t, channel.ID)

	content := "list scheduled"
	scheduled := scheduleMessage(t, client, channelID, owner.Token, content)

	{
		resp, err := client.getWithAuth("/users/@me/scheduled-messages", owner.Token)
		if err != nil {
			t.Fatalf("failed to fetch scheduled messages: %v", err)
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
				break
			}
		}

		if !found {
			t.Fatalf("scheduled message %s missing from list", scheduled.ID)
		}
	}

	{
		resp, err := client.delete(fmt.Sprintf("/users/@me/scheduled-messages/%s", scheduled.ID), owner.Token)
		if err != nil {
			t.Fatalf("failed to cancel scheduled message: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	}

	{
		resp, err := client.getWithAuth("/users/@me/scheduled-messages", owner.Token)
		if err != nil {
			t.Fatalf("failed to fetch scheduled messages after cancel: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var list scheduledMessagesListResponse
		if err := json.NewDecoder(resp.Body).Decode(&list); err != nil {
			t.Fatalf("failed to decode scheduled message list: %v", err)
		}
		resp.Body.Close()

		for _, entry := range list {
			if entry.ID == scheduled.ID {
				t.Fatalf("expected scheduled message %s removed after cancel", scheduled.ID)
			}
		}
	}
}
