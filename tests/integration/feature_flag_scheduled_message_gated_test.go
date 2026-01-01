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

func TestScheduledMessageFeatureFlagGating(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guild := createGuild(t, client, owner.Token, "scheduled-flag")
	channel := createGuildChannel(t, client, owner.Token, parseSnowflake(t, guild.ID), "scheduled-channel")
	channelID := parseSnowflake(t, channel.ID)

	payload := map[string]any{
		"content":            "trying to schedule",
		"scheduled_local_at": time.Now().UTC().Add(1 * time.Minute).Format(time.RFC3339),
		"timezone":           "UTC",
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/schedule", channelID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to schedule message before feature enabled: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusForbidden)

	adminToken := featureFlagAdminToken(t, client, []string{"feature_flag:manage"})
	updateFeatureFlagGuilds(t, client, adminToken, "message_scheduling", []string{guild.ID})

	scheduled := scheduleMessage(t, client, channelID, owner.Token, "enabled now")
	if scheduled.ID == "" {
		t.Fatalf("expected scheduled message to be created")
	}
}
