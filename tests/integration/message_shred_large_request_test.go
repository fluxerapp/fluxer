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
	"net/http"
	"testing"
)

func TestMessageShredHandlesLargeRequest(t *testing.T) {
	client := newTestClient(t)

	target := createTestAccount(t, client)
	admin := createTestAccount(t, client)
	setUserACLs(t, client, admin.UserID, []string{"admin:authenticate", "message:shred"})

	guild := createGuild(t, client, target.Token, "Large Shred Guild")
	channel := createGuildChannel(t, client, target.Token, parseSnowflake(t, guild.ID), "large-shred-channel")
	channelID := parseSnowflake(t, channel.ID)

	invite := createChannelInvite(t, client, target.Token, channelID)
	joinGuild(t, client, admin.Token, invite.Code)

	var uniqueEntries []map[string]any
	for i := 0; i < 25; i++ {
		message := sendChannelMessage(t, client, target.Token, channelID, "shred me bulk")
		messageID := parseSnowflake(t, message.ID)
		uniqueEntries = append(uniqueEntries, map[string]any{
			"channel_id": formatSnowflake(channelID),
			"message_id": formatSnowflake(messageID),
		})
	}

	foreignMessage := sendChannelMessage(t, client, admin.Token, channelID, "admin message should stay")

	var entries []map[string]any
	for len(entries) < 5200 {
		for _, entry := range uniqueEntries {
			entries = append(entries, entry)
			if len(entries) >= 5200 {
				break
			}
		}
	}

	entries = append(entries, map[string]any{
		"channel_id": formatSnowflake(channelID),
		"message_id": formatSnowflake(parseSnowflake(t, foreignMessage.ID)),
	})

	queueResp := struct {
		Success   bool   `json:"success"`
		JobID     string `json:"job_id"`
		Requested int    `json:"requested"`
	}{}

	resp := adminPostJSON(t, client, admin.Token, "/admin/messages/shred", map[string]any{
		"user_id": formatSnowflake(parseSnowflake(t, target.UserID)),
		"entries": entries,
	})
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &queueResp)

	if !queueResp.Success {
		t.Fatalf("expected queue response to be successful for large input")
	}

	if queueResp.Requested != len(entries) {
		t.Fatalf("expected requested=%d, got %d", len(entries), queueResp.Requested)
	}

	status := waitForMessageShredJobCompletion(t, client, admin.Token, queueResp.JobID)
	if status.Status != "completed" {
		t.Fatalf("unexpected status %s for job %s", status.Status, queueResp.JobID)
	}

	if status.Total != len(uniqueEntries) || status.Processed != len(uniqueEntries) {
		t.Fatalf("expected %d unique entries processed, got total=%d processed=%d", len(uniqueEntries), status.Total, status.Processed)
	}

	expectedSkipped := len(entries) - len(uniqueEntries)
	if status.Skipped != expectedSkipped {
		t.Fatalf("expected skipped=%d for duplicates/foreign entries, got %d", expectedSkipped, status.Skipped)
	}
}
