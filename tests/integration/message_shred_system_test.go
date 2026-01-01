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

func TestMessageShredWorkflow(t *testing.T) {
	client := newTestClient(t)

	target := createTestAccount(t, client)
	admin := createTestAccount(t, client)
	setUserACLs(t, client, admin.UserID, []string{"admin:authenticate", "message:shred"})

	guild := createGuild(t, client, target.Token, "Message Shred Guild")
	channel := createGuildChannel(t, client, target.Token, parseSnowflake(t, guild.ID), "shred-channel")
	channelID := parseSnowflake(t, channel.ID)

	invite := createChannelInvite(t, client, target.Token, channelID)
	joinGuild(t, client, admin.Token, invite.Code)

	firstMessage := sendChannelMessage(t, client, target.Token, channelID, "shred me 1")
	secondMessage := sendChannelMessage(t, client, target.Token, channelID, "shred me 2")
	otherAuthorMessage := sendChannelMessage(t, client, admin.Token, channelID, "keep me")

	firstMessageID := parseSnowflake(t, firstMessage.ID)
	secondMessageID := parseSnowflake(t, secondMessage.ID)
	otherMessageID := parseSnowflake(t, otherAuthorMessage.ID)
	targetUserID := parseSnowflake(t, target.UserID)

	entries := []map[string]any{
		{"channel_id": formatSnowflake(channelID), "message_id": formatSnowflake(firstMessageID)},
		{"channel_id": formatSnowflake(channelID), "message_id": formatSnowflake(secondMessageID)},
		{"channel_id": formatSnowflake(channelID), "message_id": formatSnowflake(firstMessageID)},
		{"channel_id": formatSnowflake(channelID), "message_id": formatSnowflake(otherMessageID)},
	}

	queueResp := struct {
		Success   bool   `json:"success"`
		JobID     string `json:"job_id"`
		Requested int    `json:"requested"`
	}{}

	var resp *http.Response
	var err error

	resp = adminPostJSON(t, client, admin.Token, "/admin/messages/shred", map[string]any{
		"user_id": formatSnowflake(targetUserID),
		"entries": entries,
	})
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &queueResp)

	if !queueResp.Success {
		t.Fatalf("expected queue response to be successful: %+v", queueResp)
	}

	if queueResp.Requested != len(entries) {
		t.Fatalf("expected requested=%d, got %d", len(entries), queueResp.Requested)
	}

	status := waitForMessageShredJobCompletion(t, client, admin.Token, queueResp.JobID)
	if status.Status != "completed" {
		t.Fatalf("unexpected status %s for job %s", status.Status, queueResp.JobID)
	}

	if status.Total != 2 || status.Processed != 2 {
		t.Fatalf("expected total=2 processed=2, got total=%d processed=%d", status.Total, status.Processed)
	}

	if status.Skipped != 2 {
		t.Fatalf("expected skipped=2 for duplicate/foreign entries, got %d", status.Skipped)
	}

	if status.Requested != len(entries) {
		t.Fatalf("status requested count mismatch: expected %d, got %d", len(entries), status.Requested)
	}

	for _, messageID := range []int64{firstMessageID, secondMessageID} {
		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), target.Token)
		if err != nil {
			t.Fatalf("failed to fetch deleted message %d: %v", messageID, err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	}

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, otherMessageID), admin.Token)
	if err != nil {
		t.Fatalf("failed to fetch other author message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}
