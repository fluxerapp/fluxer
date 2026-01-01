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
	"time"
)

type pendingBulkDeletionSuccessResponse struct {
	Success bool `json:"success"`
}

func TestPendingBulkMessageDeletionCancel(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Pending Bulk Delete Guild")
	channelA := createGuildChannel(t, client, account.Token, parseSnowflake(t, guild.ID), "pending-delete-1")
	channelB := createGuildChannel(t, client, account.Token, parseSnowflake(t, guild.ID), "pending-delete-2")

	sendChannelMessage(t, client, account.Token, parseSnowflake(t, channelA.ID), "Queued 1")
	sendChannelMessage(t, client, account.Token, parseSnowflake(t, channelA.ID), "Queued 2")
	sendChannelMessage(t, client, account.Token, parseSnowflake(t, channelB.ID), "Queued 3")

	resp, err := client.postJSONWithAuth("/users/@me/messages/delete", map[string]any{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to schedule bulk message deletion: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch current user: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var userResp userPrivateResponse
	decodeJSONResponse(t, resp, &userResp)
	resp.Body.Close()

	if userResp.PendingBulkMessageDeletion == nil {
		t.Fatalf("expected pending bulk deletion info after scheduling")
	}

	pending := userResp.PendingBulkMessageDeletion
	if pending.ChannelCount != 2 || pending.MessageCount != 3 {
		t.Fatalf("expected channels=2 messages=3, got channels=%d messages=%d", pending.ChannelCount, pending.MessageCount)
	}

	scheduledAt, err := time.Parse(time.RFC3339, pending.ScheduledAt)
	if err != nil {
		t.Fatalf("failed to parse scheduled time: %v", err)
	}

	delta := time.Until(scheduledAt)
	if delta < 23*time.Hour || delta > 25*time.Hour {
		t.Fatalf("scheduledAt=%s is not roughly 24h (%s)", pending.ScheduledAt, delta)
	}

	resp, err = client.deleteJSONWithAuth("/users/@me/messages/delete", map[string]any{}, account.Token)
	if err != nil {
		t.Fatalf("failed to cancel pending deletion: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var success pendingBulkDeletionSuccessResponse
	decodeJSONResponse(t, resp, &success)
	resp.Body.Close()
	if !success.Success {
		t.Fatalf("cancel endpoint did not return success")
	}

	time.Sleep(100 * time.Millisecond)

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch user after cancel: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var afterCancelResp userPrivateResponse
	decodeJSONResponse(t, resp, &afterCancelResp)
	resp.Body.Close()

	if afterCancelResp.PendingBulkMessageDeletion != nil {
		t.Fatalf(
			"expected pending bulk deletion to be cleared after cancel, got %+v",
			afterCancelResp.PendingBulkMessageDeletion,
		)
	}
}
