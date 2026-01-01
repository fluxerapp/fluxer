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

func TestAccountDeleteAnonymizesMessagesBeyondChunkSize(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	guild := createGuild(t, client, account.Token, "Message Pagination Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)
	guildID := parseSnowflake(t, guild.ID)

	const (
		chunkSize     = 100
		extraMessages = 5
	)
	totalMessages := chunkSize + extraMessages

	for i := 0; i < totalMessages; i++ {
		sendChannelMessage(t, client, account.Token, channelID, fmt.Sprintf("Message %d", i+1))
	}

	newOwner := createTestAccount(t, client)
	invite := createChannelInvite(t, client, account.Token, channelID)
	joinGuild(t, client, newOwner.Token, invite.Code)

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/guilds/%d/transfer-ownership", guildID),
		map[string]string{
			"new_owner_id": newOwner.UserID,
			"password":     account.Password,
		},
		account.Token,
	)
	if err != nil {
		t.Fatalf("failed to transfer guild ownership: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	guildResp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), account.Token)
	if err != nil {
		t.Fatalf("failed to get guild after transfer: %v", err)
	}
	assertStatus(t, guildResp, http.StatusOK)
	var guildRespBody struct {
		OwnerID string `json:"owner_id"`
	}
	decodeJSONResponse(t, guildResp, &guildRespBody)
	if guildRespBody.OwnerID != newOwner.UserID {
		t.Fatalf("expected guild owner to be %s, got %s", newOwner.UserID, guildRespBody.OwnerID)
	}
	guildResp.Body.Close()

	resp, err = client.postJSONWithAuth("/users/@me/delete", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to delete account: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	setPendingDeletionAt(t, client, account.UserID, time.Now().Add(-time.Minute))

	triggerDeletionWorker(t, client)
	waitForDeletionCompletion(t, client, account.UserID, 60*time.Second)

	countResp, err := client.get(fmt.Sprintf("/test/users/%s/messages/count", account.UserID))
	if err != nil {
		t.Fatalf("failed to fetch message count: %v", err)
	}
	assertStatus(t, countResp, http.StatusOK)
	var response struct {
		Count int `json:"count"`
	}
	decodeJSONResponse(t, countResp, &response)

	if response.Count != 0 {
		t.Fatalf("expected 0 remaining messages after anonymization, got %d", response.Count)
	}
}
