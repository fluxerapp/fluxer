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

func TestUserContentMentionsSavedMessagesAndHarvest(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guest := createTestAccount(t, client)
	ensureSessionStarted(t, client, guest.Token)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Mention Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)
	channelSnowflake := formatSnowflake(channelID)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, guest.Token)
	if err != nil {
		t.Fatalf("failed to accept guild invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	content := fmt.Sprintf("hello <@%s>", owner.UserID)
	message := sendChannelMessage(t, client, guest.Token, channelID, content)

	waitForCondition(t, 30*time.Second, func() (bool, error) {
		resp, err := client.getWithAuth("/users/@me/mentions?limit=5", owner.Token)
		if err != nil {
			return false, err
		}
		defer resp.Body.Close()
		if resp.StatusCode != http.StatusOK {
			return false, fmt.Errorf("status %d", resp.StatusCode)
		}
		var mentions mentionListResponse
		decodeJSONResponse(t, resp, &mentions)
		for _, m := range mentions {
			if m.ID == message.ID {
				return true, nil
			}
		}
		return false, nil
	})

	resp, err = client.getWithAuth("/users/@me/mentions?limit=5", owner.Token)
	if err != nil {
		t.Fatalf("failed to read mentions: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var mentions mentionListResponse
	decodeJSONResponse(t, resp, &mentions)
	if len(mentions) == 0 || mentions[0].ID != message.ID {
		t.Fatalf("expected mention list to include new message")
	}

	resp, err = client.delete(fmt.Sprintf("/users/@me/mentions/%d", parseSnowflake(t, message.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to delete mention: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	savePayload := map[string]string{
		"channel_id": channelSnowflake,
		"message_id": formatSnowflake(parseSnowflake(t, message.ID)),
	}
	resp, err = client.postJSONWithAuth("/users/@me/saved-messages", savePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to save message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/saved-messages?limit=10", owner.Token)
	if err != nil {
		t.Fatalf("failed to fetch saved messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var saved []messageResponse
	decodeJSONResponse(t, resp, &saved)
	if len(saved) == 0 || saved[0].ID != message.ID {
		t.Fatalf("expected saved messages to include message %s", message.ID)
	}

	resp, err = client.delete(fmt.Sprintf("/users/@me/saved-messages/%d", parseSnowflake(t, message.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to unsave message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	harvestResp, err := client.postJSONWithAuth("/users/@me/harvest", nil, owner.Token)
	if err != nil {
		t.Fatalf("failed to request data harvest: %v", err)
	}
	assertStatus(t, harvestResp, http.StatusOK)
	var harvestRequest struct {
		HarvestID string `json:"harvestId"`
	}
	decodeJSONResponse(t, harvestResp, &harvestRequest)

	waitForCondition(t, 90*time.Second, func() (bool, error) {
		status := fetchHarvestStatus(t, client, owner.Token, harvestRequest.HarvestID)
		if status.CompletedAt != nil && status.DownloadURLExpiresAt != nil {
			return true, nil
		}
		if status.FailedAt != nil {
			return false, fmt.Errorf("harvest failed: %v", *status.ErrorMessage)
		}
		return false, nil
	})

	status := fetchHarvestStatus(t, client, owner.Token, harvestRequest.HarvestID)
	if status.CompletedAt == nil {
		t.Fatalf("expected harvest to complete")
	}

	resp, err = client.getWithAuth(fmt.Sprintf("/users/@me/harvest/%s/download", harvestRequest.HarvestID), owner.Token)
	if err != nil {
		t.Fatalf("failed to fetch harvest download: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var download harvestDownloadResponse
	decodeJSONResponse(t, resp, &download)
	if download.DownloadURL == "" {
		t.Fatalf("expected download url in harvest response")
	}
}
