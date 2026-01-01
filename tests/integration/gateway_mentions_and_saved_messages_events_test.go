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
	"time"
)

func TestGatewayMentionsAndSavedMessagesEvents(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guest := createTestAccount(t, client)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)
	guestSocket := newGatewayClient(t, client, guest.Token)
	t.Cleanup(guestSocket.Close)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Mentions Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, guest.Token)
	if err != nil {
		t.Fatalf("failed to accept guild invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	content := fmt.Sprintf("ping <@%s>", owner.UserID)
	message := sendChannelMessage(t, client, guest.Token, channelID, content)

	waitForCondition(t, 30*time.Second, func() (bool, error) {
		resp, err := client.getWithAuth("/users/@me/mentions?limit=10", owner.Token)
		if err != nil {
			return false, err
		}
		defer resp.Body.Close()
		if resp.StatusCode != http.StatusOK {
			return false, fmt.Errorf("status %d", resp.StatusCode)
		}
		var mentions mentionListResponse
		decodeJSONResponse(t, resp, &mentions)
		for _, entry := range mentions {
			if entry.ID == message.ID {
				return true, nil
			}
		}
		return false, nil
	})

	resp, err = client.delete(fmt.Sprintf("/users/@me/mentions/%d", parseSnowflake(t, message.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to delete mention: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "RECENT_MENTION_DELETE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			MessageID string `json:"message_id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode mention delete payload: %v", err)
		}
		return payload.MessageID == message.ID
	})

	channelSnowflake := formatSnowflake(channelID)
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

	ownerSocket.WaitForEvent(t, "SAVED_MESSAGE_CREATE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode saved message payload: %v", err)
		}
		return payload.ID == message.ID
	})

	resp, err = client.delete(fmt.Sprintf("/users/@me/saved-messages/%d", parseSnowflake(t, message.ID)), owner.Token)
	if err != nil {
		t.Fatalf("failed to delete saved message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "SAVED_MESSAGE_DELETE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			MessageID string `json:"message_id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode saved message delete payload: %v", err)
		}
		return payload.MessageID == message.ID
	})
}
