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

func TestMessagePins(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	guild := createGuild(t, client, owner.Token, "Pin Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	payload := map[string]string{"content": "Pin me"}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to send message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var message struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &message)
	messageID := parseSnowflake(t, message.ID)

	t.Run("can pin message", func(t *testing.T) {
		resp, err := client.putWithAuth(fmt.Sprintf("/channels/%d/pins/%d", channelID, messageID), owner.Token)
		if err != nil {
			t.Fatalf("failed to pin message: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("can get pinned messages", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages/pins", channelID), owner.Token)
		if err != nil {
			t.Fatalf("failed to get pins: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var pinsResponse struct {
			Items []struct {
				Message struct {
					ID string `json:"id"`
				} `json:"message"`
			} `json:"items"`
			HasMore bool `json:"has_more"`
		}
		decodeJSONResponse(t, resp, &pinsResponse)
		if len(pinsResponse.Items) != 1 {
			t.Errorf("expected 1 pinned message, got %d", len(pinsResponse.Items))
		}
	})

	t.Run("can unpin message", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/channels/%d/pins/%d", channelID, messageID), owner.Token)
		if err != nil {
			t.Fatalf("failed to unpin message: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
