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

func TestMessageReadHistoryPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Read History Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	message := sendChannelMessage(t, client, owner.Token, channelID, "root message")
	messageID := parseSnowflake(t, message.ID)

	setReadHistoryDenied := func(deny bool) {
		if deny {
			resp, err = client.requestJSON(
				http.MethodPut,
				fmt.Sprintf("/channels/%d/permissions/%s", channelID, member.UserID),
				map[string]any{
					"type": 1,
					"deny": fmt.Sprintf("%d", int64(1<<16)),
				},
				owner.Token,
			)
		} else {
			resp, err = client.delete(
				fmt.Sprintf("/channels/%d/permissions/%s", channelID, member.UserID),
				owner.Token,
			)
		}

		if err != nil {
			t.Fatalf("failed to update overwrite: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	}

	t.Run("list messages returns empty when missing READ_MESSAGE_HISTORY", func(t *testing.T) {
		setReadHistoryDenied(true)
		defer setReadHistoryDenied(false)

		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), member.Token)
		if err != nil {
			t.Fatalf("failed to get messages: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var messages []map[string]any
		decodeJSONResponse(t, resp, &messages)
		if len(messages) != 0 {
			t.Fatalf("expected no messages, got %d", len(messages))
		}
	})

	t.Run("reply requires READ_MESSAGE_HISTORY", func(t *testing.T) {
		setReadHistoryDenied(true)
		defer setReadHistoryDenied(false)

		payload := map[string]any{
			"content": "cannot reply",
			"message_reference": map[string]any{
				"message_id": fmt.Sprintf("%d", messageID),
			},
		}

		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to post reply: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 when missing READ_MESSAGE_HISTORY, got %d", resp.StatusCode)
		}
		resp.Body.Close()
	})

	t.Run("cannot reply to system message", func(t *testing.T) {
		setReadHistoryDenied(false)

		systemSource := sendChannelMessage(t, client, owner.Token, channelID, "pin me")
		sourceID := parseSnowflake(t, systemSource.ID)

		resp, err := client.putWithAuth(fmt.Sprintf("/channels/%d/pins/%d", channelID, sourceID), owner.Token)
		if err != nil {
			t.Fatalf("failed to pin message: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=10", channelID), owner.Token)
		if err != nil {
			t.Fatalf("failed to fetch messages: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var messages []struct {
			ID   string `json:"id"`
			Type int    `json:"type"`
		}
		decodeJSONResponse(t, resp, &messages)

		var systemMessageID string
		for _, m := range messages {
			if m.Type != 0 && m.Type != 19 {
				systemMessageID = m.ID
				break
			}
		}
		if systemMessageID == "" {
			t.Fatalf("failed to find system message to reply to")
		}

		payload := map[string]any{
			"content": "no system replies",
			"message_reference": map[string]any{
				"message_id": systemMessageID,
			},
		}

		resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, member.Token)
		if err != nil {
			t.Fatalf("failed to post reply: %v", err)
		}
		if resp.StatusCode != http.StatusBadRequest {
			t.Fatalf("expected 400 when replying to system message, got %d", resp.StatusCode)
		}
		resp.Body.Close()
	})

	t.Run("adding reaction requires READ_MESSAGE_HISTORY", func(t *testing.T) {
		setReadHistoryDenied(true)
		defer setReadHistoryDenied(false)

		resp, err := client.putWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/reactions/👍/@me", channelID, messageID),
			member.Token,
		)
		if err != nil {
			t.Fatalf("failed to add reaction: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 when adding reaction without READ_MESSAGE_HISTORY, got %d", resp.StatusCode)
		}
		resp.Body.Close()
	})

	t.Run("search requires READ_MESSAGE_HISTORY", func(t *testing.T) {
		setReadHistoryDenied(true)
		defer setReadHistoryDenied(false)

		resp, err := client.postJSONWithAuth(
			"/search/messages",
			map[string]any{
				"content":            "root",
				"context_channel_id": fmt.Sprintf("%d", channelID),
			},
			member.Token,
		)
		if err != nil {
			t.Fatalf("failed to search messages: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 when searching without READ_MESSAGE_HISTORY, got %d", resp.StatusCode)
		}
		resp.Body.Close()
	})

	t.Run("pins are hidden without READ_MESSAGE_HISTORY", func(t *testing.T) {
		setReadHistoryDenied(true)
		defer setReadHistoryDenied(false)

		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages/pins", channelID), member.Token)
		if err != nil {
			t.Fatalf("failed to get pins: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var pinsResponse struct {
			Items   []any `json:"items"`
			HasMore bool  `json:"has_more"`
		}
		decodeJSONResponse(t, resp, &pinsResponse)
		if len(pinsResponse.Items) != 0 {
			t.Fatalf("expected no pins when missing READ_MESSAGE_HISTORY, got %d", len(pinsResponse.Items))
		}
	})
}
