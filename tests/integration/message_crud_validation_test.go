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

func TestMessageCRUDValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)

	guild := createGuild(t, client, owner.Token, "Message Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	t.Run("reject sending message without content or embeds", func(t *testing.T) {
		payload := map[string]any{}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("reject message with content too long", func(t *testing.T) {
		longContent := make([]byte, 4001)
		for i := range longContent {
			longContent[i] = 'a'
		}
		payload := map[string]string{"content": string(longContent)}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("accept valid message", func(t *testing.T) {
		payload := map[string]string{"content": "Hello, world!"}
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to send message: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var message struct {
			ID      string `json:"id"`
			Content string `json:"content"`
		}
		decodeJSONResponse(t, resp, &message)
		if message.Content != "Hello, world!" {
			t.Errorf("expected content 'Hello, world!', got '%s'", message.Content)
		}
	})

	messagePayload := map[string]string{"content": "Test message"}
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), messagePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to send message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var testMessage struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &testMessage)
	messageID := parseSnowflake(t, testMessage.ID)

	t.Run("reject getting nonexistent message", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages/999999999999999999", channelID), owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("can get specific message", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), owner.Token)
		if err != nil {
			t.Fatalf("failed to get message: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("can get messages list", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), owner.Token)
		if err != nil {
			t.Fatalf("failed to get messages: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var messages []struct {
			ID string `json:"id"`
		}
		decodeJSONResponse(t, resp, &messages)
		if len(messages) == 0 {
			t.Error("expected at least one message")
		}
	})

	t.Run("can get messages with limit", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=1", channelID), owner.Token)
		if err != nil {
			t.Fatalf("failed to get messages: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		var messages []struct {
			ID string `json:"id"`
		}
		decodeJSONResponse(t, resp, &messages)
		if len(messages) > 1 {
			t.Errorf("expected at most 1 message, got %d", len(messages))
		}
	})

	t.Run("reject edit with empty content", func(t *testing.T) {
		payload := map[string]string{"content": ""}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusBadRequest)
		resp.Body.Close()
	})

	t.Run("can edit own message", func(t *testing.T) {
		payload := map[string]string{"content": "Edited message"}
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), payload, owner.Token)
		if err != nil {
			t.Fatalf("failed to edit message: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("can delete own message", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), owner.Token)
		if err != nil {
			t.Fatalf("failed to delete message: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
