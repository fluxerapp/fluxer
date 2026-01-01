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

// TestReferencedMessageSerializationOnListMessages verifies that when fetching messages
// via the list messages endpoint, reply messages include the full referenced_message object.
func TestReferencedMessageSerializationOnListMessages(t *testing.T) {
	client := newTestClient(t)
	author := createTestAccount(t, client)
	recipient := createTestAccount(t, client)
	ensureSessionStarted(t, client, author.Token)
	ensureSessionStarted(t, client, recipient.Token)

	createFriendship(t, client, author, recipient)

	channel := createDmChannel(t, client, author.Token, parseSnowflake(t, recipient.UserID))

	originalContent := "This is the original message to be referenced"
	originalMessage := sendChannelMessage(t, client, author.Token, parseSnowflake(t, channel.ID), originalContent)

	replyPayload := map[string]any{
		"content": "This is a reply to the original message",
		"message_reference": map[string]any{
			"message_id": originalMessage.ID,
			"channel_id": channel.ID,
			"type":       0,
		},
	}

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages", parseSnowflake(t, channel.ID)),
		replyPayload,
		author.Token,
	)
	if err != nil {
		t.Fatalf("failed to post reply: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	resp, err = client.getWithAuth(
		fmt.Sprintf("/channels/%d/messages?limit=10", parseSnowflake(t, channel.ID)),
		author.Token,
	)
	if err != nil {
		t.Fatalf("failed to list messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var messages []struct {
		ID               string `json:"id"`
		Content          string `json:"content"`
		MessageReference *struct {
			MessageID string `json:"message_id"`
			ChannelID string `json:"channel_id"`
		} `json:"message_reference"`
		ReferencedMessage *struct {
			ID      string `json:"id"`
			Content string `json:"content"`
			Author  struct {
				ID string `json:"id"`
			} `json:"author"`
		} `json:"referenced_message"`
	}
	decodeJSONResponse(t, resp, &messages)

	if len(messages) < 2 {
		t.Fatalf("expected at least 2 messages, got %d", len(messages))
	}

	// Find the reply message (should be first since we're fetching newest first)
	var foundReply bool
	for _, msg := range messages {
		if msg.MessageReference != nil && msg.MessageReference.MessageID == originalMessage.ID {
			foundReply = true

			if msg.ReferencedMessage == nil {
				t.Fatalf("reply message has message_reference but missing referenced_message")
			}

			if msg.ReferencedMessage.ID != originalMessage.ID {
				t.Fatalf("referenced_message.id mismatch: expected %s, got %s", originalMessage.ID, msg.ReferencedMessage.ID)
			}

			if msg.ReferencedMessage.Content != originalContent {
				t.Fatalf("referenced_message.content mismatch: expected %q, got %q", originalContent, msg.ReferencedMessage.Content)
			}

			if msg.ReferencedMessage.Author.ID != author.UserID {
				t.Fatalf("referenced_message.author.id mismatch: expected %s, got %s", author.UserID, msg.ReferencedMessage.Author.ID)
			}

			break
		}
	}

	if !foundReply {
		t.Fatalf("could not find reply message with message_reference in response")
	}
}

// TestReferencedMessageSerializationOnGetMessage verifies that when fetching a single message
// via the get message endpoint, the referenced_message is fully populated.
func TestReferencedMessageSerializationOnGetMessage(t *testing.T) {
	client := newTestClient(t)
	author := createTestAccount(t, client)
	recipient := createTestAccount(t, client)
	ensureSessionStarted(t, client, author.Token)
	ensureSessionStarted(t, client, recipient.Token)

	createFriendship(t, client, author, recipient)

	channel := createDmChannel(t, client, author.Token, parseSnowflake(t, recipient.UserID))

	originalContent := "Original message for single message fetch test"
	originalMessage := sendChannelMessage(t, client, author.Token, parseSnowflake(t, channel.ID), originalContent)

	replyPayload := map[string]any{
		"content": "Reply for single message fetch test",
		"message_reference": map[string]any{
			"message_id": originalMessage.ID,
			"channel_id": channel.ID,
			"type":       0,
		},
	}

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages", parseSnowflake(t, channel.ID)),
		replyPayload,
		author.Token,
	)
	if err != nil {
		t.Fatalf("failed to post reply: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var replyMessage struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, resp, &replyMessage)

	resp, err = client.getWithAuth(
		fmt.Sprintf("/channels/%d/messages/%s", parseSnowflake(t, channel.ID), replyMessage.ID),
		author.Token,
	)
	if err != nil {
		t.Fatalf("failed to get single message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var fetchedMessage struct {
		ID               string `json:"id"`
		Content          string `json:"content"`
		MessageReference *struct {
			MessageID string `json:"message_id"`
			ChannelID string `json:"channel_id"`
		} `json:"message_reference"`
		ReferencedMessage *struct {
			ID      string `json:"id"`
			Content string `json:"content"`
			Author  struct {
				ID string `json:"id"`
			} `json:"author"`
		} `json:"referenced_message"`
	}
	decodeJSONResponse(t, resp, &fetchedMessage)

	if fetchedMessage.MessageReference == nil {
		t.Fatalf("expected message_reference to be present")
	}

	if fetchedMessage.ReferencedMessage == nil {
		t.Fatalf("expected referenced_message to be present but it was nil")
	}

	if fetchedMessage.ReferencedMessage.ID != originalMessage.ID {
		t.Fatalf("referenced_message.id mismatch: expected %s, got %s", originalMessage.ID, fetchedMessage.ReferencedMessage.ID)
	}

	if fetchedMessage.ReferencedMessage.Content != originalContent {
		t.Fatalf("referenced_message.content mismatch: expected %q, got %q", originalContent, fetchedMessage.ReferencedMessage.Content)
	}

	if fetchedMessage.ReferencedMessage.Author.ID != author.UserID {
		t.Fatalf("referenced_message.author.id mismatch: expected %s, got %s", author.UserID, fetchedMessage.ReferencedMessage.Author.ID)
	}
}

// TestReferencedMessageSerializationOnCreateMessage verifies that when creating a reply message,
// the response includes the full referenced_message object.
func TestReferencedMessageSerializationOnCreateMessage(t *testing.T) {
	client := newTestClient(t)
	author := createTestAccount(t, client)
	recipient := createTestAccount(t, client)
	ensureSessionStarted(t, client, author.Token)
	ensureSessionStarted(t, client, recipient.Token)

	createFriendship(t, client, author, recipient)

	channel := createDmChannel(t, client, author.Token, parseSnowflake(t, recipient.UserID))

	originalContent := "Original message for create test"
	originalMessage := sendChannelMessage(t, client, author.Token, parseSnowflake(t, channel.ID), originalContent)

	replyPayload := map[string]any{
		"content": "Reply for create response test",
		"message_reference": map[string]any{
			"message_id": originalMessage.ID,
			"channel_id": channel.ID,
			"type":       0,
		},
	}

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages", parseSnowflake(t, channel.ID)),
		replyPayload,
		author.Token,
	)
	if err != nil {
		t.Fatalf("failed to post reply: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var createdMessage struct {
		ID               string `json:"id"`
		Content          string `json:"content"`
		MessageReference *struct {
			MessageID string `json:"message_id"`
			ChannelID string `json:"channel_id"`
		} `json:"message_reference"`
		ReferencedMessage *struct {
			ID      string `json:"id"`
			Content string `json:"content"`
			Author  struct {
				ID string `json:"id"`
			} `json:"author"`
		} `json:"referenced_message"`
	}
	decodeJSONResponse(t, resp, &createdMessage)

	if createdMessage.MessageReference == nil {
		t.Fatalf("expected message_reference to be present in create response")
	}

	if createdMessage.ReferencedMessage == nil {
		t.Fatalf("expected referenced_message to be present in create response but it was nil")
	}

	if createdMessage.ReferencedMessage.ID != originalMessage.ID {
		t.Fatalf("referenced_message.id mismatch: expected %s, got %s", originalMessage.ID, createdMessage.ReferencedMessage.ID)
	}

	if createdMessage.ReferencedMessage.Content != originalContent {
		t.Fatalf("referenced_message.content mismatch: expected %q, got %q", originalContent, createdMessage.ReferencedMessage.Content)
	}

	if createdMessage.ReferencedMessage.Author.ID != author.UserID {
		t.Fatalf("referenced_message.author.id mismatch: expected %s, got %s", author.UserID, createdMessage.ReferencedMessage.Author.ID)
	}
}
