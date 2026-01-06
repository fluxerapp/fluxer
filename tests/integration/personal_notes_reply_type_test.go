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

const personalNotesMessageTypeReply = 19

// TestPersonalNotesReplyType ensures replies sent from the Personal Notes channel
// are serialized with the REPLY message type.
func TestPersonalNotesReplyType(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	ensureSessionStarted(t, client, account.Token)

	channelID := parseSnowflake(t, account.UserID)
	originalMessage := sendChannelMessage(t, client, account.Token, channelID, "Original personal note")

	replyPayload := map[string]any{
		"content": "Replying in personal notes",
		"message_reference": map[string]any{
			"message_id": originalMessage.ID,
			"channel_id": account.UserID,
			"type":       0,
		},
	}

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages", channelID),
		replyPayload,
		account.Token,
	)
	if err != nil {
		t.Fatalf("failed to post personal notes reply: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var reply struct {
		ID               string `json:"id"`
		Type             int    `json:"type"`
		MessageReference *struct {
			MessageID string `json:"message_id"`
			ChannelID string `json:"channel_id"`
		} `json:"message_reference"`
	}
	decodeJSONResponse(t, resp, &reply)

	if reply.Type != personalNotesMessageTypeReply {
		t.Fatalf("expected personal notes reply type %d but got %d", personalNotesMessageTypeReply, reply.Type)
	}

	if reply.MessageReference == nil {
		t.Fatalf("expected message_reference to be present in personal notes reply")
	}

	if reply.MessageReference.MessageID != originalMessage.ID {
		t.Fatalf("personal notes reply referenced wrong message: expected %s, got %s", originalMessage.ID, reply.MessageReference.MessageID)
	}
}
