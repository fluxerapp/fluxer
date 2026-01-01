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

// TestMessageAttachmentDelete_LastAttachmentPinnedEmptyMessage verifies that
// deleting the last attachment from a pinned empty message deletes the message and unpin
func TestMessageAttachmentDelete_LastAttachmentPinnedEmptyMessage(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	gateway := newGatewayClient(t, client, user.Token)
	t.Cleanup(gateway.Close)

	guild := createGuild(t, client, user.Token, "Attachment Pin Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	msg, attachmentID := sendChannelMessageWithAttachment(t, client, user.Token, channelID, "", "yeah.png")
	messageID := parseSnowflake(t, msg.ID)

	resp, err := client.putWithAuth(
		fmt.Sprintf("/channels/%d/pins/%d", channelID, messageID),
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to pin message: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	gateway.WaitForEvent(t, "CHANNEL_PINS_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
		var pinUpdate struct {
			ChannelID string `json:"channel_id"`
		}
		json.Unmarshal(data, &pinUpdate)
		return pinUpdate.ChannelID == msg.ChannelID
	})

	resp, err = client.deleteJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages/%d/attachments/%d", channelID, messageID, attachmentID),
		nil,
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to delete attachment: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	gateway.WaitForEvent(t, "MESSAGE_DELETE", 10*time.Second, func(data json.RawMessage) bool {
		var del struct {
			ID string `json:"id"`
		}
		json.Unmarshal(data, &del)
		return del.ID == msg.ID
	})

	gateway.WaitForEvent(t, "CHANNEL_PINS_UPDATE", 10*time.Second, func(data json.RawMessage) bool {
		var pinUpdate struct {
			ChannelID string `json:"channel_id"`
		}
		json.Unmarshal(data, &pinUpdate)
		return pinUpdate.ChannelID == msg.ChannelID
	})

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), user.Token)
	if err != nil {
		t.Fatalf("failed to check message: %v", err)
	}
	assertStatus(t, resp, http.StatusNotFound)
	resp.Body.Close()
}
