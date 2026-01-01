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

// TestEmbedAttachmentURL_NoAttachmentsError tests embed uses attachment:// but no files uploaded, expect 400 error
func TestEmbedAttachmentURL_NoAttachmentsError(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "No Attachments Test Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	payload := map[string]any{
		"content": "Embed references attachment but none provided",
		"embeds": []map[string]any{
			{
				"title":       "Invalid Reference",
				"description": "No attachment uploaded",
				"image": map[string]string{
					"url": "attachment://image.png",
				},
			},
		},
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), payload, user.Token)
	if err != nil {
		t.Fatalf("failed to send request: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusBadRequest)
}
