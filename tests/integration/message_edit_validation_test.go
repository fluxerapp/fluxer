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
	"strings"
	"testing"
)

// TestMessageEditValidation probes edge input shapes when editing messages
func TestMessageEditValidation(t *testing.T) {
	client := newTestClient(t)
	author := createTestAccount(t, client)

	ensureSessionStarted(t, client, author.Token)

	guild := createGuild(t, client, author.Token, "Message Edit Validation Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)
	msg := sendChannelMessage(t, client, author.Token, channelID, "original content")
	msgID := parseSnowflake(t, msg.ID)

	longContent := strings.Repeat("x", 6000)
	testCases := []struct {
		name    string
		payload any
	}{
		{name: "empty content string", payload: map[string]any{"content": ""}},
		{name: "content with wrong type", payload: map[string]any{"content": 12345}},
		{name: "missing content field", payload: map[string]any{}},
		{name: "overly long content", payload: map[string]any{"content": longContent}},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, msgID), tc.payload, author.Token)
			if err != nil {
				t.Fatalf("failed to attempt edit (%s): %v", tc.name, err)
			}
			if resp.StatusCode == http.StatusOK {
				t.Fatalf("expected edit to be rejected for %s, got 200 OK", tc.name)
			}
			resp.Body.Close()
		})
	}

	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, msgID), map[string]any{"content": "valid update"}, author.Token)
	if err != nil {
		t.Fatalf("failed to perform valid edit: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}
