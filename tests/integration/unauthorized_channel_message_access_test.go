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

// TestUnauthorizedChannelMessageAccess tests message access controls
func TestUnauthorizedChannelMessageAccess(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	attacker := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Message Security Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	message := sendChannelMessage(t, client, owner.Token, channelID, "secret message")
	messageID := parseSnowflake(t, message.ID)

	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=10", channelID), attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt message read: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized message read, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	ensureSessionStarted(t, client, attacker.Token)
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), map[string]string{"content": "attacker message"}, attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt message send: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized message send, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), map[string]string{"content": "hacked"}, attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt message edit: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized message edit, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), attacker.Token)
	if err != nil {
		t.Fatalf("failed to attempt message delete: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for unauthorized message delete, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
