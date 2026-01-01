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
	"time"
)

// TestOversizedInputRejection tests that oversized payloads are rejected
func TestOversizedInputRejection(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Input Test Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	longContent := strings.Repeat("a", 5000)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), map[string]string{"content": longContent}, owner.Token)
	if err != nil {
		t.Fatalf("failed to send oversized message: %v", err)
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 for oversized message content, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	longName := strings.Repeat("a", 500)
	resp, err = client.postJSONWithAuth("/guilds", map[string]string{"name": longName}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create guild with long name: %v", err)
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 for oversized guild name, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	longBio := strings.Repeat("b", 5000)
	resp, err = client.patchJSONWithAuth("/users/@me", map[string]string{"bio": longBio}, owner.Token)
	if err != nil {
		t.Fatalf("failed to update with long bio: %v", err)
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected 400 for oversized bio, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
