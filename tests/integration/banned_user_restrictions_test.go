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

// TestBannedUserRestrictions tests that banned users cannot interact
func TestBannedUserRestrictions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	target := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, target.Token)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Ban Test Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, target.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/guilds/%d/bans/%s", guildID, target.UserID), map[string]any{
		"delete_message_seconds": 0,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to ban user: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), target.Token)
	if err != nil {
		t.Fatalf("failed to check guild access after ban: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for banned user guild access, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages", channelID), map[string]string{"content": "I'm banned"}, target.Token)
	if err != nil {
		t.Fatalf("failed to attempt message send after ban: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden && resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 403/404 for banned user message send, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	newInvite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", newInvite.Code), nil, target.Token)
	if err != nil {
		t.Fatalf("failed to attempt rejoin after ban: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for banned user attempting to rejoin, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
