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
	"net/url"
	"testing"
	"time"
)

// TestReactionPermissions tests reaction permission boundaries
func TestReactionPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Reaction Perm Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	message := sendChannelMessage(t, client, owner.Token, channelID, "react to this")
	emoji := "👍"

	resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/%s/@me", channelID, parseSnowflake(t, message.ID), url.PathEscape(emoji)), member.Token)
	if err != nil {
		t.Fatalf("failed to add reaction: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions/%s/@me", channelID, parseSnowflake(t, message.ID), url.PathEscape(emoji)), member.Token)
	if err != nil {
		t.Fatalf("failed to remove own reaction: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/%s/@me", channelID, parseSnowflake(t, message.ID), url.PathEscape(emoji)), owner.Token)
	if err != nil {
		t.Fatalf("failed to add owner reaction: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerIDSnowflake := parseSnowflake(t, owner.UserID)
	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions/%s/%d", channelID, parseSnowflake(t, message.ID), url.PathEscape(emoji), ownerIDSnowflake), member.Token)
	if err != nil {
		t.Fatalf("failed to attempt remove other reaction: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for removing other's reaction without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions/%s", channelID, parseSnowflake(t, message.ID), url.PathEscape(emoji)), member.Token)
	if err != nil {
		t.Fatalf("failed to attempt remove all emoji reactions: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for removing all reactions without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions", channelID, parseSnowflake(t, message.ID)), member.Token)
	if err != nil {
		t.Fatalf("failed to attempt remove all reactions: %v", err)
	}
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 for removing all reactions without permission, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
