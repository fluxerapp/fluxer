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

func TestTemporaryGuildMembershipKickOnDisconnect(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	guest := createTestAccount(t, client)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)

	guestSocket := newGatewayClient(t, client, guest.Token)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Temporary Guild %d", time.Now().UnixNano()))
	channelID := parseSnowflake(t, guild.SystemChannel)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/invites", channelID), map[string]any{
		"temporary": true,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create temporary invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var invite inviteResponse
	decodeJSONResponse(t, resp, &invite)
	if invite.Code == "" {
		t.Fatalf("expected invite code in response")
	}

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, guest.Token)
	if err != nil {
		t.Fatalf("failed to accept temporary invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "GUILD_MEMBER_ADD", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			User struct {
				ID string `json:"id"`
			} `json:"user"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode guild member add payload: %v", err)
		}
		return payload.User.ID == guest.UserID
	})

	guestSocket.WaitForEvent(t, "GUILD_CREATE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode guild create payload: %v", err)
		}
		return payload.ID == guild.ID
	})

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/members/%s", parseSnowflake(t, guild.ID), guest.UserID), owner.Token)
	if err != nil {
		t.Fatalf("failed to get guild member: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	guestSocket.Close()

	ownerSocket.WaitForEvent(t, "GUILD_MEMBER_REMOVE", 120*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			User struct {
				ID string `json:"id"`
			} `json:"user"`
			GuildID string `json:"guild_id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode guild member remove payload: %v", err)
		}
		return payload.User.ID == guest.UserID && payload.GuildID == guild.ID
	})

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/members/%s", parseSnowflake(t, guild.ID), guest.UserID), owner.Token)
	if err != nil {
		t.Fatalf("failed to get guild member: %v", err)
	}
	assertStatus(t, resp, http.StatusNotFound)
	resp.Body.Close()
}
