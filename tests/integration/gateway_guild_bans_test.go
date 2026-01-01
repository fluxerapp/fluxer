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

func TestGatewayGuildBans(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	target := createTestAccount(t, client)

	ownerSocket := newGatewayClient(t, client, owner.Token)
	t.Cleanup(ownerSocket.Close)
	targetSocket := newGatewayClient(t, client, target.Token)
	t.Cleanup(targetSocket.Close)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Ban Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)
	invite := createChannelInvite(t, client, owner.Token, channelID)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, target.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	targetSocket.WaitForEvent(t, "GUILD_CREATE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == guild.ID
	})

	banPayload := map[string]any{
		"delete_message_seconds": 0,
	}
	resp, err = client.putJSONWithAuth(fmt.Sprintf("/guilds/%d/bans/%s", guildID, target.UserID), banPayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to ban user: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "GUILD_BAN_ADD", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			GuildID string `json:"guild_id"`
			User    struct {
				ID string `json:"id"`
			} `json:"user"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode ban add: %v", err)
		}
		return payload.GuildID == guild.ID && payload.User.ID == target.UserID
	})

	targetSocket.WaitForEvent(t, "GUILD_DELETE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode guild delete for banned user: %v", err)
		}
		return payload.ID == guild.ID
	})

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/bans", guildID), owner.Token)
	if err != nil {
		t.Fatalf("failed to list bans: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var bans []struct {
		User struct {
			ID string `json:"id"`
		} `json:"user"`
	}
	decodeJSONResponse(t, resp, &bans)
	if len(bans) != 1 || bans[0].User.ID != target.UserID {
		t.Fatalf("expected 1 ban for target user")
	}

	resp, err = client.delete(fmt.Sprintf("/guilds/%d/bans/%s", guildID, target.UserID), owner.Token)
	if err != nil {
		t.Fatalf("failed to unban user: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	ownerSocket.WaitForEvent(t, "GUILD_BAN_REMOVE", 30*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			GuildID string `json:"guild_id"`
			User    struct {
				ID string `json:"id"`
			} `json:"user"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			t.Fatalf("failed to decode ban remove: %v", err)
		}
		return payload.GuildID == guild.ID && payload.User.ID == target.UserID
	})
}
