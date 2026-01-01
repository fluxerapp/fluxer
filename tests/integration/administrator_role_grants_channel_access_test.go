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

func TestAdministratorRoleGrantsChannelAccess(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Admin Perms Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)

	administratorPermission := 1 << 3
	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%d", guildID, guildID), map[string]any{
		"permissions": fmt.Sprintf("%d", administratorPermission),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to update @everyone role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, guild.SystemChannel))
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	memberGateway := newGatewayClient(t, client, member.Token)
	t.Cleanup(memberGateway.Close)

	guildCreate := memberGateway.WaitForEvent(t, "GUILD_CREATE", 60*time.Second, func(raw json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(raw, &payload); err != nil {
			return false
		}
		return payload.ID == guild.ID
	})

	var guildPayload struct {
		ID       string            `json:"id"`
		Channels []json.RawMessage `json:"channels"`
	}
	if err := json.Unmarshal(guildCreate.Data, &guildPayload); err != nil {
		t.Fatalf("failed to decode GUILD_CREATE payload: %v", err)
	}

	if len(guildPayload.Channels) == 0 {
		t.Fatalf("expected member with ADMINISTRATOR permission on @everyone role to see channels, but got empty channels array")
	}

	systemChannelFound := false
	for _, channelRaw := range guildPayload.Channels {
		var channel struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(channelRaw, &channel); err != nil {
			continue
		}
		if channel.ID == guild.SystemChannel {
			systemChannelFound = true
			break
		}
	}

	if !systemChannelFound {
		t.Fatalf("expected system channel %s to be visible to member with ADMINISTRATOR permission", guild.SystemChannel)
	}
}
