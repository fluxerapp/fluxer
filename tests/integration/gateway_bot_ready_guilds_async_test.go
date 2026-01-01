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

// Bots should get an immediate READY with no guild payloads, followed by
// GUILD_CREATE events as guild connections are established.
func TestBotReadyDoesNotIncludeGuildsAndStreamsGuildCreateAsync(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Bot Ready Guild %d", time.Now().UnixNano()))

	appID, _, botToken := createOAuth2BotApplication(t, client, owner, fmt.Sprintf("Bot Ready App %d", time.Now().UnixNano()), nil)

	permissions := fmt.Sprintf("%d", 1<<5)
	payload := map[string]any{
		"client_id":   appID,
		"scope":       "bot",
		"guild_id":    guild.ID,
		"permissions": permissions,
	}

	resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to authorize bot into guild: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("bot authorization failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	gateway := newGatewayClient(t, client, botToken)
	defer gateway.Close()

	readyDispatch := gateway.WaitForEvent(t, "READY", 10*time.Second, nil)

	var readyPayload struct {
		Guilds []json.RawMessage `json:"guilds"`
		User   struct {
			Bot bool `json:"bot"`
		} `json:"user"`
	}
	if err := json.Unmarshal(readyDispatch.Data, &readyPayload); err != nil {
		t.Fatalf("failed to decode READY payload: %v", err)
	}
	if len(readyPayload.Guilds) != 0 {
		t.Fatalf("expected READY.guilds to be empty for bots, got %d entries", len(readyPayload.Guilds))
	}
	if !readyPayload.User.Bot {
		t.Fatalf("expected READY.user.bot to be true for bot sessions")
	}

	guildCreate := gateway.WaitForEvent(t, "GUILD_CREATE", 20*time.Second, func(data json.RawMessage) bool {
		var payload struct {
			ID string `json:"id"`
		}
		if err := json.Unmarshal(data, &payload); err != nil {
			return false
		}
		return payload.ID == guild.ID
	})

	if guildCreate.Sequence <= readyDispatch.Sequence {
		t.Fatalf(
			"expected GUILD_CREATE after READY (ready seq %d, guild_create seq %d)",
			readyDispatch.Sequence,
			guildCreate.Sequence,
		)
	}
}
