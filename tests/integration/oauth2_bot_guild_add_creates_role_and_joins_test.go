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

func TestOAuth2BotGuildAddCreatesRoleAndJoins(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	guildOwner := createTestAccount(t, client)

	guild := createGuild(t, client, guildOwner.Token, fmt.Sprintf("Bot Guild %d", time.Now().UnixNano()))

	appID, botUserID, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Bot Add %d", time.Now().UnixNano()), nil)

	permissions := fmt.Sprintf("%d", 1<<5)
	payload := map[string]any{
		"client_id":   appID,
		"scope":       "bot",
		"guild_id":    guild.ID,
		"permissions": permissions,
	}

	resp, err := client.postJSONWithAuth("/oauth2/authorize/consent", payload, guildOwner.Token)
	if err != nil {
		t.Fatalf("failed to authorize bot: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("bot authorization failed with status %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var consentResp struct {
		RedirectTo string `json:"redirect_to"`
	}
	decodeJSONResponse(t, resp, &consentResp)
	if consentResp.RedirectTo == "" {
		t.Fatal("expected redirect_to in consent response")
	}

	memberResp, err := client.getWithAuth(fmt.Sprintf("/guilds/%s/members/%s", guild.ID, botUserID), guildOwner.Token)
	if err != nil {
		t.Fatalf("failed to fetch bot member: %v", err)
	}
	if memberResp.StatusCode != http.StatusOK {
		t.Fatalf("bot member lookup failed with status %d: %s", memberResp.StatusCode, readResponseBody(memberResp))
	}

	var member struct {
		Roles []string `json:"roles"`
	}
	decodeJSONResponse(t, memberResp, &member)
	if len(member.Roles) == 0 {
		t.Fatal("expected bot to have at least one role after authorization")
	}
}
