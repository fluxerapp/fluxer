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
	"math/big"
	"net/http"
	"testing"
	"time"
)

// TestOAuth2BotAddIgnoresUnknownPermissionBits verifies that adding a bot to a guild with
// unknown permission bits succeeds and the bot's role has the unknown bits stripped.
func TestOAuth2BotAddIgnoresUnknownPermissionBits(t *testing.T) {
	client := newTestClient(t)
	appOwner := createTestAccount(t, client)
	guildOwner := createTestAccount(t, client)

	guild := createGuild(t, client, guildOwner.Token, fmt.Sprintf("Bot Unknown Bits Guild %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)

	appID, botUserID, _ := createOAuth2BotApplication(t, client, appOwner, fmt.Sprintf("Bot Unknown Bits %d", time.Now().UnixNano()), nil)

	manageGuild := new(big.Int).Lsh(big.NewInt(1), 5)
	unknownBit := new(big.Int).Lsh(big.NewInt(1), 60)
	combinedPerms := new(big.Int).Or(manageGuild, unknownBit)

	payload := map[string]any{
		"client_id":   appID,
		"scope":       "bot",
		"guild_id":    guild.ID,
		"permissions": combinedPerms.String(),
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

	rolesResp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), guildOwner.Token)
	if err != nil {
		t.Fatalf("failed to fetch roles: %v", err)
	}
	assertStatus(t, rolesResp, http.StatusOK)

	var roles []struct {
		ID          string `json:"id"`
		Name        string `json:"name"`
		Permissions string `json:"permissions"`
	}
	decodeJSONResponse(t, rolesResp, &roles)

	// Find the bot's role (the one in member.Roles that is not @everyone)
	var botRole *struct {
		ID          string `json:"id"`
		Name        string `json:"name"`
		Permissions string `json:"permissions"`
	}
	for i := range roles {
		for _, memberRoleID := range member.Roles {
			if roles[i].ID == memberRoleID && roles[i].ID != guild.ID {
				botRole = &roles[i]
				break
			}
		}
		if botRole != nil {
			break
		}
	}

	if botRole == nil {
		t.Fatal("expected to find bot's role")
	}

	rolePerms, ok := new(big.Int).SetString(botRole.Permissions, 10)
	if !ok {
		t.Fatalf("failed to parse role permissions: %s", botRole.Permissions)
	}

	hasUnknownBit := new(big.Int).And(rolePerms, unknownBit)
	if hasUnknownBit.Cmp(big.NewInt(0)) != 0 {
		t.Fatalf("expected unknown bit to be stripped from bot role, got permissions: %s", botRole.Permissions)
	}

	hasManageGuild := new(big.Int).And(rolePerms, manageGuild)
	if hasManageGuild.Cmp(manageGuild) != 0 {
		t.Fatalf("expected MANAGE_GUILD bit to be preserved in bot role, got permissions: %s", botRole.Permissions)
	}
}
