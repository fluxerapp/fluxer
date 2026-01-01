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
	"strconv"
	"testing"
	"time"
)

// Covers auto-joining a guild when logging in with invite_code.
func TestAuthLoginInviteAutoJoin(t *testing.T) {
	client := newTestClient(t)

	owner := createTestAccount(t, client)
	guild := createGuild(t, client, owner.Token, fmt.Sprintf("InviteGuild-%d", time.Now().UnixNano()))
	systemChannelID, err := strconv.ParseInt(guild.SystemChannel, 10, 64)
	if err != nil {
		t.Fatalf("failed to parse system channel id: %v", err)
	}
	invite := createChannelInvite(t, client, owner.Token, systemChannelID)

	member := createTestAccount(t, client)

	loginReq := loginRequest{
		Email:      member.Email,
		Password:   member.Password,
		InviteCode: &invite.Code,
	}

	resp, err := client.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to login with invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var loginResp loginResponse
	decodeJSONResponse(t, resp, &loginResp)
	if loginResp.Token == "" {
		t.Fatalf("expected login to return token")
	}
	resp.Body.Close()

	guildsResp, err := client.getWithAuth("/users/@me/guilds", loginResp.Token)
	if err != nil {
		t.Fatalf("failed to fetch guilds: %v", err)
	}
	assertStatus(t, guildsResp, http.StatusOK)
	var guilds []struct {
		ID string `json:"id"`
	}
	decodeJSONResponse(t, guildsResp, &guilds)
	guildsResp.Body.Close()

	found := false
	for _, g := range guilds {
		if g.ID == guild.ID {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected guild %s to be joined after login with invite", guild.ID)
	}
}
