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
	"net/http"
	"testing"
)

func TestAccountDisableAutoCancelOnLogin(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth("/users/@me/disable", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to disable account: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	loginResp := loginTestUser(t, client, account.Email, account.Password)
	if loginResp.Token == "" {
		t.Fatal("expected to be able to login")
	}

	resp, err = client.getWithAuth("/users/@me", loginResp.Token)
	if err != nil {
		t.Fatalf("failed to get user: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	_ = parseSnowflake(t, loginResp.UserID)
	otherUser := createTestAccount(t, client)
	otherUserID := parseSnowflake(t, otherUser.UserID)

	guild := createGuild(t, client, loginResp.Token, "Test Guild")
	invite := createChannelInvite(t, client, loginResp.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, otherUser.Token, invite.Code)

	dmChannel := createDmChannel(t, client, loginResp.Token, otherUserID)
	dmChannelID := parseSnowflake(t, dmChannel.ID)
	message := sendChannelMessage(t, client, loginResp.Token, dmChannelID, "test message")

	if message.ID == "" {
		t.Error("expected to be able to send messages after auto-undisable")
	}

	t.Log("Auto-undisable on login test passed")
}
