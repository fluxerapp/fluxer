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
	"strings"
	"testing"
)

func TestMessageSearchScopesInDM(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	friendA := createTestAccount(t, client)
	friendB := createTestAccount(t, client)
	friendC := createTestAccount(t, client)

	createFriendship(t, client, user, friendA)
	createFriendship(t, client, user, friendB)
	createFriendship(t, client, user, friendC)

	guild := createGuild(t, client, user.Token, "Test Guild")
	invite := createChannelInvite(t, client, user.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, friendA.Token, invite.Code)
	joinGuild(t, client, friendB.Token, invite.Code)
	joinGuild(t, client, friendC.Token, invite.Code)

	suffix := strings.ToLower(strings.ReplaceAll(t.Name(), "/", "-"))
	searchTerm := fmt.Sprintf("scope-search-dm-%s", suffix)

	dmCurrent := createDmChannel(t, client, user.Token, parseSnowflake(t, friendA.UserID))
	sendChannelMessage(t, client, user.Token, parseSnowflake(t, dmCurrent.ID), fmt.Sprintf("%s current", searchTerm))

	dmOpen := createDmChannel(t, client, user.Token, parseSnowflake(t, friendB.UserID))
	sendChannelMessage(t, client, user.Token, parseSnowflake(t, dmOpen.ID), fmt.Sprintf("%s open", searchTerm))

	dmClosed := createDmChannel(t, client, user.Token, parseSnowflake(t, friendC.UserID))
	sendChannelMessage(t, client, user.Token, parseSnowflake(t, dmClosed.ID), fmt.Sprintf("%s closed", searchTerm))

	dmClosedID := parseSnowflake(t, dmClosed.ID)
	resp, err := client.delete(fmt.Sprintf("/channels/%d", dmClosedID), user.Token)
	if err != nil {
		t.Fatalf("failed to close DM channel: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	guildSuffix := fmt.Sprintf("search-guild-%s", suffix)
	guildA := createGuild(t, client, user.Token, fmt.Sprintf("Scope Search Guild A %s", guildSuffix))
	guildB := createGuild(t, client, user.Token, fmt.Sprintf("Scope Search Guild B %s", guildSuffix))

	guildAChannel := parseSnowflake(t, guildA.SystemChannel)
	guildBChannel := parseSnowflake(t, guildB.SystemChannel)

	sendChannelMessage(t, client, user.Token, guildAChannel, fmt.Sprintf("%s community-a", searchTerm))
	sendChannelMessage(t, client, user.Token, guildBChannel, fmt.Sprintf("%s community-b", searchTerm))

	contextChannelID := dmCurrent.ID

	tests := []struct {
		scope    string
		expected []string
	}{
		{scope: "current", expected: []string{dmCurrent.ID}},
		{scope: "open_dms", expected: []string{dmCurrent.ID, dmOpen.ID}},
		{scope: "all_dms", expected: []string{dmCurrent.ID, dmOpen.ID, dmClosed.ID}},
		{scope: "all", expected: []string{dmCurrent.ID, dmOpen.ID, dmClosed.ID, guildA.SystemChannel, guildB.SystemChannel}},
		{scope: "open_dms_and_all_guilds", expected: []string{dmCurrent.ID, dmOpen.ID, guildA.SystemChannel, guildB.SystemChannel}},
	}

	for _, tc := range tests {
		payload := map[string]any{
			"scope":              tc.scope,
			"content":            searchTerm,
			"context_channel_id": contextChannelID,
		}
		resp := waitForSearchResults(t, client, user.Token, payload)
		actual := channelSetFromSearch(resp)
		requireExactChannels(t, tc.scope, actual, tc.expected)
	}
}
