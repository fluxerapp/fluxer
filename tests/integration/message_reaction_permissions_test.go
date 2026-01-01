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
)

func TestMessageReactionPermissions(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	member := createTestAccount(t, client)
	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, member.Token)

	guild := createGuild(t, client, owner.Token, "Reaction Perms Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	message := sendChannelMessage(t, client, owner.Token, channelID, "reaction test")

	t.Run("member can remove own reaction", func(t *testing.T) {
		resp, err := client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/👍/@me",
			channelID, parseSnowflake(t, message.ID)), member.Token)
		if err != nil {
			t.Fatalf("failed to add reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions/👍/@me",
			channelID, parseSnowflake(t, message.ID)), member.Token)
		if err != nil {
			t.Fatalf("failed to remove own reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("owner can remove other user reaction", func(t *testing.T) {
		resp, err := client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/❤️/@me",
			channelID, parseSnowflake(t, message.ID)), member.Token)
		if err != nil {
			t.Fatalf("failed to add reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions/❤️/%s",
			channelID, parseSnowflake(t, message.ID), member.UserID), owner.Token)
		if err != nil {
			t.Fatalf("failed to remove other user reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("member without ADD_REACTIONS can stack existing reaction but not add new", func(t *testing.T) {
		denyAddReactions := int64(1 << 6)
		resp, err := client.requestJSON(http.MethodPut, fmt.Sprintf("/channels/%d/permissions/%s", channelID, member.UserID), map[string]any{
			"type": 1,
			"deny": fmt.Sprintf("%d", denyAddReactions),
		}, owner.Token)
		if err != nil {
			t.Fatalf("failed to set overwrite: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/🔥/@me",
			channelID, parseSnowflake(t, message.ID)), owner.Token)
		if err != nil {
			t.Fatalf("owner failed to add base reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/🔥/@me",
			channelID, parseSnowflake(t, message.ID)), member.Token)
		if err != nil {
			t.Fatalf("member failed to stack existing reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/😎/@me",
			channelID, parseSnowflake(t, message.ID)), member.Token)
		if err != nil {
			t.Fatalf("member failed to add new reaction request: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 when adding new reaction without permission, got %d", resp.StatusCode)
		}
		resp.Body.Close()

		resp, err = client.delete(fmt.Sprintf("/channels/%d/permissions/%s", channelID, member.UserID), owner.Token)
		if err != nil {
			t.Fatalf("failed to delete overwrite: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("owner can remove all reactions for emoji", func(t *testing.T) {
		resp, err := client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/🎉/@me",
			channelID, parseSnowflake(t, message.ID)), member.Token)
		if err != nil {
			t.Fatalf("failed to add reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/🎉/@me",
			channelID, parseSnowflake(t, message.ID)), owner.Token)
		if err != nil {
			t.Fatalf("failed to add reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions/🎉",
			channelID, parseSnowflake(t, message.ID)), owner.Token)
		if err != nil {
			t.Fatalf("failed to remove all reactions for emoji: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("owner can remove all reactions", func(t *testing.T) {
		resp, err := client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/👍/@me",
			channelID, parseSnowflake(t, message.ID)), member.Token)
		if err != nil {
			t.Fatalf("failed to add reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/❤️/@me",
			channelID, parseSnowflake(t, message.ID)), owner.Token)
		if err != nil {
			t.Fatalf("failed to add reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.delete(fmt.Sprintf("/channels/%d/messages/%d/reactions",
			channelID, parseSnowflake(t, message.ID)), owner.Token)
		if err != nil {
			t.Fatalf("failed to remove all reactions: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
