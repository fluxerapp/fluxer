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
)

// TestUserProfileAccessControl tests the access control rules for GET /users/:userid/profile
func TestUserProfileAccessControl(t *testing.T) {
	client := newTestClient(t)

	t.Run("user can access own profile", func(t *testing.T) {
		user := createTestAccount(t, client)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile", user.UserID), user.Token)
		if err != nil {
			t.Fatalf("failed to get own profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("strangers cannot access each other profiles", func(t *testing.T) {
		user1 := createTestAccount(t, client)
		user2 := createTestAccount(t, client)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile", user2.UserID), user1.Token)
		if err != nil {
			t.Fatalf("failed to attempt profile access: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 Forbidden for stranger profile access, got %d", resp.StatusCode)
		}
		resp.Body.Close()
	})

	t.Run("friends can access each other profiles", func(t *testing.T) {
		user1 := createTestAccount(t, client)
		user2 := createTestAccount(t, client)

		createFriendship(t, client, user1, user2)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile", user2.UserID), user1.Token)
		if err != nil {
			t.Fatalf("failed to get friend's profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/users/%s/profile", user1.UserID), user2.Token)
		if err != nil {
			t.Fatalf("failed to get friend's profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("users in mutual guild can access each other profiles", func(t *testing.T) {
		owner := createTestAccount(t, client)
		member := createTestAccount(t, client)

		guild := createGuild(t, client, owner.Token, "Test Guild")

		channelID, err := strconv.ParseInt(guild.SystemChannel, 10, 64)
		if err != nil {
			t.Fatalf("failed to parse channel ID: %v", err)
		}

		invite := createChannelInvite(t, client, owner.Token, channelID)
		joinGuild(t, client, member.Token, invite.Code)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile", member.UserID), owner.Token)
		if err != nil {
			t.Fatalf("failed to get guild member's profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/users/%s/profile", owner.UserID), member.Token)
		if err != nil {
			t.Fatalf("failed to get guild owner's profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("users who share group DM can access each other profiles", func(t *testing.T) {
		owner := createTestAccount(t, client)
		member := createTestAccount(t, client)
		target := createTestAccount(t, client)

		createFriendship(t, client, owner, member)
		createFriendship(t, client, owner, target)

		createGroupDmChannel(t, client, owner.Token, member.UserID, target.UserID)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile", target.UserID), member.Token)
		if err != nil {
			t.Fatalf("failed to get group DM recipient profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("user with outgoing friend request allows recipient to view their profile", func(t *testing.T) {
		requester := createTestAccount(t, client)
		recipient := createTestAccount(t, client)

		resp, err := client.postJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", recipient.UserID), nil, requester.Token)
		if err != nil {
			t.Fatalf("failed to send friend request: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/users/%s/profile", requester.UserID), recipient.Token)
		if err != nil {
			t.Fatalf("failed to get requester's profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/users/%s/profile", recipient.UserID), requester.Token)
		if err != nil {
			t.Fatalf("failed to attempt profile access: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 Forbidden for requester viewing recipient's profile, got %d", resp.StatusCode)
		}
		resp.Body.Close()
	})

	t.Run("users who leave guild lose profile access", func(t *testing.T) {
		owner := createTestAccount(t, client)
		member := createTestAccount(t, client)

		guild := createGuild(t, client, owner.Token, "Test Guild")

		channelID, err := strconv.ParseInt(guild.SystemChannel, 10, 64)
		if err != nil {
			t.Fatalf("failed to parse channel ID: %v", err)
		}

		invite := createChannelInvite(t, client, owner.Token, channelID)
		joinGuild(t, client, member.Token, invite.Code)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile", owner.UserID), member.Token)
		if err != nil {
			t.Fatalf("failed to get profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.deleteJSONWithAuth(fmt.Sprintf("/users/@me/guilds/%s", guild.ID), nil, member.Token)
		if err != nil {
			t.Fatalf("failed to leave guild: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/users/%s/profile", owner.UserID), member.Token)
		if err != nil {
			t.Fatalf("failed to attempt profile access: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 Forbidden after leaving guild, got %d", resp.StatusCode)
		}
		resp.Body.Close()
	})

	t.Run("removing friend removes profile access", func(t *testing.T) {
		user1 := createTestAccount(t, client)
		user2 := createTestAccount(t, client)

		createFriendship(t, client, user1, user2)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile", user2.UserID), user1.Token)
		if err != nil {
			t.Fatalf("failed to get profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.deleteJSONWithAuth(fmt.Sprintf("/users/@me/relationships/%s", user2.UserID), nil, user1.Token)
		if err != nil {
			t.Fatalf("failed to remove friend: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/users/%s/profile", user2.UserID), user1.Token)
		if err != nil {
			t.Fatalf("failed to attempt profile access: %v", err)
		}
		if resp.StatusCode != http.StatusForbidden {
			t.Fatalf("expected 403 Forbidden after removing friend, got %d", resp.StatusCode)
		}
		resp.Body.Close()
	})

	t.Run("with_mutual_friends returns mutual friends", func(t *testing.T) {
		user1 := createTestAccount(t, client)
		user2 := createTestAccount(t, client)
		mutualFriend := createTestAccount(t, client)

		createFriendship(t, client, user1, mutualFriend)
		createFriendship(t, client, user2, mutualFriend)

		createFriendship(t, client, user1, user2)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile?with_mutual_friends=true", user2.UserID), user1.Token)
		if err != nil {
			t.Fatalf("failed to get profile with mutual friends: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var profile struct {
			User          userPartial   `json:"user"`
			MutualFriends []userPartial `json:"mutual_friends"`
		}
		decodeJSONResponse(t, resp, &profile)

		if len(profile.MutualFriends) != 1 {
			t.Fatalf("expected 1 mutual friend, got %d", len(profile.MutualFriends))
		}
		if profile.MutualFriends[0].ID != mutualFriend.UserID {
			t.Fatalf("expected mutual friend ID %s, got %s", mutualFriend.UserID, profile.MutualFriends[0].ID)
		}
	})

	t.Run("with_mutual_friends defaults to false", func(t *testing.T) {
		user1 := createTestAccount(t, client)
		user2 := createTestAccount(t, client)
		mutualFriend := createTestAccount(t, client)

		createFriendship(t, client, user1, mutualFriend)
		createFriendship(t, client, user2, mutualFriend)
		createFriendship(t, client, user1, user2)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile", user2.UserID), user1.Token)
		if err != nil {
			t.Fatalf("failed to get profile: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var profile struct {
			User          userPartial   `json:"user"`
			MutualFriends []userPartial `json:"mutual_friends"`
		}
		decodeJSONResponse(t, resp, &profile)

		if profile.MutualFriends != nil {
			t.Fatalf("expected mutual_friends to be nil when not requested, got %v", profile.MutualFriends)
		}
	})

	t.Run("with_mutual_friends returns empty array when no mutual friends", func(t *testing.T) {
		user1 := createTestAccount(t, client)
		user2 := createTestAccount(t, client)

		createFriendship(t, client, user1, user2)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/%s/profile?with_mutual_friends=true", user2.UserID), user1.Token)
		if err != nil {
			t.Fatalf("failed to get profile with mutual friends: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var profile struct {
			User          userPartial   `json:"user"`
			MutualFriends []userPartial `json:"mutual_friends"`
		}
		decodeJSONResponse(t, resp, &profile)

		if len(profile.MutualFriends) != 0 {
			t.Fatalf("expected 0 mutual friends, got %d", len(profile.MutualFriends))
		}
	})
}
