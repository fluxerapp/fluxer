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

func TestEmojiCreateExpressionsPermission(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	memberWithCreate := createTestAccount(t, client)
	memberWithManage := createTestAccount(t, client)
	memberWithNeither := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, memberWithCreate.Token)
	ensureSessionStarted(t, client, memberWithManage.Token)
	ensureSessionStarted(t, client, memberWithNeither.Token)

	guild := createGuild(t, client, owner.Token, "Create Expressions Guild")
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	for _, member := range []testAccount{memberWithCreate, memberWithManage, memberWithNeither} {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, member.Token)
		if err != nil {
			t.Fatalf("failed to accept invite: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	}

	// Permission bits
	const (
		createExpressions = 1 << 43
		manageExpressions = 1 << 30
	)

	defaultWithoutCreate := 137439396353

	defaultWithManageInsteadOfCreate := defaultWithoutCreate | manageExpressions

	// Create roles for each member
	var roleForCreate, roleForManage, roleForNeither struct {
		ID string `json:"id"`
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name": "Creator",
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &roleForCreate)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "Manager",
		"permissions": fmt.Sprintf("%d", defaultWithManageInsteadOfCreate),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &roleForManage)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), map[string]any{
		"name":        "NoExpressions",
		"permissions": fmt.Sprintf("%d", defaultWithoutCreate),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &roleForNeither)

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/roles/%d", guildID, guildID), map[string]any{
		"permissions": fmt.Sprintf("%d", defaultWithoutCreate),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to update @everyone role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	memberWithCreateID := parseSnowflake(t, memberWithCreate.UserID)
	memberWithManageID := parseSnowflake(t, memberWithManage.UserID)
	memberWithNeitherID := parseSnowflake(t, memberWithNeither.UserID)

	resp, err = client.requestJSON(http.MethodPut, fmt.Sprintf("/guilds/%d/members/%d/roles/%s", guildID, memberWithCreateID, roleForCreate.ID), nil, owner.Token)
	if err != nil {
		t.Fatalf("failed to assign role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.requestJSON(http.MethodPut, fmt.Sprintf("/guilds/%d/members/%d/roles/%s", guildID, memberWithManageID, roleForManage.ID), nil, owner.Token)
	if err != nil {
		t.Fatalf("failed to assign role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.requestJSON(http.MethodPut, fmt.Sprintf("/guilds/%d/members/%d/roles/%s", guildID, memberWithNeitherID, roleForNeither.ID), nil, owner.Token)
	if err != nil {
		t.Fatalf("failed to assign role: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	emojiImage := loadFixtureAsDataURL(t, "yeah.png", "image/png")

	t.Run("member with CREATE_EXPRESSIONS can create emoji", func(t *testing.T) {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), map[string]string{
			"name":  "creatoremoji",
			"image": emojiImage,
		}, memberWithCreate.Token)
		if err != nil {
			t.Fatalf("failed to create emoji: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("member without CREATE_EXPRESSIONS cannot create emoji", func(t *testing.T) {
		resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), map[string]string{
			"name":  "nopermsemoji",
			"image": emojiImage,
		}, memberWithNeither.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	// Create emoji by memberWithCreate to test edit/delete permissions
	var creatorEmoji struct {
		ID string `json:"id"`
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), map[string]string{
		"name":  "myemoji",
		"image": emojiImage,
	}, memberWithCreate.Token)
	if err != nil {
		t.Fatalf("failed to create emoji: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &creatorEmoji)

	// Create emoji by owner to test cross-user permissions
	var ownerEmoji struct {
		ID string `json:"id"`
	}
	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis", guildID), map[string]string{
		"name":  "owneremoji",
		"image": emojiImage,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to create emoji: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &ownerEmoji)

	t.Run("creator can update their own emoji with CREATE_EXPRESSIONS", func(t *testing.T) {
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, creatorEmoji.ID), map[string]string{
			"name": "myupdatedemoji",
		}, memberWithCreate.Token)
		if err != nil {
			t.Fatalf("failed to update emoji: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("creator cannot update another user's emoji with only CREATE_EXPRESSIONS", func(t *testing.T) {
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, ownerEmoji.ID), map[string]string{
			"name": "hackedemoji",
		}, memberWithCreate.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("member with MANAGE_EXPRESSIONS can update another user's emoji", func(t *testing.T) {
		resp, err := client.patchJSONWithAuth(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, ownerEmoji.ID), map[string]string{
			"name": "managedupdated",
		}, memberWithManage.Token)
		if err != nil {
			t.Fatalf("failed to update emoji: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("creator cannot delete another user's emoji with only CREATE_EXPRESSIONS", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, ownerEmoji.ID), memberWithCreate.Token)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		assertStatus(t, resp, http.StatusForbidden)
		resp.Body.Close()
	})

	t.Run("member with MANAGE_EXPRESSIONS can delete another user's emoji", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, ownerEmoji.ID), memberWithManage.Token)
		if err != nil {
			t.Fatalf("failed to delete emoji: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})

	t.Run("creator can delete their own emoji with CREATE_EXPRESSIONS", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/guilds/%d/emojis/%s", guildID, creatorEmoji.ID), memberWithCreate.Token)
		if err != nil {
			t.Fatalf("failed to delete emoji: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
