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
	"net/url"
	"testing"
)

func TestEmoji_ReactionValidation_Guild(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Reaction Test Guild")
	guildID := parseSnowflake(t, guild.ID)

	resp, err := client.getWithAuth(fmt.Sprintf("/guilds/%d", guildID), user.Token)
	if err != nil {
		t.Fatalf("failed to get guild: %v", err)
	}
	var guildData struct {
		SystemChannelID string `json:"system_channel_id"`
	}
	decodeJSONResponse(t, resp, &guildData)
	resp.Body.Close()
	systemChannelID := parseSnowflake(t, guildData.SystemChannelID)

	msg := sendChannelMessage(t, client, user.Token, systemChannelID, "Guild react test")
	msgID := msg.ID

	t.Run("unicode emoji allowed in guild", func(t *testing.T) {
		emoji := url.PathEscape("👍")

		resp, err := client.putWithAuth(
			fmt.Sprintf("/channels/%d/messages/%s/reactions/%s/@me", systemChannelID, msgID, emoji),
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to add reaction: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusNoContent {
			t.Errorf("expected guild reaction to succeed, got %d", resp.StatusCode)
		}
	})

	t.Run("external emoji requires premium in guild", func(t *testing.T) {
		emoji := url.PathEscape("external:888888888888888888")

		resp, err := client.putWithAuth(
			fmt.Sprintf("/channels/%d/messages/%s/reactions/%s/@me", systemChannelID, msgID, emoji),
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusNoContent {
			t.Error("expected external emoji to require premium in guild")
		}
	})

	t.Run("premium user with permission can use external emoji", func(t *testing.T) {
		premiumUser := createTestAccount(t, client)
		ensureSessionStarted(t, client, premiumUser.Token)
		grantPremium(t, client, premiumUser.UserID, PremiumTypeSubscription)

		emojiGuild := createGuild(t, client, premiumUser.Token, "Emoji Source Guild")
		emojiGuildID := parseSnowflake(t, emojiGuild.ID)
		emojiObj := createGuildEmoji(t, client, premiumUser.Token, emojiGuildID, "premium_ext")

		invite := createChannelInvite(t, client, user.Token, systemChannelID)
		joinGuild(t, client, premiumUser.Token, invite.Code)

		emoji := url.PathEscape(fmt.Sprintf("premium_ext:%s", emojiObj.ID))

		resp, err := client.putWithAuth(
			fmt.Sprintf("/channels/%d/messages/%s/reactions/%s/@me", systemChannelID, msgID, emoji),
			premiumUser.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusNoContent && resp.StatusCode != http.StatusOK {
			t.Errorf("expected premium user to add external emoji reaction, got status %d", resp.StatusCode)
		}
	})
}
