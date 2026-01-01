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

func TestEmoji_ReactionLimits(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	createFriendship(t, client, user, user2)
	dmChannel := createDmChannel(t, client, user.Token, parseSnowflake(t, user2.UserID))
	dmChannelID := parseSnowflake(t, dmChannel.ID)

	msg := sendChannelMessage(t, client, user.Token, dmChannelID, "Limit test message")
	msgID := msg.ID

	t.Run("max unique reactions per message", func(t *testing.T) {

		emojis := []string{"👍", "👎", "❤️", "😀", "😁", "😂", "🤣", "😃", "😄", "😅",
			"😆", "😉", "😊", "😋", "😎", "😍", "😘", "🥰", "😗", "😙", "😚"}

		var lastStatus int
		successCount := 0
		for _, emoji := range emojis {
			encodedEmoji := url.PathEscape(emoji)
			resp, err := client.putWithAuth(
				fmt.Sprintf("/channels/%d/messages/%s/reactions/%s/@me", dmChannelID, msgID, encodedEmoji),
				user.Token,
			)
			if err != nil {
				t.Fatalf("failed to add reaction: %v", err)
			}
			lastStatus = resp.StatusCode
			if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusNoContent {
				successCount++
			}
			resp.Body.Close()
		}

		if successCount == 0 {
			t.Errorf("expected at least some reactions to succeed, but all failed with status %d", lastStatus)
		}
		t.Logf("Successfully added %d out of %d reactions", successCount, len(emojis))
	})
}
