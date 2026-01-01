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

func TestMessageReactionValidation(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	guild := createGuild(t, client, user.Token, "Reaction Validation Guild")
	channelID := parseSnowflake(t, guild.SystemChannel)
	message := sendChannelMessage(t, client, user.Token, channelID, "test message")

	t.Run("reject invalid limit parameters for GET reactions", func(t *testing.T) {
		testCases := []struct {
			name       string
			limitParam string
		}{
			{"limit too high", "limit=6000"},
			{"limit zero", "limit=0"},
			{"limit negative", "limit=-1"},
			{"limit non-integer", "limit=3.5"},
		}

		for _, tc := range testCases {
			t.Run(tc.name, func(t *testing.T) {
				resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/👍?%s",
					channelID, parseSnowflake(t, message.ID), tc.limitParam), user.Token)
				if err != nil {
					t.Fatalf("failed to make request: %v", err)
				}
				assertStatus(t, resp, http.StatusBadRequest)
				resp.Body.Close()
			})
		}
	})

	t.Run("accept valid limit and pagination parameters", func(t *testing.T) {
		resp, err := client.putWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/👍/@me",
			channelID, parseSnowflake(t, message.ID)), user.Token)
		if err != nil {
			t.Fatalf("failed to add reaction: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/👍?limit=10",
			channelID, parseSnowflake(t, message.ID)), user.Token)
		if err != nil {
			t.Fatalf("failed to get reactions with valid limit: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()

		resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d/reactions/👍?limit=50&after=%s",
			channelID, parseSnowflake(t, message.ID), user.UserID), user.Token)
		if err != nil {
			t.Fatalf("failed to get reactions with limit and after: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})
}
