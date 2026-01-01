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

func TestDMChannelManagement(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	ensureSessionStarted(t, client, user1.Token)
	ensureSessionStarted(t, client, user2.Token)
	createFriendship(t, client, user1, user2)

	t.Run("can create DM channel", func(t *testing.T) {
		dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))
		if dm.ID == "" {
			t.Error("expected DM channel ID")
		}
	})

	dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))
	dmID := parseSnowflake(t, dm.ID)

	t.Run("can get DM channel", func(t *testing.T) {
		resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d", dmID), user1.Token)
		if err != nil {
			t.Fatalf("failed to get DM: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)
		resp.Body.Close()
	})

	t.Run("can close DM channel", func(t *testing.T) {
		resp, err := client.delete(fmt.Sprintf("/channels/%d", dmID), user1.Token)
		if err != nil {
			t.Fatalf("failed to close DM: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()
	})
}
