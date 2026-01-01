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

import "testing"

func TestGroupDMManagement(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)

	ensureSessionStarted(t, client, user1.Token)
	ensureSessionStarted(t, client, user2.Token)
	ensureSessionStarted(t, client, user3.Token)
	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)

	t.Run("can create group DM with multiple recipients", func(t *testing.T) {
		groupDM := createGroupDmChannel(t, client, user1.Token, user2.UserID, user3.UserID)
		if groupDM.ID == "" {
			t.Error("expected group DM channel ID")
		}
	})
}
