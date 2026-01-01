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

func TestAddRecipientPermissions(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)
	user3 := createTestAccount(t, client)
	user4 := createTestAccount(t, client)

	createFriendship(t, client, user1, user2)
	createFriendship(t, client, user1, user3)

	groupDmChannel := createGroupDmChannel(t, client, user1.Token, user2.UserID, user3.UserID)

	t.Run("cannot add non-friend", func(t *testing.T) {
		resp, err := client.putJSONWithAuth(fmt.Sprintf("/channels/%d/recipients/%s", parseSnowflake(t, groupDmChannel.ID), user4.UserID), nil, user1.Token)
		if err != nil {
			t.Fatalf("failed to send add recipient request: %v", err)
		}
		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusNoContent {
			t.Fatalf("expected adding non-friend to fail")
		}
		resp.Body.Close()
	})
}
