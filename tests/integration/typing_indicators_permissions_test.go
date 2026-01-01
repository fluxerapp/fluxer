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

func TestTypingIndicatorsPermissions(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	createFriendship(t, client, user1, user2)
	channel := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	user3 := createTestAccount(t, client)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/typing", parseSnowflake(t, channel.ID)), nil, user3.Token)
	if err != nil {
		t.Fatalf("failed to send typing request: %v", err)
	}
	if resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected typing to fail for user without access to channel")
	}
	resp.Body.Close()
}
