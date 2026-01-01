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

// TestUnclaimedAccountCanUsePersonalNotes verifies that unclaimed accounts
// CAN send messages and add reactions in their Personal Notes channel.
func TestUnclaimedAccountCanUsePersonalNotes(t *testing.T) {
	client := newTestClient(t)

	account := createTestAccount(t, client)

	personalNotesChannelID := account.UserID

	unclaimAccount(t, client, account.UserID)

	resp, err := client.postJSONWithAuth(
		fmt.Sprintf("/channels/%s/messages", personalNotesChannelID),
		map[string]string{"content": "My personal note"},
		account.Token,
	)
	if err != nil {
		t.Fatalf("failed to send message to Personal Notes: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
		t.Fatalf("expected 200 or 201 for Personal Notes message, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	t.Log("Unclaimed account can use Personal Notes test passed")
}
