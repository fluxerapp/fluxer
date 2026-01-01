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
	"net/http"
	"testing"
	"time"
)

// TestUnclaimedAccountDeleteWithoutPassword verifies that unclaimed accounts
// can delete their accounts without providing a password.
func TestUnclaimedAccountDeleteWithoutPassword(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	unclaimAccount(t, client, account.UserID)

	resp, err := client.postJSONWithAuth("/users/@me/delete", map[string]interface{}{}, account.Token)
	if err != nil {
		t.Fatalf("failed to delete account: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("expected 204 for unclaimed account deletion without password, got %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	setPendingDeletionAt(t, client, account.UserID, time.Now().Add(-time.Minute))
	triggerDeletionWorker(t, client)
	waitForDeletionCompletion(t, client, account.UserID, 60*time.Second)
	verifyUserDataDeleted(t, client, account.UserID)

	t.Log("Unclaimed account deletion without password test passed")
}
