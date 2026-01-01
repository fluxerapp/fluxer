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
)

// TestNonexistentResourceReturns404 tests 404 handling
func TestNonexistentResourceReturns404(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	resp, err := client.getWithAuth("/guilds/999999999999999999", user.Token)
	if err != nil {
		t.Fatalf("failed to check nonexistent guild: %v", err)
	}
	if resp.StatusCode != http.StatusNotFound && resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 404/403 for nonexistent guild, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/channels/999999999999999999", user.Token)
	if err != nil {
		t.Fatalf("failed to check nonexistent channel: %v", err)
	}
	if resp.StatusCode != http.StatusNotFound && resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 404/403 for nonexistent channel, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/999999999999999999", user.Token)
	if err != nil {
		t.Fatalf("failed to check nonexistent user: %v", err)
	}
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("expected 404 for nonexistent user, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
