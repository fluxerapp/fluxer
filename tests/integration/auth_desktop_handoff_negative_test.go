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

// Covers error paths for desktop handoff: unknown code and mismatched token.
func TestAuthDesktopHandoffNegativePaths(t *testing.T) {
	client := newTestClient(t)

	resp, err := client.get("/auth/handoff/unknown-code/status")
	if err != nil {
		t.Fatalf("failed to call handoff status: %v", err)
	}
	if resp.StatusCode == http.StatusOK {
		t.Fatalf("expected unknown handoff code to fail")
	}
	resp.Body.Close()

	resp, err = client.postJSON("/auth/handoff/complete", map[string]string{
		"code":    "bad-code",
		"token":   "bad-token",
		"user_id": "123",
	})
	if err != nil {
		t.Fatalf("failed to call handoff complete: %v", err)
	}
	if resp.StatusCode == http.StatusNoContent || resp.StatusCode == http.StatusOK {
		t.Fatalf("expected handoff complete with bad code/token to fail, got %d", resp.StatusCode)
	}
	resp.Body.Close()

	resp, err = client.delete("/auth/handoff/unknown-code", "")
	if err != nil {
		t.Fatalf("failed to call handoff cancel: %v", err)
	}
	if resp.StatusCode >= 500 {
		t.Fatalf("expected cancel unknown code not to 5xx, got %d", resp.StatusCode)
	}
	resp.Body.Close()
}
