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

// TestAuthIPAuthorizationInvalidToken validates that attempting to authorize
// an IP with an invalid token fails with appropriate error.
func TestAuthIPAuthorizationInvalidToken(t *testing.T) {
	client := newTestClient(t)

	resp, err := client.postJSON("/auth/authorize-ip", map[string]string{"token": "invalid-token-12345"})
	if err != nil {
		t.Fatalf("failed to call authorize-ip endpoint: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected invalid token to be rejected, got 204 success")
	}

	if resp.StatusCode != http.StatusUnauthorized && resp.StatusCode != http.StatusBadRequest {
		body := readResponseBody(resp)
		t.Fatalf("expected invalid token to return 401 or 400, got %d: %s", resp.StatusCode, body)
	}
}
