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

// TestAuthIPAuthorizationResendInvalidTicket validates that attempting to
// resend with an invalid ticket fails appropriately.
func TestAuthIPAuthorizationResendInvalidTicket(t *testing.T) {
	client := newTestClient(t)

	resp, err := client.postJSON("/auth/ip-authorization/resend", map[string]string{
		"ticket": "invalid-ticket-xyz-12345",
	})
	if err != nil {
		t.Fatalf("failed to call resend endpoint: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusNoContent {
		t.Fatalf("expected invalid ticket to be rejected, got success status %d", resp.StatusCode)
	}

	if resp.StatusCode != http.StatusUnauthorized && resp.StatusCode != http.StatusBadRequest && resp.StatusCode != http.StatusNotFound {
		body := readResponseBody(resp)
		t.Fatalf("expected invalid ticket to return 401, 400, or 404, got %d: %s", resp.StatusCode, body)
	}
}
