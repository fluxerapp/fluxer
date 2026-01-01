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

// Covers invalid/expired reset token path.
func TestAuthResetPasswordInvalidToken(t *testing.T) {
	client := newTestClient(t)
	_ = createTestAccount(t, client)

	resp, err := client.postJSON("/auth/reset", map[string]string{
		"token":    "invalid-reset-token",
		"password": uniquePassword(),
	})
	if err != nil {
		t.Fatalf("failed to call reset password: %v", err)
	}
	if resp.StatusCode == http.StatusOK {
		t.Fatalf("expected invalid token to fail, got 200")
	}
	assertStatus(t, resp, http.StatusBadRequest)
	resp.Body.Close()
}
