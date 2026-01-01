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

func TestUserBanner_Premium_ValidPNG(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	grantSubscriptionPremium(t, client, user.UserID)

	payload := map[string]any{
		"banner": "data:image/png;base64," + getValidPNGBase64(),
	}

	resp, err := client.patchJSONWithAuth("/users/@me", payload, user.Token)
	if err != nil {
		t.Fatalf("failed to update banner: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)

	var result struct {
		Banner string `json:"banner"`
	}
	decodeJSONResponse(t, resp, &result)

	if result.Banner == "" {
		t.Error("expected banner hash to be set")
	}
}
