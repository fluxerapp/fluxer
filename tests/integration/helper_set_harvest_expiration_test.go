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

type setHarvestExpirationPayload struct {
	ExpiresAt string `json:"expires_at"`
}

func setHarvestExpiration(t testing.TB, client *testClient, userID string, harvestID string, expiresAt string) {
	t.Helper()
	payload := setHarvestExpirationPayload{
		ExpiresAt: expiresAt,
	}
	resp, err := client.postJSON(fmt.Sprintf("/test/users/%s/harvest/%s/set-expiration", userID, harvestID), payload)
	if err != nil {
		t.Fatalf("failed to set harvest expiration: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}
