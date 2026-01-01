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
	"time"
)

func TestHarvestDownloadExpired(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	harvestResp, err := client.postJSONWithAuth("/users/@me/harvest", nil, user.Token)
	if err != nil {
		t.Fatalf("failed to request data harvest: %v", err)
	}
	assertStatus(t, harvestResp, http.StatusOK)
	var harvestRequest struct {
		HarvestID string `json:"harvestId"`
	}
	decodeJSONResponse(t, harvestResp, &harvestRequest)

	waitForCondition(t, 90*time.Second, func() (bool, error) {
		status := fetchHarvestStatus(t, client, user.Token, harvestRequest.HarvestID)
		if status.CompletedAt != nil && status.DownloadURLExpiresAt != nil {
			return true, nil
		}
		if status.FailedAt != nil {
			return false, fmt.Errorf("harvest failed: %v", *status.ErrorMessage)
		}
		return false, nil
	})

	status := fetchHarvestStatus(t, client, user.Token, harvestRequest.HarvestID)
	if status.CompletedAt == nil {
		t.Fatalf("expected harvest to complete")
	}

	expiredTime := time.Now().Add(-1 * time.Hour)
	setHarvestExpiration(t, client, user.UserID, harvestRequest.HarvestID, expiredTime.Format(time.RFC3339))

	resp, err := client.getWithAuth(fmt.Sprintf("/users/@me/harvest/%s/download", harvestRequest.HarvestID), user.Token)
	if err != nil {
		t.Fatalf("failed to fetch harvest download: %v", err)
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected status 400, got %d", resp.StatusCode)
	}

	var errorResponse struct {
		Code    string `json:"code"`
		Message string `json:"message"`
	}
	decodeJSONResponse(t, resp, &errorResponse)
	if errorResponse.Code != "HARVEST_EXPIRED" {
		t.Fatalf("expected error code HARVEST_EXPIRED, got %s", errorResponse.Code)
	}
}
