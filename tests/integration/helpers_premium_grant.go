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

const (
	PremiumTypeNone         = 0
	PremiumTypeSubscription = 1
	PremiumTypeLifetime     = 2
)

// setPremiumStatus updates the premium metadata for a user in the test harness.
func setPremiumStatus(t testing.TB, client *testClient, userID string, premiumType int, premiumUntil *time.Time) {
	t.Helper()
	payload := map[string]any{
		"premium_type": premiumType,
	}
	if premiumUntil != nil {
		payload["premium_until"] = premiumUntil.Format(time.RFC3339)
	} else {
		payload["premium_until"] = nil
	}

	resp, err := client.postJSON(fmt.Sprintf("/test/users/%s/premium", userID), payload)
	if err != nil {
		t.Fatalf("failed to set premium status: %v", err)
	}
	defer resp.Body.Close()
	assertStatus(t, resp, http.StatusOK)
}

// grantPremium grants premium status to a user via the test harness.
// premiumType: 0 = none, 1 = subscription, 2 = lifetime
func grantPremium(t testing.TB, client *testClient, userID string, premiumType int) {
	t.Helper()
	premiumUntil := time.Now().Add(30 * 24 * time.Hour)
	setPremiumStatus(t, client, userID, premiumType, &premiumUntil)
}

// revokePremium removes premium status from a user via the test harness.
func revokePremium(t testing.TB, client *testClient, userID string) {
	t.Helper()
	setPremiumStatus(t, client, userID, PremiumTypeNone, nil)
}
