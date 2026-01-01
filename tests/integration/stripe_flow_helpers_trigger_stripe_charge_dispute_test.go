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
	"io"
	"net/http"
	"testing"
	"time"
)

// TriggerStripeChargeDispute initiates a dispute via Stripe test helper API.
func TriggerStripeChargeDispute(t testing.TB, chargeID string) {
	t.Helper()
	key := RequireStripeSecret(t)

	endpoint := fmt.Sprintf("https://api.stripe.com/v1/test_helpers/charges/%s/dispute", chargeID)
	req, err := http.NewRequest(http.MethodPost, endpoint, nil)
	if err != nil {
		t.Fatalf("failed to build dispute request: %v", err)
	}
	req.SetBasicAuth(key, "")

	client := &http.Client{Timeout: 15 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		t.Fatalf("charge dispute request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode >= 300 {
		body, _ := io.ReadAll(resp.Body)
		t.Fatalf("Stripe dispute helper returned %d: %s", resp.StatusCode, string(body))
	}
}
