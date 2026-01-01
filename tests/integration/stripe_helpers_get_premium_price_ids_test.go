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

// getPremiumPriceIDs retrieves premium price IDs, optionally filtered by country
func getPremiumPriceIDs(t testing.TB, client *testClient, countryCode string) premiumPriceIDs {
	t.Helper()

	url := "/premium/price-ids"
	if countryCode != "" {
		url += fmt.Sprintf("?country_code=%s", countryCode)
	}

	resp, err := client.get(url)
	if err != nil {
		t.Fatalf("failed to get premium price IDs: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusOK)

	var priceIDs premiumPriceIDs
	decodeJSONResponse(t, resp, &priceIDs)

	return priceIDs
}
