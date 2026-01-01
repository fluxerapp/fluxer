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
	"testing"
)

func TestStripePremiumPriceIDs(t *testing.T) {
	client := newTestClient(t)

	t.Run("gets premium price IDs without country code", func(t *testing.T) {
		priceIDs := getPremiumPriceIDs(t, client, "")

		if priceIDs.Currency == "" {
			t.Fatal("currency should not be empty")
		}

		if priceIDs.Currency == "USD" && (priceIDs.Monthly == nil || *priceIDs.Monthly == "") {
			t.Fatal("monthly USD price ID should not be empty when currency is USD")
		}

		t.Logf("Price IDs: Currency=%s, Monthly=%v, Yearly=%v, Visionary=%v",
			priceIDs.Currency, priceIDs.Monthly, priceIDs.Yearly, priceIDs.Visionary)
	})

	t.Run("gets premium price IDs with US country code", func(t *testing.T) {
		priceIDs := getPremiumPriceIDs(t, client, "US")

		if priceIDs.Currency != "USD" {
			t.Fatalf("expected USD currency, got %s", priceIDs.Currency)
		}
	})

	t.Run("gets premium price IDs with EU country code", func(t *testing.T) {
		priceIDs := getPremiumPriceIDs(t, client, "DE")

		if priceIDs.Currency != "EUR" {
			t.Fatalf("expected EUR currency, got %s", priceIDs.Currency)
		}
	})
}
