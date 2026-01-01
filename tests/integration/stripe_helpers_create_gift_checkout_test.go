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

// CreateGiftCheckout creates a Stripe checkout session for a gift purchase.
func CreateGiftCheckout(t testing.TB, client *testClient, token, priceID string) CheckoutResponse {
	t.Helper()

	req := CreateCheckoutRequest{
		PriceID: priceID,
	}

	resp, err := client.postJSONWithAuth("/stripe/checkout/gift", req, token)
	if err != nil {
		t.Fatalf("failed to create gift checkout: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusOK)

	var checkout CheckoutResponse
	decodeJSONResponse(t, resp, &checkout)

	if checkout.URL == "" {
		t.Fatal("checkout URL is empty")
	}

	return checkout
}
