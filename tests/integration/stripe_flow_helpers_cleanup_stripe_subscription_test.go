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
	stripe "github.com/stripe/stripe-go/v79"
	sub "github.com/stripe/stripe-go/v79/subscription"
	"testing"
)

// CleanupStripeSubscription cancels a subscription to avoid leaking resources.
func CleanupStripeSubscription(t testing.TB, subscriptionID string) {
	t.Helper()
	if subscriptionID == "" {
		return
	}

	stripe.Key = RequireStripeSecret(t)
	params := &stripe.SubscriptionCancelParams{}
	params.InvoiceNow = stripe.Bool(false)
	params.Prorate = stripe.Bool(false)

	if _, err := sub.Cancel(subscriptionID, params); err != nil {
		t.Logf("failed to clean up subscription %s: %v", subscriptionID, err)
	}
}
