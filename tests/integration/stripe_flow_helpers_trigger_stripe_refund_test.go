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
	"github.com/stripe/stripe-go/v79/refund"
	"testing"
)

// TriggerStripeRefund creates a refund for the provided payment intent.
func TriggerStripeRefund(t testing.TB, paymentIntentID string) {
	t.Helper()
	stripe.Key = RequireStripeSecret(t)

	params := &stripe.RefundParams{
		PaymentIntent: stripe.String(paymentIntentID),
	}
	params.Reason = stripe.String(string(stripe.RefundReasonRequestedByCustomer))

	if _, err := refund.New(params); err != nil {
		t.Fatalf("failed to create Stripe refund for %s: %v", paymentIntentID, err)
	}
}
