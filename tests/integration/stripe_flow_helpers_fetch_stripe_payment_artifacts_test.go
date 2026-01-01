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
	checkoutsession "github.com/stripe/stripe-go/v79/checkout/session"
	"testing"
)

// FetchStripePaymentArtifacts resolves related Stripe objects for the checkout session.
func FetchStripePaymentArtifacts(t testing.TB, sessionID string) StripePaymentArtifacts {
	t.Helper()
	stripe.Key = RequireStripeSecret(t)

	params := &stripe.CheckoutSessionParams{}
	params.AddExpand("payment_intent")
	params.AddExpand("payment_intent.latest_charge")
	params.AddExpand("subscription")
	params.AddExpand("customer")

	session, err := checkoutsession.Get(sessionID, params)
	if err != nil {
		t.Fatalf("failed to fetch checkout session %s: %v", sessionID, err)
	}

	var paymentIntentID, chargeID, subscriptionID, customerID string
	if session.PaymentIntent != nil {
		paymentIntentID = session.PaymentIntent.ID
		if session.PaymentIntent.LatestCharge != nil {
			chargeID = session.PaymentIntent.LatestCharge.ID
		}
	}
	if session.Subscription != nil {
		subscriptionID = session.Subscription.ID
	}
	if session.Customer != nil {
		customerID = session.Customer.ID
	}

	return StripePaymentArtifacts{
		SessionID:       sessionID,
		PaymentIntentID: paymentIntentID,
		SubscriptionID:  subscriptionID,
		ChargeID:        chargeID,
		CustomerID:      customerID,
	}
}
