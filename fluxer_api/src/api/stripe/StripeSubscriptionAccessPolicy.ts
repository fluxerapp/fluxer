// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';

const PROVISIONABLE_SUBSCRIPTION_STATUSES: ReadonlySet<Stripe.Subscription.Status> = new Set(['active', 'trialing']);
const ACCESS_AFFECTING_INVOICE_FAILURE_REASONS = new Set(['subscription_create', 'subscription_cycle']);

export function canProvisionPremiumFromSubscriptionStatus(
	status: Stripe.Subscription.Status | null | undefined,
): boolean {
	return status != null && PROVISIONABLE_SUBSCRIPTION_STATUSES.has(status);
}

export function getPremiumWillCancelFromSubscription(subscription: {
	status: Stripe.Subscription.Status;
	cancel_at?: number | null;
	cancel_at_period_end?: boolean | null;
}): boolean {
	if (!canProvisionPremiumFromSubscriptionStatus(subscription.status)) {
		return true;
	}
	return Boolean(subscription.cancel_at_period_end || subscription.cancel_at != null);
}

export function shouldTreatInvoicePaymentFailureAsAccessChange(invoice: Stripe.Invoice): boolean {
	const billingReason = invoice.billing_reason ?? null;
	return billingReason != null && ACCESS_AFFECTING_INVOICE_FAILURE_REASONS.has(billingReason);
}

export function shouldTreatInvoiceUpdatedAsCollectionIssue(invoice: Stripe.Invoice): boolean {
	if (!shouldTreatInvoicePaymentFailureAsAccessChange(invoice)) {
		return false;
	}
	if (invoice.status !== 'open') {
		return false;
	}
	const attemptCount = invoice.attempt_count ?? 0;
	if (attemptCount > 0) {
		return true;
	}
	if (invoice.attempted === true) {
		return true;
	}
	return invoice.next_payment_attempt != null;
}
