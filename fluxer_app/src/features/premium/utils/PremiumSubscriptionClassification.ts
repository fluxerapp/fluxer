// SPDX-License-Identifier: AGPL-3.0-or-later

const BLOCKING_SUBSCRIPTION_STATUSES: ReadonlySet<string> = new Set([
	'active',
	'trialing',
	'past_due',
	'unpaid',
	'incomplete',
	'paused',
]);

const RENEWING_SUBSCRIPTION_STATUSES: ReadonlySet<string> = new Set(['active', 'trialing']);

export interface BillingSubscriptionPeriod {
	status: string | null;
	current_period_end: string | null;
}

export interface PremiumSubscriptionClassificationInput {
	hasPaidPremium: boolean;
	isVisionary: boolean;
	hasStoreSubscription: boolean;
	billingCycle: string | null;
	premiumUntil: Date | null;
	isInGracePeriod: boolean;
	isFullyExpired: boolean;
	billingKnown: boolean;
	storeOwnershipKnown: boolean;
	billingSubscription: BillingSubscriptionPeriod | null;
}

export interface PremiumSubscriptionClassification {
	isGiftSubscription: boolean;
	isGiftGrace: boolean;
	canStartSubscription: boolean;
}

export function classifyPremiumSubscription(
	input: PremiumSubscriptionClassificationInput,
): PremiumSubscriptionClassification {
	const hasBlockingSubscription = input.billingKnown
		? BLOCKING_SUBSCRIPTION_STATUSES.has(input.billingSubscription?.status ?? '')
		: input.billingCycle != null && input.hasPaidPremium && !input.isFullyExpired;
	const subscriptionPeriodEnd = input.billingSubscription?.current_period_end
		? new Date(input.billingSubscription.current_period_end)
		: null;
	const isPaidLapse = input.billingKnown
		? subscriptionPeriodEnd != null &&
			input.premiumUntil != null &&
			subscriptionPeriodEnd.getTime() >= input.premiumUntil.getTime()
		: input.billingCycle != null;
	const canStartSubscription =
		!input.isVisionary &&
		!input.hasStoreSubscription &&
		!hasBlockingSubscription &&
		(input.storeOwnershipKnown || !input.hasPaidPremium);
	return {
		isGiftSubscription:
			input.hasPaidPremium &&
			!input.isVisionary &&
			input.billingCycle == null &&
			input.premiumUntil != null &&
			!input.isInGracePeriod &&
			!input.isFullyExpired,
		isGiftGrace: input.isInGracePeriod && canStartSubscription && !isPaidLapse,
		canStartSubscription,
	};
}

export function getSubscriptionRenewalDate(
	premiumUntil: Date | null,
	billingSubscription: BillingSubscriptionPeriod | null,
): Date | null {
	if (
		!billingSubscription?.current_period_end ||
		!RENEWING_SUBSCRIPTION_STATUSES.has(billingSubscription.status ?? '')
	) {
		return premiumUntil;
	}
	const periodEnd = new Date(billingSubscription.current_period_end);
	return Number.isNaN(periodEnd.getTime()) ? premiumUntil : periodEnd;
}
