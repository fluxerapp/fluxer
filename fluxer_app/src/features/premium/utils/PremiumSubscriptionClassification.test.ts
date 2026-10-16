// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	classifyPremiumSubscription,
	type PremiumSubscriptionClassificationInput,
} from '@app/features/premium/utils/PremiumSubscriptionClassification';
import {describe, expect, test} from 'vitest';

const NOW = Date.parse('2026-10-08T12:00:00Z');
const DAY = 24 * 60 * 60 * 1000;

function input(overrides: Partial<PremiumSubscriptionClassificationInput>): PremiumSubscriptionClassificationInput {
	return {
		hasPaidPremium: true,
		isVisionary: false,
		hasStoreSubscription: false,
		billingCycle: null,
		premiumUntil: new Date(NOW + 10 * DAY),
		isInGracePeriod: false,
		isFullyExpired: false,
		billingKnown: true,
		storeOwnershipKnown: true,
		billingSubscription: null,
		...overrides,
	};
}

describe('classifyPremiumSubscription', () => {
	test('active gift can start a subscription', () => {
		expect(classifyPremiumSubscription(input({}))).toEqual({
			isGiftSubscription: true,
			isGiftGrace: false,
			canStartSubscription: true,
		});
	});

	test('gift grace with no Stripe subscription', () => {
		expect(classifyPremiumSubscription(input({premiumUntil: new Date(NOW - DAY), isInGracePeriod: true}))).toEqual({
			isGiftSubscription: false,
			isGiftGrace: true,
			canStartSubscription: true,
		});
	});

	test('gift grace with an old cancelled subscription row', () => {
		expect(
			classifyPremiumSubscription(
				input({
					premiumUntil: new Date(NOW - DAY),
					isInGracePeriod: true,
					billingSubscription: {
						status: 'canceled',
						current_period_end: new Date(NOW - 60 * DAY).toISOString(),
					},
				}),
			),
		).toEqual({isGiftSubscription: false, isGiftGrace: true, canStartSubscription: true});
	});

	test('payment_failed grace on a past_due subscription', () => {
		expect(
			classifyPremiumSubscription(
				input({
					billingCycle: 'monthly',
					premiumUntil: new Date(NOW - DAY),
					isInGracePeriod: true,
					billingSubscription: {status: 'past_due', current_period_end: new Date(NOW + 29 * DAY).toISOString()},
				}),
			),
		).toEqual({isGiftSubscription: false, isGiftGrace: false, canStartSubscription: false});
	});

	test('grace after a subscription was deleted for failed payment is not gift grace', () => {
		expect(
			classifyPremiumSubscription(
				input({
					premiumUntil: new Date(NOW - DAY),
					isInGracePeriod: true,
					billingSubscription: {status: 'canceled', current_period_end: new Date(NOW + 29 * DAY).toISOString()},
				}),
			),
		).toEqual({isGiftSubscription: false, isGiftGrace: false, canStartSubscription: true});
	});

	test('store subscription cannot start a Stripe subscription', () => {
		expect(classifyPremiumSubscription(input({billingCycle: 'monthly', hasStoreSubscription: true}))).toEqual({
			isGiftSubscription: false,
			isGiftGrace: false,
			canStartSubscription: false,
		});
	});

	test('lifetime cannot start a subscription', () => {
		expect(classifyPremiumSubscription(input({isVisionary: true, premiumUntil: null}))).toEqual({
			isGiftSubscription: false,
			isGiftGrace: false,
			canStartSubscription: false,
		});
	});

	test('active Stripe subscription blocks a new one', () => {
		expect(
			classifyPremiumSubscription(
				input({
					billingCycle: 'yearly',
					billingSubscription: {status: 'active', current_period_end: new Date(NOW + 10 * DAY).toISOString()},
				}),
			),
		).toEqual({isGiftSubscription: false, isGiftGrace: false, canStartSubscription: false});
	});

	test('unknown billing falls back to the billing cycle', () => {
		expect(
			classifyPremiumSubscription(input({billingKnown: false, billingCycle: 'monthly'})).canStartSubscription,
		).toBe(false);
		expect(classifyPremiumSubscription(input({billingKnown: false})).canStartSubscription).toBe(true);
	});

	test('unknown billing lets a lapsed subscriber resubscribe', () => {
		expect(
			classifyPremiumSubscription(
				input({
					billingKnown: false,
					hasPaidPremium: false,
					billingCycle: 'yearly',
					premiumUntil: new Date(NOW - 35 * DAY),
					isFullyExpired: true,
				}),
			).canStartSubscription,
		).toBe(true);
		expect(
			classifyPremiumSubscription(
				input({
					billingKnown: false,
					hasPaidPremium: false,
					billingCycle: 'yearly',
					premiumUntil: new Date(NOW - 5 * DAY),
					isInGracePeriod: true,
				}),
			),
		).toEqual({isGiftSubscription: false, isGiftGrace: false, canStartSubscription: true});
	});

	test('paid premium cannot start a subscription before store ownership is known', () => {
		expect(
			classifyPremiumSubscription(input({billingKnown: false, storeOwnershipKnown: false})).canStartSubscription,
		).toBe(false);
		expect(
			classifyPremiumSubscription(
				input({billingKnown: false, storeOwnershipKnown: false, hasPaidPremium: false, premiumUntil: null}),
			).canStartSubscription,
		).toBe(true);
	});
});
