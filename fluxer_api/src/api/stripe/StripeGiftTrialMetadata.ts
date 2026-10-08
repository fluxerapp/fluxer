// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';

const GIFT_TRIAL_KEY_PREFIX = 'gtrial_';
const GIFT_TRIAL_PAID_UNTIL_KEY = `${GIFT_TRIAL_KEY_PREFIX}paid_until`;

export function getGiftTrialMetadataKey(giftCode: string): string {
	return `${GIFT_TRIAL_KEY_PREFIX}${giftCode}`;
}

function isGiftTrialRunning(subscription: Stripe.Subscription, nowMs: number): boolean {
	return subscription.trial_end != null && subscription.trial_end * 1000 > nowMs;
}

export function buildGiftTrialMetadata(
	subscription: Stripe.Subscription,
	giftCode: string,
	baseUnix: number,
	newTrialEndUnix: number,
	nowMs: number,
): Record<string, string> {
	const metadata: Record<string, string> = {};
	if (!isGiftTrialRunning(subscription, nowMs)) {
		for (const key of Object.keys(subscription.metadata ?? {})) {
			if (key.startsWith(GIFT_TRIAL_KEY_PREFIX)) {
				metadata[key] = '';
			}
		}
		metadata[GIFT_TRIAL_PAID_UNTIL_KEY] = String(baseUnix);
	}
	metadata[getGiftTrialMetadataKey(giftCode)] = String(newTrialEndUnix - baseUnix);
	return metadata;
}

export function getGiftTrialSeconds(subscription: Stripe.Subscription, giftCode: string): number | null {
	const seconds = Number(subscription.metadata?.[getGiftTrialMetadataKey(giftCode)]);
	return Number.isFinite(seconds) && seconds > 0 ? seconds : null;
}

export function getGiftTrialPaidUntil(subscription: Stripe.Subscription, nowMs: number): Date | null {
	if (!isGiftTrialRunning(subscription, nowMs)) {
		return null;
	}
	const paidUntilUnix = Number(subscription.metadata?.[GIFT_TRIAL_PAID_UNTIL_KEY]);
	return Number.isFinite(paidUntilUnix) && paidUntilUnix > 0 ? new Date(paidUntilUnix * 1000) : null;
}
