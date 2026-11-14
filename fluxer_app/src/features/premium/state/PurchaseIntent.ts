// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MS_PER_HOUR} from '@fluxer/date_utils/src/DateConstants';
import {isObjectLike} from '@fluxer/record_utils/src/RecordGuards';

export type PurchasePlan = 'monthly' | 'yearly' | 'gift_1_month' | 'gift_1_year';

const logger = new Logger('PurchaseIntent');
const PURCHASE_INTENT_KEY = 'fluxer.purchase_intent';

const PRODUCT_PLANS: Readonly<Record<string, PurchasePlan>> = Object.freeze({
	'plutonium-monthly': 'monthly',
	'plutonium-yearly': 'yearly',
	'gift-1-month': 'gift_1_month',
	'gift-1-year': 'gift_1_year',
});

export function planForCheckoutProduct(product: string): PurchasePlan | null {
	return Object.hasOwn(PRODUCT_PLANS, product) ? PRODUCT_PLANS[product] : null;
}

function parsePurchaseIntent(raw: string | null, now: number): PurchasePlan | null {
	if (raw === null) {
		return null;
	}
	try {
		const value: unknown = JSON.parse(raw);
		if (!isObjectLike(value)) {
			return null;
		}
		if (typeof value.expiresAt !== 'number' || value.expiresAt <= now) {
			return null;
		}
		if (typeof value.plan !== 'string') {
			return null;
		}
		switch (value.plan) {
			case 'monthly':
			case 'yearly':
			case 'gift_1_month':
			case 'gift_1_year':
				return value.plan;
			default:
				return null;
		}
	} catch (error) {
		if (!(error instanceof SyntaxError)) {
			logger.warn('Failed to decode purchase intent', error);
		}
		return null;
	}
}

export function rememberPurchaseIntent(plan: PurchasePlan): void {
	const now = Date.now();
	const intent = {plan, expiresAt: now + MS_PER_HOUR};
	try {
		getProtectedLocalStorage()?.setItem(PURCHASE_INTENT_KEY, JSON.stringify(intent));
	} catch (error) {
		logger.warn('Failed to store purchase intent', error);
	}
}

export function forgetPurchaseIntent(): void {
	try {
		getProtectedLocalStorage()?.removeItem(PURCHASE_INTENT_KEY);
	} catch (error) {
		logger.warn('Failed to remove purchase intent', error);
	}
}

export function peekPurchaseIntent(): PurchasePlan | null {
	const now = Date.now();
	try {
		const raw = getProtectedLocalStorage()?.getItem(PURCHASE_INTENT_KEY) ?? null;
		const plan = parsePurchaseIntent(raw, now);
		if (plan === null && raw !== null) {
			forgetPurchaseIntent();
		}
		return plan;
	} catch (error) {
		logger.warn('Failed to read purchase intent', error);
		return null;
	}
}

export function hasPurchaseIntent(): boolean {
	return peekPurchaseIntent() !== null;
}
