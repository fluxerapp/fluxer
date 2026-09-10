// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, describe, expect, test} from 'vitest';
import {Config} from '../../Config';
import {ProductRegistry, ProductType} from '../ProductRegistry';

// Mirrors the live BRL/TRY configuration after the repricing: the BRL slots point at the new list
// prices, TRY is not configured at all (production has no TRY prices), and every retired price id
// lives only in the legacy map.
const MOCK_PRICES = {
	monthlyUsd: 'price_monthly_usd',
	monthlyBrl: 'price_1TMbqsFPC94Os7FdfElrIIaZ',
	yearlyUsd: 'price_yearly_usd',
	yearlyBrl: 'price_1TMbqtFPC94Os7Fd7VJqmDyt',
	gift1MonthBrl: 'price_gift_1_month_brl',
	gift1YearBrl: 'price_gift_1_year_brl',
};

const LEGACY_MONTHLY_BRL = 'price_legacy_monthly_brl';
const LEGACY_YEARLY_BRL = 'price_legacy_yearly_brl';
const LEGACY_GIFT_1_MONTH_BRL = 'price_legacy_gift_1_month_brl';
const LEGACY_GIFT_1_YEAR_BRL = 'price_legacy_gift_1_year_brl';
const LEGACY_MONTHLY_TRY = 'price_1TMYpdFPC94Os7FdZVRx98Up';

const MOCK_LEGACY_PRICES: Record<string, Array<string> | undefined> = {
	monthly_brl: [LEGACY_MONTHLY_BRL],
	yearly_brl: [LEGACY_YEARLY_BRL],
	gift_1_month_brl: [LEGACY_GIFT_1_MONTH_BRL],
	gift_1_year_brl: [LEGACY_GIFT_1_YEAR_BRL],
	monthly_try: [LEGACY_MONTHLY_TRY],
};

describe('ProductRegistry - legacy prices', () => {
	let originalPrices: typeof Config.stripe.prices | undefined;
	let originalLegacyPrices: typeof Config.stripe.legacyPrices | undefined;

	beforeAll(() => {
		originalPrices = Config.stripe.prices;
		originalLegacyPrices = Config.stripe.legacyPrices;
	});

	afterAll(() => {
		Config.stripe.prices = originalPrices;
		Config.stripe.legacyPrices = originalLegacyPrices;
	});

	function buildRegistry(
		prices: typeof Config.stripe.prices,
		legacyPrices: typeof Config.stripe.legacyPrices,
	): ProductRegistry {
		Config.stripe.prices = prices;
		Config.stripe.legacyPrices = legacyPrices;
		return new ProductRegistry();
	}

	describe('slot shapes', () => {
		test('a legacy monthly slot resolves to a monthly recurring product in its currency', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);
			expect(registry.getProduct(LEGACY_MONTHLY_BRL)).toEqual({
				type: ProductType.MONTHLY_SUBSCRIPTION,
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				durationMonths: 1,
				isGift: false,
				currency: 'BRL',
				billingCycle: 'monthly',
			});
		});

		test('a legacy yearly slot resolves to a yearly recurring product in its currency', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);
			expect(registry.getProduct(LEGACY_YEARLY_BRL)).toEqual({
				type: ProductType.YEARLY_SUBSCRIPTION,
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				durationMonths: 12,
				isGift: false,
				currency: 'BRL',
				billingCycle: 'yearly',
			});
		});

		test('a legacy gift_1_month slot resolves to a one-month gift with no billing cycle', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);
			const info = registry.getProduct(LEGACY_GIFT_1_MONTH_BRL);
			expect(info).toEqual({
				type: ProductType.GIFT_1_MONTH,
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				durationMonths: 1,
				isGift: true,
				currency: 'BRL',
			});
			expect(info?.billingCycle).toBeUndefined();
		});

		test('a legacy gift_1_year slot resolves to a one-year gift with no billing cycle', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);
			const info = registry.getProduct(LEGACY_GIFT_1_YEAR_BRL);
			expect(info).toEqual({
				type: ProductType.GIFT_1_YEAR,
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				durationMonths: 12,
				isGift: true,
				currency: 'BRL',
			});
			expect(info?.billingCycle).toBeUndefined();
		});

		test('the currency comes from the slot suffix, not from the price id', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);
			// Same shape of id, different slot: only the slot suffix decides the currency.
			expect(registry.getProduct(LEGACY_MONTHLY_TRY)?.currency).toBe('TRY');
			expect(registry.getProduct(LEGACY_MONTHLY_BRL)?.currency).toBe('BRL');
		});

		test('every supported currency suffix is recognised', () => {
			const registry = buildRegistry(
				{},
				{
					monthly_usd: ['legacy_usd'],
					monthly_eur: ['legacy_eur'],
					monthly_brl: ['legacy_brl'],
					monthly_inr: ['legacy_inr'],
					monthly_pln: ['legacy_pln'],
					monthly_try: ['legacy_try'],
				},
			);
			expect(registry.getProduct('legacy_usd')?.currency).toBe('USD');
			expect(registry.getProduct('legacy_eur')?.currency).toBe('EUR');
			expect(registry.getProduct('legacy_brl')?.currency).toBe('BRL');
			expect(registry.getProduct('legacy_inr')?.currency).toBe('INR');
			expect(registry.getProduct('legacy_pln')?.currency).toBe('PLN');
			expect(registry.getProduct('legacy_try')?.currency).toBe('TRY');
		});

		test('an uppercased currency suffix is still recognised', () => {
			const registry = buildRegistry({}, {monthly_BRL: ['legacy_upper_brl']});
			expect(registry.getProduct('legacy_upper_brl')?.currency).toBe('BRL');
		});

		test('a slot may retire more than one price id', () => {
			const registry = buildRegistry({}, {monthly_brl: ['legacy_one', 'legacy_two', 'legacy_three']});
			expect(registry.getProduct('legacy_one')?.type).toBe(ProductType.MONTHLY_SUBSCRIPTION);
			expect(registry.getProduct('legacy_two')?.type).toBe(ProductType.MONTHLY_SUBSCRIPTION);
			expect(registry.getProduct('legacy_three')?.type).toBe(ProductType.MONTHLY_SUBSCRIPTION);
		});

		test('legacy prices register even when no prices are configured at all', () => {
			const registry = buildRegistry(undefined, MOCK_LEGACY_PRICES);
			expect(registry.getProduct(LEGACY_MONTHLY_BRL)?.type).toBe(ProductType.MONTHLY_SUBSCRIPTION);
			expect(registry.getProduct(MOCK_PRICES.monthlyBrl)).toBeNull();
		});

		test('an unknown price id still resolves to null', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);
			expect(registry.getProduct('price_never_seen')).toBeNull();
		});
	});

	describe('honoured but unpurchasable', () => {
		test('a legacy recurring price resolves through getProduct but is never the checkout price', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);

			// Direction 1: the retired price is honoured on renewal.
			expect(registry.getProduct(LEGACY_MONTHLY_BRL)).not.toBeNull();
			expect(registry.getProduct(LEGACY_YEARLY_BRL)).not.toBeNull();

			// Direction 2: checkout only ever offers the authored list price.
			expect(registry.getRecurringSubscriptionPriceId('monthly', 'BRL')).toBe(MOCK_PRICES.monthlyBrl);
			expect(registry.getRecurringSubscriptionPriceId('monthly', 'BRL')).not.toBe(LEGACY_MONTHLY_BRL);
			expect(registry.getRecurringSubscriptionPriceId('yearly', 'BRL')).toBe(MOCK_PRICES.yearlyBrl);
			expect(registry.getRecurringSubscriptionPriceId('yearly', 'BRL')).not.toBe(LEGACY_YEARLY_BRL);
		});

		test('a legacy gift price resolves through getProduct but is never the gift checkout price', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);

			expect(registry.getProduct(LEGACY_GIFT_1_MONTH_BRL)).not.toBeNull();
			expect(registry.getProduct(LEGACY_GIFT_1_YEAR_BRL)).not.toBeNull();

			expect(registry.getGiftPriceId('gift_1_month', 'BRL')).toBe(MOCK_PRICES.gift1MonthBrl);
			expect(registry.getGiftPriceId('gift_1_month', 'BRL')).not.toBe(LEGACY_GIFT_1_MONTH_BRL);
			expect(registry.getGiftPriceId('gift_1_year', 'BRL')).toBe(MOCK_PRICES.gift1YearBrl);
			expect(registry.getGiftPriceId('gift_1_year', 'BRL')).not.toBe(LEGACY_GIFT_1_YEAR_BRL);
		});

		test('an archived price in a currency the instance does not configure renews but cannot be bought', () => {
			// The real production case: one live subscription sits on an archived TRY price that
			// production never configured. getProduct must resolve it; checkout must still refuse TRY.
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);

			expect(registry.getProduct(LEGACY_MONTHLY_TRY)).toEqual({
				type: ProductType.MONTHLY_SUBSCRIPTION,
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				durationMonths: 1,
				isGift: false,
				currency: 'TRY',
				billingCycle: 'monthly',
			});
			expect(registry.getRecurringSubscriptionPriceId('monthly', 'TRY')).toBeNull();
			expect(registry.getRecurringSubscriptionPriceId('yearly', 'TRY')).toBeNull();
			expect(registry.getGiftPriceId('gift_1_month', 'TRY')).toBeNull();
			expect(registry.getGiftPriceId('gift_1_year', 'TRY')).toBeNull();
		});

		test('no legacy price id is ever returned by either price getter, for any cycle or currency', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);
			const legacyIds = new Set(Object.values(MOCK_LEGACY_PRICES).flatMap((ids) => ids ?? []));
			const currencies = ['USD', 'EUR', 'BRL', 'INR', 'PLN', 'TRY'];

			const offered: Array<string> = [];
			for (const currency of currencies) {
				for (const cycle of ['monthly', 'yearly'] as const) {
					const priceId = registry.getRecurringSubscriptionPriceId(cycle, currency);
					if (priceId) offered.push(priceId);
				}
				for (const duration of ['gift_1_month', 'gift_1_year'] as const) {
					const priceId = registry.getGiftPriceId(duration, currency);
					if (priceId) offered.push(priceId);
				}
			}

			expect(offered.length).toBeGreaterThan(0);
			expect(offered.filter((priceId) => legacyIds.has(priceId))).toEqual([]);
			// And every legacy id is still honoured by the registry.
			for (const legacyId of legacyIds) {
				expect(registry.getProduct(legacyId)).not.toBeNull();
			}
		});

		test('a legacy recurring product is still classified as a recurring subscription', () => {
			const registry = buildRegistry(MOCK_PRICES, MOCK_LEGACY_PRICES);
			const recurring = registry.getProduct(LEGACY_MONTHLY_BRL);
			const gift = registry.getProduct(LEGACY_GIFT_1_YEAR_BRL);
			expect(recurring).not.toBeNull();
			expect(gift).not.toBeNull();
			expect(registry.isRecurringSubscription(recurring!)).toBe(true);
			expect(registry.isRecurringSubscription(gift!)).toBe(false);
		});
	});

	describe('precedence', () => {
		test('an authored price wins over a legacy entry claiming the same id', () => {
			// Deliberate collision: the live monthly BRL list price is also listed as a retired yearly
			// USD price. The authored ProductInfo must survive.
			const registry = buildRegistry(MOCK_PRICES, {
				yearly_usd: [MOCK_PRICES.monthlyBrl],
			});
			expect(registry.getProduct(MOCK_PRICES.monthlyBrl)).toEqual({
				type: ProductType.MONTHLY_SUBSCRIPTION,
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				durationMonths: 1,
				isGift: false,
				currency: 'BRL',
				billingCycle: 'monthly',
			});
		});

		test('an authored gift price wins over a legacy entry claiming the same id', () => {
			const registry = buildRegistry(MOCK_PRICES, {
				monthly_try: [MOCK_PRICES.gift1YearBrl],
			});
			expect(registry.getProduct(MOCK_PRICES.gift1YearBrl)).toEqual({
				type: ProductType.GIFT_1_YEAR,
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				durationMonths: 12,
				isGift: true,
				currency: 'BRL',
			});
		});

		test('the first legacy slot to claim an id wins over a later one', () => {
			const registry = buildRegistry(
				{},
				{
					monthly_brl: ['legacy_shared'],
					yearly_usd: ['legacy_shared'],
				},
			);
			expect(registry.getProduct('legacy_shared')).toEqual({
				type: ProductType.MONTHLY_SUBSCRIPTION,
				premiumType: UserPremiumTypes.SUBSCRIPTION,
				durationMonths: 1,
				isGift: false,
				currency: 'BRL',
				billingCycle: 'monthly',
			});
		});
	});

	describe('malformed configuration', () => {
		test('a slot whose value is a bare string is ignored and the rest still registers', () => {
			const registry = buildRegistry(MOCK_PRICES, {
				monthly_brl: 'price_legacy_bare_string' as unknown as Array<string>,
				yearly_brl: [LEGACY_YEARLY_BRL],
			});
			expect(registry.getProduct('price_legacy_bare_string')).toBeNull();
			// Not accidentally registered character by character either.
			expect(registry.getProduct('p')).toBeNull();
			expect(registry.getProduct(LEGACY_YEARLY_BRL)?.type).toBe(ProductType.YEARLY_SUBSCRIPTION);
		});

		test('a slot with an unrecognised name is ignored and the rest still registers', () => {
			const registry = buildRegistry(MOCK_PRICES, {
				weekly_brl: ['legacy_weekly'],
				visionary_usd: ['legacy_visionary'],
				brl: ['legacy_no_separator'],
				_brl: ['legacy_leading_separator'],
				yearly_brl: [LEGACY_YEARLY_BRL],
			});
			expect(registry.getProduct('legacy_weekly')).toBeNull();
			expect(registry.getProduct('legacy_visionary')).toBeNull();
			expect(registry.getProduct('legacy_no_separator')).toBeNull();
			expect(registry.getProduct('legacy_leading_separator')).toBeNull();
			expect(registry.getProduct(LEGACY_YEARLY_BRL)?.type).toBe(ProductType.YEARLY_SUBSCRIPTION);
		});

		test('a slot with an unknown currency is ignored and the rest still registers', () => {
			const registry = buildRegistry(MOCK_PRICES, {
				monthly_jpy: ['legacy_jpy'],
				gift_1_year_gbp: ['legacy_gbp'],
				monthly_brl: [LEGACY_MONTHLY_BRL],
			});
			expect(registry.getProduct('legacy_jpy')).toBeNull();
			expect(registry.getProduct('legacy_gbp')).toBeNull();
			expect(registry.getProduct(LEGACY_MONTHLY_BRL)?.currency).toBe('BRL');
		});

		test('empty, blank and undefined entries inside a slot are skipped', () => {
			const registry = buildRegistry(MOCK_PRICES, {
				monthly_brl: ['', undefined as unknown as string, LEGACY_MONTHLY_BRL],
			});
			expect(registry.getProduct('')).toBeNull();
			expect(registry.getProduct(LEGACY_MONTHLY_BRL)?.currency).toBe('BRL');
		});

		test('an undefined slot value is ignored', () => {
			const registry = buildRegistry(MOCK_PRICES, {
				monthly_try: undefined,
				monthly_brl: [LEGACY_MONTHLY_BRL],
			});
			expect(registry.getProduct(LEGACY_MONTHLY_BRL)?.currency).toBe('BRL');
		});

		test('an array-valued legacy map is ignored without throwing', () => {
			const registry = buildRegistry(MOCK_PRICES, [
				'price_legacy_array',
			] as unknown as typeof Config.stripe.legacyPrices);
			expect(registry.getProduct('price_legacy_array')).toBeNull();
			// The authored prices are untouched.
			expect(registry.getProduct(MOCK_PRICES.monthlyBrl)?.currency).toBe('BRL');
			expect(registry.getRecurringSubscriptionPriceId('monthly', 'BRL')).toBe(MOCK_PRICES.monthlyBrl);
		});

		test('a string-valued legacy map is ignored without throwing', () => {
			const registry = buildRegistry(MOCK_PRICES, 'nonsense' as unknown as typeof Config.stripe.legacyPrices);
			expect(registry.getProduct('nonsense')).toBeNull();
			expect(registry.getProduct(MOCK_PRICES.monthlyBrl)?.currency).toBe('BRL');
		});

		test('a number-valued legacy map is ignored without throwing', () => {
			const registry = buildRegistry(MOCK_PRICES, 42 as unknown as typeof Config.stripe.legacyPrices);
			expect(registry.getProduct(MOCK_PRICES.monthlyBrl)?.currency).toBe('BRL');
		});

		test('an absent or empty legacy map leaves the authored registry intact', () => {
			expect(buildRegistry(MOCK_PRICES, undefined).getProduct(MOCK_PRICES.monthlyBrl)?.currency).toBe('BRL');
			expect(buildRegistry(MOCK_PRICES, {}).getProduct(MOCK_PRICES.monthlyBrl)?.currency).toBe('BRL');
		});
	});
});
