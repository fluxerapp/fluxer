// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	CurrentSubscriptionPriceResponse,
	PriceIdsResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {GeoipResult} from '@pkgs/geoip/src/GeoipLookup';
import {afterAll, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {Config} from '../../Config';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '../../test/msw/handlers/StripeApiHandlers';
import {server} from '../../test/msw/server';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

const {lookupGeoipMock} = vi.hoisted(() => ({
	lookupGeoipMock: vi.fn(),
}));

vi.mock('../../utils/IpUtils', async (importOriginal) => ({
	...(await importOriginal<typeof import('../../utils/IpUtils')>()),
	lookupGeoip: lookupGeoipMock,
}));

const MOCK_PRICES = {
	monthlyUsd: 'price_nordic_monthly_usd',
	yearlyUsd: 'price_nordic_yearly_usd',
	monthlyEur: 'price_nordic_monthly_eur',
	yearlyEur: 'price_nordic_yearly_eur',
	monthlyDkk: 'price_nordic_monthly_dkk',
	yearlyDkk: 'price_nordic_yearly_dkk',
	monthlyNok: 'price_nordic_monthly_nok',
	yearlyNok: 'price_nordic_yearly_nok',
	monthlyPln: 'price_nordic_monthly_pln',
	yearlyPln: 'price_nordic_yearly_pln',
	monthlySek: 'price_nordic_monthly_sek',
	yearlySek: 'price_nordic_yearly_sek',
	gift1MonthUsd: 'price_nordic_gift_1_month_usd',
	gift1YearUsd: 'price_nordic_gift_1_year_usd',
	gift1MonthEur: 'price_nordic_gift_1_month_eur',
	gift1YearEur: 'price_nordic_gift_1_year_eur',
};

const MOCK_PRICE_SEEDS = {
	[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month' as const},
	[MOCK_PRICES.yearlyUsd]: {unit_amount: 4999, currency: 'usd', interval: 'year' as const},
	[MOCK_PRICES.monthlyEur]: {unit_amount: 499, currency: 'eur', interval: 'month' as const},
	[MOCK_PRICES.yearlyEur]: {unit_amount: 4999, currency: 'eur', interval: 'year' as const},
	[MOCK_PRICES.monthlyDkk]: {unit_amount: 3700, currency: 'dkk', interval: 'month' as const},
	[MOCK_PRICES.yearlyDkk]: {unit_amount: 37000, currency: 'dkk', interval: 'year' as const},
	[MOCK_PRICES.monthlyNok]: {unit_amount: 5400, currency: 'nok', interval: 'month' as const},
	[MOCK_PRICES.yearlyNok]: {unit_amount: 54000, currency: 'nok', interval: 'year' as const},
	[MOCK_PRICES.monthlyPln]: {unit_amount: 1900, currency: 'pln', interval: 'month' as const},
	[MOCK_PRICES.yearlyPln]: {unit_amount: 19000, currency: 'pln', interval: 'year' as const},
	[MOCK_PRICES.monthlySek]: {unit_amount: 5400, currency: 'sek', interval: 'month' as const},
	[MOCK_PRICES.yearlySek]: {unit_amount: 54000, currency: 'sek', interval: 'year' as const},
};

const EUR_SUBSCRIPTION_ID = 'sub_nordic_legacy_eur';

function geoipCountry(countryCode: string | null): GeoipResult {
	return {
		countryCode,
		normalizedIp: '203.0.113.10',
		city: null,
		region: null,
		countryName: null,
	};
}

describe('Nordic localized currencies', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let originalPrices: typeof Config.stripe.prices | undefined;

	async function createPurchaser(): Promise<string> {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: true})
			.execute();
		return account.token;
	}

	function getPriceIds(countryCode: string | null): Promise<PriceIdsResponse> {
		lookupGeoipMock.mockResolvedValue(geoipCountry(countryCode));
		return createBuilder<PriceIdsResponse>(harness, '').get('/premium/price-ids').expect(HTTP_STATUS.OK).execute();
	}

	beforeAll(async () => {
		originalPrices = Config.stripe.prices;
		Config.stripe.prices = MOCK_PRICES;
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.prices = originalPrices;
	});
	beforeEach(() => {
		Config.stripe.prices = MOCK_PRICES;
		lookupGeoipMock.mockReset();
		lookupGeoipMock.mockResolvedValue(geoipCountry(null));
		stripeHandlers = createStripeApiHandlers({
			prices: MOCK_PRICE_SEEDS,
			subscriptionsListEmpty: true,
			subscriptions: {
				[EUR_SUBSCRIPTION_ID]: {
					customer: `cus_${EUR_SUBSCRIPTION_ID}`,
					price_id: MOCK_PRICES.monthlyEur,
					unit_amount: 499,
					currency: 'eur',
					interval: 'month',
					item_id: `si_${EUR_SUBSCRIPTION_ID}`,
				},
			},
		});
		server.use(...stripeHandlers.handlers);
	});

	describe('country routing', () => {
		test('Sweden resolves to the SEK subscription catalog', async () => {
			const priceIds = await getPriceIds('SE');
			expect(priceIds.currency).toBe('SEK');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlySek);
			expect(priceIds.yearly).toBe(MOCK_PRICES.yearlySek);
		});

		test('Denmark resolves to the DKK subscription catalog', async () => {
			const priceIds = await getPriceIds('DK');
			expect(priceIds.currency).toBe('DKK');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyDkk);
			expect(priceIds.yearly).toBe(MOCK_PRICES.yearlyDkk);
		});

		test('Norway resolves to the NOK subscription catalog', async () => {
			const priceIds = await getPriceIds('NO');
			expect(priceIds.currency).toBe('NOK');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyNok);
			expect(priceIds.yearly).toBe(MOCK_PRICES.yearlyNok);
		});

		test('Poland still resolves to the PLN subscription catalog', async () => {
			const priceIds = await getPriceIds('PL');
			expect(priceIds.currency).toBe('PLN');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyPln);
		});

		test.each(['DE', 'NL', 'FI', 'IE', 'IS'])('%s still resolves to the EUR subscription catalog', async (country) => {
			const priceIds = await getPriceIds(country);
			expect(priceIds.currency).toBe('EUR');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyEur);
			expect(priceIds.yearly).toBe(MOCK_PRICES.yearlyEur);
		});

		test.each(['SE', 'DK', 'NO'])('%s keeps gifts on the base currency', async (country) => {
			const priceIds = await getPriceIds(country);
			expect(priceIds.gift_currency).toBe('EUR');
			expect(priceIds.gift_1_month).toBe(MOCK_PRICES.gift1MonthEur);
			expect(priceIds.gift_1_year).toBe(MOCK_PRICES.gift1YearEur);
		});

		test('a Nordic gift falls through to USD when no EUR gift price is configured', async () => {
			Config.stripe.prices = {
				...MOCK_PRICES,
				gift1MonthEur: undefined,
				gift1YearEur: undefined,
			};
			const priceIds = await getPriceIds('SE');
			expect(priceIds.currency).toBe('SEK');
			expect(priceIds.gift_currency).toBe('USD');
			expect(priceIds.gift_1_month).toBe(MOCK_PRICES.gift1MonthUsd);
		});

		test('Sweden falls back to EUR while the SEK prices are still unconfigured', async () => {
			Config.stripe.prices = {
				...MOCK_PRICES,
				monthlySek: undefined,
				yearlySek: undefined,
			};
			const priceIds = await getPriceIds('SE');
			expect(priceIds.currency).toBe('EUR');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyEur);
			expect(priceIds.gift_currency).toBe('EUR');
		});
	});

	describe('checkout enforcement', () => {
		test('rejects the EUR subscription price for a purchase that geolocates to Sweden', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('SE'));
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyEur, country_code: 'SE'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});

		test('rejects the USD subscription price for a purchase that geolocates to Sweden', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('SE'));
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyUsd, country_code: 'US'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});

		test('rejects a neighbouring Nordic price for a purchase that geolocates to Sweden', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('SE'));
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyDkk, country_code: 'DK'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});

		test('accepts the SEK subscription price for a purchase that geolocates to Sweden', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('SE'));
			const token = await createPurchaser();
			const response = await createBuilder<{url: string}>(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlySek, country_code: 'SE'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(response.url).toContain('checkout.stripe.com');
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
		});

		test('accepts the base gift price from inside Sweden', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('SE'));
			const token = await createPurchaser();
			const response = await createBuilder<{url: string}>(harness, token)
				.post('/stripe/checkout/gift')
				.body({price_id: MOCK_PRICES.gift1MonthEur, country_code: 'SE'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(response.url).toContain('checkout.stripe.com');
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
		});

		test('rejects the SEK subscription price on the gift endpoint', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('SE'));
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/gift')
				.body({price_id: MOCK_PRICES.monthlySek, country_code: 'SE'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});
	});

	describe('existing subscribers', () => {
		test('a Swedish customer billed in EUR keeps the EUR price and is not repriced to SEK', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('SE'));
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					stripe_subscription_id: EUR_SUBSCRIPTION_ID,
					premium_type: 1,
					premium_billing_cycle: 'monthly',
					premium_until: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString(),
					premium_will_cancel: false,
				})
				.execute();
			const price = await createBuilder<CurrentSubscriptionPriceResponse>(harness, account.token)
				.get('/premium/current-subscription-price')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(price).toMatchObject({
				price_id: MOCK_PRICES.monthlyEur,
				currency: 'EUR',
				billing_cycle: 'monthly',
				is_grandfathered: false,
				list_price_id: MOCK_PRICES.monthlyEur,
			});
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
			expect(stripeHandlers.spies.createdSubscriptionSchedules).toHaveLength(0);
		});
	});
});
