// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	CurrentSubscriptionPriceResponse,
	PriceIdsResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {GeoipResult} from '@pkgs/geoip/src/GeoipLookup';
import {afterAll, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const {lookupGeoipMock} = vi.hoisted(() => ({
	lookupGeoipMock: vi.fn(),
}));

vi.mock('@app/api/utils/IpUtils', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/api/utils/IpUtils')>()),
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
	monthlyIsk: 'price_nordic_monthly_isk',
	yearlyIsk: 'price_nordic_yearly_isk',
	gift1MonthUsd: 'price_nordic_gift_1_month_usd',
	gift1YearUsd: 'price_nordic_gift_1_year_usd',
	gift1MonthEur: 'price_nordic_gift_1_month_eur',
	gift1YearEur: 'price_nordic_gift_1_year_eur',
	gift1MonthDkk: 'price_nordic_gift_1_month_dkk',
	gift1YearDkk: 'price_nordic_gift_1_year_dkk',
	gift1MonthNok: 'price_nordic_gift_1_month_nok',
	gift1YearNok: 'price_nordic_gift_1_year_nok',
	gift1MonthSek: 'price_nordic_gift_1_month_sek',
	gift1YearSek: 'price_nordic_gift_1_year_sek',
	gift1MonthIsk: 'price_nordic_gift_1_month_isk',
	gift1YearIsk: 'price_nordic_gift_1_year_isk',
};

const MOCK_PRICE_SEEDS = {
	[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month' as const},
	[MOCK_PRICES.yearlyUsd]: {unit_amount: 4999, currency: 'usd', interval: 'year' as const},
	[MOCK_PRICES.monthlyEur]: {unit_amount: 499, currency: 'eur', interval: 'month' as const},
	[MOCK_PRICES.yearlyEur]: {unit_amount: 4999, currency: 'eur', interval: 'year' as const},
	[MOCK_PRICES.monthlyDkk]: {unit_amount: 3500, currency: 'dkk', interval: 'month' as const},
	[MOCK_PRICES.yearlyDkk]: {unit_amount: 35000, currency: 'dkk', interval: 'year' as const},
	[MOCK_PRICES.monthlyNok]: {unit_amount: 4900, currency: 'nok', interval: 'month' as const},
	[MOCK_PRICES.yearlyNok]: {unit_amount: 49000, currency: 'nok', interval: 'year' as const},
	[MOCK_PRICES.monthlyPln]: {unit_amount: 1900, currency: 'pln', interval: 'month' as const},
	[MOCK_PRICES.yearlyPln]: {unit_amount: 19000, currency: 'pln', interval: 'year' as const},
	[MOCK_PRICES.monthlySek]: {unit_amount: 4900, currency: 'sek', interval: 'month' as const},
	[MOCK_PRICES.yearlySek]: {unit_amount: 49000, currency: 'sek', interval: 'year' as const},
	[MOCK_PRICES.monthlyIsk]: {unit_amount: 59000, currency: 'isk', interval: 'month' as const},
	[MOCK_PRICES.yearlyIsk]: {unit_amount: 590000, currency: 'isk', interval: 'year' as const},
	[MOCK_PRICES.gift1MonthDkk]: {unit_amount: 3500, currency: 'dkk'},
	[MOCK_PRICES.gift1YearDkk]: {unit_amount: 35000, currency: 'dkk'},
	[MOCK_PRICES.gift1MonthNok]: {unit_amount: 4900, currency: 'nok'},
	[MOCK_PRICES.gift1YearNok]: {unit_amount: 49000, currency: 'nok'},
	[MOCK_PRICES.gift1MonthSek]: {unit_amount: 4900, currency: 'sek'},
	[MOCK_PRICES.gift1YearSek]: {unit_amount: 49000, currency: 'sek'},
	[MOCK_PRICES.gift1MonthIsk]: {unit_amount: 59000, currency: 'isk'},
	[MOCK_PRICES.gift1YearIsk]: {unit_amount: 590000, currency: 'isk'},
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
			expect(priceIds.monthly_amount_minor).toBe(4900);
			expect(priceIds.yearly_amount_minor).toBe(49000);
		});

		test.each(['DK', 'FO', 'GL'])('%s resolves to the DKK subscription catalog', async (country) => {
			const priceIds = await getPriceIds(country);
			expect(priceIds.currency).toBe('DKK');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyDkk);
			expect(priceIds.yearly).toBe(MOCK_PRICES.yearlyDkk);
			expect(priceIds.monthly_amount_minor).toBe(3500);
			expect(priceIds.yearly_amount_minor).toBe(35000);
		});

		test.each(['NO', 'SJ'])('%s resolves to the NOK subscription catalog', async (country) => {
			const priceIds = await getPriceIds(country);
			expect(priceIds.currency).toBe('NOK');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyNok);
			expect(priceIds.yearly).toBe(MOCK_PRICES.yearlyNok);
			expect(priceIds.monthly_amount_minor).toBe(4900);
			expect(priceIds.yearly_amount_minor).toBe(49000);
		});

		test('Iceland resolves to the ISK subscription catalog', async () => {
			const priceIds = await getPriceIds('IS');
			expect(priceIds.currency).toBe('ISK');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyIsk);
			expect(priceIds.yearly).toBe(MOCK_PRICES.yearlyIsk);
			expect(priceIds.monthly_amount_minor).toBe(59000);
			expect(priceIds.yearly_amount_minor).toBe(590000);
		});

		test('Poland still resolves to the PLN subscription catalog', async () => {
			const priceIds = await getPriceIds('PL');
			expect(priceIds.currency).toBe('PLN');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyPln);
		});

		test.each(['DE', 'NL', 'FI', 'IE', 'AX'])('%s still resolves to the EUR subscription catalog', async (country) => {
			const priceIds = await getPriceIds(country);
			expect(priceIds.currency).toBe('EUR');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyEur);
			expect(priceIds.yearly).toBe(MOCK_PRICES.yearlyEur);
		});

		test.each([
			['SE', 'SEK', MOCK_PRICES.gift1MonthSek, MOCK_PRICES.gift1YearSek, 4900],
			['DK', 'DKK', MOCK_PRICES.gift1MonthDkk, MOCK_PRICES.gift1YearDkk, 3500],
			['NO', 'NOK', MOCK_PRICES.gift1MonthNok, MOCK_PRICES.gift1YearNok, 4900],
			['IS', 'ISK', MOCK_PRICES.gift1MonthIsk, MOCK_PRICES.gift1YearIsk, 59000],
		])('%s sells gifts in %s at the subscription price', async (country, currency, giftMonth, giftYear, amount) => {
			const priceIds = await getPriceIds(country);
			expect(priceIds.gift_currency).toBe(currency);
			expect(priceIds.gift_1_month).toBe(giftMonth);
			expect(priceIds.gift_1_year).toBe(giftYear);
			expect(priceIds.gift_1_month_amount_minor).toBe(amount);
			expect(priceIds.gift_1_month_amount_minor).toBe(priceIds.monthly_amount_minor);
			expect(priceIds.gift_1_year_amount_minor).toBe(priceIds.yearly_amount_minor);
		});

		test('a Nordic gift falls back to EUR while the native gift prices are unconfigured', async () => {
			Config.stripe.prices = {
				...MOCK_PRICES,
				gift1MonthSek: undefined,
				gift1YearSek: undefined,
			};
			const priceIds = await getPriceIds('SE');
			expect(priceIds.currency).toBe('SEK');
			expect(priceIds.gift_currency).toBe('EUR');
			expect(priceIds.gift_1_month).toBe(MOCK_PRICES.gift1MonthEur);
		});

		test('a Nordic gift falls through to USD when no native or EUR gift price is configured', async () => {
			Config.stripe.prices = {
				...MOCK_PRICES,
				gift1MonthSek: undefined,
				gift1YearSek: undefined,
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
			expect(priceIds.gift_currency).toBe('SEK');
		});

		test('Iceland falls back to EUR while the ISK prices are still unconfigured', async () => {
			Config.stripe.prices = {
				...MOCK_PRICES,
				monthlyIsk: undefined,
				yearlyIsk: undefined,
				gift1MonthIsk: undefined,
				gift1YearIsk: undefined,
			};
			const priceIds = await getPriceIds('IS');
			expect(priceIds.currency).toBe('EUR');
			expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyEur);
			expect(priceIds.yearly).toBe(MOCK_PRICES.yearlyEur);
			expect(priceIds.gift_currency).toBe('EUR');
			expect(priceIds.gift_1_month).toBe(MOCK_PRICES.gift1MonthEur);
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

		test('accepts the SEK gift price from inside Sweden', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('SE'));
			const token = await createPurchaser();
			const response = await createBuilder<{url: string}>(harness, token)
				.post('/stripe/checkout/gift')
				.body({price_id: MOCK_PRICES.gift1MonthSek, country_code: 'SE'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(response.url).toContain('checkout.stripe.com');
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
		});

		test('rejects the EUR gift price from inside Sweden once SEK gifts exist', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('SE'));
			const token = await createPurchaser();
			await createBuilder(harness, token)
				.post('/stripe/checkout/gift')
				.body({price_id: MOCK_PRICES.gift1MonthEur, country_code: 'SE'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});

		test('accepts the ISK subscription price for a purchase that geolocates to Iceland', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('IS'));
			const token = await createPurchaser();
			await createBuilder<{url: string}>(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.yearlyIsk, country_code: 'IS'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
			expect(stripeHandlers.spies.createdCheckoutSessions[0]?.line_items?.[0]?.price).toBe(MOCK_PRICES.yearlyIsk);
		});

		test('marks Åland as EEA so the withdrawal waiver applies', async () => {
			lookupGeoipMock.mockResolvedValue(geoipCountry('AX'));
			const token = await createPurchaser();
			await createBuilder<{url: string}>(harness, token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyEur, country_code: 'AX'})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions[0]?.metadata?.eu_withdrawal_waiver_required).toBe('true');
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

	describe('adaptive pricing', () => {
		async function checkout(path: 'subscription' | 'gift', priceId: string, country: string) {
			lookupGeoipMock.mockResolvedValue(geoipCountry(country));
			const token = await createPurchaser();
			await createBuilder<{url: string}>(harness, token)
				.post(`/stripe/checkout/${path}`)
				.body({price_id: priceId, country_code: country})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
			return stripeHandlers.spies.createdCheckoutSessions[0];
		}

		test.each([
			['subscription', MOCK_PRICES.monthlySek, 'SE'],
			['subscription', MOCK_PRICES.yearlySek, 'SE'],
			['gift', MOCK_PRICES.gift1MonthSek, 'SE'],
			['subscription', MOCK_PRICES.monthlyNok, 'NO'],
			['subscription', MOCK_PRICES.monthlyDkk, 'DK'],
			['subscription', MOCK_PRICES.monthlyIsk, 'IS'],
			['gift', MOCK_PRICES.gift1YearIsk, 'IS'],
		] as const)('turns adaptive pricing off for a %s checkout with %s', async (path, priceId, country) => {
			const session = await checkout(path, priceId, country);
			expect(session?.adaptive_pricing).toEqual({enabled: 'false'});
		});

		test.each([
			['subscription', MOCK_PRICES.monthlyEur, 'DE'],
			['gift', MOCK_PRICES.gift1MonthEur, 'FI'],
			['subscription', MOCK_PRICES.monthlyUsd, 'US'],
			['subscription', MOCK_PRICES.monthlyPln, 'PL'],
		] as const)('leaves adaptive pricing unset for a %s checkout with %s', async (path, priceId, country) => {
			const session = await checkout(path, priceId, country);
			expect(session?.adaptive_pricing).toBeUndefined();
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
