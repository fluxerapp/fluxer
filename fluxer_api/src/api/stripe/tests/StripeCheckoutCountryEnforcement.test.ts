// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
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
	monthlyUsd: 'price_enforce_monthly_usd',
	yearlyUsd: 'price_enforce_yearly_usd',
	monthlyBrl: 'price_enforce_monthly_brl',
	yearlyBrl: 'price_enforce_yearly_brl',
	gift1MonthUsd: 'price_enforce_gift_1_month_usd',
	gift1YearUsd: 'price_enforce_gift_1_year_usd',
	gift1MonthBrl: 'price_enforce_gift_1_month_brl',
	gift1YearBrl: 'price_enforce_gift_1_year_brl',
};

const MOCK_PRICE_SEEDS = {
	[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month' as const},
	[MOCK_PRICES.yearlyUsd]: {unit_amount: 4999, currency: 'usd', interval: 'year' as const},
	[MOCK_PRICES.monthlyBrl]: {unit_amount: 1890, currency: 'brl', interval: 'month' as const},
	[MOCK_PRICES.yearlyBrl]: {unit_amount: 18900, currency: 'brl', interval: 'year' as const},
};

function geoipCountry(countryCode: string | null): GeoipResult {
	return {
		countryCode,
		normalizedIp: '203.0.113.10',
		city: null,
		region: null,
		countryName: null,
	};
}

describe('StripeCheckoutCountryEnforcement', () => {
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
		stripeHandlers = createStripeApiHandlers({prices: MOCK_PRICE_SEEDS, subscriptionsListEmpty: true});
		server.use(...stripeHandlers.handlers);
	});

	test('rejects a base-currency subscription price when the request geolocates to a localized market', async () => {
		lookupGeoipMock.mockResolvedValue(geoipCountry('BR'));
		const token = await createPurchaser();
		await createBuilder(harness, token)
			.post('/stripe/checkout/subscription')
			.body({price_id: MOCK_PRICES.monthlyUsd, country_code: 'US'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
			.execute();
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
	});

	test('rejects a base-currency subscription price when country_code is omitted entirely', async () => {
		lookupGeoipMock.mockResolvedValue(geoipCountry('BR'));
		const token = await createPurchaser();
		await createBuilder(harness, token)
			.post('/stripe/checkout/subscription')
			.body({price_id: MOCK_PRICES.monthlyUsd})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
			.execute();
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
	});

	test('rejects a localized gift price when the request geolocates outside that market', async () => {
		lookupGeoipMock.mockResolvedValue(geoipCountry('US'));
		const token = await createPurchaser();
		await createBuilder(harness, token)
			.post('/stripe/checkout/gift')
			.body({price_id: MOCK_PRICES.gift1MonthBrl, country_code: 'BR'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.STRIPE_INVALID_PRODUCT_CONFIGURATION)
			.execute();
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
	});

	test('accepts the localized price for a request that geolocates to that market', async () => {
		lookupGeoipMock.mockResolvedValue(geoipCountry('BR'));
		const token = await createPurchaser();
		const response = await createBuilder<{url: string}>(harness, token)
			.post('/stripe/checkout/subscription')
			.body({price_id: MOCK_PRICES.monthlyBrl, country_code: 'BR'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.url).toContain('checkout.stripe.com');
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
	});

	test('falls back to the declared country when the request cannot be geolocated', async () => {
		lookupGeoipMock.mockResolvedValue(geoipCountry(null));
		const token = await createPurchaser();
		const response = await createBuilder<{url: string}>(harness, token)
			.post('/stripe/checkout/subscription')
			.body({price_id: MOCK_PRICES.monthlyBrl, country_code: 'BR'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.url).toContain('checkout.stripe.com');
	});

	test('resolves price ids from the request geolocation rather than the query parameter', async () => {
		lookupGeoipMock.mockResolvedValue(geoipCountry('BR'));
		const priceIds = await createBuilder<{currency: string; monthly: string | null}>(harness, '')
			.get('/premium/price-ids?country_code=US')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(priceIds.currency).toBe('BRL');
		expect(priceIds.monthly).toBe(MOCK_PRICES.monthlyBrl);
	});
});
