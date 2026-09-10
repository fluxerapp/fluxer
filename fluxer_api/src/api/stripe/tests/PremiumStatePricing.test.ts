// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import Stripe from 'stripe';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {Config} from '../../Config';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '../../test/msw/handlers/StripeApiHandlers';
import {server} from '../../test/msw/server';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {STRIPE_API_VERSION} from '../StripeApiVersion';

const MOCK_PRICES = {
	monthlyUsd: 'price_state_pricing_monthly_usd',
	yearlyUsd: 'price_state_pricing_yearly_usd',
	monthlyBrl: 'price_state_pricing_monthly_brl',
	yearlyBrl: 'price_state_pricing_yearly_brl',
	gift1MonthUsd: 'price_state_pricing_gift_1_month_usd',
	gift1YearUsd: 'price_state_pricing_gift_1_year_usd',
	gift1MonthBrl: 'price_state_pricing_gift_1_month_brl',
	gift1YearBrl: 'price_state_pricing_gift_1_year_brl',
};

const MOCK_PRICE_SEEDS = {
	[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month' as const},
	[MOCK_PRICES.yearlyUsd]: {unit_amount: 4999, currency: 'usd', interval: 'year' as const},
	[MOCK_PRICES.monthlyBrl]: {unit_amount: 1890, currency: 'brl', interval: 'month' as const},
	[MOCK_PRICES.yearlyBrl]: {unit_amount: 18900, currency: 'brl', interval: 'year' as const},
	[MOCK_PRICES.gift1MonthUsd]: {unit_amount: 499, currency: 'usd', interval: 'month' as const},
	[MOCK_PRICES.gift1YearUsd]: {unit_amount: 4999, currency: 'usd', interval: 'year' as const},
	[MOCK_PRICES.gift1MonthBrl]: {unit_amount: 1890, currency: 'brl', interval: 'month' as const},
	[MOCK_PRICES.gift1YearBrl]: {unit_amount: 18900, currency: 'brl', interval: 'year' as const},
};

describe('PremiumStatePricing', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let originalPrices: typeof Config.stripe.prices | undefined;

	async function mirrorPrices(): Promise<void> {
		const stripe = new Stripe(Config.stripe.secretKey ?? 'sk_test_fluxer', {
			apiVersion: STRIPE_API_VERSION,
			httpClient: Stripe.createFetchHttpClient(),
		});
		for (const priceId of Object.keys(MOCK_PRICE_SEEDS)) {
			const price = await stripe.prices.retrieve(priceId);
			await getBillingRepository().prices.upsertFromStripe(price);
		}
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
	beforeEach(async () => {
		await harness.resetData();
		Config.stripe.prices = MOCK_PRICES;
		stripeHandlers = createStripeApiHandlers({prices: MOCK_PRICE_SEEDS, subscriptionsListEmpty: true});
		server.use(...stripeHandlers.handlers);
		await mirrorPrices();
	});

	test('resolves the localized BRL catalog for a Brazilian request', async () => {
		const account = await createTestAccount(harness);
		const state = await createBuilder<PremiumStateResponse>(harness, account.token)
			.get('/premium/state?country_code=BR')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(state.pricing.country_code).toBe('BR');
		expect(state.pricing.localized?.currency).toBe('BRL');
		expect(state.pricing.localized?.monthly_amount_minor).toBe(1890);
		expect(state.pricing.localized?.yearly_amount_minor).toBe(18900);
		expect(state.pricing.localized?.monthly).toBe(MOCK_PRICES.monthlyBrl);
		expect(state.pricing.localized?.yearly).toBe(MOCK_PRICES.yearlyBrl);
	});

	test('resolves the USD catalog when the request declares no country', async () => {
		const account = await createTestAccount(harness);
		const state = await createBuilder<PremiumStateResponse>(harness, account.token)
			.get('/premium/state')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(state.pricing.country_code).toBeNull();
		expect(state.pricing.localized?.currency).toBe('USD');
		expect(state.pricing.localized?.monthly_amount_minor).toBe(499);
	});
});
