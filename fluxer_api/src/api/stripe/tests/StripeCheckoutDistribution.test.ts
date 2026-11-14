// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import type {PriceIdsResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {HttpResponse, http} from 'msw';
import {afterAll, beforeAll, beforeEach, expect, test} from 'vitest';

const STRIPE_API_BASE = 'https://api.stripe.com';
const MOCK_PRICES = {
	monthlyEur: 'price_dist_monthly_eur',
	yearlyEur: 'price_dist_yearly_eur',
	gift1MonthEur: 'price_dist_gift_1_month_eur',
	gift1YearEur: 'price_dist_gift_1_year_eur',
	monthlyUsd: 'price_dist_monthly_usd',
	yearlyUsd: 'price_dist_yearly_usd',
	gift1MonthUsd: 'price_dist_gift_1_month_usd',
	gift1YearUsd: 'price_dist_gift_1_year_usd',
};

let harness: ApiTestHarness;
let stripeHandlers: StripeApiHandlers;
let originalPrices: typeof Config.stripe.prices | undefined;
let createdPrices: Array<Record<string, string>>;

function priceHandlers() {
	return [
		http.get(`${STRIPE_API_BASE}/v1/prices`, () =>
			HttpResponse.json({object: 'list', data: [], has_more: false, url: '/v1/prices'}),
		),
		http.post(`${STRIPE_API_BASE}/v1/prices`, async ({request}) => {
			const params = Object.fromEntries(new URLSearchParams(await request.text()));
			createdPrices.push(params);
			return HttpResponse.json({
				id: `price_channel_${createdPrices.length}`,
				object: 'price',
				currency: params['currency'],
				unit_amount: Number(params['unit_amount']),
				product: params['product'],
			});
		}),
	];
}

beforeAll(async () => {
	originalPrices = Config.stripe.prices;
	harness = await createApiTestHarness();
});

afterAll(async () => {
	await harness.shutdown();
	Config.stripe.prices = originalPrices;
});

beforeEach(async () => {
	await harness.reset();
	Config.stripe.prices = MOCK_PRICES;
	createdPrices = [];
	stripeHandlers = createStripeApiHandlers({
		prices: {
			[MOCK_PRICES.monthlyEur]: {unit_amount: 499, currency: 'eur', interval: 'month'},
			[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month'},
		},
		subscriptionsListEmpty: true,
	});
	server.use(...stripeHandlers.handlers, ...priceHandlers());
	await getInstanceConfigRepository().updateDistributionPricingConfig(() => ({
		rules: [
			{
				id: 'ios_marketplace',
				active: true,
				rate_bps: 500,
				products: ['plutonium_subscription', 'plutonium_gift'],
				rule_version: 1,
				prices: {},
			},
		],
	}));
});

async function verifiedAccount() {
	const account = await createTestAccount(harness);
	await createBuilderWithoutAuth(harness)
		.post(`/test/users/${account.userId}/security-flags`)
		.body({email_verified: true})
		.expect(HTTP_STATUS.OK)
		.execute();
	return account;
}

async function checkout(token: string, body: Record<string, unknown>) {
	await createBuilder(harness, token)
		.post('/stripe/checkout/subscription')
		.body({price_id: MOCK_PRICES.monthlyUsd, ...body})
		.expect(HTTP_STATUS.OK)
		.execute();
	return stripeHandlers.spies.createdCheckoutSessions.at(-1) as unknown as {
		id?: string;
		metadata: Record<string, string>;
		line_items: Array<{price: string}>;
		subscription_data: {metadata: Record<string, string>};
	};
}

test('a marketplace checkout charges the channel price once and tags every record', async () => {
	const account = await verifiedAccount();
	const token = Buffer.from(JSON.stringify({externalPurchaseId: 'ext-42'})).toString('base64url');
	const session = await checkout(account.token, {distribution: 'ios_marketplace', distribution_token: token});

	expect(createdPrices).toHaveLength(1);
	expect(createdPrices[0]).toMatchObject({
		unit_amount: '526',
		currency: 'usd',
		'recurring[interval]': 'month',
		lookup_key: `dist_ios_marketplace_1_500_${MOCK_PRICES.monthlyUsd}`,
		'metadata[distribution]': 'ios_marketplace',
	});
	expect(session.line_items[0]?.price).toBe('price_channel_1');
	const expected = {
		distribution: 'ios_marketplace',
		distribution_rate_bps: '500',
		distribution_rule_version: '1',
		distribution_base_unit_amount: '499',
		distribution_unit_amount: '526',
		distribution_extra_unit_amount: '27',
		distribution_purchase_ref: 'ext-42',
	};
	expect(session.metadata).toMatchObject(expected);
	expect(session.subscription_data.metadata).toMatchObject(expected);

	const [payment] = await new PaymentRepository().findPaymentsByUserId(createUserID(BigInt(account.userId)));
	expect(payment).toMatchObject({
		priceId: 'price_channel_1',
		distribution: 'ios_marketplace',
		distributionRateBps: 500,
		distributionRuleVersion: 1,
		distributionBaseAmountMinor: 499,
		distributionPurchaseRef: 'ext-42',
	});

	const quote = await createBuilder<PriceIdsResponse>(harness, account.token)
		.get('/premium/price-ids?country_code=US&distribution=ios_marketplace')
		.expect(HTTP_STATUS.OK)
		.execute();
	expect(quote.monthly).toBe('price_channel_1');
	expect(quote.monthly_amount_minor).toBe(526);
	expect(quote.distribution).toEqual({id: 'ios_marketplace', rate_bps: 500, rule_version: 1});

	const second = await verifiedAccount();
	const reused = await checkout(second.token, {price_id: 'price_channel_1', distribution: 'ios_marketplace'});
	expect(createdPrices).toHaveLength(1);
	expect(reused.line_items[0]?.price).toBe('price_channel_1');
});

test('an unknown distribution or a channel price without one pays the standard price', async () => {
	const account = await verifiedAccount();
	const unknown = await checkout(account.token, {distribution: 'not_a_channel'});
	expect(unknown.line_items[0]?.price).toBe(MOCK_PRICES.monthlyUsd);
	expect(unknown.metadata['distribution']).toBeUndefined();
	expect(createdPrices).toHaveLength(0);

	const quote = await createBuilder<PriceIdsResponse>(harness, account.token)
		.get('/premium/price-ids?country_code=US&distribution=NOT%20VALID')
		.expect(HTTP_STATUS.OK)
		.execute();
	expect(quote.monthly_amount_minor).toBe(499);
	expect(quote.distribution).toBeNull();

	await checkout(account.token, {distribution: 'ios_marketplace'});
	expect(createdPrices).toHaveLength(1);
	const second = await verifiedAccount();
	const stripped = await checkout(second.token, {price_id: 'price_channel_1'});
	expect(stripped.line_items[0]?.price).toBe(MOCK_PRICES.monthlyUsd);
	expect(stripped.metadata['distribution']).toBeUndefined();
});
