// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {getInstanceConfigRepository, getLimitConfigService} from '@app/api/middleware/ServiceSingletons';
import {getStoredBillingConfig, setStoredBillingConfig} from '@app/api/stripe/BillingConfigCache';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const OPERATOR_WEBHOOK_SECRET = 'whsec_operator_secret';

const USD_PRICES = {
	monthlyUsd: 'price_monthlyusd',
	yearlyUsd: 'price_yearlyusd',
	gift1MonthUsd: 'price_gift1monthusd',
	gift1YearUsd: 'price_gift1yearusd',
	monthlyEur: 'price_monthlyeur',
	yearlyEur: 'price_yearlyeur',
	gift1MonthEur: 'price_gift1montheur',
	gift1YearEur: 'price_gift1yeareur',
};

type StripePrices = ReturnType<typeof getConfig>['stripe']['prices'];

interface GlobalState {
	selfHosted: boolean;
	premiumMode: ReturnType<typeof getCachedInstancePremiumMode>;
	storedBilling: ReturnType<typeof getStoredBillingConfig>;
	prices: StripePrices;
}

function captureGlobalState(): GlobalState {
	const config = getConfig();
	return {
		selfHosted: config.instance.selfHosted,
		premiumMode: getCachedInstancePremiumMode(),
		storedBilling: getStoredBillingConfig(),
		prices: config.stripe.prices,
	};
}

function restoreGlobalState(state: GlobalState): void {
	const config = getConfig();
	config.instance.selfHosted = state.selfHosted;
	config.stripe.prices = state.prices;
	setCachedInstancePremiumMode(state.premiumMode);
	setStoredBillingConfig(state.storedBilling);
}

function signWebhook(payload: string, secret: string): string {
	const timestamp = Math.floor(Date.now() / 1000);
	const signature = crypto.createHmac('sha256', secret).update(`${timestamp}.${payload}`).digest('hex');
	return `t=${timestamp},v1=${signature}`;
}

function webhookPayload(): string {
	return JSON.stringify({
		id: `evt_${crypto.randomBytes(8).toString('hex')}`,
		object: 'event',
		type: 'customer.created',
		created: Math.floor(Date.now() / 1000),
		data: {object: {id: 'cus_test_selfhosted'}},
	});
}

async function setPremiumMode(mode: 'mirror' | 'everyone'): Promise<void> {
	await getLimitConfigService().updatePolicyConfig({premium_mode: mode});
	setCachedInstancePremiumMode(mode);
}

async function generateGiftCode(harness: ApiTestHarness): Promise<string> {
	const admin = await setUserACLs(harness, await createTestAccount(harness), [
		AdminACLs.AUTHENTICATE,
		AdminACLs.GIFT_CODES_GENERATE,
	]);
	const {codes} = await createBuilder<{codes: Array<string>}>(harness, admin.token)
		.post('/admin/gift-codes')
		.body({count: 1, duration_type: 'months', duration_quantity: 1})
		.execute();
	const code = codes[0]?.split('/').pop();
	if (!code) throw new Error('no gift code generated');
	return code;
}

async function expectRoutesNotFound(
	harness: ApiTestHarness,
	token: string,
	routes: ReadonlyArray<readonly [RouteMethod, string]>,
): Promise<void> {
	for (const [method, path] of routes) {
		expect({path, ...(await readRouteStatus(harness, token, method, path))}).toEqual({
			path,
			status: HTTP_STATUS.NOT_FOUND,
			code: APIErrorCodes.NOT_FOUND,
		});
	}
}

async function expectRoutesRegistered(
	harness: ApiTestHarness,
	token: string,
	routes: ReadonlyArray<readonly [RouteMethod, string]>,
): Promise<void> {
	for (const [method, path] of routes) {
		const result = await readRouteStatus(harness, token, method, path);
		expect({path, code: result.code}).not.toEqual({path, code: APIErrorCodes.NOT_FOUND});
	}
}

async function postSignedWebhook(harness: ApiTestHarness, secret: string): Promise<number> {
	const payload = webhookPayload();
	const {response} = await createBuilderWithoutAuth(harness)
		.post('/stripe/webhook')
		.header('stripe-signature', signWebhook(payload, secret))
		.header('content-type', 'application/json')
		.body(payload)
		.executeRaw();
	return response.status;
}

async function readRouteStatus(
	harness: ApiTestHarness,
	token: string | null,
	method: RouteMethod,
	path: string,
): Promise<{status: number; code: string | undefined}> {
	const builder = token === null ? createBuilderWithoutAuth(harness) : createBuilder(harness, token);
	const request =
		method === 'GET' ? builder.get(path) : method === 'DELETE' ? builder.delete(path) : builder.post(path).body({});
	const {response, json} = await request.executeRaw();
	return {status: response.status, code: (json as {code?: string} | undefined)?.code};
}

type RouteMethod = 'GET' | 'POST' | 'DELETE';

const PURCHASE_ROUTES: ReadonlyArray<[RouteMethod, string]> = [
	['POST', '/stripe/checkout/subscription'],
	['POST', '/stripe/checkout/gift'],
	['GET', '/premium/price-ids'],
];

const SERVICING_ROUTES: ReadonlyArray<[RouteMethod, string]> = [
	['GET', '/premium/current-subscription-price'],
	['POST', '/premium/customer-portal'],
	['POST', '/premium/grace/end'],
	['POST', '/premium/cancel-subscription'],
	['POST', '/premium/reactivate-subscription'],
	['POST', '/premium/change-subscription'],
	['POST', '/premium/cancel-pending-subscription-change'],
];

const BILLING_ROUTES: ReadonlyArray<[RouteMethod, string]> = [...PURCHASE_ROUTES, ...SERVICING_ROUTES];

const GIFT_ROUTES: ReadonlyArray<[RouteMethod, string]> = [
	['GET', '/gifts/somegiftcode'],
	['POST', '/gifts/somegiftcode/redeem'],
	['GET', '/users/@me/gifts'],
];

const HOSTED_ONLY_ROUTES: ReadonlyArray<[RouteMethod, string]> = [
	['POST', '/users/@me/age-verification'],
	['GET', '/premium/refund-eligibility'],
	['POST', '/premium/refund-latest'],
	['POST', '/premium/switch-to-list-price'],
	['POST', '/premium/visionary/rejoin'],
];

const STORE_BILLING_ROUTES: ReadonlyArray<[RouteMethod, string]> = [
	['GET', '/premium/store'],
	['POST', '/premium/store/app-store/transactions'],
	['POST', '/premium/store/google-play/purchases'],
	['GET', '/premium/store/purchases'],
	['DELETE', '/premium/store/purchases/1234567890123456789'],
	['POST', '/webhooks/app-store'],
	['POST', '/webhooks/google-play'],
	['GET', '/admin/users/1234567890123456789/store-purchases'],
	['POST', '/admin/users/1234567890123456789/store-purchases/refresh'],
];

describe('self-hosted premium routes', () => {
	let harness: ApiTestHarness;
	let original: GlobalState;

	beforeAll(async () => {
		original = captureGlobalState();
		getConfig().instance.selfHosted = true;
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		setStoredBillingConfig(null);
		setCachedInstancePremiumMode('everyone');
	});

	afterEach(() => {
		getConfig().stripe.prices = original.prices;
		setStoredBillingConfig(null);
	});

	afterAll(async () => {
		await harness.shutdown();
		restoreGlobalState(original);
	});

	describe('everyone mode', () => {
		test('answers every Stripe and gift route like an unregistered route', async () => {
			const account = await createTestAccount(harness);
			const unregistered = await readRouteStatus(harness, account.token, 'GET', '/premium/not-a-route');
			expect(unregistered).toEqual({status: HTTP_STATUS.NOT_FOUND, code: APIErrorCodes.NOT_FOUND});
			for (const [method, path] of [
				...BILLING_ROUTES,
				...GIFT_ROUTES,
				...HOSTED_ONLY_ROUTES,
				...STORE_BILLING_ROUTES,
				['POST', '/stripe/webhook'] as const,
			]) {
				expect({path, ...(await readRouteStatus(harness, account.token, method, path))}).toEqual({
					path,
					...unregistered,
				});
			}
		});

		test('answers before authentication runs', async () => {
			expect(await readRouteStatus(harness, null, 'GET', '/users/@me/gifts')).toEqual({
				status: HTTP_STATUS.NOT_FOUND,
				code: APIErrorCodes.NOT_FOUND,
			});
		});

		test('rejects admin gift code generation', async () => {
			const admin = await setUserACLs(harness, await createTestAccount(harness), [
				AdminACLs.AUTHENTICATE,
				AdminACLs.GIFT_CODES_GENERATE,
			]);
			await createBuilder(harness, admin.token)
				.post('/admin/gift-codes')
				.body({count: 1, duration_type: 'months', duration_quantity: 1})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.FEATURE_NOT_AVAILABLE_SELF_HOSTED)
				.execute();
		});

		test('reports premium and Stripe as disabled in discovery', async () => {
			getConfig().stripe.prices = {...USD_PRICES};
			const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
				.get('/.well-known/fluxer')
				.execute();
			expect(discovery.features).toMatchObject({
				premium_enabled: false,
				stripe_enabled: false,
				stripe_serviceable: false,
				self_hosted: true,
			});
		});
	});

	describe('mirror mode without billing', () => {
		beforeEach(async () => {
			await setPremiumMode('mirror');
		});

		test('serves admin generated gift codes', async () => {
			const code = await generateGiftCode(harness);
			const gift = await createBuilderWithoutAuth<{code: string}>(harness).get(`/gifts/${code}`).execute();
			expect(gift.code).toBe(code);
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post(`/gifts/${code}/redeem`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const gifts = await createBuilder<Array<unknown>>(harness, account.token).get('/users/@me/gifts').execute();
			expect(gifts).toEqual([]);
		});

		test('credits admin generated gift codes to the issuing admin', async () => {
			const admin = await setUserACLs(harness, await createTestAccount(harness), [
				AdminACLs.AUTHENTICATE,
				AdminACLs.GIFT_CODES_GENERATE,
			]);
			const {codes} = await createBuilder<{codes: Array<string>}>(harness, admin.token)
				.post('/admin/gift-codes')
				.body({count: 1, duration_type: 'months', duration_quantity: 1})
				.execute();
			const code = codes[0]?.split('/').pop();
			const gift = await createBuilderWithoutAuth<{created_by: {id: string} | null}>(harness)
				.get(`/gifts/${code}`)
				.execute();
			expect(gift.created_by?.id).toBe(admin.userId);
		});

		test('keeps purchase and hosted-only routes unavailable', async () => {
			const account = await createTestAccount(harness);
			await expectRoutesNotFound(harness, account.token, [
				...PURCHASE_ROUTES,
				...HOSTED_ONLY_ROUTES,
				...STORE_BILLING_ROUTES,
			]);
		});

		test('keeps servicing routes and the webhook available while a Stripe key is configured', async () => {
			const account = await createTestAccount(harness);
			await expectRoutesRegistered(harness, account.token, SERVICING_ROUTES);
			expect(await postSignedWebhook(harness, 'whsec_test_fluxer')).toBe(HTTP_STATUS.OK);
		});

		test('hides servicing routes and the webhook without a Stripe key', async () => {
			const config = getConfig();
			const secretKey = config.stripe.secretKey;
			config.stripe.secretKey = undefined;
			try {
				const account = await createTestAccount(harness);
				await expectRoutesNotFound(harness, account.token, [...SERVICING_ROUTES, ['POST', '/stripe/webhook'] as const]);
				const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
					.get('/.well-known/fluxer')
					.execute();
				expect(discovery.features).toMatchObject({premium_enabled: true, stripe_serviceable: false});
			} finally {
				config.stripe.secretKey = secretKey;
			}
		});

		test('reports premium enabled and Stripe disabled in discovery', async () => {
			const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
				.get('/.well-known/fluxer')
				.execute();
			expect(discovery.features).toMatchObject({
				premium_enabled: true,
				stripe_enabled: false,
				stripe_serviceable: true,
			});
		});
	});

	describe('mirror mode with billing configured', () => {
		beforeEach(async () => {
			await setPremiumMode('mirror');
		});

		test('serves price ids from the env catalog', async () => {
			getConfig().stripe.prices = {...USD_PRICES};
			server.use(...createStripeApiHandlers().handlers);
			const priceIds = await createBuilderWithoutAuth<Record<string, unknown>>(harness)
				.get('/premium/price-ids?country_code=US')
				.execute();
			expect(priceIds).toMatchObject({monthly: USD_PRICES.monthlyUsd, yearly: USD_PRICES.yearlyUsd});
			const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
				.get('/.well-known/fluxer')
				.execute();
			expect(discovery.features).toMatchObject({premium_enabled: true, stripe_enabled: true});
		});

		test('verifies webhooks with the operator webhook secret', async () => {
			await getInstanceConfigRepository().setInstanceBillingConfig({
				enabled: true,
				stripe_webhook_secret: OPERATOR_WEBHOOK_SECRET,
				prices: {GBP: {monthly: 'price_monthlygbp', yearly: 'price_yearlygbp'}},
			});
			const missing = await createBuilderWithoutAuth(harness).post('/stripe/webhook').body('{}').executeRaw();
			expect(missing.response.status).toBe(HTTP_STATUS.BAD_REQUEST);
			const payload = webhookPayload();
			const accepted = await createBuilderWithoutAuth(harness)
				.post('/stripe/webhook')
				.header('stripe-signature', signWebhook(payload, OPERATOR_WEBHOOK_SECRET))
				.header('content-type', 'application/json')
				.body(payload)
				.executeRaw();
			expect(accepted.response.status).toBe(HTTP_STATUS.OK);
			const rejected = await createBuilderWithoutAuth(harness)
				.post('/stripe/webhook')
				.header('stripe-signature', signWebhook(payload, 'whsec_test_fluxer'))
				.header('content-type', 'application/json')
				.body(payload)
				.executeRaw();
			expect(rejected.response.status).toBe(HTTP_STATUS.UNAUTHORIZED);
		});

		test('keeps servicing routes and the webhook available after billing is disabled', async () => {
			await getInstanceConfigRepository().setInstanceBillingConfig({
				enabled: false,
				stripe_webhook_secret: OPERATOR_WEBHOOK_SECRET,
				prices: {GBP: {monthly: 'price_monthlygbp', yearly: 'price_yearlygbp'}},
			});
			const account = await createTestAccount(harness);
			await expectRoutesNotFound(harness, account.token, PURCHASE_ROUTES);
			await expectRoutesRegistered(harness, account.token, SERVICING_ROUTES);
			expect(await postSignedWebhook(harness, OPERATOR_WEBHOOK_SECRET)).toBe(HTTP_STATUS.OK);
			const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
				.get('/.well-known/fluxer')
				.execute();
			expect(discovery.features).toMatchObject({
				premium_enabled: true,
				stripe_enabled: false,
				stripe_serviceable: true,
			});
		});

		test('keeps hosted-only routes unavailable', async () => {
			getConfig().stripe.prices = {...USD_PRICES};
			const account = await createTestAccount(harness);
			for (const [method, path] of [...HOSTED_ONLY_ROUTES, ...STORE_BILLING_ROUTES]) {
				expect({path, ...(await readRouteStatus(harness, account.token, method, path))}).toEqual({
					path,
					status: HTTP_STATUS.NOT_FOUND,
					code: APIErrorCodes.NOT_FOUND,
				});
			}
		});
	});
});

describe('hosted premium routes', () => {
	let harness: ApiTestHarness;
	let original: GlobalState;

	beforeAll(async () => {
		original = captureGlobalState();
		getConfig().instance.selfHosted = false;
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		setStoredBillingConfig(null);
	});

	afterAll(async () => {
		await harness.shutdown();
		restoreGlobalState(original);
	});

	test('keeps every Stripe and gift route registered regardless of premium mode', async () => {
		setCachedInstancePremiumMode('everyone');
		const account = await createTestAccount(harness);
		const routes = [...BILLING_ROUTES, ...GIFT_ROUTES, ...HOSTED_ONLY_ROUTES, ...STORE_BILLING_ROUTES].filter(
			([, path]) => path !== '/users/@me/age-verification',
		);
		for (const [method, path] of routes) {
			const result = await readRouteStatus(harness, account.token, method, path);
			expect({path, code: result.code}).not.toEqual({path, code: APIErrorCodes.NOT_FOUND});
		}
		const webhook = await createBuilderWithoutAuth(harness).post('/stripe/webhook').body('{}').executeRaw();
		expect(webhook.response.status).toBe(HTTP_STATUS.BAD_REQUEST);
	});

	test('serves admin generated gift codes', async () => {
		setCachedInstancePremiumMode('everyone');
		const code = await generateGiftCode(harness);
		const gift = await createBuilderWithoutAuth<{code: string}>(harness).get(`/gifts/${code}`).execute();
		expect(gift.code).toBe(code);
	});

	test('reports premium enabled and Stripe from the configured catalog in discovery', async () => {
		const before = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
			.get('/.well-known/fluxer')
			.execute();
		expect(before.features).toMatchObject({premium_enabled: true, stripe_enabled: false, self_hosted: false});
		getConfig().stripe.prices = {...USD_PRICES};
		try {
			const after = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
				.get('/.well-known/fluxer')
				.execute();
			expect(after.features).toMatchObject({premium_enabled: true, stripe_enabled: true});
		} finally {
			getConfig().stripe.prices = original.prices;
		}
	});
});
