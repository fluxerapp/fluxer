// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	SelfServeRefundEligibilityResponse,
	SelfServeRefundResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, type TestAccount} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createStripeApiHandlers} from '../../test/msw/handlers/StripeApiHandlers';
import {server} from '../../test/msw/server';
import {createBuilder} from '../../test/TestRequestBuilder';

const MOCK_CUSTOMER_ID = 'cus_self_serve_refund';
const MOCK_SUBSCRIPTION_ID = 'sub_self_serve_refund';
const SECONDS_PER_DAY = 24 * 60 * 60;
const STRIPE_API_BASE = 'https://api.stripe.com';

interface MockStripeInvoice {
	id: string;
	object: 'invoice';
	customer: string;
	parent: {subscription_details: {subscription: string}} | null;
	amount_due: number;
	amount_paid: number;
	currency: string;
	status: 'paid';
	created: number;
	payments: {
		object: 'list';
		data: Array<{
			id: string;
			invoice: string;
			object: 'invoice_payment';
			status: 'paid';
			status_transitions: {paid_at: number};
			payment: {
				charge: string;
				payment_intent: string;
				type: 'payment_intent';
			};
		}>;
		has_more: boolean;
		url: string;
	};
	paymentIntentId: string;
	chargeId: string;
}

function buildInvoice(opts: {
	id: string;
	customerId?: string;
	subscriptionId?: string | null;
	paidAtSecondsAgo: number;
	paymentIntentId?: string;
	chargeId?: string;
}): MockStripeInvoice {
	const customerId = opts.customerId ?? MOCK_CUSTOMER_ID;
	const subscriptionId = opts.subscriptionId === undefined ? MOCK_SUBSCRIPTION_ID : opts.subscriptionId;
	const paymentIntentId = opts.paymentIntentId ?? `pi_${opts.id}`;
	const chargeId = opts.chargeId ?? `ch_${opts.id}`;
	const paidAt = Math.floor(Date.now() / 1000) - opts.paidAtSecondsAgo;
	return {
		id: opts.id,
		object: 'invoice',
		customer: customerId,
		parent: subscriptionId == null ? null : {subscription_details: {subscription: subscriptionId}},
		amount_due: 2500,
		amount_paid: 2500,
		currency: 'usd',
		status: 'paid',
		created: paidAt,
		payments: {
			object: 'list',
			data: [
				{
					id: `inpay_${opts.id}`,
					invoice: opts.id,
					object: 'invoice_payment',
					status: 'paid',
					status_transitions: {paid_at: paidAt},
					payment: {
						charge: chargeId,
						payment_intent: paymentIntentId,
						type: 'payment_intent',
					},
				},
			],
			has_more: false,
			url: `/v1/invoices/${opts.id}/payments`,
		},
		paymentIntentId,
		chargeId,
	};
}

function invoiceListHandler(invoices: ReadonlyArray<MockStripeInvoice>) {
	return http.get(`${STRIPE_API_BASE}/v1/invoices`, ({request}) => {
		const requestUrl = new URL(request.url);
		const customer = requestUrl.searchParams.get('customer');
		const data = invoices.filter((invoice) => !customer || invoice.customer === customer);
		return HttpResponse.json({object: 'list', data, has_more: false, url: '/v1/invoices'});
	});
}

function refundCreateHandler(opts?: {
	status?: 'succeeded' | 'pending' | 'failed';
	failureReason?: string;
	onRequest?: (idempotencyKey: string | null) => void;
}) {
	return http.post(`${STRIPE_API_BASE}/v1/refunds`, async ({request}) => {
		opts?.onRequest?.(request.headers.get('idempotency-key'));
		const formData = await request.formData();
		const params = Object.fromEntries(formData.entries());
		const metadata: Record<string, string> = {};
		for (const [key, value] of Object.entries(params)) {
			const match = key.match(/^metadata\[(.+)\]$/);
			if (match) metadata[match[1]] = value as string;
		}
		return HttpResponse.json({
			id: 're_test_self_serve',
			object: 'refund',
			amount: Number.parseInt((params.amount as string) ?? '0', 10),
			currency: 'usd',
			status: opts?.status ?? 'succeeded',
			failure_reason: opts?.failureReason ?? null,
			payment_intent: params.payment_intent ?? null,
			charge: params.charge ?? null,
			metadata,
		});
	});
}

function subscriptionDeleteHandler() {
	return http.delete(`${STRIPE_API_BASE}/v1/subscriptions/:id`, ({params}) => {
		return HttpResponse.json({id: params.id, object: 'subscription', status: 'canceled'});
	});
}

async function setStripeIds(
	harness: ApiTestHarness,
	account: TestAccount,
	body: {
		stripe_customer_id?: string | null;
		stripe_subscription_id?: string | null;
		first_refund_at?: string | null;
	},
): Promise<void> {
	await createBuilder(harness, account.token).post(`/test/users/${account.userId}/premium`).body(body).execute();
}

describe('StripeRefundService self-serve refund', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		server.resetHandlers();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	describe('GET /premium/refund-eligibility', () => {
		test('reports no_refundable_purchase when user has no Stripe customer', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const account = await createTestAccount(harness);
			const response = await createBuilder<SelfServeRefundEligibilityResponse>(harness, account.token)
				.get('/premium/refund-eligibility')
				.execute();
			expect(response.eligible).toBe(false);
			expect(response.reason).toBe('no_refundable_purchase');
			expect(response.invoice_id).toBeNull();
		});
		test('reports eligible within 3-day window', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY})]));
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const response = await createBuilder<SelfServeRefundEligibilityResponse>(harness, account.token)
				.get('/premium/refund-eligibility')
				.execute();
			expect(response.eligible).toBe(true);
			expect(response.reason).toBeNull();
			expect(response.invoice_id).toBe('in_recent');
			expect(response.cancels_subscription).toBe(true);
		});
		test('reports outside_refund_window past 3 days', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(invoiceListHandler([buildInvoice({id: 'in_old', paidAtSecondsAgo: 5 * SECONDS_PER_DAY})]));
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID});
			const response = await createBuilder<SelfServeRefundEligibilityResponse>(harness, account.token)
				.get('/premium/refund-eligibility')
				.execute();
			expect(response.eligible).toBe(false);
			expect(response.reason).toBe('outside_refund_window');
		});
		test('reports cooldown_active within 30 days of prior refund', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY})]));
			const account = await createTestAccount(harness);
			const tenDaysAgo = new Date(Date.now() - 10 * SECONDS_PER_DAY * 1000);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				first_refund_at: tenDaysAgo.toISOString(),
			});
			const response = await createBuilder<SelfServeRefundEligibilityResponse>(harness, account.token)
				.get('/premium/refund-eligibility')
				.execute();
			expect(response.eligible).toBe(false);
			expect(response.reason).toBe('cooldown_active');
			expect(response.cooldown_expires_at).not.toBeNull();
		});
	});
	describe('POST /premium/refund-latest', () => {
		test('refunds latest invoice (one-off purchase, no subscription)', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(
				invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY, subscriptionId: null})]),
				refundCreateHandler(),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post('/premium/refund-latest')
				.execute();
			expect(response.invoice_id).toBe('in_recent');
			expect(response.refund_id).toBe('re_test_self_serve');
			expect(response.refunded_amount_cents).toBe(2500);
			expect(response.subscription_id).toBeNull();
		});
		test('refunds latest invoice and cancels subscription when present', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(
				invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY})]),
				refundCreateHandler(),
				subscriptionDeleteHandler(),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post('/premium/refund-latest')
				.execute();
			expect(response.subscription_id).toBe(MOCK_SUBSCRIPTION_ID);
		});
		test('rejects refund outside the 3-day window', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(invoiceListHandler([buildInvoice({id: 'in_old', paidAtSecondsAgo: 10 * SECONDS_PER_DAY})]));
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID});
			await createBuilder(harness, account.token)
				.post('/premium/refund-latest')
				.expect(403, APIErrorCodes.STRIPE_REFUND_OUTSIDE_WINDOW)
				.execute();
		});
		test('rejects refund during 30-day cooldown', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY})]));
			const account = await createTestAccount(harness);
			const fiveDaysAgo = new Date(Date.now() - 5 * SECONDS_PER_DAY * 1000);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				first_refund_at: fiveDaysAgo.toISOString(),
			});
			await createBuilder(harness, account.token)
				.post('/premium/refund-latest')
				.expect(403, APIErrorCodes.STRIPE_REFUND_COOLDOWN_ACTIVE)
				.execute();
		});
		test('rejects when there is no refundable purchase', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const account = await createTestAccount(harness);
			await createBuilder(harness, account.token)
				.post('/premium/refund-latest')
				.expect(400, APIErrorCodes.STRIPE_NO_PURCHASE_HISTORY)
				.execute();
		});
		test('does not finalize cooldown or cancel the subscription while the refund is still pending at the provider', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(
				invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY})]),
				refundCreateHandler({status: 'pending'}),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post('/premium/refund-latest')
				.execute();
			expect(response.status).toBe('pending');
			expect(response.refunded_amount_cents).toBe(0);
			expect(response.subscription_id).toBeNull();
			const {UserRepository} = await import('../../user/repositories/UserRepository');
			const updatedUser = await new UserRepository().findUnique(createUserID(BigInt(account.userId)));
			expect(updatedUser!.firstRefundAt).toBeNull();
		});
		test('does not finalize cooldown or cancel the subscription when the refund fails at the provider', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(
				invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY})]),
				refundCreateHandler({status: 'failed', failureReason: 'unknown'}),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post('/premium/refund-latest')
				.execute();
			expect(response.status).toBe('failed');
			expect(response.refunded_amount_cents).toBe(0);
			expect(response.subscription_id).toBeNull();
			const {UserRepository} = await import('../../user/repositories/UserRepository');
			const updatedUser = await new UserRepository().findUnique(createUserID(BigInt(account.userId)));
			expect(updatedUser!.firstRefundAt).toBeNull();
		});
		test('retries with a fresh idempotency key once a prior attempt has failed at the provider', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY})]));
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID});
			const idempotencyKeys: Array<string | null> = [];
			server.use(refundCreateHandler({status: 'failed', onRequest: (key) => idempotencyKeys.push(key)}));
			await createBuilder<SelfServeRefundResponse>(harness, account.token).post('/premium/refund-latest').execute();
			await createBuilder<SelfServeRefundResponse>(harness, account.token).post('/premium/refund-latest').execute();
			expect(idempotencyKeys).toHaveLength(2);
			expect(idempotencyKeys[0]).not.toBeNull();
			expect(idempotencyKeys[1]).not.toBeNull();
			expect(idempotencyKeys[1]).not.toBe(idempotencyKeys[0]);
			expect(idempotencyKeys[1]).toContain('retry-1');
		});
	});
});
