// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {sendStripeWebhook, setupSyncStripeWebhookWorker} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeWebhookEventData} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {PremiumFlags} from '@fluxer/constants/src/UserConstants';
import type {
	PlutoniumWithdrawalListResponse,
	SelfServeRefundEligibilityResponse,
	SelfServeRefundResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const MOCK_CUSTOMER_ID = 'cus_self_serve_refund';
const MOCK_SUBSCRIPTION_ID = 'sub_self_serve_refund';
const SECONDS_PER_DAY = 24 * 60 * 60;
const STRIPE_API_BASE = 'https://api.stripe.com';

interface MockStripeInvoice {
	id: string;
	object: 'invoice';
	customer: string;
	parent: {subscription_details: {subscription: string; metadata?: Record<string, string>}} | null;
	billing_reason?: string;
	amount_due: number;
	amount_paid: number;
	currency: string;
	status: 'paid';
	created: number;
	lines?: {object: 'list'; data: Array<Record<string, unknown>>};
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
	amount?: number;
	billingReason?: string;
	metadata?: Record<string, string>;
	priceId?: string;
	periodDays?: number;
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
		parent:
			subscriptionId == null
				? null
				: {subscription_details: {subscription: subscriptionId, ...(opts.metadata ? {metadata: opts.metadata} : {})}},
		...(opts.billingReason ? {billing_reason: opts.billingReason} : {}),
		amount_due: opts.amount ?? 2500,
		amount_paid: opts.amount ?? 2500,
		currency: 'usd',
		status: 'paid',
		created: paidAt,
		...(opts.periodDays
			? {
					lines: {
						object: 'list',
						data: [
							{
								amount: opts.amount ?? 2500,
								period: {start: paidAt, end: paidAt + opts.periodDays * SECONDS_PER_DAY},
								pricing: {price_details: {price: opts.priceId ?? 'price_monthly'}},
								parent: null,
							},
						],
					},
				}
			: {}),
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
	refundId?: string;
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
			id: opts?.refundId ?? 're_test_self_serve',
			object: 'refund',
			amount: Number.parseInt((params.amount as string) ?? '0', 10),
			currency: 'usd',
			status: opts?.status ?? 'succeeded',
			created: Math.floor(Date.now() / 1000),
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
		setupSyncStripeWebhookWorker();
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
	});
	describe('Plutonium withdrawal', () => {
		const EU_CONSUMER = {eu_withdrawal_waiver_required: 'true', purchase_geoip_country_code: 'SE'};
		test('withdraws an EU subscription on day 10 during a refund cooldown, refunds it and ends the subscription', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const refundKinds: Array<string | undefined> = [];
			const deleted: Array<string> = [];
			server.use(
				invoiceListHandler([
					buildInvoice({
						id: 'in_create',
						paidAtSecondsAgo: 10 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata: EU_CONSUMER,
					}),
				]),
				refundCreateHandler({
					onRequest: (key) => refundKinds.push(key?.split(':')[0]),
				}),
				http.delete(`${STRIPE_API_BASE}/v1/subscriptions/:id`, ({params}) => {
					deleted.push(String(params.id));
					return HttpResponse.json({id: params.id, object: 'subscription', status: 'canceled'});
				}),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
				first_refund_at: new Date(Date.now() - 5 * SECONDS_PER_DAY * 1000).toISOString(),
			});
			const open = await createBuilder<PlutoniumWithdrawalListResponse>(harness, account.token)
				.get('/premium/withdrawals')
				.execute();
			expect(open).toMatchObject([{id: MOCK_SUBSCRIPTION_ID, kind: 'subscription', amount_minor: 2500}]);
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post(`/premium/withdrawals/${MOCK_SUBSCRIPTION_ID}`)
				.body({name: 'Alex Lindqvist'})
				.execute();
			expect(response).toMatchObject({refunded_amount_cents: 2500, subscription_id: MOCK_SUBSCRIPTION_ID});
			expect(refundKinds).toEqual(['plutonium-withdrawal']);
			expect(deleted).toEqual([MOCK_SUBSCRIPTION_ID]);
		});
		test('keeps the withdrawal open after the subscription already ended and refunds without canceling', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const refundKinds: Array<string | undefined> = [];
			const deleted: Array<string> = [];
			server.use(
				invoiceListHandler([
					buildInvoice({
						id: 'in_ended',
						paidAtSecondsAgo: 10 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata: EU_CONSUMER,
					}),
				]),
				refundCreateHandler({onRequest: (key) => refundKinds.push(key?.split(':')[0])}),
				http.delete(`${STRIPE_API_BASE}/v1/subscriptions/:id`, ({params}) => {
					deleted.push(String(params.id));
					return HttpResponse.json({id: params.id, object: 'subscription', status: 'canceled'});
				}),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID, stripe_subscription_id: null});
			const open = await createBuilder<PlutoniumWithdrawalListResponse>(harness, account.token)
				.get('/premium/withdrawals')
				.execute();
			expect(open).toMatchObject([{id: MOCK_SUBSCRIPTION_ID, kind: 'subscription'}]);
			await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post(`/premium/withdrawals/${MOCK_SUBSCRIPTION_ID}`)
				.body({name: 'Alex Lindqvist'})
				.execute();
			expect(refundKinds).toEqual(['plutonium-withdrawal']);
			expect(deleted).toEqual([]);
		});
		test('refunds a Brazilian Pix withdrawal in full', async () => {
			server.use(
				...createStripeApiHandlers({charges: {ch_in_create: {payment_method_details: {type: 'pix'}}}}).handlers,
			);
			const amounts: Array<string> = [];
			server.use(
				invoiceListHandler([
					buildInvoice({
						id: 'in_create',
						paidAtSecondsAgo: 2 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata: {withdrawal_country: 'BR'},
					}),
				]),
				http.post(`${STRIPE_API_BASE}/v1/refunds`, async ({request}) => {
					const amount = String((await request.formData()).get('amount'));
					amounts.push(amount);
					return HttpResponse.json({
						id: 're_pix_withdrawal',
						object: 'refund',
						amount: Number(amount),
						currency: 'brl',
						status: 'succeeded',
						created: Math.floor(Date.now() / 1000),
						payment_intent: 'pi_in_create',
						metadata: {},
					});
				}),
				subscriptionDeleteHandler(),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post(`/premium/withdrawals/${MOCK_SUBSCRIPTION_ID}`)
				.body({name: 'Ana Souza'})
				.execute();
			expect(amounts).toEqual(['2500']);
			expect(response.refunded_amount_cents).toBe(2500);
		});
		test('records a withdrawal whose refund Stripe refused, so pressing the button again refunds nothing', async () => {
			server.use(...createStripeApiHandlers().handlers);
			let refundRequests = 0;
			server.use(
				invoiceListHandler([
					buildInvoice({
						id: 'in_create',
						paidAtSecondsAgo: 3 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata: EU_CONSUMER,
					}),
				]),
				http.post(`${STRIPE_API_BASE}/v1/refunds`, () => {
					refundRequests += 1;
					return HttpResponse.json({error: {type: 'invalid_request_error', message: 'Refund refused'}}, {status: 400});
				}),
				subscriptionDeleteHandler(),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const first = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post(`/premium/withdrawals/${MOCK_SUBSCRIPTION_ID}`)
				.body({name: 'Alex Lindqvist'})
				.execute();
			expect(first.status).toBe('failed');
			const second = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post(`/premium/withdrawals/${MOCK_SUBSCRIPTION_ID}`)
				.body({name: 'Alex Lindqvist'})
				.execute();
			expect(second).toEqual(first);
			expect(refundRequests).toBe(1);
			const open = await createBuilder<PlutoniumWithdrawalListResponse>(harness, account.token)
				.get('/premium/withdrawals')
				.execute();
			expect(open).toEqual([]);
		});
		test('lists an EU renewal only for a subscription started before checkout asked for the days used', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const renewal = (metadata: Record<string, string>) =>
				invoiceListHandler([
					buildInvoice({
						id: 'in_renewal',
						paidAtSecondsAgo: 2 * SECONDS_PER_DAY,
						billingReason: 'subscription_cycle',
						metadata,
						priceId: 'price_monthly',
						amount: 3000,
						periodDays: 30,
					}),
					buildInvoice({
						id: 'in_first',
						paidAtSecondsAgo: 32 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata,
						priceId: 'price_monthly',
						amount: 3000,
						periodDays: 30,
					}),
				]);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const list = () =>
				createBuilder<PlutoniumWithdrawalListResponse>(harness, account.token).get('/premium/withdrawals').execute();
			server.use(renewal({...EU_CONSUMER, withdrawal_pro_rata: 'true'}));
			expect(await list()).toEqual([]);
			server.use(renewal(EU_CONSUMER));
			expect(await list()).toMatchObject([{id: MOCK_SUBSCRIPTION_ID, amount_minor: 3000, paid_minor: 3000}]);
		});
		test('charges an EU subscription started under the current checkout for the days used', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(
				invoiceListHandler([
					buildInvoice({
						id: 'in_eu',
						paidAtSecondsAgo: 2.5 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata: {...EU_CONSUMER, withdrawal_pro_rata: 'true'},
						amount: 3000,
						periodDays: 30,
					}),
				]),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const open = await createBuilder<PlutoniumWithdrawalListResponse>(harness, account.token)
				.get('/premium/withdrawals')
				.execute();
			expect(open).toMatchObject([{amount_minor: 2700, paid_minor: 3000}]);
		});
		test('lists no Brazilian renewal, only a first payment still inside its 7 days', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const brazil = {withdrawal_country: 'BR'};
			server.use(
				invoiceListHandler([
					buildInvoice({
						id: 'in_renewal',
						paidAtSecondsAgo: 2 * SECONDS_PER_DAY,
						billingReason: 'subscription_cycle',
						metadata: brazil,
						priceId: 'price_monthly',
						periodDays: 30,
					}),
					buildInvoice({
						id: 'in_first',
						paidAtSecondsAgo: 32 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata: brazil,
						priceId: 'price_monthly',
						periodDays: 30,
					}),
				]),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const open = await createBuilder<PlutoniumWithdrawalListResponse>(harness, account.token)
				.get('/premium/withdrawals')
				.execute();
			expect(open).toEqual([]);
		});
		test('refunds a UK consumer told about it at checkout the price less the days of Plutonium used', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const amounts: Array<string> = [];
			server.use(
				invoiceListHandler([
					buildInvoice({
						id: 'in_uk',
						paidAtSecondsAgo: 2.5 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata: {
							eu_withdrawal_waiver_required: 'true',
							purchase_geoip_country_code: 'GB',
							withdrawal_pro_rata: 'true',
						},
						amount: 3000,
						periodDays: 30,
					}),
				]),
				http.post(`${STRIPE_API_BASE}/v1/refunds`, async ({request}) => {
					const amount = String((await request.formData()).get('amount'));
					amounts.push(amount);
					return HttpResponse.json({
						id: 're_uk',
						object: 'refund',
						amount: Number(amount),
						currency: 'usd',
						status: 'succeeded',
						created: Math.floor(Date.now() / 1000),
						payment_intent: 'pi_in_uk',
						metadata: {},
					});
				}),
				subscriptionDeleteHandler(),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const [open] = await createBuilder<PlutoniumWithdrawalListResponse>(harness, account.token)
				.get('/premium/withdrawals')
				.execute();
			expect(open).toMatchObject({amount_minor: 2700, paid_minor: 3000});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post(`/premium/withdrawals/${MOCK_SUBSCRIPTION_ID}`)
				.body({name: 'Sam Taylor', quoted_at: open!.quoted_at})
				.execute();
			expect(amounts).toEqual(['2700']);
			expect(response.refunded_amount_cents).toBe(2700);
		});
		test('refunds a UK consumer in full when checkout never told them about paying for the days used', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const amounts: Array<string> = [];
			server.use(
				invoiceListHandler([
					buildInvoice({
						id: 'in_uk',
						paidAtSecondsAgo: 2.5 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata: {eu_withdrawal_waiver_required: 'true', purchase_geoip_country_code: 'GB'},
						amount: 3000,
						periodDays: 30,
					}),
				]),
				http.post(`${STRIPE_API_BASE}/v1/refunds`, async ({request}) => {
					const amount = String((await request.formData()).get('amount'));
					amounts.push(amount);
					return HttpResponse.json({
						id: 're_uk',
						object: 'refund',
						amount: Number(amount),
						currency: 'usd',
						status: 'succeeded',
						created: Math.floor(Date.now() / 1000),
						payment_intent: 'pi_in_uk',
						metadata: {},
					});
				}),
				subscriptionDeleteHandler(),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const [open] = await createBuilder<PlutoniumWithdrawalListResponse>(harness, account.token)
				.get('/premium/withdrawals')
				.execute();
			expect(open).toMatchObject({amount_minor: 3000, paid_minor: 3000});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post(`/premium/withdrawals/${MOCK_SUBSCRIPTION_ID}`)
				.body({name: 'Sam Taylor', quoted_at: open!.quoted_at})
				.execute();
			expect(amounts).toEqual(['3000']);
			expect(response.refunded_amount_cents).toBe(3000);
		});
		test('refuses a withdrawal after the withdrawal period and lists nothing', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(
				invoiceListHandler([
					buildInvoice({
						id: 'in_old',
						paidAtSecondsAgo: 25 * SECONDS_PER_DAY,
						billingReason: 'subscription_create',
						metadata: EU_CONSUMER,
					}),
				]),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const open = await createBuilder<PlutoniumWithdrawalListResponse>(harness, account.token)
				.get('/premium/withdrawals')
				.execute();
			expect(open).toEqual([]);
			await createBuilder(harness, account.token)
				.post(`/premium/withdrawals/${MOCK_SUBSCRIPTION_ID}`)
				.body({name: 'Alex Lindqvist'})
				.expect(403, APIErrorCodes.STRIPE_REFUND_OUTSIDE_WINDOW)
				.execute();
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
		test('refunds every payment inside the 3-day window, such as an upgrade and the payment before it', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const refunded: Array<{paymentIntent: string; amount: number; subscription: string | undefined}> = [];
			const deleted: Array<string> = [];
			server.use(
				invoiceListHandler([
					buildInvoice({id: 'in_upgrade', paidAtSecondsAgo: 60 * 60, amount: 4500}),
					buildInvoice({id: 'in_first', paidAtSecondsAgo: SECONDS_PER_DAY, amount: 499}),
					buildInvoice({id: 'in_last_month', paidAtSecondsAgo: 31 * SECONDS_PER_DAY, amount: 499}),
				]),
				http.post(`${STRIPE_API_BASE}/v1/refunds`, async ({request}) => {
					const params = Object.fromEntries((await request.formData()).entries()) as Record<string, string>;
					refunded.push({
						paymentIntent: params.payment_intent!,
						amount: Number(params.amount),
						subscription: params['metadata[subscription_id]'],
					});
					return HttpResponse.json({
						id: `re_${params.payment_intent}`,
						object: 'refund',
						amount: Number(params.amount),
						currency: 'usd',
						status: 'succeeded',
						created: Math.floor(Date.now() / 1000),
						payment_intent: params.payment_intent,
						metadata: {
							refund_kind: 'self_serve',
							user_id: params['metadata[user_id]'],
							invoice_id: params['metadata[invoice_id]'],
							subscription_id: params['metadata[subscription_id]'],
						},
					});
				}),
				http.delete(`${STRIPE_API_BASE}/v1/subscriptions/:id`, ({params}) => {
					deleted.push(String(params.id));
					return HttpResponse.json({id: params.id, object: 'subscription', status: 'canceled'});
				}),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			const eligibility = await createBuilder<SelfServeRefundEligibilityResponse>(harness, account.token)
				.get('/premium/refund-eligibility')
				.execute();
			expect(eligibility).toMatchObject({eligible: true, invoice_id: 'in_upgrade', invoice_amount_paid_cents: 4999});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post('/premium/refund-latest')
				.execute();
			expect(refunded).toEqual([
				{paymentIntent: 'pi_in_upgrade', amount: 4500, subscription: MOCK_SUBSCRIPTION_ID},
				{paymentIntent: 'pi_in_first', amount: 499, subscription: MOCK_SUBSCRIPTION_ID},
			]);
			expect(deleted).toEqual([MOCK_SUBSCRIPTION_ID]);
			expect(response).toMatchObject({
				invoice_id: 'in_upgrade',
				refunded_amount_cents: 4999,
				invoice_amount_paid_cents: 4999,
				subscription_id: MOCK_SUBSCRIPTION_ID,
				status: 'succeeded',
			});
			await createBuilder(harness, account.token)
				.post('/premium/refund-latest')
				.expect(403, APIErrorCodes.STRIPE_REFUND_COOLDOWN_ACTIVE)
				.execute();
		});
		test('requests one cent under the captured amount when the underlying charge was paid via PIX', async () => {
			server.use(
				...createStripeApiHandlers({charges: {ch_in_recent: {payment_method_details: {type: 'pix'}}}}).handlers,
			);
			server.use(
				invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY, subscriptionId: null})]),
				refundCreateHandler(),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post('/premium/refund-latest')
				.execute();
			expect(response.invoice_amount_paid_cents).toBe(2500);
			expect(response.refunded_amount_cents).toBe(2499);
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
		test('accepts a refund just inside the 3-day window', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(
				invoiceListHandler([
					buildInvoice({id: 'in_edge', paidAtSecondsAgo: 3 * SECONDS_PER_DAY - 60, subscriptionId: null}),
				]),
				refundCreateHandler(),
			);
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post('/premium/refund-latest')
				.execute();
			expect(response.status).toBe('succeeded');
		});
		test('rejects a refund just past the 3-day window', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(invoiceListHandler([buildInvoice({id: 'in_edge', paidAtSecondsAgo: 3 * SECONDS_PER_DAY + 60})]));
			const account = await createTestAccount(harness);
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID});
			await createBuilder(harness, account.token)
				.post('/premium/refund-latest')
				.expect(403, APIErrorCodes.STRIPE_REFUND_OUTSIDE_WINDOW)
				.execute();
		});
		test('rejects a refund when the last one was just under 30 days ago', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY})]));
			const account = await createTestAccount(harness);
			const lastRefundAt = new Date(Date.now() - (30 * SECONDS_PER_DAY - 60) * 1000);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				first_refund_at: lastRefundAt.toISOString(),
			});
			const eligibility = await createBuilder<SelfServeRefundEligibilityResponse>(harness, account.token)
				.get('/premium/refund-eligibility')
				.execute();
			expect(eligibility.eligible).toBe(false);
			expect(eligibility.reason).toBe('cooldown_active');
			expect(eligibility.cooldown_expires_at).toBe(
				new Date(lastRefundAt.getTime() + 30 * SECONDS_PER_DAY * 1000).toISOString(),
			);
			await createBuilder(harness, account.token)
				.post('/premium/refund-latest')
				.expect(403, APIErrorCodes.STRIPE_REFUND_COOLDOWN_ACTIVE)
				.execute();
		});
		test('allows another refund once 30 days have passed, never blocks purchases, and restarts the cooldown', async () => {
			server.use(...createStripeApiHandlers().handlers);
			server.use(
				invoiceListHandler([buildInvoice({id: 'in_recent', paidAtSecondsAgo: SECONDS_PER_DAY, subscriptionId: null})]),
				refundCreateHandler({refundId: 're_second_window'}),
			);
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const lastRefundAt = new Date(Date.now() - (30 * SECONDS_PER_DAY + 60) * 1000);
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				first_refund_at: lastRefundAt.toISOString(),
			});
			const eligibility = await createBuilder<SelfServeRefundEligibilityResponse>(harness, account.token)
				.get('/premium/refund-eligibility')
				.execute();
			expect(eligibility.eligible).toBe(true);
			expect(eligibility.cooldown_expires_at).toBeNull();
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post('/premium/refund-latest')
				.execute();
			expect(response.status).toBe('succeeded');
			const updatedUser = await new UserRepository().findUnique(userId);
			expect(updatedUser!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
			expect(updatedUser!.lastRefundAt!.getTime()).toBeGreaterThan(Date.now() - 60 * 1000);
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
		test('starts the cooldown but keeps the subscription while the refund is still pending at the provider', async () => {
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
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const updatedUser = await new UserRepository().findUnique(createUserID(BigInt(account.userId)));
			expect(updatedUser!.lastRefundAt).not.toBeNull();
			await createBuilder(harness, account.token)
				.post('/premium/refund-latest')
				.expect(403, APIErrorCodes.STRIPE_REFUND_COOLDOWN_ACTIVE)
				.execute();
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
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const updatedUser = await new UserRepository().findUnique(createUserID(BigInt(account.userId)));
			expect(updatedUser!.lastRefundAt).toBeNull();
		});
		test('starts the cooldown once even when charge.refunded arrives afterwards and is retried', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const invoice = buildInvoice({
				id: 'in_cross_path',
				paidAtSecondsAgo: SECONDS_PER_DAY,
				subscriptionId: null,
			});
			server.use(invoiceListHandler([invoice]), refundCreateHandler({refundId: 're_cross_path'}));
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID});
			const response = await createBuilder<SelfServeRefundResponse>(harness, account.token)
				.post('/premium/refund-latest')
				.execute();
			expect(response.refund_id).toBe('re_cross_path');
			expect(response.status).toBe('succeeded');
			const userRepository = new UserRepository();
			const afterSelfServe = await userRepository.findUnique(userId);
			expect(afterSelfServe!.lastRefundAt).not.toBeNull();
			expect(afterSelfServe!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
			const chargeEvent = {
				type: 'charge.refunded' as const,
				data: {
					object: {
						id: invoice.chargeId,
						payment_intent: invoice.paymentIntentId,
						customer: MOCK_CUSTOMER_ID,
						amount_refunded: 2500,
						refunds: {
							object: 'list',
							has_more: false,
							url: `/v1/charges/${invoice.chargeId}/refunds`,
							data: [
								{
									id: 're_cross_path',
									object: 'refund',
									charge: invoice.chargeId,
									payment_intent: invoice.paymentIntentId,
									amount: 2500,
									currency: 'usd',
									status: 'succeeded',
									created: Math.floor(Date.now() / 1000),
									metadata: {
										refund_kind: 'self_serve',
										user_id: account.userId,
										invoice_id: invoice.id,
									},
								},
							],
						},
					},
				},
			};
			await sendStripeWebhook(harness, {...chargeEvent, id: 'evt_cross_path_1'});
			await sendStripeWebhook(harness, {...chargeEvent, id: 'evt_cross_path_2'});
			const afterWebhook = await userRepository.findUnique(userId);
			expect(afterWebhook!.lastRefundAt!.getTime()).toBe(afterSelfServe!.lastRefundAt!.getTime());
			expect(afterWebhook!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
		});
		test('starts the cooldown once even when refund.updated confirms it after the endpoint already did', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const invoice = buildInvoice({
				id: 'in_double_confirm',
				paidAtSecondsAgo: SECONDS_PER_DAY,
				subscriptionId: null,
			});
			server.use(invoiceListHandler([invoice]), refundCreateHandler({refundId: 're_double_confirm'}));
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			await setStripeIds(harness, account, {stripe_customer_id: MOCK_CUSTOMER_ID});
			await createBuilder<SelfServeRefundResponse>(harness, account.token).post('/premium/refund-latest').execute();
			const userRepository = new UserRepository();
			const afterSelfServe = await userRepository.findUnique(userId);
			expect(afterSelfServe!.lastRefundAt).not.toBeNull();
			await sendStripeWebhook(harness, {
				id: 'evt_double_confirm_1',
				type: 'refund.updated',
				data: {
					object: {
						id: 're_double_confirm',
						object: 'refund',
						status: 'succeeded',
						amount: 2500,
						currency: 'usd',
						created: Math.floor(afterSelfServe!.lastRefundAt!.getTime() / 1000),
						metadata: {
							refund_kind: 'self_serve',
							user_id: account.userId,
							invoice_id: invoice.id,
						},
					},
				},
			});
			const afterWebhook = await userRepository.findUnique(userId);
			expect(afterWebhook!.lastRefundAt!.getTime()).toBe(afterSelfServe!.lastRefundAt!.getTime());
			expect(afterWebhook!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
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
	describe('self-serve refund teardown targeting', () => {
		function trackingSubscriptionDeleteHandler(deleted: Array<string>) {
			return http.delete(`${STRIPE_API_BASE}/v1/subscriptions/:id`, ({params}) => {
				deleted.push(String(params.id));
				return HttpResponse.json({id: params.id, object: 'subscription', status: 'canceled'});
			});
		}

		function buildRefundUpdatedEvent(opts: {
			eventId: string;
			refundId: string;
			userId: string;
			invoiceId: string;
			subscriptionId: string;
		}): StripeWebhookEventData {
			return {
				id: opts.eventId,
				type: 'refund.updated',
				data: {
					object: {
						id: opts.refundId,
						object: 'refund',
						status: 'succeeded',
						amount: 2500,
						currency: 'usd',
						metadata: {
							refund_kind: 'self_serve',
							user_id: opts.userId,
							invoice_id: opts.invoiceId,
							subscription_id: opts.subscriptionId,
						},
					},
				},
			};
		}

		test('leaves a newer subscription alone when the refunded one is no longer current', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const deleted: Array<string> = [];
			server.use(trackingSubscriptionDeleteHandler(deleted));
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: 'sub_bought_after_the_refund',
			});
			await sendStripeWebhook(
				harness,
				buildRefundUpdatedEvent({
					eventId: 'evt_stale_teardown',
					refundId: 're_stale_teardown',
					userId: account.userId,
					invoiceId: 'in_stale_teardown',
					subscriptionId: 'sub_refunded_and_already_gone',
				}),
			);
			expect(deleted).toEqual([]);
			const userRepository = new UserRepository();
			const user = await userRepository.findUnique(userId);
			expect(user!.stripeSubscriptionId).toBe('sub_bought_after_the_refund');
		});

		test('cancels the subscription when the refunded one is still current', async () => {
			server.use(...createStripeApiHandlers().handlers);
			const deleted: Array<string> = [];
			server.use(trackingSubscriptionDeleteHandler(deleted));
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			await setStripeIds(harness, account, {
				stripe_customer_id: MOCK_CUSTOMER_ID,
				stripe_subscription_id: MOCK_SUBSCRIPTION_ID,
			});
			await sendStripeWebhook(
				harness,
				buildRefundUpdatedEvent({
					eventId: 'evt_current_teardown',
					refundId: 're_current_teardown',
					userId: account.userId,
					invoiceId: 'in_current_teardown',
					subscriptionId: MOCK_SUBSCRIPTION_ID,
				}),
			);
			expect(deleted).toEqual([MOCK_SUBSCRIPTION_ID]);
			const userRepository = new UserRepository();
			const user = await userRepository.findUnique(userId);
			expect(user!.stripeSubscriptionId).toBeNull();
		});
	});
});
