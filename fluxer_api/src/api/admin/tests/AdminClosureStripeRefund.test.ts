// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {close, closeRequest, createStaff} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {createTestUserWithPremium} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type Stripe from 'stripe';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const CUSTOMER_ID = 'cus_test_closure_refund';
const SUBSCRIPTION_ID = 'sub_test_closure_refund';
const INVOICE_ID = 'in_test_closure_refund';
const PAYMENT_INTENT_ID = 'pi_test_closure_refund';

async function mirrorPaidSubscription(): Promise<void> {
	const created = Math.floor(Date.now() / 1000) - 600;
	const billing = getBillingRepository();
	await billing.subscriptions.upsertFromStripe({
		id: SUBSCRIPTION_ID,
		object: 'subscription',
		customer: CUSTOMER_ID,
		status: 'active',
		latest_invoice: INVOICE_ID,
		created,
		metadata: {},
		items: {object: 'list', data: [], has_more: false, url: ''},
	} as never);
	await billing.invoices.upsertFromStripe({
		id: INVOICE_ID,
		object: 'invoice',
		customer: CUSTOMER_ID,
		status: 'paid',
		created,
		currency: 'usd',
		total: 499,
		amount_paid: 499,
		lines: {object: 'list', data: [], has_more: false, url: ''},
		parent: {type: 'subscription_details', subscription_details: {subscription: SUBSCRIPTION_ID}},
		payments: {
			object: 'list',
			has_more: false,
			url: '',
			data: [
				{
					id: 'inpay_test_closure_refund',
					object: 'invoice_payment',
					invoice: INVOICE_ID,
					is_default: true,
					status: 'paid',
					amount_paid: 499,
					amount_requested: 499,
					currency: 'usd',
					created,
					payment: {type: 'payment_intent', payment_intent: PAYMENT_INTENT_ID},
					status_transitions: {paid_at: created, canceled_at: null},
				},
			],
		},
	} as unknown as Stripe.Invoice);
}

describe('Account closure with an active Stripe subscription', () => {
	let harness: ApiTestHarness;
	let staff: TestAccount;
	let stripeHandlers: StripeApiHandlers;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		stripeHandlers = createStripeApiHandlers({subscriptions: {[SUBSCRIPTION_ID]: {customer: CUSTOMER_ID}}});
		server.use(...stripeHandlers.handlers);
		staff = await createStaff(harness);
		await mirrorPaidSubscription();
	});

	afterEach(() => {
		server.resetHandlers();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function subscriber(): Promise<TestAccount> {
		return createTestUserWithPremium(harness, UserPremiumTypes.SUBSCRIPTION, {
			premiumUntil: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
			stripeCustomerId: CUSTOMER_ID,
			stripeSubscriptionId: SUBSCRIPTION_ID,
		});
	}

	test('an enforcement closure cancels the subscription and refunds a payment the invoice paid by payment intent', async () => {
		const account = await subscriber();
		await close(harness, staff, account.userId);
		expect(stripeHandlers.spies.cancelledSubscriptions).toEqual([SUBSCRIPTION_ID]);
		expect(stripeHandlers.spies.createdRefunds).toEqual([
			expect.objectContaining({payment_intent: PAYMENT_INTENT_ID, reason: 'fraudulent'}),
		]);
	});

	test('a closure the holder asked for cancels the subscription without a refund', async () => {
		const account = await subscriber();
		await closeRequest(harness, staff, account.userId, {days_until_deletion: 14, basis: {type: 'holder_request'}})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(stripeHandlers.spies.cancelledSubscriptions).toEqual([SUBSCRIPTION_ID]);
		expect(stripeHandlers.spies.createdRefunds).toEqual([]);
	});
});
