// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestUserWithPremium} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {
	deleteAccount,
	expectDataExists,
	setPendingDeletionAt,
	triggerDeletionWorker,
} from '@app/api/user/tests/UserTestUtils';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const CUSTOMER_ID = 'cus_test_deletion_ended_sub';
const SUBSCRIPTION_ID = 'sub_test_deletion_ended_sub';

describe('Account deletion with a subscription that already ended', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		stripeHandlers = createStripeApiHandlers({subscriptions: {[SUBSCRIPTION_ID]: {customer: CUSTOMER_ID}}});
		server.use(...stripeHandlers.handlers);
	});

	afterEach(() => {
		server.resetHandlers();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function scheduledSubscriber(status: 'active' | 'canceled'): Promise<string> {
		const account = await createTestUserWithPremium(harness, UserPremiumTypes.SUBSCRIPTION, {
			premiumUntil: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
			stripeCustomerId: CUSTOMER_ID,
			stripeSubscriptionId: SUBSCRIPTION_ID,
		});
		await deleteAccount(harness, account.token, account.password);
		await setPendingDeletionAt(harness, account.userId, new Date(Date.now() - 60_000));
		stripeHandlers.put.subscription(SUBSCRIPTION_ID, {status});
		server.use(
			http.delete(`https://api.stripe.com/v1/subscriptions/${SUBSCRIPTION_ID}`, () =>
				HttpResponse.json(
					{error: {type: 'invalid_request_error', message: 'Stripe refused the cancellation'}},
					{status: 400},
				),
			),
		);
		return account.userId;
	}

	test('deletes the account when the stored subscription is already canceled at Stripe', async () => {
		const userId = await scheduledSubscriber('canceled');
		const result = await triggerDeletionWorker(harness);
		expect(result).toMatchObject({scheduled: 1});
		expect((await expectDataExists(harness, userId)).hasDeletedFlag).toBe(true);
	});

	test('still halts the deletion when a live subscription cannot be canceled', async () => {
		const userId = await scheduledSubscriber('active');
		const result = await triggerDeletionWorker(harness);
		expect(result).toMatchObject({scheduled: 0});
		expect((await expectDataExists(harness, userId)).hasDeletedFlag).toBe(false);
	});
});
