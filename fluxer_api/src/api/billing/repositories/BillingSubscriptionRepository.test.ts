// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StripeSubscriptionPayload} from '@app/api/billing/mappers/StripeToBillingMapper';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const DAY_SECONDS = 24 * 60 * 60;

function buildSubscription(params: {
	customerId: string;
	periodEnd: number;
	periodStart: number;
	priceId: string;
	subscriptionId: string;
	userId: string;
}): StripeSubscriptionPayload {
	return {
		id: params.subscriptionId,
		cancel_at: null,
		cancel_at_period_end: false,
		canceled_at: null,
		cancellation_details: null,
		collection_method: 'charge_automatically',
		created: 1_779_000_000,
		currency: 'usd',
		customer: params.customerId,
		default_payment_method: null,
		ended_at: null,
		items: {
			data: [
				{
					id: `si_${params.subscriptionId}`,
					current_period_start: params.periodStart,
					current_period_end: params.periodEnd,
					price: {
						id: params.priceId,
						product: 'prod_subscription',
						unit_amount: 499,
					},
					quantity: 1,
				},
			],
		},
		latest_invoice: null,
		livemode: false,
		metadata: {user_id: params.userId},
		start_date: 1_779_000_000,
		status: 'active',
		trial_end: null,
		trial_start: null,
	};
}

describe('BillingSubscriptionRepository', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	test('updates subscription period data when Stripe timestamp is unchanged', async () => {
		const repository = getBillingRepository().subscriptions;
		const userId = '1471345545862947484';
		const subscriptionId = 'sub_equal_timestamp_refresh';
		const customerId = 'cus_equal_timestamp_refresh';
		const oldPeriodStart = 1_779_000_000;
		const oldPeriodEnd = oldPeriodStart + 30 * DAY_SECONDS;
		const newPeriodStart = oldPeriodEnd;
		const newPeriodEnd = newPeriodStart + 31 * DAY_SECONDS;
		await repository.upsertFromStripe(
			buildSubscription({
				customerId,
				periodStart: oldPeriodStart,
				periodEnd: oldPeriodEnd,
				priceId: 'price_monthly_usd',
				subscriptionId,
				userId,
			}),
			{knownUserId: BigInt(userId)},
		);
		await repository.upsertFromStripe(
			buildSubscription({
				customerId,
				periodStart: newPeriodStart,
				periodEnd: newPeriodEnd,
				priceId: 'price_monthly_usd',
				subscriptionId,
				userId,
			}),
			{knownUserId: BigInt(userId)},
		);
		const row = await repository.findById(subscriptionId);
		expect(row?.current_period_start?.toISOString()).toBe(new Date(newPeriodStart * 1000).toISOString());
		expect(row?.current_period_end?.toISOString()).toBe(new Date(newPeriodEnd * 1000).toISOString());
		expect(row?.user_id?.toString()).toBe(userId);
	});
});
