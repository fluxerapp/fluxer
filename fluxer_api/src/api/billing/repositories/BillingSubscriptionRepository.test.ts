// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StripeSubscriptionPayload} from '@app/api/billing/mappers/StripeToBillingMapper';
import {deleteOneOrMany} from '@app/api/database/CassandraQueryExecution';
import {getBillingRepository} from '@app/api/middleware/ServiceRegistry';
import {BillingSubscriptionsByCustomer, BillingSubscriptionsByUser} from '@app/api/Tables';
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
	status?: StripeSubscriptionPayload['status'];
	canceledAt?: number | null;
}): StripeSubscriptionPayload {
	return {
		id: params.subscriptionId,
		cancel_at: null,
		cancel_at_period_end: false,
		canceled_at: params.canceledAt ?? null,
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
		status: params.status ?? 'active',
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
	test('rewrites lost index rows when the same subscription is mirrored again', async () => {
		const repository = getBillingRepository().subscriptions;
		const userId = '1471345545862947485';
		const subscriptionId = 'sub_lost_index_rows';
		const customerId = 'cus_lost_index_rows';
		const payload = buildSubscription({
			customerId,
			periodStart: 1_779_000_000,
			periodEnd: 1_779_000_000 + 30 * DAY_SECONDS,
			priceId: 'price_monthly_usd',
			subscriptionId,
			userId,
			status: 'canceled',
			canceledAt: 1_779_100_000,
		});
		await repository.upsertFromStripe(payload, {knownUserId: BigInt(userId)});
		await deleteOneOrMany(
			BillingSubscriptionsByUser.deleteByPk({user_id: BigInt(userId), provider_id: subscriptionId}),
		);
		await deleteOneOrMany(
			BillingSubscriptionsByCustomer.deleteByPk({customer_id: customerId, provider_id: subscriptionId}),
		);
		expect(await repository.listByUser(BigInt(userId))).toHaveLength(0);
		const replay = await repository.upsertFromStripe(payload, {knownUserId: BigInt(userId)});
		expect(replay.changed).toBe(false);
		const byUser = await repository.listByUser(BigInt(userId));
		const byCustomer = await repository.listByCustomer(customerId);
		expect(byUser.map((row) => row.provider_id)).toEqual([subscriptionId]);
		expect(byCustomer.map((row) => row.provider_id)).toEqual([subscriptionId]);
	});
	test('keeps index rows on the stored state when an older snapshot arrives', async () => {
		const repository = getBillingRepository().subscriptions;
		const userId = '1471345545862947486';
		const subscriptionId = 'sub_stale_snapshot_index';
		const customerId = 'cus_stale_snapshot_index';
		const base = {
			customerId,
			periodStart: 1_779_000_000,
			periodEnd: 1_779_000_000 + 30 * DAY_SECONDS,
			priceId: 'price_monthly_usd',
			subscriptionId,
			userId,
		};
		await repository.upsertFromStripe(buildSubscription({...base, status: 'canceled', canceledAt: 1_779_200_000}), {
			knownUserId: BigInt(userId),
		});
		await deleteOneOrMany(
			BillingSubscriptionsByUser.deleteByPk({user_id: BigInt(userId), provider_id: subscriptionId}),
		);
		const stale = await repository.upsertFromStripe(buildSubscription({...base, status: 'active'}), {
			knownUserId: BigInt(userId),
			snapshotCapturedAt: new Date(1_779_100_000 * 1000),
		});
		expect(stale.changed).toBe(false);
		const byUser = await repository.listByUser(BigInt(userId));
		expect(byUser.map((row) => [row.provider_id, row.status])).toEqual([[subscriptionId, 'canceled']]);
	});
});
