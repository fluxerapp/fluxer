// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('user lookup indexes after a write from a stale snapshot', () => {
	let harness: ApiTestHarness;
	let repository: UserRepository;
	let userId: UserID;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
	});
	beforeEach(async () => {
		await harness.resetData();
		repository = new UserRepository();
		userId = createUserID(BigInt((await createTestAccount(harness)).userId));
	});

	test('a cleared subscription is not indexed again by a later stale patch', async () => {
		await repository.patchUpsert(userId, {stripe_subscription_id: 'sub_old', stripe_customer_id: 'cus_a'});
		const stale = await repository.findUniqueAssert(userId);
		await repository.patchUpsert(userId, {stripe_subscription_id: null});
		expect(await repository.findByStripeSubscriptionId('sub_old')).toBeNull();

		await repository.patchUpsert(userId, {premium_will_cancel: true}, stale.toRow());

		expect((await repository.findUniqueAssert(userId)).stripeSubscriptionId).toBeNull();
		expect(await repository.findByStripeSubscriptionId('sub_old')).toBeNull();
		expect((await repository.findByStripeCustomerId('cus_a'))?.id).toBe(userId);
	});

	test('a replaced subscription keeps only the new one indexed after a stale patch', async () => {
		await repository.patchUpsert(userId, {stripe_subscription_id: 'sub_old'});
		const stale = await repository.findUniqueAssert(userId);
		await repository.patchUpsert(userId, {stripe_subscription_id: 'sub_new'});

		await repository.patchUpsert(userId, {premium_will_cancel: false}, stale.toRow());

		expect(await repository.findByStripeSubscriptionId('sub_old')).toBeNull();
		expect((await repository.findByStripeSubscriptionId('sub_new'))?.id).toBe(userId);
	});

	test('a cleared customer id is not indexed again by a later stale patch', async () => {
		await repository.patchUpsert(userId, {stripe_customer_id: 'cus_old'});
		const stale = await repository.findUniqueAssert(userId);
		await repository.patchUpsert(userId, {stripe_customer_id: 'cus_new'});

		await repository.patchUpsert(userId, {premium_will_cancel: true}, stale.toRow());

		expect(await repository.findByStripeCustomerId('cus_old')).toBeNull();
		expect((await repository.findByStripeCustomerId('cus_new'))?.id).toBe(userId);
	});

	test('a stale patch that sets the same subscription again restores its index row', async () => {
		await repository.patchUpsert(userId, {stripe_subscription_id: 'sub_a'});
		const stale = await repository.findUniqueAssert(userId);
		await repository.patchUpsert(userId, {stripe_subscription_id: null});

		await repository.patchUpsert(userId, {stripe_subscription_id: 'sub_a'}, stale.toRow());

		expect((await repository.findUniqueAssert(userId)).stripeSubscriptionId).toBe('sub_a');
		expect((await repository.findByStripeSubscriptionId('sub_a'))?.id).toBe(userId);
	});
});
