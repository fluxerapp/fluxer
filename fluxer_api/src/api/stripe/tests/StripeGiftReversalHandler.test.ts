// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {getPremiumStateReconciliationQueueService, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {findUser} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {ms} from 'itty-time';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('StripeGiftReversalHandler', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	beforeEach(async () => {
		await harness.resetData();
	});

	function createHandler(users: IUserRepository = getUserRepository()): StripeGiftReversalHandler {
		return new StripeGiftReversalHandler(users, getGatewayService(), getPremiumStateReconciliationQueueService());
	}

	async function createRedeemedGift(creatorId: UserID, redeemerId: UserID, redeemedAt: Date): Promise<string> {
		const code = `giftreversal${randomUUID().replaceAll('-', '').slice(0, 20)}`;
		await getUserRepository().createGiftCode({
			code,
			duration_months: null,
			duration_type: 'months',
			duration_quantity: 1,
			created_at: redeemedAt,
			created_by_user_id: creatorId,
			redeemed_at: redeemedAt,
			redeemed_by_user_id: redeemerId,
			stripe_payment_intent_id: `pi_${code}`,
			visionary_sequence_number: null,
			checkout_session_id: null,
			version: 1,
		});
		return code;
	}

	async function createGiftedUser(giftCount: number): Promise<{accountId: string; codes: Array<string>}> {
		const gifter = await createTestAccount(harness);
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const creatorId = createUserID(BigInt(gifter.userId));
		const codes: Array<string> = [];
		for (let index = 0; index < giftCount; index++) {
			codes.push(await createRedeemedGift(creatorId, userId, new Date(Date.now() - ms('1 hour') + index * 1000)));
		}
		const user = await findUser(account.userId);
		await getUserRepository().patchUpsert(
			userId,
			{
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: null,
				premium_gift_extension_ends_at: new Date(Date.now() + giftCount * ms('30 days')),
			},
			user.toRow(),
		);
		return {accountId: account.userId, codes};
	}

	async function reverse(code: string, handler = createHandler()): Promise<void> {
		const gift = await getUserRepository().findGiftCode(code);
		await handler.handleGiftPremiumReversal(gift!, {reason: 'charge_dispute'});
	}

	it('revokes premium on a retry after an earlier reversal claimed the gift but never wrote the user', async () => {
		const {accountId, codes} = await createGiftedUser(1);
		const gift = await getUserRepository().findGiftCode(codes[0]!);
		expect(await getUserRepository().markGiftPremiumReversed(gift!, 2_592_000)).toBe(true);

		await reverse(codes[0]!);

		const after = await findUser(accountId);
		expect(after.premiumType).toBe(UserPremiumTypes.NONE);
		expect(after.premiumGiftExtensionEndsAt).toBeNull();
	});

	it('surfaces the write failure when releasing the reversal claim also fails', async () => {
		const {codes} = await createGiftedUser(1);
		const users = getUserRepository();
		const failing = new Proxy(users, {
			get(target, property, receiver) {
				if (property === 'patchUpsert') {
					return async () => {
						throw new Error('user write failed');
					};
				}
				if (property === 'clearGiftPremiumReversed') {
					return async () => {
						throw new Error('release failed');
					};
				}
				const value = Reflect.get(target, property, receiver);
				return typeof value === 'function' ? value.bind(target) : value;
			},
		});

		await expect(reverse(codes[0]!, createHandler(failing))).rejects.toThrow('user write failed');
	});

	it('removes premium once every stacked gift has been reversed', async () => {
		const {accountId, codes} = await createGiftedUser(2);

		await reverse(codes[1]!);
		const between = await findUser(accountId);
		expect(between.premiumType).toBe(UserPremiumTypes.SUBSCRIPTION);
		await reverse(codes[0]!);

		const after = await findUser(accountId);
		expect(after.premiumType).toBe(UserPremiumTypes.NONE);
		expect(after.premiumGiftExtensionEndsAt).toBeNull();
	});
});
