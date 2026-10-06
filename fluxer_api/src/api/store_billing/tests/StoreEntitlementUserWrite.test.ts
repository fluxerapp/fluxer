// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getStoreBillingRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {
	createTestStoreEntitlementService,
	expectCloseTo,
	findUser,
	seedStorePurchase,
} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {ms} from 'itty-time';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Store entitlement user write', () => {
	let harness: ApiTestHarness;
	let service: StoreEntitlementService;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	beforeEach(async () => {
		await harness.resetData();
		service = createTestStoreEntitlementService();
	});

	it('shifts stacked gift time from the old paid-through date when a subscription recovers in grace', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const lapsedAt = new Date(Date.now() - ms('2 days'));
		const graceEnd = new Date(Date.now() + ms('14 days'));
		const giftEnd = new Date(lapsedAt.getTime() + ms('30 days'));
		const row = await seedStorePurchase(userId, {
			state: 'grace',
			expires_at: lapsedAt,
			grace_ends_at: graceEnd,
		});
		await getUserRepository().patchUpsert(
			userId,
			{
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: lapsedAt,
				premium_grace_ends_at: graceEnd,
				premium_gift_extension_ends_at: giftEnd,
			},
			(await findUser(account.userId)).toRow(),
		);
		await service.applyStoreEntitlementToUser(userId);
		expect((await findUser(account.userId)).premiumGiftExtensionEndsAt?.getTime()).toBe(giftEnd.getTime());
		const renewedExpiry = new Date(lapsedAt.getTime() + ms('30 days'));
		await getStoreBillingRepository().updatePurchase(row, {
			state: 'active',
			expires_at: renewedExpiry,
			grace_ends_at: null,
		});
		await service.applyStoreEntitlementToUser(userId);
		const recovered = await findUser(account.userId);
		expect(recovered.premiumUntil?.getTime()).toBe(renewedExpiry.getTime());
		expect(recovered.premiumGraceEndsAt).toBeNull();
		expect(recovered.premiumGiftExtensionEndsAt?.getTime()).toBe(
			giftEnd.getTime() + (renewedExpiry.getTime() - lapsedAt.getTime()),
		);
	});

	it('shifts stacked gift time from now when an ended Stripe subscription left its grace behind', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const stripeEnd = new Date(Date.now() - ms('150 days'));
		const giftEnd = new Date(stripeEnd.getTime() + ms('365 days'));
		await getUserRepository().patchUpsert(
			userId,
			{
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: stripeEnd,
				premium_grace_ends_at: new Date(stripeEnd.getTime() + ms('3 days')),
				premium_gift_extension_ends_at: giftEnd,
			},
			(await findUser(account.userId)).toRow(),
		);
		const expiresAt = new Date(Date.now() + ms('30 days'));
		await seedStorePurchase(userId, {expires_at: expiresAt});
		const grantedAt = Date.now();
		await service.applyStoreEntitlementToUser(userId);
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(expiresAt.getTime());
		expectCloseTo(user.premiumGiftExtensionEndsAt, giftEnd.getTime() + (expiresAt.getTime() - grantedAt));
	});
});
