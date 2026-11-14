// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {
	createTestStoreEntitlementService,
	seedStorePurchase,
	setTestPremium,
} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {createTestUserWithPremium} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import {ms} from 'itty-time';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Premium state subscription provider', () => {
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
		server.use(...createStripeApiHandlers().handlers);
		service = createTestStoreEntitlementService();
	});

	afterEach(() => {
		server.resetHandlers();
	});

	async function getState(account: TestAccount): Promise<PremiumStateResponse> {
		return createBuilder<PremiumStateResponse>(harness, account.token)
			.get('/premium/state')
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function makeStripeSubscriber(account: TestAccount, premiumUntil: Date): Promise<void> {
		await setTestPremium(harness, account.token, account.userId, {
			premium_type: UserPremiumTypes.SUBSCRIPTION,
			premium_until: premiumUntil.toISOString(),
			premium_billing_cycle: 'monthly',
			stripe_customer_id: `cus_provider_${account.userId}`,
			stripe_subscription_id: `sub_provider_${account.userId}`,
		});
	}

	it('reports null for a stale Stripe subscription id without a billing cycle', async () => {
		const account = await createTestUserWithPremium(harness, UserPremiumTypes.SUBSCRIPTION, {
			premiumUntil: new Date(Date.now() + ms('30 days')),
			stripeSubscriptionId: 'sub_provider_stale',
		});
		const state = await getState(account);
		expect(state.actual.premium_billing_cycle).toBeNull();
		expect(state.subscription_provider).toBeNull();
	});

	it('reports the provider paid through later when Stripe and a store both bill', async () => {
		const stripeLater = await createTestAccount(harness);
		const stripeLaterId = createUserID(BigInt(stripeLater.userId));
		await makeStripeSubscriber(stripeLater, new Date(Date.now() + ms('60 days')));
		await seedStorePurchase(stripeLaterId, {expires_at: new Date(Date.now() + ms('20 days'))});
		await service.applyStoreEntitlementToUser(stripeLaterId);
		const stripeLaterState = await getState(stripeLater);
		expect(stripeLaterState.store?.provider).toBe('app_store');
		expect(stripeLaterState.subscription_provider).toBe('stripe');

		const storeLater = await createTestAccount(harness);
		const storeLaterId = createUserID(BigInt(storeLater.userId));
		await makeStripeSubscriber(storeLater, new Date(Date.now() + ms('10 days')));
		await seedStorePurchase(storeLaterId, {expires_at: new Date(Date.now() + ms('90 days'))});
		await service.applyStoreEntitlementToUser(storeLaterId);
		const storeLaterState = await getState(storeLater);
		expect(storeLaterState.subscription_provider).toBe('app_store');
	});

	it('reports stripe on a self-hosted instance and ignores store rows', async () => {
		const account = await createTestAccount(harness);
		await makeStripeSubscriber(account, new Date(Date.now() + ms('30 days')));
		await seedStorePurchase(createUserID(BigInt(account.userId)), {expires_at: new Date(Date.now() + ms('90 days'))});
		const originalSelfHosted = Config.instance.selfHosted;
		Config.instance.selfHosted = true;
		try {
			const state = await getState(account);
			expect(state.store).toBeNull();
			expect(state.subscription_provider).toBe('stripe');
		} finally {
			Config.instance.selfHosted = originalSelfHosted;
		}
	});
});
