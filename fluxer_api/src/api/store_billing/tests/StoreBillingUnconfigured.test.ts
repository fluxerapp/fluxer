// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {buildAPIConfigFromMaster, getConfig} from '@app/api/Config';
import type {APIConfig} from '@app/api/config/APIConfig';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {STORE_PURCHASE_REFRESH_QUEUE_KEY} from '@app/api/store_billing/StorePurchaseRefresh';
import {
	getStoreContext,
	installStoreBillingWorker,
	setTestPremium,
	uninstallStoreBillingWorker,
} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {
	createGooglePlayDeveloperApiHandlers,
	type FakeGooglePlayDeveloperApi,
} from '@app/api/test/msw/handlers/GooglePlayDeveloperApiHandlers';
import {createStripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import pollAppStoreNotificationHistory from '@app/api/worker/tasks/PollAppStoreNotificationHistory';
import pollGooglePlayVoidedPurchases from '@app/api/worker/tasks/PollGooglePlayVoidedPurchases';
import processStorePurchaseRefreshQueue from '@app/api/worker/tasks/ProcessStorePurchaseRefreshQueue';
import {loadConfig} from '@fluxer/config/src/ConfigLoader';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

type StoreConfigState = Pick<APIConfig, 'appStore' | 'googlePlay' | 'storeBilling'>;

function captureStoreConfig(): StoreConfigState {
	const config = getConfig();
	return {appStore: config.appStore, googlePlay: config.googlePlay, storeBilling: config.storeBilling};
}

function applyStoreConfig(state: StoreConfigState): void {
	const config = getConfig();
	config.appStore = state.appStore;
	config.googlePlay = state.googlePlay;
	config.storeBilling = state.storeBilling;
}

function createWorkerHelpers(scheduled: Array<string>): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		attempt: {isLastAttempt: false},
		addJob: async (taskType) => {
			scheduled.push(taskType);
			return 0n;
		},
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

describe('Store billing with no store configuration', () => {
	let harness: ApiTestHarness;
	let google: FakeGooglePlayDeveloperApi;
	let original: StoreConfigState;
	let unset: StoreConfigState;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		google = createGooglePlayDeveloperApiHandlers();
		original = captureStoreConfig();
		const master = await loadConfig();
		unset = buildAPIConfigFromMaster(master);
	});

	afterAll(async () => {
		applyStoreConfig(original);
		await harness.shutdown();
	});

	beforeEach(async () => {
		await harness.resetData();
		google.reset();
		server.use(...createStripeApiHandlers().handlers, ...google.handlers);
		installStoreBillingWorker();
		applyStoreConfig(unset);
	});

	afterEach(() => {
		applyStoreConfig(original);
		uninstallStoreBillingWorker();
		server.resetHandlers();
	});

	it('reports both stores disabled with no products', async () => {
		const account = await createTestAccount(harness);
		const context = await getStoreContext(harness, account.token);
		expect(context.app_store).toEqual({enabled: false, bundle_ids: [], products: []});
		expect(context.google_play).toEqual({enabled: false, package_names: [], products: []});
		expect(context.purchase_blocked_reason).toBeNull();
		expect(context.blocking_provider).toBeNull();
		expect(context.app_account_token).toEqual(expect.any(String));
	});

	it('answers both claim routes with STORE_BILLING_UNAVAILABLE', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/premium/store/app-store/transactions')
			.body({signed_transaction: 'not-a-transaction'})
			.expect(HTTP_STATUS.SERVICE_UNAVAILABLE, APIErrorCodes.STORE_BILLING_UNAVAILABLE)
			.execute();
		await createBuilder(harness, account.token)
			.post('/premium/store/google-play/purchases')
			.body({purchase_token: 'not-a-token', product_id: 'plutonium', package_name: 'com.fluxer'})
			.expect(HTTP_STATUS.SERVICE_UNAVAILABLE, APIErrorCodes.STORE_BILLING_UNAVAILABLE)
			.execute();
	});

	it('answers both store webhooks with 503', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/webhooks/app-store')
			.body({signedPayload: 'not-a-payload'})
			.expect(HTTP_STATUS.SERVICE_UNAVAILABLE, APIErrorCodes.STORE_BILLING_UNAVAILABLE)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/webhooks/google-play')
			.header('authorization', 'Bearer not-a-token')
			.body({message: {messageId: '1', data: ''}})
			.expect(HTTP_STATUS.SERVICE_UNAVAILABLE, APIErrorCodes.STORE_BILLING_UNAVAILABLE)
			.execute();
		expect(google.spies.jwksFetches).toBe(0);
	});

	it('serves premium state with no store block', async () => {
		const account = await createTestAccount(harness);
		const state = await createBuilder<PremiumStateResponse>(harness, account.token)
			.get('/premium/state')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(state.store).toBeNull();
		expect(state.subscription_provider).toBeNull();
	});

	it('leaves a Stripe subscriber unaffected', async () => {
		const account = await createTestAccount(harness);
		const premiumUntil = new Date(Date.now() + ms('30 days'));
		await setTestPremium(harness, account.token, account.userId, {
			premium_type: UserPremiumTypes.SUBSCRIPTION,
			premium_until: premiumUntil.toISOString(),
			premium_billing_cycle: 'monthly',
			stripe_customer_id: 'cus_unconfigured_store',
			stripe_subscription_id: 'sub_unconfigured_store',
		});
		const state = await createBuilder<PremiumStateResponse>(harness, account.token)
			.get('/premium/state')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(state.store).toBeNull();
		expect(state.subscription_provider).toBe('stripe');
		expect(state.actual).toMatchObject({
			premium_type: UserPremiumTypes.SUBSCRIPTION,
			premium_until: premiumUntil.toISOString(),
			premium_billing_cycle: 'monthly',
			has_active_paid_premium: true,
		});
		expect(state.effective.is_premium).toBe(true);
		const context = await getStoreContext(harness, account.token);
		expect(context).toMatchObject({purchase_blocked_reason: 'existing_subscription', blocking_provider: 'stripe'});
	});

	it('does nothing in the store cron tasks', async () => {
		const storeKey = 'gp:unconfigured-store-refresh';
		const dueAt = Date.now() - 1000;
		await getKVClient().zadd(STORE_PURCHASE_REFRESH_QUEUE_KEY, dueAt, storeKey);
		google.addVoidedPurchase('com.fluxer', {
			purchaseToken: 'unconfigured-voided-token',
			orderId: 'GPA.unconfigured',
			voidedTimeMillis: String(Date.now()),
		});
		const scheduled: Array<string> = [];
		await processStorePurchaseRefreshQueue({}, createWorkerHelpers(scheduled));
		await pollGooglePlayVoidedPurchases({}, createWorkerHelpers(scheduled));
		await pollAppStoreNotificationHistory({}, createWorkerHelpers(scheduled));
		expect(scheduled).toEqual([]);
		expect(await getKVClient().zrangebyscore(STORE_PURCHASE_REFRESH_QUEUE_KEY, dueAt, dueAt)).toEqual([storeKey]);
		expect(google.spies.voidedQueries).toEqual([]);
	});
});
