// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getStoreBillingRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {SubscriptionPurchaseV2} from '@app/api/store_billing/google_play/GooglePlayDeveloperApiClient';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {googlePlayMessageClaimKey, StoreNotificationClaims} from '@app/api/store_billing/StoreNotificationClaims';
import {STORE_PURCHASE_REFRESH_QUEUE_KEY} from '@app/api/store_billing/StorePurchaseRefresh';
import {
	findUser,
	getStoreContext,
	installStoreBillingWorker,
	setTestPremium,
	uninstallStoreBillingWorker,
} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {
	buildFakeProductPurchase,
	buildFakeSubscriptionPurchase,
	buildGooglePlayPushEnvelope,
	createGooglePlayDeveloperApiHandlers,
	encodeGooglePlayDeveloperNotification,
	type FakeGooglePlayDeveloperApi,
} from '@app/api/test/msw/handlers/GooglePlayDeveloperApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import pollGooglePlayVoidedPurchases from '@app/api/worker/tasks/PollGooglePlayVoidedPurchases';
import processGooglePlayNotification from '@app/api/worker/tasks/ProcessGooglePlayNotification';
import processStorePurchaseRefreshQueue from '@app/api/worker/tasks/ProcessStorePurchaseRefreshQueue';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {
	StorePurchaseClaimResponse,
	StorePurchaseResponse,
} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const PACKAGE_NAME = 'com.fluxer';
const SUBSCRIPTION_PRODUCT = 'plutonium';

interface SubscriptionSeed {
	state?: string;
	expiresAt: number;
	basePlanId?: string;
	accountToken?: string;
	linkedPurchaseToken?: string;
	orderId?: string;
	testPurchase?: boolean;
}

interface ScheduledJob {
	taskType: string;
	payload: unknown;
	runAt: Date | undefined;
}

function createWorkerHelpers(isLastAttempt = false, scheduled: Array<ScheduledJob> = []): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		attempt: {isLastAttempt},
		addJob: async (taskType, payload, options) => {
			scheduled.push({taskType, payload, runAt: options?.runAt});
			return 0n;
		},
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

function buildSubscription(seed: SubscriptionSeed): SubscriptionPurchaseV2 {
	return buildFakeSubscriptionPurchase({
		subscriptionState: seed.state ?? 'SUBSCRIPTION_STATE_ACTIVE',
		startTime: new Date(Date.now() - ms('1 day')).toISOString(),
		lineItems: [
			{
				productId: SUBSCRIPTION_PRODUCT,
				expiryTime: new Date(seed.expiresAt).toISOString(),
				latestSuccessfulOrderId: seed.orderId ?? `GPA.${randomUUID()}`,
				autoRenewingPlan: {autoRenewEnabled: true},
				offerDetails: {basePlanId: seed.basePlanId ?? 'monthly'},
			},
		],
		...(seed.accountToken ? {externalAccountIdentifiers: {obfuscatedExternalAccountId: seed.accountToken}} : {}),
		...(seed.linkedPurchaseToken ? {linkedPurchaseToken: seed.linkedPurchaseToken} : {}),
		...(seed.testPurchase ? {testPurchase: {}} : {}),
	});
}

describe('Google Play purchases', () => {
	let harness: ApiTestHarness;
	let fake: FakeGooglePlayDeveloperApi;
	let service: StoreEntitlementService;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		fake = createGooglePlayDeveloperApiHandlers();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	beforeEach(async () => {
		await harness.resetData();
		server.use(...fake.handlers);
		service = installStoreBillingWorker();
	});

	afterEach(() => {
		uninstallStoreBillingWorker();
	});

	function newToken(): string {
		return `token-${randomUUID()}`;
	}

	async function claim(
		account: TestAccount,
		purchaseToken: string,
		productId = SUBSCRIPTION_PRODUCT,
		status = 200,
		code?: string,
	): Promise<StorePurchaseClaimResponse> {
		return createBuilder<StorePurchaseClaimResponse>(harness, account.token)
			.post('/premium/store/google-play/purchases')
			.body({purchase_token: purchaseToken, product_id: productId, package_name: PACKAGE_NAME})
			.expect(status, code)
			.execute();
	}

	async function sendRtdn(notification: Record<string, unknown>, authorization?: string, status = 204): Promise<void> {
		const header = authorization ?? `Bearer ${await fake.signPushToken()}`;
		await createBuilderWithoutAuth(harness)
			.post('/webhooks/google-play')
			.header('Authorization', header)
			.body(buildGooglePlayPushEnvelope({packageName: PACKAGE_NAME, ...notification}, randomUUID()))
			.expect(status)
			.execute();
	}

	async function sendSubscriptionRtdn(purchaseToken: string, notificationType: number): Promise<void> {
		await sendRtdn({
			version: '1.0',
			eventTimeMillis: String(Date.now()),
			subscriptionNotification: {version: '1.0', notificationType, purchaseToken, subscriptionId: SUBSCRIPTION_PRODUCT},
		});
	}

	async function accountToken(account: TestAccount): Promise<string> {
		return (await getStoreContext(harness, account.token)).app_account_token;
	}

	async function refreshDueAt(storeKey: string): Promise<number | null> {
		const kv = getKVClient();
		const keys = await kv.zrangebyscore(STORE_PURCHASE_REFRESH_QUEUE_KEY, '-inf', '+inf');
		if (!keys.includes(storeKey)) {
			return null;
		}
		for (const window of [ms('1 hour'), ms('2 hours'), ms('400 days')]) {
			const due = await kv.zrangebyscore(STORE_PURCHASE_REFRESH_QUEUE_KEY, '-inf', Date.now() + window);
			if (due.includes(storeKey)) {
				return window;
			}
		}
		return null;
	}

	async function runRefreshQueueFor(storeKey: string): Promise<void> {
		await getKVClient().zadd(STORE_PURCHASE_REFRESH_QUEUE_KEY, Date.now() - 1000, storeKey);
		await processStorePurchaseRefreshQueue({}, createWorkerHelpers());
	}

	it('acknowledges a claimed subscription and grants premium', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const purchaseToken = newToken();
		const expiresAt = Date.now() + ms('30 days');
		fake.setSubscription(PACKAGE_NAME, purchaseToken, buildSubscription({expiresAt, accountToken: token}));
		const first = await claim(account, purchaseToken);
		expect(first.purchase).toMatchObject({provider: 'google_play', slot: 'monthly', state: 'active', entitled: true});
		expect(fake.spies.acknowledged.filter((entry) => entry.purchaseToken === purchaseToken)).toHaveLength(1);
		const second = await claim(account, purchaseToken);
		expect(second.purchase.id).toBe(first.purchase.id);
		expect(fake.spies.acknowledged.filter((entry) => entry.purchaseToken === purchaseToken)).toHaveLength(1);
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(expiresAt);
		expect(user.premiumBillingCycle).toBeNull();
		expect(user.isPremium()).toBe(true);
		const state = await createBuilder<PremiumStateResponse>(harness, account.token).get('/premium/state').execute();
		expect(state.store).toMatchObject({
			provider: 'google_play',
			billing_cycle: 'monthly',
			manage_url: `https://play.google.com/store/account/subscriptions?sku=${SUBSCRIPTION_PRODUCT}&package=${PACKAGE_NAME}`,
		});
		const context = await getStoreContext(harness, account.token);
		expect(context).toMatchObject({purchase_blocked_reason: 'existing_subscription', blocking_provider: 'google_play'});
		expect(context.google_play.products).toContainEqual({
			product_id: SUBSCRIPTION_PRODUCT,
			base_plan_id: 'monthly',
			slot: 'monthly',
		});
	});

	it('moves the entitlement to the new token on an upgrade and supersedes the old one', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const oldToken = newToken();
		fake.setSubscription(
			PACKAGE_NAME,
			oldToken,
			buildSubscription({expiresAt: Date.now() + ms('10 days'), accountToken: token}),
		);
		await claim(account, oldToken);
		fake.setSubscription(
			PACKAGE_NAME,
			oldToken,
			buildSubscription({state: 'SUBSCRIPTION_STATE_EXPIRED', expiresAt: Date.now() - 1000, accountToken: token}),
		);
		const newPurchaseToken = newToken();
		const yearlyExpiry = Date.now() + ms('365 days');
		fake.setSubscription(
			PACKAGE_NAME,
			newPurchaseToken,
			buildSubscription({expiresAt: yearlyExpiry, basePlanId: 'yearly', linkedPurchaseToken: oldToken}),
		);
		await sendSubscriptionRtdn(newPurchaseToken, 4);
		const purchases = await createBuilder<Array<StorePurchaseResponse>>(harness, account.token)
			.get('/premium/store/purchases')
			.execute();
		expect(purchases).toHaveLength(2);
		const oldRow = await getStoreBillingRepository().findPurchase(`google_play:${oldToken}`);
		expect(oldRow).toMatchObject({
			state: 'superseded',
			entitled: false,
			superseded_by_store_key: `google_play:${newPurchaseToken}`,
		});
		const newRow = await getStoreBillingRepository().findPurchase(`google_play:${newPurchaseToken}`);
		expect(newRow).toMatchObject({user_id: oldRow?.user_id, slot: 'yearly', entitled: true, acknowledged: true});
		expect(purchases.filter((purchase) => purchase.entitled)).toHaveLength(1);
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(yearlyExpiry);
		expect(await service.getActiveStoreEntitlement(user.id)).toMatchObject({provider: 'google_play', slot: 'yearly'});
	});

	it('keeps the paid subscription while a linked replacement is pending or its payment is cancelled', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const oldToken = newToken();
		const oldExpiry = Date.now() + ms('10 days');
		fake.setSubscription(PACKAGE_NAME, oldToken, buildSubscription({expiresAt: oldExpiry, accountToken: token}));
		await claim(account, oldToken);
		const newPurchaseToken = newToken();
		const yearlyExpiry = Date.now() + ms('365 days');
		for (const state of ['SUBSCRIPTION_STATE_PENDING', 'SUBSCRIPTION_STATE_PENDING_PURCHASE_CANCELED']) {
			fake.setSubscription(
				PACKAGE_NAME,
				newPurchaseToken,
				buildSubscription({state, expiresAt: yearlyExpiry, basePlanId: 'yearly', linkedPurchaseToken: oldToken}),
			);
			await sendSubscriptionRtdn(newPurchaseToken, 4);
			const oldRow = await getStoreBillingRepository().findPurchase(`google_play:${oldToken}`);
			expect(oldRow).toMatchObject({state: 'active', entitled: true, superseded_by_store_key: null});
			const user = await findUser(account.userId);
			expect(user.premiumUntil?.getTime()).toBe(oldExpiry);
			expect(user.isPremium()).toBe(true);
		}
		fake.setSubscription(
			PACKAGE_NAME,
			newPurchaseToken,
			buildSubscription({expiresAt: yearlyExpiry, basePlanId: 'yearly', linkedPurchaseToken: oldToken}),
		);
		await sendSubscriptionRtdn(newPurchaseToken, 4);
		expect(await getStoreBillingRepository().findPurchase(`google_play:${oldToken}`)).toMatchObject({
			state: 'superseded',
			superseded_by_store_key: `google_play:${newPurchaseToken}`,
		});
		expect((await findUser(account.userId)).premiumUntil?.getTime()).toBe(yearlyExpiry);
	});

	it('lets another account claim an upgraded subscription after its owner releases it', async () => {
		const owner = await createTestAccount(harness);
		const other = await createTestAccount(harness);
		const token = await accountToken(owner);
		const oldToken = newToken();
		fake.setSubscription(
			PACKAGE_NAME,
			oldToken,
			buildSubscription({expiresAt: Date.now() + ms('10 days'), accountToken: token}),
		);
		await claim(owner, oldToken);
		const upgradedToken = newToken();
		fake.setSubscription(
			PACKAGE_NAME,
			upgradedToken,
			buildSubscription({
				expiresAt: Date.now() + ms('365 days'),
				basePlanId: 'yearly',
				accountToken: token,
				linkedPurchaseToken: oldToken,
			}),
		);
		const upgraded = await claim(owner, upgradedToken);
		await createBuilder(harness, owner.token)
			.delete(`/premium/store/purchases/${upgraded.purchase.id}`)
			.body({password: owner.password})
			.expect(204)
			.execute();
		await sendSubscriptionRtdn(upgradedToken, 2);
		const storeKey = `google_play:${upgradedToken}`;
		expect((await getStoreBillingRepository().findPurchase(storeKey))?.user_id).toBeNull();
		expect((await findUser(owner.userId)).isPremium()).toBe(false);
		const moved = await claim(other, upgradedToken);
		expect(moved.purchase.id).toBe(upgraded.purchase.id);
		await sendSubscriptionRtdn(upgradedToken, 2);
		const row = await getStoreBillingRepository().findPurchase(storeKey);
		expect(row?.user_id?.toString()).toBe(other.userId);
		expect(row?.released_at).toBeNull();
		expect((await findUser(other.userId)).isPremium()).toBe(true);
		expect((await findUser(owner.userId)).isPremium()).toBe(false);
	});

	it('stores an unbound purchase unacknowledged and binds it on a later claim', async () => {
		const purchaseToken = newToken();
		fake.setSubscription(PACKAGE_NAME, purchaseToken, buildSubscription({expiresAt: Date.now() + ms('30 days')}));
		await sendSubscriptionRtdn(purchaseToken, 4);
		const stored = await getStoreBillingRepository().findPurchase(`google_play:${purchaseToken}`);
		expect(stored).toMatchObject({user_id: null, acknowledged: false, entitled: false, state: 'active'});
		expect(fake.spies.acknowledged.some((entry) => entry.purchaseToken === purchaseToken)).toBe(false);
		const account = await createTestAccount(harness);
		const result = await claim(account, purchaseToken);
		expect(result.purchase.id).toBe(stored?.id.toString());
		expect(result.purchase.entitled).toBe(true);
		expect(fake.spies.acknowledged.some((entry) => entry.purchaseToken === purchaseToken)).toBe(true);
		expect((await findUser(account.userId)).isPremium()).toBe(true);
	});

	it('cuts access while on hold and restores it on recovery', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const purchaseToken = newToken();
		const expiresAt = Date.now() - ms('1 hour');
		fake.setSubscription(PACKAGE_NAME, purchaseToken, buildSubscription({expiresAt, accountToken: token}));
		await claim(account, purchaseToken);
		expect((await findUser(account.userId)).isPremium()).toBe(true);
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({state: 'SUBSCRIPTION_STATE_ON_HOLD', expiresAt, accountToken: token}),
		);
		await sendSubscriptionRtdn(purchaseToken, 5);
		const held = await findUser(account.userId);
		expect(held.premiumGraceEndsAt?.getTime()).toBe(expiresAt);
		expect(held.isPremium()).toBe(false);
		const recoveredExpiry = Date.now() + ms('30 days');
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: recoveredExpiry, accountToken: token}),
		);
		await sendSubscriptionRtdn(purchaseToken, 1);
		const recovered = await findUser(account.userId);
		expect(recovered.premiumUntil?.getTime()).toBe(recoveredExpiry);
		expect(recovered.premiumGraceEndsAt).toBeNull();
		expect(recovered.isPremium()).toBe(true);
	});

	it('records a test purchase without entitlement for an account that is not allowed', async () => {
		const account = await createTestAccount(harness);
		const purchaseToken = newToken();
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: Date.now() + ms('1 day'), testPurchase: true}),
		);
		await claim(account, purchaseToken, SUBSCRIPTION_PRODUCT, 403, APIErrorCodes.STORE_PURCHASE_SANDBOX_NOT_ENTITLED);
		const stored = await getStoreBillingRepository().findPurchase(`google_play:${purchaseToken}`);
		expect(stored).toMatchObject({environment: 'sandbox', entitled: false});
		expect(stored?.user_id?.toString()).toBe(account.userId);
		expect((await findUser(account.userId)).isPremium()).toBe(false);
	});

	it('refunds a subscription bought by a lifetime account', async () => {
		const account = await createTestAccount(harness);
		await setTestPremium(harness, account.token, account.userId, {premium_type: UserPremiumTypes.LIFETIME});
		const purchaseToken = newToken();
		const orderId = `GPA.${randomUUID()}`;
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: Date.now() + ms('30 days'), orderId}),
		);
		const {json} = await createBuilder<{reason: string; provider: string}>(harness, account.token)
			.post('/premium/store/google-play/purchases')
			.body({purchase_token: purchaseToken, product_id: SUBSCRIPTION_PRODUCT})
			.expect(403, APIErrorCodes.PREMIUM_PURCHASE_BLOCKED)
			.executeWithResponse();
		expect(json).toMatchObject({reason: 'lifetime', provider: 'google_play'});
		expect(fake.spies.refunded).toContainEqual({packageName: PACKAGE_NAME, orderId, revoke: true});
		expect(fake.spies.acknowledged.some((entry) => entry.purchaseToken === purchaseToken)).toBe(false);
		const user = await findUser(account.userId);
		expect(user.premiumType).toBe(UserPremiumTypes.LIFETIME);
		expect(user.premiumUntil).toBeNull();
		await createBuilder(harness, account.token)
			.post('/premium/store/google-play/purchases')
			.body({purchase_token: purchaseToken, product_id: SUBSCRIPTION_PRODUCT})
			.expect(403, APIErrorCodes.PREMIUM_PURCHASE_BLOCKED)
			.execute();
		expect(fake.spies.refunded.filter((entry) => entry.orderId === orderId)).toHaveLength(1);
	});

	it('mints and consumes a gift purchase', async () => {
		const account = await createTestAccount(harness);
		const purchaseToken = newToken();
		fake.setProduct(PACKAGE_NAME, purchaseToken, buildFakeProductPurchase({orderId: `GPA.${randomUUID()}`}));
		const result = await claim(account, purchaseToken, 'gift_1_month');
		expect(result.purchase).toMatchObject({kind: 'gift', state: 'fulfilled'});
		expect(result.gift_code).toBeTruthy();
		expect(fake.spies.consumed.some((entry) => entry.purchaseToken === purchaseToken)).toBe(true);
		const again = await claim(account, purchaseToken, 'gift_1_month');
		expect(again.gift_code).toBe(result.gift_code);
		const gifts = await getUserRepository().findGiftCodesByCreator((await findUser(account.userId)).id);
		expect(gifts.map((gift) => gift.code)).toEqual([result.gift_code]);
	});

	it('rejects push requests without a valid Google token', async () => {
		const purchaseToken = newToken();
		fake.setSubscription(PACKAGE_NAME, purchaseToken, buildSubscription({expiresAt: Date.now() + ms('30 days')}));
		const notification = {subscriptionNotification: {notificationType: 4, purchaseToken}};
		await sendRtdn(notification, `Bearer ${await fake.signPushToken({signer: 'untrusted'})}`, 401);
		await sendRtdn(notification, 'Bearer not-a-token', 401);
		await sendRtdn(notification, `Bearer ${await fake.signPushToken({claims: {email_verified: false}})}`, 401);
		expect(await getStoreBillingRepository().findPurchase(`google_play:${purchaseToken}`)).toBeNull();
	});

	it('rejects a product that is not on sale', async () => {
		const account = await createTestAccount(harness);
		await claim(account, newToken(), 'not_a_fluxer_product', 400, APIErrorCodes.STORE_PURCHASE_INVALID);
		await createBuilder(harness, account.token)
			.post('/premium/store/google-play/purchases')
			.body({purchase_token: newToken(), product_id: SUBSCRIPTION_PRODUCT, package_name: 'com.example.other'})
			.expect(400, APIErrorCodes.STORE_PURCHASE_INVALID)
			.execute();
	});

	it('schedules refreshes and re-reads due purchases from the refresh queue', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const purchaseToken = newToken();
		const storeKey = `google_play:${purchaseToken}`;
		const expiresAt = Date.now() + ms('2 days');
		fake.setSubscription(PACKAGE_NAME, purchaseToken, buildSubscription({expiresAt, accountToken: token}));
		await claim(account, purchaseToken);
		const kv = getKVClient();
		const scheduled = await kv.zrangebyscore(STORE_PURCHASE_REFRESH_QUEUE_KEY, expiresAt, expiresAt + ms('10 minutes'));
		expect(scheduled).toEqual([storeKey]);
		await kv.zadd(STORE_PURCHASE_REFRESH_QUEUE_KEY, Date.now() - 1000, storeKey);
		const renewedExpiry = Date.now() + ms('32 days');
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: renewedExpiry, accountToken: token}),
		);
		await processStorePurchaseRefreshQueue({}, createWorkerHelpers());
		expect((await findUser(account.userId)).premiumUntil?.getTime()).toBe(renewedExpiry);
		const rescheduled = await kv.zrangebyscore(STORE_PURCHASE_REFRESH_QUEUE_KEY, '-inf', '+inf');
		expect(rescheduled).toEqual([storeKey]);
	});

	it('revokes the gift code of a voided gift purchase found by the daily poll', async () => {
		const account = await createTestAccount(harness);
		const purchaseToken = newToken();
		const orderId = `GPA.${randomUUID()}`;
		fake.setProduct(PACKAGE_NAME, purchaseToken, buildFakeProductPurchase({orderId}));
		const result = await claim(account, purchaseToken, 'gift_1_month');
		fake.setProduct(
			PACKAGE_NAME,
			purchaseToken,
			buildFakeProductPurchase({orderId, purchaseStateContext: {purchaseState: 'CANCELLED'}}),
		);
		fake.addVoidedPurchase(PACKAGE_NAME, {
			purchaseToken,
			orderId,
			voidedTimeMillis: String(Date.now()),
			voidedSource: 1,
			voidedReason: 1,
		});
		await pollGooglePlayVoidedPurchases({}, createWorkerHelpers());
		const gift = await getUserRepository().findGiftCode(result.gift_code!);
		expect(gift?.revokedAt).not.toBeNull();
		const row = await getStoreBillingRepository().findPurchase(`google_play:${purchaseToken}`);
		expect(row?.state).toBe('refunded');
	});

	it('rejects a purchase token Google does not know', async () => {
		const account = await createTestAccount(harness);
		await claim(account, newToken(), SUBSCRIPTION_PRODUCT, 400, APIErrorCodes.STORE_PURCHASE_INVALID);
	});

	it('keeps retrying a subscription acknowledgement that failed', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const purchaseToken = newToken();
		const storeKey = `google_play:${purchaseToken}`;
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: Date.now() + ms('30 days'), accountToken: token}),
		);
		fake.failNext('subscriptions.acknowledge', {status: 503, reason: 'backendError'});
		const result = await claim(account, purchaseToken);
		expect(result.purchase.entitled).toBe(true);
		expect((await getStoreBillingRepository().findPurchase(storeKey))?.acknowledged).toBe(false);
		expect(await refreshDueAt(storeKey)).toBe(ms('1 hour'));
		await runRefreshQueueFor(storeKey);
		expect((await getStoreBillingRepository().findPurchase(storeKey))?.acknowledged).toBe(true);
		expect(fake.spies.acknowledged.filter((entry) => entry.purchaseToken === purchaseToken)).toHaveLength(1);
		expect(await refreshDueAt(storeKey)).toBe(ms('400 days'));
	});

	it('retries a failed test purchase acknowledgement within minutes', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const purchaseToken = newToken();
		const storeKey = `google_play:${purchaseToken}`;
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: Date.now() + ms('1 day'), accountToken: token, testPurchase: true}),
		);
		fake.failNext('subscriptions.acknowledge', {status: 503, reason: 'backendError'});
		const savedSandboxUserIds = Config.storeBilling.sandboxUserIds;
		Config.storeBilling.sandboxUserIds = [account.userId];
		try {
			const result = await claim(account, purchaseToken);
			expect(result.purchase).toMatchObject({environment: 'sandbox', entitled: true});
		} finally {
			Config.storeBilling.sandboxUserIds = savedSandboxUserIds;
		}
		expect((await getStoreBillingRepository().findPurchase(storeKey))?.acknowledged).toBe(false);
		const due = await getKVClient().zrangebyscore(
			STORE_PURCHASE_REFRESH_QUEUE_KEY,
			'-inf',
			Date.now() + ms('2 minutes'),
		);
		expect(due).toContain(storeKey);
	});

	it('keeps retrying a gift consumption that failed', async () => {
		const account = await createTestAccount(harness);
		const purchaseToken = newToken();
		const storeKey = `google_play:${purchaseToken}`;
		fake.setProduct(PACKAGE_NAME, purchaseToken, buildFakeProductPurchase({orderId: `GPA.${randomUUID()}`}));
		fake.failNext('products.consume', {status: 503, reason: 'backendError'});
		const result = await claim(account, purchaseToken, 'gift_1_month');
		expect(result.purchase.state).toBe('fulfilled');
		expect(await refreshDueAt(storeKey)).toBe(ms('1 hour'));
		await runRefreshQueueFor(storeKey);
		expect(fake.spies.consumed.some((entry) => entry.purchaseToken === purchaseToken)).toBe(true);
		expect((await getStoreBillingRepository().findPurchase(storeKey))?.acknowledged).toBe(true);
		expect(await refreshDueAt(storeKey)).toBeNull();
	});

	it('consumes a gift that was acknowledged but never consumed', async () => {
		const account = await createTestAccount(harness);
		const purchaseToken = newToken();
		fake.setProduct(
			PACKAGE_NAME,
			purchaseToken,
			buildFakeProductPurchase({
				orderId: `GPA.${randomUUID()}`,
				acknowledgementState: 'ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED',
			}),
		);
		await claim(account, purchaseToken, 'gift_1_month');
		expect(fake.spies.consumed.some((entry) => entry.purchaseToken === purchaseToken)).toBe(true);
	});

	it('re-reads a subscription past its expiry about hourly while Google still reports it active', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const purchaseToken = newToken();
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: Date.now() - ms('1 hour'), accountToken: token}),
		);
		await claim(account, purchaseToken);
		const due = await getKVClient().zrangebyscore(
			STORE_PURCHASE_REFRESH_QUEUE_KEY,
			Date.now() + ms('50 minutes'),
			Date.now() + ms('70 minutes'),
		);
		expect(due).toContain(`google_play:${purchaseToken}`);
	});

	it('cuts access when Google revokes a subscription and moves its expiry', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const purchaseToken = newToken();
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: Date.now() + ms('365 days'), basePlanId: 'yearly', accountToken: token}),
		);
		await claim(account, purchaseToken);
		expect((await findUser(account.userId)).isPremium()).toBe(true);
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({
				state: 'SUBSCRIPTION_STATE_EXPIRED',
				expiresAt: Date.now(),
				basePlanId: 'yearly',
				accountToken: token,
			}),
		);
		await sendSubscriptionRtdn(purchaseToken, 12);
		const user = await findUser(account.userId);
		expect(user.premiumUntil!.getTime()).toBeLessThanOrEqual(Date.now());
		expect(user.premiumGraceEndsAt?.getTime()).toBe(user.premiumUntil?.getTime());
	});

	it('refunds a gift whose voided purchase Google still reports as purchased', async () => {
		const account = await createTestAccount(harness);
		const purchaseToken = newToken();
		const orderId = `GPA.${randomUUID()}`;
		fake.setProduct(PACKAGE_NAME, purchaseToken, buildFakeProductPurchase({orderId}));
		const result = await claim(account, purchaseToken, 'gift_1_month');
		fake.addVoidedPurchase(PACKAGE_NAME, {
			purchaseToken,
			orderId,
			voidedTimeMillis: String(Date.now() - ms('1 minute')),
			voidedSource: 1,
			voidedReason: 1,
		});
		await pollGooglePlayVoidedPurchases({}, createWorkerHelpers());
		const storeKey = `google_play:${purchaseToken}`;
		expect(await getStoreBillingRepository().findPurchase(storeKey)).toMatchObject({
			state: 'refunded',
			entitled: false,
		});
		const gift = await getUserRepository().findGiftCode(result.gift_code!);
		expect(gift?.revokedAt).not.toBeNull();
		await pollGooglePlayVoidedPurchases({}, createWorkerHelpers());
		await sendRtdn({
			version: '1.0',
			oneTimeProductNotification: {version: '1.0', notificationType: 1, purchaseToken, sku: 'gift_1_month'},
		});
		expect((await getStoreBillingRepository().findPurchase(storeKey))?.state).toBe('refunded');
		expect((await getUserRepository().findGiftCode(result.gift_code!))?.revokedAt?.getTime()).toBe(
			gift?.revokedAt?.getTime(),
		);
	});

	it('revokes a subscription whose latest order Google voided while the token stays active', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const purchaseToken = newToken();
		const orderId = `GPA.${randomUUID()}`;
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: Date.now() + ms('30 days'), accountToken: token, orderId}),
		);
		await claim(account, purchaseToken);
		const voidedAt = Date.now() - ms('1 minute');
		await sendRtdn({
			version: '1.0',
			eventTimeMillis: String(voidedAt),
			voidedPurchaseNotification: {purchaseToken, orderId, productType: 1, refundType: 1},
		});
		const storeKey = `google_play:${purchaseToken}`;
		expect(await getStoreBillingRepository().findPurchase(storeKey)).toMatchObject({
			state: 'revoked',
			entitled: false,
		});
		let user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(voidedAt);
		expect(user.premiumGraceEndsAt?.getTime()).toBe(voidedAt);
		await sendSubscriptionRtdn(purchaseToken, 2);
		expect((await getStoreBillingRepository().findPurchase(storeKey))?.state).toBe('revoked');
		user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(voidedAt);
	});

	it('stops billing a voided subscription that Google still renews when its owner is deleted or banned', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const purchaseToken = newToken();
		const orderId = `GPA.${randomUUID()}`;
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildSubscription({expiresAt: Date.now() + ms('30 days'), accountToken: token, orderId}),
		);
		await claim(account, purchaseToken);
		await sendRtdn({
			version: '1.0',
			eventTimeMillis: String(Date.now()),
			voidedPurchaseNotification: {purchaseToken, orderId, productType: 1, refundType: 1},
		});
		expect(await getStoreBillingRepository().findPurchase(`google_play:${purchaseToken}`)).toMatchObject({
			state: 'revoked',
			auto_renew: true,
		});
		expect(await getKVClient().zrangebyscore(STORE_PURCHASE_REFRESH_QUEUE_KEY, '-inf', '+inf')).toContain(
			`google_play:${purchaseToken}`,
		);
		const userId = (await findUser(account.userId)).id;
		await service.revokeForBannedUser(userId);
		expect(fake.spies.revoked.map((entry) => entry.purchaseToken)).toEqual([purchaseToken]);
		await service.stopBillingForDeletedUser(userId);
		expect(fake.spies.canceled.map((entry) => entry.purchaseToken)).toEqual([purchaseToken]);
	});

	it('refunds a gift bought in a quantity above one without minting', async () => {
		const account = await createTestAccount(harness);
		const purchaseToken = newToken();
		const orderId = `GPA.${randomUUID()}`;
		fake.setProduct(
			PACKAGE_NAME,
			purchaseToken,
			buildFakeProductPurchase({
				orderId,
				productLineItem: [
					{
						productId: 'gift_1_month',
						productOfferDetails: {quantity: 3, consumptionState: 'CONSUMPTION_STATE_YET_TO_BE_CONSUMED'},
					},
				],
			}),
		);
		await claim(account, purchaseToken, 'gift_1_month', 400, APIErrorCodes.STORE_PURCHASE_INVALID);
		await claim(account, purchaseToken, 'gift_1_month', 400, APIErrorCodes.STORE_PURCHASE_INVALID);
		expect(fake.spies.refunded.filter((entry) => entry.orderId === orderId)).toEqual([
			{packageName: PACKAGE_NAME, orderId, revoke: true},
		]);
		expect(fake.spies.consumed.some((entry) => entry.purchaseToken === purchaseToken)).toBe(false);
		expect(await getUserRepository().findGiftCodesByCreator((await findUser(account.userId)).id)).toHaveLength(0);
	});

	it('reschedules a notification that still fails on its last delivery', async () => {
		const purchaseToken = newToken();
		fake.setSubscription(PACKAGE_NAME, purchaseToken, buildSubscription({expiresAt: Date.now() + ms('30 days')}));
		const messageId = randomUUID();
		const data = encodeGooglePlayDeveloperNotification({
			version: '1.0',
			packageName: PACKAGE_NAME,
			subscriptionNotification: {version: '1.0', notificationType: 4, purchaseToken},
		});
		fake.failNext('subscriptions.get', {status: 503, reason: 'backendError'});
		await expect(processGooglePlayNotification({messageId, data}, createWorkerHelpers(false))).rejects.toThrow();
		const scheduled: Array<ScheduledJob> = [];
		fake.failNext('subscriptions.get', {status: 503, reason: 'backendError'});
		const before = Date.now();
		await processGooglePlayNotification({messageId, data}, createWorkerHelpers(true, scheduled));
		expect(scheduled).toHaveLength(1);
		expect(scheduled[0]).toMatchObject({
			taskType: 'processGooglePlayNotification',
			payload: {messageId, data, retry: 1},
		});
		expect(scheduled[0].runAt!.getTime()).toBeGreaterThanOrEqual(before + ms('5 minutes'));
		const claims = new StoreNotificationClaims(getKVClient());
		expect(await claims.tryClaim(googlePlayMessageClaimKey(messageId))).toBe('claimed');
		await claims.releaseClaim(googlePlayMessageClaimKey(messageId));
		await processGooglePlayNotification({messageId, data, retry: 1}, createWorkerHelpers(true));
		expect(await getStoreBillingRepository().findPurchase(`google_play:${purchaseToken}`)).not.toBeNull();
	});
});
