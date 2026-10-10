// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getStoreBillingRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {addGiftCodeDuration} from '@app/api/models/GiftCode';
import type {User} from '@app/api/models/User';
import {setInjectedAppleRootCertificates} from '@app/api/store_billing/app_store/AppleRootCertificates';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {STORE_PURCHASE_REFRESH_QUEUE_KEY} from '@app/api/store_billing/StorePurchaseRefresh';
import {type AppleTestPki, createAppleTestPki} from '@app/api/store_billing/tests/AppleTestPki';
import {
	expectCloseTo,
	findUser,
	getStoreContext,
	installStoreBillingWorker,
	setTestPremium,
	uninstallStoreBillingWorker,
} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {GiftCodes} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {
	type AppStoreFakePayload,
	type AppStoreServerApiFake,
	createAppStoreServerApiFake,
} from '@app/api/test/msw/handlers/AppStoreServerApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import pollAppStoreNotificationHistory from '@app/api/worker/tasks/PollAppStoreNotificationHistory';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {PremiumStateResponse} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {
	StorePurchaseClaimResponse,
	StorePurchaseResponse,
} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const BUNDLE_ID = 'com.fluxer';
const APP_APPLE_ID = 1234567890;
const MONTHLY = 'com.fluxer.plutonium.monthly';
const GIFT_MONTH = 'com.fluxer.gift.1month';

type FakeEnvironment = 'production' | 'sandbox';

interface SubscriptionSeed {
	environment?: FakeEnvironment;
	status?: number;
	expiresDate: number;
	appAccountToken?: string;
	autoRenewStatus?: number;
	gracePeriodExpiresDate?: number;
	recentSubscriptionStartDate?: number;
	productId?: string;
}

let transactionCounter = 0;

function createWorkerHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

function expectAccessCut(user: User): void {
	expect(user.premiumUntil!.getTime()).toBeLessThanOrEqual(Date.now());
	expect(user.premiumGraceEndsAt?.getTime()).toBe(user.premiumUntil!.getTime());
}

function nextTransactionId(): string {
	transactionCounter += 1;
	return `2000000${Date.now().toString().slice(-6)}${transactionCounter.toString().padStart(4, '0')}`;
}

describe('App Store purchases', () => {
	let harness: ApiTestHarness;
	let pki: AppleTestPki;
	let fake: AppStoreServerApiFake;
	let service: StoreEntitlementService;
	let savedStoreBilling: typeof Config.storeBilling;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		pki = createAppleTestPki();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	beforeEach(async () => {
		await harness.resetData();
		savedStoreBilling = {...Config.storeBilling};
		setInjectedAppleRootCertificates([pki.root]);
		fake = createAppStoreServerApiFake({sign: (payload) => pki.signJws(payload)});
		server.use(...fake.handlers);
		service = installStoreBillingWorker();
	});

	afterEach(() => {
		Object.assign(Config.storeBilling, savedStoreBilling);
		setInjectedAppleRootCertificates(undefined);
		uninstallStoreBillingWorker();
	});

	function environmentName(environment: FakeEnvironment): string {
		return environment === 'production' ? 'Production' : 'Sandbox';
	}

	function seedSubscription(seed: SubscriptionSeed): string {
		const environment = seed.environment ?? 'production';
		const originalTransactionId = nextTransactionId();
		const now = Date.now();
		fake.addTransaction(environment, {
			transactionId: originalTransactionId,
			originalTransactionId,
			productId: seed.productId ?? MONTHLY,
			purchaseDate: now - ms('1 day'),
			expiresDate: seed.expiresDate,
			...(seed.appAccountToken ? {appAccountToken: seed.appAccountToken} : {}),
		});
		const renewalInfo: AppStoreFakePayload = {autoRenewStatus: seed.autoRenewStatus ?? 1};
		if (seed.gracePeriodExpiresDate !== undefined) {
			renewalInfo.gracePeriodExpiresDate = seed.gracePeriodExpiresDate;
		}
		if (seed.recentSubscriptionStartDate !== undefined) {
			renewalInfo.recentSubscriptionStartDate = seed.recentSubscriptionStartDate;
		}
		if (seed.appAccountToken) {
			renewalInfo.appAccountToken = seed.appAccountToken;
		}
		fake.putSubscription(environment, {
			originalTransactionId,
			status: seed.status ?? 1,
			latestTransactionId: originalTransactionId,
			renewalInfo,
		});
		return originalTransactionId;
	}

	function seedGift(environment: FakeEnvironment, appAccountToken?: string): string {
		const transactionId = nextTransactionId();
		fake.addTransaction(environment, {
			transactionId,
			originalTransactionId: transactionId,
			productId: GIFT_MONTH,
			type: 'Consumable',
			purchaseDate: Date.now() - ms('1 minute'),
			...(appAccountToken ? {appAccountToken} : {}),
		});
		return transactionId;
	}

	async function signStoredTransaction(environment: FakeEnvironment, transactionId: string): Promise<string> {
		const transaction = fake.getTransaction(environment, transactionId);
		if (!transaction) {
			throw new Error(`Unknown fake transaction ${transactionId}`);
		}
		return pki.signJws({
			bundleId: BUNDLE_ID,
			environment: environmentName(environment),
			type: 'Auto-Renewable Subscription',
			inAppOwnershipType: 'PURCHASED',
			purchaseDate: Date.now(),
			...transaction,
			signedDate: Date.now(),
		});
	}

	async function signNotification(
		notificationType: string,
		environment: FakeEnvironment,
		transactionId: string,
		signer: AppleTestPki = pki,
	): Promise<string> {
		const signedTransactionInfo = await signStoredTransaction(environment, transactionId);
		return signer.signJws({
			notificationType,
			notificationUUID: randomUUID(),
			signedDate: Date.now(),
			version: '2.0',
			data: {
				bundleId: BUNDLE_ID,
				environment: environmentName(environment),
				...(environment === 'production' ? {appAppleId: APP_APPLE_ID} : {}),
				signedTransactionInfo,
			},
		});
	}

	async function claim(
		account: TestAccount,
		signedTransaction: string,
		status = 200,
		code?: string,
	): Promise<StorePurchaseClaimResponse> {
		return createBuilder<StorePurchaseClaimResponse>(harness, account.token)
			.post('/premium/store/app-store/transactions')
			.body({signed_transaction: signedTransaction})
			.expect(status, code)
			.execute();
	}

	async function notify(signedPayload: string, status = 200): Promise<void> {
		await createBuilderWithoutAuth(harness).post('/webhooks/app-store').body({signedPayload}).expect(status).execute();
	}

	async function accountToken(account: TestAccount): Promise<string> {
		return (await getStoreContext(harness, account.token)).app_account_token;
	}

	async function release(account: TestAccount, purchaseId: string): Promise<void> {
		await createBuilder(harness, account.token)
			.delete(`/premium/store/purchases/${purchaseId}`)
			.body({password: account.password})
			.expect(204)
			.execute();
	}

	function addRenewal(originalTransactionId: string, expiresDate: number, appAccountToken?: string): string {
		const renewalId = nextTransactionId();
		fake.addTransaction('production', {
			transactionId: renewalId,
			originalTransactionId,
			productId: MONTHLY,
			purchaseDate: Date.now(),
			expiresDate,
			...(appAccountToken ? {appAccountToken} : {}),
		});
		const subscription = fake.getSubscription('production', originalTransactionId);
		fake.putSubscription('production', {
			...subscription!,
			status: 1,
			latestTransactionId: renewalId,
			renewalInfo: {autoRenewStatus: 1, ...(appAccountToken ? {appAccountToken} : {})},
		});
		return renewalId;
	}

	async function redeem(account: TestAccount, code: string): Promise<void> {
		await createBuilder(harness, account.token).post(`/gifts/${code}/redeem`).expect(204).execute();
	}

	function giftSyncParams(transactionId: string) {
		return {
			environment: 'production' as const,
			bundleId: BUNDLE_ID,
			productId: GIFT_MONTH,
			transactionId,
			originalTransactionId: transactionId,
			hintAccountToken: null,
		};
	}

	it('grants premium for a claim that includes the account token and answers repeats the same way', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const expiresDate = Date.now() + ms('30 days');
		const startedAt = Date.now() - ms('40 days');
		const originalTransactionId = seedSubscription({
			expiresDate,
			appAccountToken: token,
			recentSubscriptionStartDate: startedAt,
		});
		const jws = await signStoredTransaction('production', originalTransactionId);
		const first = await claim(account, jws);
		expect(first.purchase).toMatchObject({
			provider: 'app_store',
			kind: 'subscription',
			slot: 'monthly',
			state: 'active',
			entitled: true,
			will_renew: true,
			gift_code: null,
			environment: 'production',
		});
		expect(first.purchase.expires_at).toBe(new Date(expiresDate).toISOString());
		const second = await claim(account, jws);
		expect(second.purchase.id).toBe(first.purchase.id);
		const user = await findUser(account.userId);
		expect(user.premiumType).toBe(UserPremiumTypes.SUBSCRIPTION);
		expect(user.premiumUntil?.getTime()).toBe(expiresDate);
		expect(user.premiumBillingCycle).toBeNull();
		expect(user.premiumWillCancel).toBe(false);
		expect(user.premiumGraceEndsAt).toBeNull();
		expect(user.hasEverPurchased).toBe(true);
		expect(user.premiumSince?.getTime()).toBe(startedAt);
		expect(user.isPremium()).toBe(true);
		expect(fake.requests.some((request) => request.method === 'PUT')).toBe(false);
		const state = await createBuilder<PremiumStateResponse>(harness, account.token).get('/premium/state').execute();
		expect(state.store).toMatchObject({
			provider: 'app_store',
			purchase_id: first.purchase.id,
			slot: 'monthly',
			billing_cycle: 'monthly',
			state: 'active',
			grace_ends_at: null,
			will_renew: true,
			manage_url: 'https://apps.apple.com/account/subscriptions',
			environment: 'production',
		});
		const purchases = await createBuilder<Array<StorePurchaseResponse>>(harness, account.token)
			.get('/premium/store/purchases')
			.execute();
		expect(purchases.map((purchase) => purchase.id)).toEqual([first.purchase.id]);
		const entitlement = await service.getActiveStoreEntitlement(user.id);
		expect(entitlement).toMatchObject({provider: 'app_store', slot: 'monthly', autoRenew: true, graceEndsAt: null});
	});

	it('rejects a claim whose account token belongs to another account', async () => {
		const owner = await createTestAccount(harness);
		const other = await createTestAccount(harness);
		const token = await accountToken(owner);
		const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('30 days'), appAccountToken: token});
		const jws = await signStoredTransaction('production', originalTransactionId);
		await claim(other, jws, 403, APIErrorCodes.STORE_PURCHASE_OWNED_BY_OTHER_ACCOUNT);
		const user = await findUser(other.userId);
		expect(user.isPremium()).toBe(false);
		await claim(owner, jws);
	});

	it('binds a purchase without a token to the first claimer and sets the account token', async () => {
		const account = await createTestAccount(harness);
		const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('30 days')});
		const jws = await signStoredTransaction('production', originalTransactionId);
		await claim(account, jws);
		const token = await accountToken(account);
		const put = fake.requests.find((request) => request.method === 'PUT');
		expect(put?.path).toBe(`/inApps/v1/transactions/${originalTransactionId}/appAccountToken`);
		expect(put?.body).toEqual({appAccountToken: token});
		expect(fake.getTransaction('production', originalTransactionId)?.appAccountToken).toBe(token);
		expect((await findUser(account.userId)).isPremium()).toBe(true);
	});

	it('grants premium when a claim binds an unbound purchase that a newer sync already stored', async () => {
		const account = await createTestAccount(harness);
		const expiresDate = Date.now() + ms('30 days');
		const originalTransactionId = seedSubscription({expiresDate});
		const stored = await service.syncAppStorePurchase({
			environment: 'production',
			bundleId: BUNDLE_ID,
			productId: MONTHLY,
			transactionId: originalTransactionId,
			originalTransactionId,
			hintAccountToken: null,
		});
		expect(stored).toMatchObject({user_id: null, entitled: false, state: 'active'});
		await getStoreBillingRepository().updatePurchase(stored!, {last_event_at: new Date(Date.now() + ms('1 hour'))});
		const result = await claim(account, await signStoredTransaction('production', originalTransactionId));
		expect(result.purchase).toMatchObject({state: 'active', entitled: true});
		expect((await findUser(account.userId)).premiumUntil?.getTime()).toBe(expiresDate);
	});

	it('extends premium_until on DID_RENEW', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('1 day'), appAccountToken: token});
		await claim(account, await signStoredTransaction('production', originalTransactionId));
		const renewedExpiry = Date.now() + ms('31 days');
		const renewalId = nextTransactionId();
		fake.addTransaction('production', {
			transactionId: renewalId,
			originalTransactionId,
			productId: MONTHLY,
			purchaseDate: Date.now(),
			expiresDate: renewedExpiry,
			appAccountToken: token,
		});
		const subscription = fake.getSubscription('production', originalTransactionId);
		fake.putSubscription('production', {...subscription!, latestTransactionId: renewalId});
		await notify(await signNotification('DID_RENEW', 'production', renewalId));
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(renewedExpiry);
		expect(user.premiumGraceEndsAt).toBeNull();
	});

	it('sets premium_grace_ends_at while Apple reports a billing grace period', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const expiresDate = Date.now() - ms('1 day');
		const graceEnd = Date.now() + ms('5 days');
		const originalTransactionId = seedSubscription({
			status: 4,
			expiresDate,
			gracePeriodExpiresDate: graceEnd,
			appAccountToken: token,
		});
		const result = await claim(account, await signStoredTransaction('production', originalTransactionId));
		expect(result.purchase.state).toBe('grace');
		expect(result.purchase.entitled_until).toBe(new Date(graceEnd).toISOString());
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(expiresDate);
		expect(user.premiumGraceEndsAt?.getTime()).toBe(graceEnd);
		expect(user.isPremium()).toBe(true);
	});

	it('cuts access when Apple reports the subscription expired', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const expiresDate = Date.now() - ms('1 hour');
		const originalTransactionId = seedSubscription({expiresDate, appAccountToken: token});
		await claim(account, await signStoredTransaction('production', originalTransactionId));
		expect((await findUser(account.userId)).isPremium()).toBe(true);
		const subscription = fake.getSubscription('production', originalTransactionId);
		fake.putSubscription('production', {...subscription!, status: 2, renewalInfo: {autoRenewStatus: 0}});
		await notify(await signNotification('EXPIRED', 'production', originalTransactionId));
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(expiresDate);
		expect(user.premiumGraceEndsAt?.getTime()).toBe(expiresDate);
		expect(user.isPremium()).toBe(false);
	});

	it('cuts access at the revocation date on REFUND', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('20 days'), appAccountToken: token});
		await claim(account, await signStoredTransaction('production', originalTransactionId));
		const revocationDate = Date.now() - ms('1 minute');
		fake.updateTransaction('production', originalTransactionId, {revocationDate, revocationReason: 0});
		const subscription = fake.getSubscription('production', originalTransactionId);
		fake.putSubscription('production', {...subscription!, status: 5});
		await notify(await signNotification('REFUND', 'production', originalTransactionId));
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(revocationDate);
		expect(user.premiumGraceEndsAt?.getTime()).toBe(revocationDate);
		expect(user.isPremium()).toBe(false);
		const purchases = await service.listStorePurchases(user.id);
		expect(purchases[0]).toMatchObject({state: 'revoked', entitled: false});
	});

	it('revokes an unredeemed gift code when the gift purchase is refunded', async () => {
		const account = await createTestAccount(harness);
		const transactionId = seedGift('production');
		const result = await claim(account, await signStoredTransaction('production', transactionId));
		expect(result.purchase).toMatchObject({kind: 'gift', slot: 'gift_1_month', state: 'fulfilled'});
		const giftCode = result.gift_code;
		expect(giftCode).toBeTruthy();
		await createBuilder(harness, account.token).get(`/gifts/${giftCode}`).execute();
		fake.updateTransaction('production', transactionId, {revocationDate: Date.now() - 1000, revocationReason: 0});
		await notify(await signNotification('REFUND', 'production', transactionId));
		const gift = await getUserRepository().findGiftCode(giftCode!);
		expect(gift?.revokedAt).not.toBeNull();
		await createBuilder(harness, account.token)
			.get(`/gifts/${giftCode}`)
			.expect(404, APIErrorCodes.UNKNOWN_GIFT_CODE)
			.execute();
	});

	it('mints exactly one gift code when a claim and a notification race', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const transactionId = seedGift('production', token);
		const jws = await signStoredTransaction('production', transactionId);
		const notification = await signNotification('ONE_TIME_CHARGE', 'production', transactionId);
		const [claimed] = await Promise.all([claim(account, jws), notify(notification)]);
		const gifts = await getUserRepository().findGiftCodesByCreator((await findUser(account.userId)).id);
		expect(gifts).toHaveLength(1);
		expect(claimed.gift_code).toBe(gifts[0]?.code);
		expect(gifts[0]).toMatchObject({durationType: 'months', durationQuantity: 1});
		const purchaser = await findUser(account.userId);
		expect(purchaser.giftInventoryServerSeq).toBe(1);
		expect(purchaser.hasEverPurchased).toBe(true);
		expect(purchaser.isPremium()).toBe(false);
	});

	it('records a sandbox purchase without entitlement unless the account is allowed', async () => {
		const tester = await createTestAccount(harness);
		const originalTransactionId = seedSubscription({environment: 'sandbox', expiresDate: Date.now() + ms('1 day')});
		await claim(
			tester,
			await signStoredTransaction('sandbox', originalTransactionId),
			403,
			APIErrorCodes.STORE_PURCHASE_SANDBOX_NOT_ENTITLED,
		);
		const purchases = await createBuilder<Array<StorePurchaseResponse>>(harness, tester.token)
			.get('/premium/store/purchases')
			.execute();
		expect(purchases).toHaveLength(1);
		expect(purchases[0]).toMatchObject({environment: 'sandbox', entitled: false, state: 'active'});
		expect((await findUser(tester.userId)).isPremium()).toBe(false);
		const giftTransactionId = seedGift('sandbox');
		await claim(
			tester,
			await signStoredTransaction('sandbox', giftTransactionId),
			403,
			APIErrorCodes.STORE_PURCHASE_SANDBOX_NOT_ENTITLED,
		);
		expect(await getUserRepository().findGiftCodesByCreator((await findUser(tester.userId)).id)).toHaveLength(0);

		const allowed = await createTestAccount(harness);
		Config.storeBilling.sandboxUserIds = [allowed.userId];
		const allowedTransactionId = seedSubscription({environment: 'sandbox', expiresDate: Date.now() + ms('1 day')});
		const result = await claim(allowed, await signStoredTransaction('sandbox', allowedTransactionId));
		expect(result.purchase).toMatchObject({environment: 'sandbox', entitled: true});
		expect((await findUser(allowed.userId)).isPremium()).toBe(true);
	});

	it('moves a sandbox purchase to an allowed account that restores it after another account bought it', async () => {
		const tester = await createTestAccount(harness);
		const allowed = await createTestAccount(harness);
		const originalTransactionId = seedSubscription({
			environment: 'sandbox',
			expiresDate: Date.now() + ms('1 day'),
			appAccountToken: await accountToken(tester),
		});
		const jws = await signStoredTransaction('sandbox', originalTransactionId);
		await claim(tester, jws, 403, APIErrorCodes.STORE_PURCHASE_SANDBOX_NOT_ENTITLED);
		Config.storeBilling.sandboxUserIds = [allowed.userId];
		const result = await claim(allowed, jws);
		expect(result.purchase).toMatchObject({environment: 'sandbox', entitled: true});
		expect((await findUser(allowed.userId)).isPremium()).toBe(true);
		expect(fake.getTransaction('sandbox', originalTransactionId)?.appAccountToken).toBe(await accountToken(allowed));
		expect(await service.listStorePurchases((await findUser(tester.userId)).id)).toHaveLength(0);
		expect((await findUser(tester.userId)).isPremium()).toBe(false);
	});

	it('blocks a lifetime account from a subscription without granting anything', async () => {
		const account = await createTestAccount(harness);
		await setTestPremium(harness, account.token, account.userId, {premium_type: UserPremiumTypes.LIFETIME});
		const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('30 days')});
		const {json} = await createBuilder<{code: string; reason: string; provider: string}>(harness, account.token)
			.post('/premium/store/app-store/transactions')
			.body({signed_transaction: await signStoredTransaction('production', originalTransactionId)})
			.expect(403, APIErrorCodes.PREMIUM_PURCHASE_BLOCKED)
			.executeWithResponse();
		expect(json).toMatchObject({reason: 'lifetime', provider: 'app_store'});
		const user = await findUser(account.userId);
		expect(user.premiumType).toBe(UserPremiumTypes.LIFETIME);
		expect(user.premiumUntil).toBeNull();
	});

	it('moves a released subscription to the next account that claims it', async () => {
		const first = await createTestAccount(harness);
		const second = await createTestAccount(harness);
		const firstToken = await accountToken(first);
		const originalTransactionId = seedSubscription({
			expiresDate: Date.now() + ms('30 days'),
			appAccountToken: firstToken,
		});
		const jws = await signStoredTransaction('production', originalTransactionId);
		const claimed = await claim(first, jws);
		await claim(second, jws, 403, APIErrorCodes.STORE_PURCHASE_OWNED_BY_OTHER_ACCOUNT);
		await createBuilder(harness, first.token)
			.delete(`/premium/store/purchases/${claimed.purchase.id}`)
			.body({})
			.expect(403, APIErrorCodes.SUDO_MODE_REQUIRED)
			.execute();
		await createBuilder(harness, first.token)
			.delete(`/premium/store/purchases/${claimed.purchase.id}`)
			.body({password: first.password})
			.expect(204)
			.execute();
		const released = await findUser(first.userId);
		expect(released.premiumGraceEndsAt?.getTime()).toBe(released.premiumUntil?.getTime());
		expect(released.premiumUntil!.getTime()).toBeLessThanOrEqual(Date.now());
		expect(released.premiumUntil!.getTime()).toBeLessThan(new Date(claimed.purchase.expires_at!).getTime());
		expect(await service.listStorePurchases(released.id)).toHaveLength(0);
		const moved = await claim(second, jws);
		expect(moved.purchase.id).toBe(claimed.purchase.id);
		const secondToken = await accountToken(second);
		expect(fake.getTransaction('production', originalTransactionId)?.appAccountToken).toBe(secondToken);
		expect((await findUser(second.userId)).isPremium()).toBe(true);
	});

	it('rejects notifications that fail signature checks', async () => {
		const account = await createTestAccount(harness);
		const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('30 days')});
		const foreign = createAppleTestPki();
		await notify(await signNotification('DID_RENEW', 'production', originalTransactionId, foreign), 401);
		await createBuilderWithoutAuth(harness)
			.post('/webhooks/app-store')
			.body({signedPayload: 'not-a-jws'})
			.expect(401, APIErrorCodes.STORE_NOTIFICATION_UNAUTHORIZED)
			.execute();
		expect(await service.listStorePurchases((await findUser(account.userId)).id)).toHaveLength(0);
	});

	it('stores a notification for an unknown account without binding it', async () => {
		const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('30 days')});
		await notify(await signNotification('SUBSCRIBED', 'production', originalTransactionId));
		const row = await getStoreBillingRepository().findPurchase(`app_store:production:${originalTransactionId}`);
		expect(row).toMatchObject({user_id: null, entitled: false, state: 'active'});
	});

	it('rejects a transaction for a product that is not on sale', async () => {
		const account = await createTestAccount(harness);
		const originalTransactionId = seedSubscription({
			expiresDate: Date.now() + ms('30 days'),
			productId: 'com.fluxer.unknown',
		});
		await claim(
			account,
			await signStoredTransaction('production', originalTransactionId),
			400,
			APIErrorCodes.STORE_PURCHASE_INVALID,
		);
	});

	it('writes the store portion of the users row as specified', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const giftEnd = Date.now() + ms('20 days');
		const premiumSince = Date.now() - ms('100 days');
		await setTestPremium(harness, account.token, account.userId, {
			premium_type: UserPremiumTypes.SUBSCRIPTION,
			premium_until: null,
			premium_gift_extension_ends_at: new Date(giftEnd).toISOString(),
			premium_since: new Date(premiumSince).toISOString(),
			premium_billing_cycle: 'monthly',
		});
		const expiresDate = Date.now() + ms('30 days');
		const originalTransactionId = seedSubscription({
			expiresDate,
			appAccountToken: token,
			autoRenewStatus: 0,
			recentSubscriptionStartDate: Date.now() - ms('3 days'),
		});
		const grantedAt = Date.now();
		const result = await claim(account, await signStoredTransaction('production', originalTransactionId));
		expect(result.purchase).toMatchObject({state: 'canceled', will_renew: false, entitled: true});
		const user = await findUser(account.userId);
		expect(user.premiumType).toBe(UserPremiumTypes.SUBSCRIPTION);
		expect(user.premiumUntil?.getTime()).toBe(expiresDate);
		expect(user.premiumBillingCycle).toBeNull();
		expect(user.premiumWillCancel).toBe(true);
		expect(user.premiumGraceEndsAt).toBeNull();
		expect(user.hasEverPurchased).toBe(true);
		expect(user.premiumSince?.getTime()).toBe(premiumSince);
		expectCloseTo(user.premiumGiftExtensionEndsAt, giftEnd + (expiresDate - grantedAt));
		await claim(account, await signStoredTransaction('production', originalTransactionId));
		const unchanged = await findUser(account.userId);
		expect(unchanged.premiumGiftExtensionEndsAt?.getTime()).toBe(user.premiumGiftExtensionEndsAt?.getTime());
	});

	it('uses the subscription start as premium_since for a first premium grant', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const startedAt = Date.now() - ms('2 days');
		const originalTransactionId = seedSubscription({
			expiresDate: Date.now() + ms('30 days'),
			appAccountToken: token,
			recentSubscriptionStartDate: startedAt,
		});
		await notify(await signNotification('SUBSCRIBED', 'production', originalTransactionId));
		const user = await findUser(account.userId);
		expect(user.premiumSince?.getTime()).toBe(startedAt);
		expect(user.isPremium()).toBe(true);
	});

	it('reinstates an unredeemed gift code when a gift refund is reversed', async () => {
		const account = await createTestAccount(harness);
		const transactionId = seedGift('production');
		const result = await claim(account, await signStoredTransaction('production', transactionId));
		const giftCode = result.gift_code!;
		fake.updateTransaction('production', transactionId, {revocationDate: Date.now() - 1000, revocationReason: 0});
		await notify(await signNotification('REFUND', 'production', transactionId));
		expect((await getUserRepository().findGiftCode(giftCode))?.revokedAt).not.toBeNull();
		fake.updateTransaction('production', transactionId, {revocationDate: undefined, revocationReason: undefined});
		await notify(await signNotification('REFUND_REVERSED', 'production', transactionId));
		expect((await getUserRepository().findGiftCode(giftCode))?.revokedAt).toBeNull();
		const row = await getStoreBillingRepository().findPurchase(`app_store:production:${transactionId}`);
		expect(row).toMatchObject({state: 'fulfilled', entitled: true, revoked_at: null, revocation_reason: null});
		await createBuilder(harness, account.token).get(`/gifts/${giftCode}`).execute();
	});

	it('restores the gift time of a redeemed gift when its refund is reversed', async () => {
		const purchaser = await createTestAccount(harness);
		const redeemer = await createTestAccount(harness);
		const transactionId = seedGift('production');
		const result = await claim(purchaser, await signStoredTransaction('production', transactionId));
		const giftCode = result.gift_code!;
		await redeem(redeemer, giftCode);
		const granted = await findUser(redeemer.userId);
		expect(granted.isPremium()).toBe(true);
		const grantedGiftEnd = granted.premiumGiftExtensionEndsAt!.getTime();
		fake.updateTransaction('production', transactionId, {revocationDate: Date.now() - 1000, revocationReason: 0});
		await notify(await signNotification('REFUND', 'production', transactionId));
		expect((await findUser(redeemer.userId)).isPremium()).toBe(false);
		fake.updateTransaction('production', transactionId, {revocationDate: undefined, revocationReason: undefined});
		await notify(await signNotification('REFUND_REVERSED', 'production', transactionId));
		expect((await getUserRepository().findGiftCode(giftCode))?.revokedAt).toBeNull();
		const restored = await findUser(redeemer.userId);
		expect(restored.isPremium()).toBe(true);
		expectCloseTo(restored.premiumGiftExtensionEndsAt, grantedGiftEnd, ms('1 minute'));
	});

	it('retries the gift reversal when it failed after the refund was stored', async () => {
		const account = await createTestAccount(harness);
		const transactionId = seedGift('production');
		const result = await claim(account, await signStoredTransaction('production', transactionId));
		fake.updateTransaction('production', transactionId, {revocationDate: Date.now() - 1000, revocationReason: 0});
		const revoke = vi
			.spyOn(getUserRepository(), 'revokeGiftCode')
			.mockRejectedValueOnce(new Error('Cassandra write timed out'));
		try {
			await expect(service.syncAppStorePurchase(giftSyncParams(transactionId))).rejects.toThrow();
			const stored = await getStoreBillingRepository().findPurchase(`app_store:production:${transactionId}`);
			expect(stored?.state).toBe('refunded');
			await service.syncAppStorePurchase(giftSyncParams(transactionId));
		} finally {
			revoke.mockRestore();
		}
		expect((await getUserRepository().findGiftCode(result.gift_code!))?.revokedAt).not.toBeNull();
	});

	it('clears a reversed subscription refund from the stored purchase', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const expiresDate = Date.now() + ms('20 days');
		const originalTransactionId = seedSubscription({expiresDate, appAccountToken: token});
		await claim(account, await signStoredTransaction('production', originalTransactionId));
		fake.updateTransaction('production', originalTransactionId, {
			revocationDate: Date.now() - ms('1 minute'),
			revocationReason: 0,
		});
		const subscription = fake.getSubscription('production', originalTransactionId);
		fake.putSubscription('production', {...subscription!, status: 5});
		await notify(await signNotification('REFUND', 'production', originalTransactionId));
		fake.updateTransaction('production', originalTransactionId, {
			revocationDate: undefined,
			revocationReason: undefined,
		});
		fake.putSubscription('production', {...subscription!, status: 1});
		await notify(await signNotification('REFUND_REVERSED', 'production', originalTransactionId));
		const row = await getStoreBillingRepository().findPurchase(`app_store:production:${originalTransactionId}`);
		expect(row).toMatchObject({state: 'active', entitled: true, revoked_at: null, revocation_reason: null});
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(expiresDate);
		expect(user.isPremium()).toBe(true);
	});

	it('keeps the stacked gift time fixed across release and reclaim', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const giftDuration = ms('20 days');
		await setTestPremium(harness, account.token, account.userId, {
			premium_type: UserPremiumTypes.SUBSCRIPTION,
			premium_until: null,
			premium_gift_extension_ends_at: new Date(Date.now() + giftDuration).toISOString(),
		});
		const expiresDate = Date.now() + ms('30 days');
		const originalTransactionId = seedSubscription({expiresDate, appAccountToken: token});
		const jws = await signStoredTransaction('production', originalTransactionId);
		const claimed = await claim(account, jws);
		for (let cycle = 0; cycle < 2; cycle++) {
			await release(account, claimed.purchase.id);
			const released = await findUser(account.userId);
			expect(released.premiumGiftExtensionEndsAt!.getTime()).toBeLessThanOrEqual(Date.now() + giftDuration + 5000);
			await claim(account, jws);
			const reclaimed = await findUser(account.userId);
			expect(reclaimed.premiumUntil?.getTime()).toBe(expiresDate);
			expectCloseTo(reclaimed.premiumGiftExtensionEndsAt, expiresDate + giftDuration);
		}
	});

	it('pulls stacked gift time back to the revocation date on a refund', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const giftDuration = ms('20 days');
		await setTestPremium(harness, account.token, account.userId, {
			premium_type: UserPremiumTypes.SUBSCRIPTION,
			premium_until: null,
			premium_gift_extension_ends_at: new Date(Date.now() + giftDuration).toISOString(),
		});
		const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('30 days'), appAccountToken: token});
		await claim(account, await signStoredTransaction('production', originalTransactionId));
		const revocationDate = Date.now() - ms('1 minute');
		fake.updateTransaction('production', originalTransactionId, {revocationDate, revocationReason: 0});
		const subscription = fake.getSubscription('production', originalTransactionId);
		fake.putSubscription('production', {...subscription!, status: 5});
		await notify(await signNotification('REFUND', 'production', originalTransactionId));
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(revocationDate);
		expectCloseTo(user.premiumGiftExtensionEndsAt, revocationDate + giftDuration);
	});

	it('moves a lapsed subscription to the account that resubscribes with the same Apple Account', async () => {
		const first = await createTestAccount(harness);
		const second = await createTestAccount(harness);
		const firstToken = await accountToken(first);
		const secondToken = await accountToken(second);
		const originalTransactionId = seedSubscription({
			expiresDate: Date.now() + ms('30 days'),
			appAccountToken: firstToken,
		});
		await claim(first, await signStoredTransaction('production', originalTransactionId));
		const lapsedAt = Date.now() - ms('1 hour');
		fake.updateTransaction('production', originalTransactionId, {expiresDate: lapsedAt});
		const subscription = fake.getSubscription('production', originalTransactionId);
		fake.putSubscription('production', {...subscription!, status: 2, renewalInfo: {autoRenewStatus: 0}});
		await notify(await signNotification('EXPIRED', 'production', originalTransactionId));
		expectAccessCut(await findUser(first.userId));
		const renewedExpiry = Date.now() + ms('30 days');
		const renewalId = addRenewal(originalTransactionId, renewedExpiry, secondToken);
		await notify(await signNotification('SUBSCRIBED', 'production', renewalId));
		const row = await getStoreBillingRepository().findPurchase(`app_store:production:${originalTransactionId}`);
		expect(row?.user_id?.toString()).toBe(second.userId);
		expect(row?.account_token).toBe(secondToken);
		expectAccessCut(await findUser(first.userId));
		const resubscribed = await findUser(second.userId);
		expect(resubscribed.premiumUntil?.getTime()).toBe(renewedExpiry);
		expect(resubscribed.isPremium()).toBe(true);
		const restored = await claim(second, await signStoredTransaction('production', renewalId));
		expect(restored.purchase.id).toBe(row?.id.toString());
		expect(await service.listStorePurchases((await findUser(first.userId)).id)).toHaveLength(0);
	});

	it('takes only the refunded gift off the stacked gift time of a store subscriber', async () => {
		const purchaser = await createTestAccount(harness);
		const redeemer = await createTestAccount(harness);
		const redeemerToken = await accountToken(redeemer);
		const storeExpiry = Date.now() + ms('20 days');
		const originalTransactionId = seedSubscription({expiresDate: storeExpiry, appAccountToken: redeemerToken});
		await claim(redeemer, await signStoredTransaction('production', originalTransactionId));
		const otherCode = `storegift${randomUUID().replaceAll('-', '').slice(0, 20)}`;
		await getUserRepository().createGiftCode({
			code: otherCode,
			duration_months: null,
			duration_type: 'months',
			duration_quantity: 1,
			created_at: new Date(),
			created_by_user_id: createUserID(BigInt(purchaser.userId)),
			redeemed_at: null,
			redeemed_by_user_id: null,
			stripe_payment_intent_id: null,
			visionary_sequence_number: null,
			checkout_session_id: null,
			version: 1,
		});
		await redeem(redeemer, otherCode);
		const transactionId = seedGift('production');
		const result = await claim(purchaser, await signStoredTransaction('production', transactionId));
		await redeem(redeemer, result.gift_code!);
		const stackedEnd = (await findUser(redeemer.userId)).premiumGiftExtensionEndsAt!;
		expect(stackedEnd.getTime()).toBeGreaterThan(storeExpiry + ms('55 days'));
		fake.updateTransaction('production', transactionId, {revocationDate: Date.now() - 1000, revocationReason: 0});
		await notify(await signNotification('REFUND', 'production', transactionId));
		const after = await findUser(redeemer.userId);
		const oneMonth = addGiftCodeDuration(stackedEnd, 'months', 1)!.getTime() - stackedEnd.getTime();
		expect(after.premiumGiftExtensionEndsAt?.getTime()).toBe(stackedEnd.getTime() - oneMonth);
		expect(after.premiumUntil?.getTime()).toBe(storeExpiry);
	});

	it('takes a refunded gift off a store subscriber once when the refund is retried', async () => {
		const purchaser = await createTestAccount(harness);
		const redeemer = await createTestAccount(harness);
		const redeemerToken = await accountToken(redeemer);
		const storeExpiry = Date.now() + ms('20 days');
		const originalTransactionId = seedSubscription({expiresDate: storeExpiry, appAccountToken: redeemerToken});
		await claim(redeemer, await signStoredTransaction('production', originalTransactionId));
		const firstGift = seedGift('production');
		await redeem(redeemer, (await claim(purchaser, await signStoredTransaction('production', firstGift))).gift_code!);
		const transactionId = seedGift('production');
		const result = await claim(purchaser, await signStoredTransaction('production', transactionId));
		await redeem(redeemer, result.gift_code!);
		const stackedEnd = (await findUser(redeemer.userId)).premiumGiftExtensionEndsAt!;
		fake.updateTransaction('production', transactionId, {revocationDate: Date.now() - 1000, revocationReason: 0});
		const revoke = vi
			.spyOn(getUserRepository(), 'revokeGiftCode')
			.mockRejectedValueOnce(new Error('Cassandra write timed out'));
		try {
			await expect(service.syncAppStorePurchase(giftSyncParams(transactionId))).rejects.toThrow();
			await service.syncAppStorePurchase(giftSyncParams(transactionId));
		} finally {
			revoke.mockRestore();
		}
		const oneMonth = addGiftCodeDuration(stackedEnd, 'months', 1)!.getTime() - stackedEnd.getTime();
		expect((await findUser(redeemer.userId)).premiumGiftExtensionEndsAt?.getTime()).toBe(
			stackedEnd.getTime() - oneMonth,
		);
		expect((await getUserRepository().findGiftCode(result.gift_code!))?.revokedAt).not.toBeNull();
	});

	it('credits a reversed gift refund once when the reinstatement is retried', async () => {
		const purchaser = await createTestAccount(harness);
		const redeemer = await createTestAccount(harness);
		const transactionId = seedGift('production');
		const result = await claim(purchaser, await signStoredTransaction('production', transactionId));
		const giftCode = result.gift_code!;
		await redeem(redeemer, giftCode);
		const grantedGiftEnd = (await findUser(redeemer.userId)).premiumGiftExtensionEndsAt!.getTime();
		fake.updateTransaction('production', transactionId, {revocationDate: Date.now() - 1000, revocationReason: 0});
		await notify(await signNotification('REFUND', 'production', transactionId));
		fake.updateTransaction('production', transactionId, {revocationDate: undefined, revocationReason: undefined});
		const unrevoke = vi
			.spyOn(getUserRepository(), 'unrevokeGiftCode')
			.mockRejectedValueOnce(new Error('Cassandra write timed out'));
		try {
			await expect(service.syncAppStorePurchase(giftSyncParams(transactionId))).rejects.toThrow();
			await service.syncAppStorePurchase(giftSyncParams(transactionId));
		} finally {
			unrevoke.mockRestore();
		}
		expect((await getUserRepository().findGiftCode(giftCode))?.revokedAt).toBeNull();
		expectCloseTo((await findUser(redeemer.userId)).premiumGiftExtensionEndsAt, grantedGiftEnd, ms('1 minute'));
	});

	it('restores nothing when a refund of a used gift is reversed', async () => {
		const purchaser = await createTestAccount(harness);
		const redeemer = await createTestAccount(harness);
		const transactionId = seedGift('production');
		const result = await claim(purchaser, await signStoredTransaction('production', transactionId));
		const giftCode = result.gift_code!;
		await redeem(redeemer, giftCode);
		await upsertOne(GiftCodes.patchByPk({code: giftCode}, {redeemed_at: Db.set(new Date(Date.now() - ms('40 days')))}));
		const redeemed = await findUser(redeemer.userId);
		await getUserRepository().patchUpsert(
			redeemed.id,
			{premium_gift_extension_ends_at: new Date(Date.now() - ms('10 days'))},
			redeemed.toRow(),
		);
		fake.updateTransaction('production', transactionId, {revocationDate: Date.now() - 1000, revocationReason: 0});
		await notify(await signNotification('REFUND', 'production', transactionId));
		fake.updateTransaction('production', transactionId, {revocationDate: undefined, revocationReason: undefined});
		await notify(await signNotification('REFUND_REVERSED', 'production', transactionId));
		expect((await getUserRepository().findGiftCode(giftCode))?.revokedAt).toBeNull();
		expect((await findUser(redeemer.userId)).isPremium()).toBe(false);
	});

	it('keeps a subscription whose product was removed from the config while refreshing it', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const expiresDate = Date.now() + ms('30 days');
		const originalTransactionId = seedSubscription({expiresDate, appAccountToken: token});
		await claim(account, await signStoredTransaction('production', originalTransactionId));
		const storeKey = `app_store:production:${originalTransactionId}`;
		const savedProducts = Config.appStore.products;
		Config.appStore.products = Object.fromEntries(
			Object.entries(savedProducts).filter(([productId]) => productId !== MONTHLY),
		);
		try {
			const refreshed = await service.refreshStorePurchase(storeKey);
			expect(refreshed).toMatchObject({state: 'active', entitled: true});
		} finally {
			Config.appStore.products = savedProducts;
		}
		expect(await getStoreBillingRepository().findPurchase(storeKey)).toMatchObject({state: 'active', entitled: true});
		expect(await getKVClient().zrangebyscore(STORE_PURCHASE_REFRESH_QUEUE_KEY, '-inf', '+inf')).toContain(storeKey);
		const user = await findUser(account.userId);
		expect(user.premiumUntil?.getTime()).toBe(expiresDate);
		expect(user.isPremium()).toBe(true);
	});

	it('recovers a gift refund from the notification history when its delivery was lost', async () => {
		const account = await createTestAccount(harness);
		const transactionId = seedGift('production');
		const result = await claim(account, await signStoredTransaction('production', transactionId));
		fake.updateTransaction('production', transactionId, {revocationDate: Date.now() - 1000, revocationReason: 0});
		fake.addNotificationHistory('production', {
			signedPayload: await signNotification('REFUND', 'production', transactionId),
			notificationType: 'REFUND',
			sentAt: Date.now() - ms('1 hour'),
			failed: false,
		});
		fake.addNotificationHistory('production', {
			signedPayload: await signNotification('DID_RENEW', 'production', transactionId),
			notificationType: 'DID_RENEW',
			sentAt: Date.now() - ms('3 days'),
			failed: true,
		});
		await pollAppStoreNotificationHistory({}, createWorkerHelpers());
		expect((await getUserRepository().findGiftCode(result.gift_code!))?.revokedAt).not.toBeNull();
		expect((await getStoreBillingRepository().findPurchase(`app_store:production:${transactionId}`))?.state).toBe(
			'refunded',
		);
		const historyRequests = fake.requests.filter((request) => request.path === '/inApps/v1/notifications/history');
		expect(historyRequests.map((request) => request.body)).toContainEqual(
			expect.objectContaining({onlyFailures: true}),
		);
		await pollAppStoreNotificationHistory({}, createWorkerHelpers());
	});

	describe('restore', () => {
		it('answers a second device that sends a later renewal of the same subscription', async () => {
			const account = await createTestAccount(harness);
			const token = await accountToken(account);
			const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('1 day'), appAccountToken: token});
			const first = await claim(account, await signStoredTransaction('production', originalTransactionId));
			const renewedExpiry = Date.now() + ms('31 days');
			const renewalId = addRenewal(originalTransactionId, renewedExpiry, token);
			const restored = await claim(account, await signStoredTransaction('production', renewalId));
			expect(restored.purchase.id).toBe(first.purchase.id);
			expect(restored.purchase.expires_at).toBe(new Date(renewedExpiry).toISOString());
			const again = await claim(account, await signStoredTransaction('production', renewalId));
			expect(again.purchase.id).toBe(first.purchase.id);
			expect(await service.listStorePurchases((await findUser(account.userId)).id)).toHaveLength(1);
			expect((await findUser(account.userId)).premiumUntil?.getTime()).toBe(renewedExpiry);
		});

		it('moves the purchase to the restoring account after the owner was deleted', async () => {
			const owner = await createTestAccount(harness);
			const restorer = await createTestAccount(harness);
			const ownerToken = await accountToken(owner);
			const expiresDate = Date.now() + ms('30 days');
			const originalTransactionId = seedSubscription({expiresDate, appAccountToken: ownerToken});
			const jws = await signStoredTransaction('production', originalTransactionId);
			const claimed = await claim(owner, jws);
			const ownerUser = await findUser(owner.userId);
			await getUserRepository().patchUpsert(
				ownerUser.id,
				{flags: ownerUser.flags | UserFlags.DELETED},
				ownerUser.toRow(),
			);
			const restored = await claim(restorer, jws);
			expect(restored.purchase.id).toBe(claimed.purchase.id);
			const row = await getStoreBillingRepository().findPurchase(`app_store:production:${originalTransactionId}`);
			expect(row?.user_id?.toString()).toBe(restorer.userId);
			expect(fake.getTransaction('production', originalTransactionId)?.appAccountToken).toBe(
				await accountToken(restorer),
			);
			expect((await findUser(restorer.userId)).premiumUntil?.getTime()).toBe(expiresDate);
		});

		it('refuses a restore while another live account owns the purchase without naming it', async () => {
			const owner = await createTestAccount(harness);
			const other = await createTestAccount(harness);
			const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('30 days')});
			const jws = await signStoredTransaction('production', originalTransactionId);
			await claim(owner, jws);
			const {json} = await createBuilder<Record<string, unknown>>(harness, other.token)
				.post('/premium/store/app-store/transactions')
				.body({signed_transaction: jws})
				.expect(403, APIErrorCodes.STORE_PURCHASE_OWNED_BY_OTHER_ACCOUNT)
				.executeWithResponse();
			const body = JSON.stringify(json);
			expect(body).not.toContain(owner.userId);
			expect(body).not.toContain(await accountToken(owner));
			expect((await findUser(other.userId)).isPremium()).toBe(false);
		});

		it('records an expired subscription on restore without granting premium', async () => {
			const account = await createTestAccount(harness);
			const token = await accountToken(account);
			const originalTransactionId = seedSubscription({
				status: 2,
				expiresDate: Date.now() - ms('10 days'),
				appAccountToken: token,
				autoRenewStatus: 0,
			});
			const result = await claim(account, await signStoredTransaction('production', originalTransactionId));
			expect(result.purchase).toMatchObject({state: 'expired', entitled: false, entitled_until: null});
			const user = await findUser(account.userId);
			expect(user.premiumUntil).toBeNull();
			expect(user.isPremium()).toBe(false);
		});

		it('accepts an old transaction signed while its leaf certificate was valid', async () => {
			const account = await createTestAccount(harness);
			const token = await accountToken(account);
			const now = Date.now();
			const oldPki = createAppleTestPki({
				leaf: {notBefore: new Date(now - ms('400 days')), notAfter: new Date(now - ms('10 days'))},
			});
			setInjectedAppleRootCertificates([pki.root, oldPki.root]);
			const expiresDate = now + ms('30 days');
			const originalTransactionId = seedSubscription({expiresDate, appAccountToken: token});
			const transaction = fake.getTransaction('production', originalTransactionId);
			const oldJws = await oldPki.signJws({
				bundleId: BUNDLE_ID,
				environment: 'Production',
				type: 'Auto-Renewable Subscription',
				inAppOwnershipType: 'PURCHASED',
				...transaction,
				purchaseDate: now - ms('20 days'),
				signedDate: now - ms('20 days'),
			});
			const result = await claim(account, oldJws);
			expect(result.purchase).toMatchObject({state: 'active', entitled: true});
			expect((await findUser(account.userId)).premiumUntil?.getTime()).toBe(expiresDate);
		});
	});

	it('marks a subscription Apple no longer knows as expired and stops refreshing it', async () => {
		const account = await createTestAccount(harness);
		const token = await accountToken(account);
		const originalTransactionId = seedSubscription({expiresDate: Date.now() + ms('30 days'), appAccountToken: token});
		await claim(account, await signStoredTransaction('production', originalTransactionId));
		const storeKey = `app_store:production:${originalTransactionId}`;
		fake.failNext(
			{path: `/inApps/v1/subscriptions/${originalTransactionId}`},
			{status: 404, errorCode: 4040010, errorMessage: 'Transaction id not found.'},
		);
		const refreshed = await service.refreshStorePurchase(storeKey);
		expect(refreshed).toMatchObject({state: 'expired', entitled: false});
		expect(await getKVClient().zrangebyscore(STORE_PURCHASE_REFRESH_QUEUE_KEY, '-inf', '+inf')).not.toContain(storeKey);
		expectAccessCut(await findUser(account.userId));
	});

	it('refuses to fulfil a gift bought in a quantity above one', async () => {
		const account = await createTestAccount(harness);
		const transactionId = seedGift('production');
		fake.updateTransaction('production', transactionId, {quantity: 2});
		await claim(
			account,
			await signStoredTransaction('production', transactionId),
			400,
			APIErrorCodes.STORE_PURCHASE_INVALID,
		);
		const row = await getStoreBillingRepository().findPurchase(`app_store:production:${transactionId}`);
		expect(row).toMatchObject({entitled: false, gift_code: null, revocation_reason: 'unsupported_quantity'});
		expect(await getUserRepository().findGiftCodesByCreator((await findUser(account.userId)).id)).toHaveLength(0);
	});
});
