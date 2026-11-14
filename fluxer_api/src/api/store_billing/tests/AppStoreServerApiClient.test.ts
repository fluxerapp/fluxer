// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {setInjectedAppleRootCertificates} from '@app/api/store_billing/app_store/AppleRootCertificates';
import {AppStoreJwsVerificationError} from '@app/api/store_billing/app_store/AppStoreJwsVerifier';
import {
	APP_STORE_PRODUCTION_BASE_URL,
	APP_STORE_SANDBOX_BASE_URL,
	AppStoreServerApiClient,
	AppStoreServerApiError,
	isAppStoreTransactionNotFound,
} from '@app/api/store_billing/app_store/AppStoreServerApiClient';
import {type AppleTestPki, createAppleTestPki} from '@app/api/store_billing/tests/AppleTestPki';
import {
	type AppStoreServerApiFake,
	createAppStoreServerApiFake,
} from '@app/api/test/msw/handlers/AppStoreServerApiHandlers';
import {server} from '@app/api/test/msw/server';
import {ms} from 'itty-time';
import {delay, HttpResponse, http} from 'msw';
import {afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const MONTHLY = 'com.fluxer.plutonium.monthly';
const GIFT = 'com.fluxer.gift.1month';

async function captureError(promise: Promise<unknown>): Promise<unknown> {
	return await promise.then(
		() => null,
		(error: unknown) => error,
	);
}

function seedSubscription(fake: AppStoreServerApiFake, environment: 'production' | 'sandbox', id: string): void {
	fake.addTransaction(environment, {
		transactionId: id,
		originalTransactionId: id,
		productId: MONTHLY,
		expiresDate: Date.now() + ms('30 days'),
	});
	fake.putSubscription(environment, {originalTransactionId: id, status: 1, latestTransactionId: id});
}

describe('AppStoreServerApiClient', () => {
	let pki: AppleTestPki;
	let fake: AppStoreServerApiFake;
	let savedAppStore: typeof Config.appStore;

	beforeAll(() => {
		pki = createAppleTestPki();
	});

	beforeEach(() => {
		savedAppStore = {...Config.appStore};
		setInjectedAppleRootCertificates([pki.root]);
		fake = createAppStoreServerApiFake({sign: (payload) => pki.signJws(payload)});
		server.use(...fake.handlers);
	});

	afterEach(() => {
		Object.assign(Config.appStore, savedAppStore);
		setInjectedAppleRootCertificates(undefined);
	});

	describe('environment routing', () => {
		it('calls only the sandbox host for a sandbox transaction', async () => {
			seedSubscription(fake, 'sandbox', '200');
			const client = new AppStoreServerApiClient();
			const result = await client.getAllSubscriptionStatuses({
				environment: 'sandbox',
				bundleId: 'com.fluxer',
				transactionId: '200',
			});
			expect(result.environment).toBe('sandbox');
			expect(result.appAppleId).toBeNull();
			expect(result.groups[0].lastTransactions[0].transaction.environment).toBe('Sandbox');
			expect(fake.requests.map((request) => request.environment)).toEqual(['sandbox']);
		});

		it('tries production first and falls back to sandbox on a missing transaction', async () => {
			seedSubscription(fake, 'sandbox', '201');
			const client = new AppStoreServerApiClient();
			const result = await client.getTransactionInfo({environment: null, bundleId: 'com.fluxer', transactionId: '201'});
			expect(result.environment).toBe('sandbox');
			expect(fake.requests.map((request) => request.environment)).toEqual(['production', 'sandbox']);
		});

		it('stays on production when the transaction exists there', async () => {
			seedSubscription(fake, 'production', '202');
			const client = new AppStoreServerApiClient();
			const result = await client.getAllSubscriptionStatuses({
				environment: null,
				bundleId: 'com.fluxer',
				transactionId: '202',
			});
			expect(result.environment).toBe('production');
			expect(result.appAppleId).toBe(1234567890);
			expect(fake.requests.map((request) => request.environment)).toEqual(['production']);
		});

		it('does not fall back on other errors', async () => {
			seedSubscription(fake, 'sandbox', '203');
			fake.failNext({environment: 'production'}, {status: 500, errorCode: 5000001});
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getTransactionInfo({environment: null, bundleId: 'com.fluxer', transactionId: '203'}),
			);
			expect((error as AppStoreServerApiError).errorCode).toBe(5000001);
			expect(fake.requests.map((request) => request.environment)).toEqual(['production']);
		});

		it('reports a transaction missing from both environments', async () => {
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getTransactionInfo({environment: null, bundleId: 'com.fluxer', transactionId: '204'}),
			);
			expect(isAppStoreTransactionNotFound(error)).toBe(true);
			expect((error as AppStoreServerApiError).retryable).toBe(false);
			expect(fake.requests.map((request) => request.environment)).toEqual(['production', 'sandbox']);
		});

		it('never falls back when the environment is known', async () => {
			seedSubscription(fake, 'sandbox', '205');
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getTransactionInfo({environment: 'production', bundleId: 'com.fluxer', transactionId: '205'}),
			);
			expect(isAppStoreTransactionNotFound(error)).toBe(true);
			expect(fake.requests.map((request) => request.environment)).toEqual(['production']);
		});
	});

	describe('errors', () => {
		it.each([
			[4040002, 404],
			[4040004, 404],
			[4040006, 404],
			[5000001, 500],
		])('marks error code %i as retryable', async (errorCode, status) => {
			seedSubscription(fake, 'production', '300');
			fake.failNext({}, {status, errorCode});
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getAllSubscriptionStatuses({environment: 'production', bundleId: 'com.fluxer', transactionId: '300'}),
			);
			expect(error).toBeInstanceOf(AppStoreServerApiError);
			expect(error).toMatchObject({status, errorCode, retryable: true});
		});

		it.each([
			[4000006, 400],
			[4040001, 404],
			[4040005, 404],
			[5000000, 500],
		])('marks error code %i as final', async (errorCode, status) => {
			seedSubscription(fake, 'production', '301');
			fake.failNext({}, {status, errorCode});
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getAllSubscriptionStatuses({environment: 'production', bundleId: 'com.fluxer', transactionId: '301'}),
			);
			expect(error).toMatchObject({status, errorCode, retryable: false});
		});

		it('reads Retry-After as epoch milliseconds on a rate limit', async () => {
			seedSubscription(fake, 'production', '302');
			const retryAt = Date.parse('2026-09-29T12:00:00Z');
			fake.failNext({}, {status: 429, errorCode: 4290000, retryAfter: String(retryAt)});
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getTransactionInfo({environment: 'production', bundleId: 'com.fluxer', transactionId: '302'}),
			);
			expect(error).toMatchObject({status: 429, errorCode: 4290000, retryable: true});
			expect((error as AppStoreServerApiError).retryAfter?.getTime()).toBe(retryAt);
		});

		it('treats a server error without a body as retryable', async () => {
			seedSubscription(fake, 'production', '303');
			fake.failNext({}, {status: 503});
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getTransactionInfo({environment: 'production', bundleId: 'com.fluxer', transactionId: '303'}),
			);
			expect(error).toMatchObject({status: 503, errorCode: null, retryable: true});
		});

		it('treats a network failure as retryable', async () => {
			server.use(http.get(`${APP_STORE_SANDBOX_BASE_URL}/inApps/v1/transactions/:id`, () => HttpResponse.error()));
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getTransactionInfo({environment: 'sandbox', bundleId: 'com.fluxer', transactionId: '304'}),
			);
			expect(error).toMatchObject({status: null, retryable: true});
		});

		it('times out a slow request', async () => {
			server.use(
				http.get(`${APP_STORE_SANDBOX_BASE_URL}/inApps/v1/transactions/:id`, async () => {
					await delay(500);
					return HttpResponse.json({});
				}),
			);
			const client = new AppStoreServerApiClient({timeoutMs: 20});
			const error = await captureError(
				client.getTransactionInfo({environment: 'sandbox', bundleId: 'com.fluxer', transactionId: '305'}),
			);
			expect(error).toMatchObject({status: null, retryable: true});
		});

		it('rejects a response body of the wrong shape', async () => {
			server.use(
				http.get(`${APP_STORE_PRODUCTION_BASE_URL}/inApps/v1/subscriptions/:id`, () =>
					HttpResponse.json({data: 'nope'}),
				),
			);
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getAllSubscriptionStatuses({environment: 'production', bundleId: 'com.fluxer', transactionId: '306'}),
			);
			expect(error).toMatchObject({status: 200, retryable: false});
		});
	});

	describe('signed response data', () => {
		it('verifies every signed field in the statuses response', async () => {
			seedSubscription(fake, 'production', '400');
			fake.addTransaction('production', {
				transactionId: '401',
				originalTransactionId: '401',
				productId: 'com.fluxer.plutonium.yearly',
				expiresDate: Date.now() + ms('365 days'),
			});
			fake.putSubscription('production', {
				originalTransactionId: '401',
				status: 2,
				latestTransactionId: '401',
				customerId: '400',
				subscriptionGroupIdentifier: '21000001',
				renewalInfo: null,
			});
			fake.putSubscription('production', {
				originalTransactionId: '400',
				status: 1,
				latestTransactionId: '400',
				customerId: '400',
			});
			const client = new AppStoreServerApiClient();
			const result = await client.getAllSubscriptionStatuses({
				environment: 'production',
				bundleId: 'com.fluxer',
				transactionId: '400',
			});
			expect(result.groups).toHaveLength(2);
			const items = result.groups.flatMap((group) => group.lastTransactions);
			expect(items.map((item) => [item.originalTransactionId, item.status])).toEqual([
				['400', 1],
				['401', 2],
			]);
			expect(items[0].renewalInfo?.autoRenewStatus).toBe(1);
			expect(items[0].transaction.productId).toBe(MONTHLY);
			expect(items[1].renewalInfo).toBeNull();
		});

		it('rejects signed data from a chain that is not trusted', async () => {
			const other = createAppleTestPki();
			const untrusted = createAppStoreServerApiFake({sign: (payload) => other.signJws(payload)});
			server.use(...untrusted.handlers);
			seedSubscription(untrusted, 'production', '402');
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getAllSubscriptionStatuses({environment: 'production', bundleId: 'com.fluxer', transactionId: '402'}),
			);
			expect(error).toBeInstanceOf(AppStoreJwsVerificationError);
			expect((error as AppStoreJwsVerificationError).reason).toBe('untrusted_chain');
		});

		it('rejects signed data from the other environment', async () => {
			fake.addTransaction('production', {
				transactionId: '403',
				originalTransactionId: '403',
				productId: GIFT,
				type: 'Consumable',
				environment: 'Sandbox',
			});
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getTransactionInfo({environment: 'production', bundleId: 'com.fluxer', transactionId: '403'}),
			);
			expect((error as AppStoreJwsVerificationError).reason).toBe('invalid_environment');
		});

		it('rejects a transaction for another bundle', async () => {
			Config.appStore.apps = [...savedAppStore.apps, {bundleId: 'com.fluxer.beta', appAppleId: 1234567891}];
			fake.addTransaction('production', {
				transactionId: '404',
				originalTransactionId: '404',
				productId: GIFT,
				type: 'Consumable',
				bundleId: 'com.fluxer.beta',
			});
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.getTransactionInfo({environment: 'production', bundleId: 'com.fluxer', transactionId: '404'}),
			);
			expect(error).toMatchObject({status: 200, retryable: false});
		});
	});

	describe('app account token', () => {
		it('sets a lowercase token on the original transaction', async () => {
			seedSubscription(fake, 'production', '500');
			const client = new AppStoreServerApiClient();
			await client.setAppAccountToken({
				environment: 'production',
				bundleId: 'com.fluxer',
				originalTransactionId: '500',
				appAccountToken: '7E3FB20B-4CDB-47CC-936D-99D65F608138',
			});
			const put = fake.requests.find((request) => request.method === 'PUT');
			expect(put).toMatchObject({
				environment: 'production',
				path: '/inApps/v1/transactions/500/appAccountToken',
				body: {appAccountToken: '7e3fb20b-4cdb-47cc-936d-99d65f608138'},
				bundleId: 'com.fluxer',
			});
			const result = await client.getAllSubscriptionStatuses({
				environment: 'production',
				bundleId: 'com.fluxer',
				transactionId: '500',
			});
			const [item] = result.groups[0].lastTransactions;
			expect(item.transaction.appAccountToken).toBe('7e3fb20b-4cdb-47cc-936d-99d65f608138');
			expect(item.renewalInfo?.appAccountToken).toBe('7e3fb20b-4cdb-47cc-936d-99d65f608138');
		});

		it('surfaces the family sharing refusal as final', async () => {
			fake.addTransaction('production', {
				transactionId: '501',
				originalTransactionId: '501',
				productId: MONTHLY,
				inAppOwnershipType: 'FAMILY_SHARED',
			});
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.setAppAccountToken({
					environment: 'production',
					bundleId: 'com.fluxer',
					originalTransactionId: '501',
					appAccountToken: '7e3fb20b-4cdb-47cc-936d-99d65f608138',
				}),
			);
			expect(error).toMatchObject({status: 400, errorCode: 4000185, retryable: false});
		});

		it('surfaces a renewal transaction id as final', async () => {
			fake.addTransaction('production', {transactionId: '503', originalTransactionId: '502', productId: MONTHLY});
			const client = new AppStoreServerApiClient();
			const error = await captureError(
				client.setAppAccountToken({
					environment: 'production',
					bundleId: 'com.fluxer',
					originalTransactionId: '503',
					appAccountToken: '7e3fb20b-4cdb-47cc-936d-99d65f608138',
				}),
			);
			expect(error).toMatchObject({status: 400, errorCode: 4000187, retryable: false});
		});
	});
});
