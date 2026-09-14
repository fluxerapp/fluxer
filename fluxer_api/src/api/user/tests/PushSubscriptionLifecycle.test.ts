// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createSessionFromLogin,
	createTestAccount,
	loginAccount,
	logoutSpecificSessions,
} from '@app/api/auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	deleteMobileDevice,
	deletePushSubscription,
	listMobileDevices,
	listPushSubscriptions,
	registerMobileDevice,
	subscribePush,
	unregisterMobileDevice,
} from '@app/api/user/tests/UserTestUtils';
import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {beforeEach, describe, expect, test} from 'vitest';

describe('Push Subscription Lifecycle', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('subscribe returns a 32-character hex subscription id', async () => {
		const account = await createTestAccount(harness);
		const result = await subscribePush(harness, account.token, 'https://push.example.com/endpoint-1');
		expect(result.subscription_id).toBeDefined();
		expect(result.subscription_id).toMatch(/^[a-f0-9]{32}$/);
	});
	test('same endpoint produces the same subscription id', async () => {
		const account = await createTestAccount(harness);
		const endpoint = 'https://push.example.com/deterministic';
		const first = await subscribePush(harness, account.token, endpoint);
		const second = await subscribePush(harness, account.token, endpoint);
		expect(first.subscription_id).toBe(second.subscription_id);
	});
	test('different endpoints produce different subscription ids', async () => {
		const account = await createTestAccount(harness);
		const first = await subscribePush(harness, account.token, 'https://push.example.com/endpoint-a');
		const second = await subscribePush(harness, account.token, 'https://push.example.com/endpoint-b');
		expect(first.subscription_id).not.toBe(second.subscription_id);
	});
	test('list subscriptions returns empty array initially', async () => {
		const account = await createTestAccount(harness);
		const result = await listPushSubscriptions(harness, account.token);
		expect(result.subscriptions).toEqual([]);
	});
	test('list subscriptions returns registered subscriptions', async () => {
		const account = await createTestAccount(harness);
		const endpoint = 'https://push.example.com/list-test';
		const userAgent = 'TestBrowser/1.0';
		const subscribed = await subscribePush(harness, account.token, endpoint, {userAgent});
		const result = await listPushSubscriptions(harness, account.token);
		expect(result.subscriptions).toHaveLength(1);
		expect(result.subscriptions[0].subscription_id).toBe(subscribed.subscription_id);
		expect(result.subscriptions[0].user_agent).toBe(userAgent);
	});
	test('register mobile FCM device returns a 32-character hex device id', async () => {
		const account = await createTestAccount(harness);
		const registered = await registerMobileDevice(harness, account.token, {
			platform: 'android_fcm',
			token: 'fcm-token-1',
			app_id: 'stable',
			user_agent: 'FluxerAndroid/1.0',
		});
		expect(registered.device_id).toMatch(/^[a-f0-9]{32}$/);
	});
	test('mobile devices are listed separately from web push subscriptions', async () => {
		const account = await createTestAccount(harness);
		await subscribePush(harness, account.token, 'https://push.example.com/web-only');
		const registered = await registerMobileDevice(harness, account.token, {
			platform: 'ios_apns',
			token: '0123456789abcdef',
			app_id: 'canary',
			provider_environment: 'development',
			user_agent: 'FluxeriOS/1.0',
		});
		const webSubscriptions = await listPushSubscriptions(harness, account.token);
		const mobileDevices = await listMobileDevices(harness, account.token);
		expect(webSubscriptions.subscriptions).toHaveLength(1);
		expect(mobileDevices.devices).toEqual([
			{
				device_id: registered.device_id,
				platform: 'ios_apns',
				app_id: 'canary',
				provider_environment: 'development',
				user_agent: 'FluxeriOS/1.0',
			},
		]);
	});
	test('delete mobile device removes it from the mobile list', async () => {
		const account = await createTestAccount(harness);
		const registered = await registerMobileDevice(harness, account.token, {
			platform: 'android_fcm',
			token: 'fcm-delete-token',
		});
		await deleteMobileDevice(harness, account.token, registered.device_id);
		const mobileDevices = await listMobileDevices(harness, account.token);
		expect(mobileDevices.devices).toHaveLength(0);
	});
	test('unregister mobile device removes deterministic registration', async () => {
		const account = await createTestAccount(harness);
		await registerMobileDevice(harness, account.token, {
			platform: 'ios_apns',
			token: 'apns-unregister-token',
			app_id: 'canary',
			provider_environment: 'production',
		});
		await unregisterMobileDevice(harness, account.token, {
			platform: 'ios_apns',
			token: 'apns-unregister-token',
			app_id: 'canary',
			provider_environment: 'production',
		});
		const mobileDevices = await listMobileDevices(harness, account.token);
		expect(mobileDevices.devices).toHaveLength(0);
	});
	test('UnifiedPush registration requires Web Push encryption keys', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/mobile-devices')
			.body({
				platform: 'android_unified_push',
				token: 'https://unifiedpush.example.com/device',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('UnifiedPush registration accepts endpoint and encryption keys', async () => {
		const account = await createTestAccount(harness);
		const registered = await registerMobileDevice(harness, account.token, {
			platform: 'android_unified_push',
			token: 'https://unifiedpush.example.com/device-with-keys',
			encryption_key: 'test-p256dh-key',
			auth_secret: 'test-auth-secret',
			app_id: 'stable',
		});
		const mobileDevices = await listMobileDevices(harness, account.token);
		expect(mobileDevices.devices[0].device_id).toBe(registered.device_id);
		expect(mobileDevices.devices[0].platform).toBe('android_unified_push');
	});
	test('list subscriptions returns multiple subscriptions', async () => {
		const account = await createTestAccount(harness);
		const first = await subscribePush(harness, account.token, 'https://push.example.com/multi-1');
		const second = await subscribePush(harness, account.token, 'https://push.example.com/multi-2');
		const result = await listPushSubscriptions(harness, account.token);
		expect(result.subscriptions).toHaveLength(2);
		const ids = result.subscriptions.map((s: {subscription_id: string}) => s.subscription_id).sort();
		expect(ids).toContain(first.subscription_id);
		expect(ids).toContain(second.subscription_id);
	});
	test('subscription without user_agent returns null', async () => {
		const account = await createTestAccount(harness);
		await subscribePush(harness, account.token, 'https://push.example.com/no-ua');
		const result = await listPushSubscriptions(harness, account.token);
		expect(result.subscriptions).toHaveLength(1);
		expect(result.subscriptions[0].user_agent).toBeNull();
	});
	test('delete subscription removes it from the list', async () => {
		const account = await createTestAccount(harness);
		const subscribed = await subscribePush(harness, account.token, 'https://push.example.com/delete-test');
		await deletePushSubscription(harness, account.token, subscribed.subscription_id);
		const result = await listPushSubscriptions(harness, account.token);
		expect(result.subscriptions).toHaveLength(0);
	});
	test('delete one subscription does not affect others', async () => {
		const account = await createTestAccount(harness);
		const first = await subscribePush(harness, account.token, 'https://push.example.com/keep');
		const second = await subscribePush(harness, account.token, 'https://push.example.com/remove');
		await deletePushSubscription(harness, account.token, second.subscription_id);
		const result = await listPushSubscriptions(harness, account.token);
		expect(result.subscriptions).toHaveLength(1);
		expect(result.subscriptions[0].subscription_id).toBe(first.subscription_id);
	});
	test('logout removes push subscriptions registered by the current session', async () => {
		let account = await createTestAccount(harness);
		await subscribePush(harness, account.token, 'https://push.example.com/logout-current');
		await createBuilder(harness, account.token).post('/auth/logout').expect(204).execute();
		account = await loginAccount(harness, account);
		const result = await listPushSubscriptions(harness, account.token);
		expect(result.subscriptions).toHaveLength(0);
	});
	test('logging out another session removes that session push subscription only', async () => {
		const account = await createTestAccount(harness);
		const sessionsBefore = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		const knownSessionHashes = new Set(sessionsBefore.map((session) => session.id_hash));
		const otherToken = await createSessionFromLogin(harness, account);
		const sessionsAfterLogin = await createBuilder<Array<AuthSessionResponse>>(harness, account.token)
			.get('/auth/sessions')
			.execute();
		const otherSession = sessionsAfterLogin.find((session) => !knownSessionHashes.has(session.id_hash));
		expect(otherSession).toBeDefined();
		const kept = await subscribePush(harness, account.token, 'https://push.example.com/session-kept');
		const removed = await subscribePush(harness, otherToken, 'https://push.example.com/session-removed');
		await logoutSpecificSessions(harness, account.token, [otherSession!.id_hash], account.password);
		const result = await listPushSubscriptions(harness, account.token);
		const ids = result.subscriptions.map((subscription) => subscription.subscription_id);
		expect(ids).toEqual([kept.subscription_id]);
		expect(ids).not.toContain(removed.subscription_id);
	});
	test('subscriptions are isolated between users', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);
		await subscribePush(harness, alice.token, 'https://push.example.com/alice');
		await subscribePush(harness, bob.token, 'https://push.example.com/bob');
		const aliceSubs = await listPushSubscriptions(harness, alice.token);
		const bobSubs = await listPushSubscriptions(harness, bob.token);
		expect(aliceSubs.subscriptions).toHaveLength(1);
		expect(bobSubs.subscriptions).toHaveLength(1);
		expect(aliceSubs.subscriptions[0].subscription_id).not.toBe(bobSubs.subscriptions[0].subscription_id);
	});
	test('subscribe rejects invalid endpoint', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/push/subscribe')
			.body({
				endpoint: 'not-a-url',
				keys: {p256dh: 'key', auth: 'auth'},
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('subscribe rejects missing keys', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/push/subscribe')
			.body({
				endpoint: 'https://push.example.com/missing-keys',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('subscribe requires authentication', async () => {
		await createBuilder(harness, '')
			.post('/users/@me/push/subscribe')
			.body({
				endpoint: 'https://push.example.com/no-auth',
				keys: {p256dh: 'key', auth: 'auth'},
			})
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
	test('list subscriptions requires authentication', async () => {
		await createBuilder(harness, '').get('/users/@me/push/subscriptions').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
	test('delete subscription requires authentication', async () => {
		await createBuilder(harness, '')
			.delete('/users/@me/push/subscriptions/abc123')
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
});
