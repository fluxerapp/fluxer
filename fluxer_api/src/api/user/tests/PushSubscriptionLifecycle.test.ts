// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createSessionFromLogin,
	createTestAccount,
	loginAccount,
	logoutSpecificSessions,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import type {PushSubscription} from '@app/api/models/PushSubscription';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {PushSubscriptionRepository} from '@app/api/user/repositories/PushSubscriptionRepository';
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

async function findStoredSubscription(userId: string, subscriptionId: string): Promise<PushSubscription> {
	const subscriptions = await new PushSubscriptionRepository().listPushSubscriptions(createUserID(BigInt(userId)));
	const subscription = subscriptions.find((entry) => entry.subscriptionId === subscriptionId);
	if (!subscription) {
		throw new Error(`Stored push subscription ${subscriptionId} not found`);
	}
	return subscription;
}

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
	test('APNs Web Push registration stores the endpoint and encryption keys', async () => {
		const account = await createTestAccount(harness);
		const endpoint = 'https://relay.example.com/apns/device-1';
		const registered = await registerMobileDevice(harness, account.token, {
			platform: 'ios_apns',
			token: endpoint,
			encryption_key: 'relay-p256dh-key',
			auth_secret: 'relay-auth-secret',
			app_id: 'stable',
		});
		const subscription = await findStoredSubscription(account.userId, registered.device_id);
		expect(subscription.platform).toBe('ios_apns');
		expect(subscription.endpoint).toBe(endpoint);
		expect(subscription.p256dhKey).toBe('relay-p256dh-key');
		expect(subscription.authKey).toBe('relay-auth-secret');
	});
	test('FCM Web Push registration stores the endpoint and encryption keys', async () => {
		const account = await createTestAccount(harness);
		const endpoint = 'https://relay.example.com/fcm/device-1';
		const registered = await registerMobileDevice(harness, account.token, {
			platform: 'android_fcm',
			token: endpoint,
			encryption_key: 'fcm-relay-p256dh-key',
			auth_secret: 'fcm-relay-auth-secret',
		});
		const subscription = await findStoredSubscription(account.userId, registered.device_id);
		expect(subscription.platform).toBe('android_fcm');
		expect(subscription.endpoint).toBe(endpoint);
		expect(subscription.p256dhKey).toBe('fcm-relay-p256dh-key');
		expect(subscription.authKey).toBe('fcm-relay-auth-secret');
	});
	test('raw token registration stores the token without encryption keys', async () => {
		const account = await createTestAccount(harness);
		const registered = await registerMobileDevice(harness, account.token, {
			platform: 'ios_apns',
			token: '0123456789abcdef',
			provider_environment: 'production',
		});
		const subscription = await findStoredSubscription(account.userId, registered.device_id);
		expect(subscription.platform).toBe('ios_apns');
		expect(subscription.endpoint).toBe('0123456789abcdef');
		expect(subscription.p256dhKey).toBeNull();
		expect(subscription.authKey).toBeNull();
	});
	test('Web Push and raw token registrations coexist for one platform', async () => {
		const account = await createTestAccount(harness);
		const rawDevice = await registerMobileDevice(harness, account.token, {
			platform: 'android_fcm',
			token: 'fcm-legacy-token',
		});
		const webPushDevice = await registerMobileDevice(harness, account.token, {
			platform: 'android_fcm',
			token: 'https://relay.example.com/fcm/device-2',
			encryption_key: 'coexist-p256dh-key',
			auth_secret: 'coexist-auth-secret',
		});
		expect(rawDevice.device_id).not.toBe(webPushDevice.device_id);
		const rawSubscription = await findStoredSubscription(account.userId, rawDevice.device_id);
		const webPushSubscription = await findStoredSubscription(account.userId, webPushDevice.device_id);
		expect(rawSubscription.p256dhKey).toBeNull();
		expect(rawSubscription.authKey).toBeNull();
		expect(webPushSubscription.p256dhKey).toBe('coexist-p256dh-key');
		expect(webPushSubscription.authKey).toBe('coexist-auth-secret');
		const mobileDevices = await listMobileDevices(harness, account.token);
		expect(mobileDevices.devices).toHaveLength(2);
	});
	test('endpoint registration without encryption keys is rejected', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/mobile-devices')
			.body({
				platform: 'ios_apns',
				token: 'https://relay.example.com/apns/no-keys',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('endpoint registration with only one encryption key is rejected', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/mobile-devices')
			.body({
				platform: 'android_fcm',
				token: 'https://relay.example.com/fcm/half-keys',
				encryption_key: 'half-p256dh-key',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('raw token registration with encryption keys is rejected', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/users/@me/mobile-devices')
			.body({
				platform: 'ios_apns',
				token: '0123456789abcdef',
				encryption_key: 'raw-p256dh-key',
				auth_secret: 'raw-auth-secret',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('Web Push registration rejects an endpoint that is not publicly routable', async () => {
		const account = await createTestAccount(harness);
		const response = await createBuilder(harness, account.token)
			.post('/users/@me/mobile-devices')
			.body({
				platform: 'ios_apns',
				token: 'https://127.0.0.1/apns/device',
				encryption_key: 'local-p256dh-key',
				auth_secret: 'local-auth-secret',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(JSON.stringify(response)).toContain('URL_NOT_PUBLICLY_ROUTABLE');
	});
	test('unregister removes a Web Push mobile registration', async () => {
		const account = await createTestAccount(harness);
		const endpoint = 'https://relay.example.com/apns/unregister';
		await registerMobileDevice(harness, account.token, {
			platform: 'ios_apns',
			token: endpoint,
			encryption_key: 'unregister-p256dh-key',
			auth_secret: 'unregister-auth-secret',
			app_id: 'stable',
			provider_environment: 'production',
		});
		await unregisterMobileDevice(harness, account.token, {
			platform: 'ios_apns',
			token: endpoint,
			app_id: 'stable',
			provider_environment: 'production',
		});
		const mobileDevices = await listMobileDevices(harness, account.token);
		expect(mobileDevices.devices).toHaveLength(0);
	});
	test('mobile Web Push registrations stay out of the web push subscription list', async () => {
		const account = await createTestAccount(harness);
		await registerMobileDevice(harness, account.token, {
			platform: 'android_fcm',
			token: 'https://relay.example.com/fcm/separate',
			encryption_key: 'separate-p256dh-key',
			auth_secret: 'separate-auth-secret',
		});
		const webSubscriptions = await listPushSubscriptions(harness, account.token);
		const mobileDevices = await listMobileDevices(harness, account.token);
		expect(webSubscriptions.subscriptions).toHaveLength(0);
		expect(mobileDevices.devices).toHaveLength(1);
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
