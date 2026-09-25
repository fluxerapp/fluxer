// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {classifyWebPushOrigin, sameUserAgentFamily} from '@app/api/user/services/WebPushOriginReplacement';
import {listPushSubscriptions} from '@app/api/user/tests/UserTestUtils';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const LEGACY_ORIGIN = 'https://web.fluxer.app';
const TARGET_ORIGIN = 'https://fluxer.com';
const IPHONE_UA =
	'Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1';
const IPHONE_UPDATED_UA =
	'Mozilla/5.0 (iPhone; CPU iPhone OS 18_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.1 Mobile/15E148 Safari/604.1';
const DESKTOP_CHROME_UA =
	'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36';
const ANDROID_CHROME_UA =
	'Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Mobile Safari/537.36';

interface SubscribeOptions {
	userAgent?: string;
	installedApp?: boolean;
}

interface PushSubscribeResponse {
	subscription_id: string;
}

interface HandoffInitiateResponse {
	code: string;
	poll_secret: string;
}

interface HandoffStatusResponse {
	status: 'pending' | 'completed' | 'expired';
	token?: string;
}

describe('classifyWebPushOrigin', () => {
	it.each([
		{origin: 'https://web.fluxer.app', kind: 'legacy'},
		{origin: 'https://web.canary.fluxer.app', kind: 'legacy'},
		{origin: 'https://fluxer.com', kind: 'target'},
		{origin: 'https://canary.fluxer.com', kind: 'target'},
		{origin: 'https://fluxer.app', kind: null},
		{origin: 'https://example.com', kind: null},
		{origin: undefined, kind: null},
		{origin: null, kind: null},
	])('classifies $origin as $kind on the official instance', ({origin, kind}) => {
		expect(classifyWebPushOrigin(origin, false)).toBe(kind);
	});

	it('never classifies an origin on a self-hosted instance', () => {
		expect(classifyWebPushOrigin(LEGACY_ORIGIN, true)).toBeNull();
		expect(classifyWebPushOrigin(TARGET_ORIGIN, true)).toBeNull();
	});
});

describe('sameUserAgentFamily', () => {
	it('matches the same browser across version updates', () => {
		expect(sameUserAgentFamily(IPHONE_UA, IPHONE_UPDATED_UA)).toBe(true);
	});

	it('tells devices and browsers apart', () => {
		expect(sameUserAgentFamily(DESKTOP_CHROME_UA, ANDROID_CHROME_UA)).toBe(false);
		expect(sameUserAgentFamily(IPHONE_UA, DESKTOP_CHROME_UA)).toBe(false);
	});

	it('never matches a missing user agent', () => {
		expect(sameUserAgentFamily(null, null)).toBe(false);
		expect(sameUserAgentFamily(IPHONE_UA, undefined)).toBe(false);
	});
});

describe('web push origin replacement', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	afterEach(() => {
		vi.restoreAllMocks();
	});

	async function subscribeFrom(
		token: string,
		origin: string | null,
		endpoint: string,
		options: SubscribeOptions = {},
	): Promise<string> {
		const builder = createBuilder<PushSubscribeResponse>(harness, token).post('/users/@me/push/subscribe');
		if (origin) builder.header('Origin', origin);
		const response = await builder
			.body({
				endpoint,
				keys: {p256dh: 'test-p256dh-key', auth: 'test-auth-key'},
				user_agent: options.userAgent,
				installed_app: options.installedApp,
			})
			.execute();
		return response.subscription_id;
	}

	async function rotateFrom(
		token: string,
		origin: string,
		oldEndpoint: string,
		endpoint: string,
		installedApp?: boolean,
	): Promise<string> {
		const response = await createBuilder<PushSubscribeResponse>(harness, token)
			.post('/users/@me/push/rotate')
			.header('Origin', origin)
			.body({
				old_endpoint: oldEndpoint,
				endpoint,
				keys: {p256dh: 'test-p256dh-key', auth: 'test-auth-key'},
				installed_app: installedApp,
			})
			.execute();
		return response.subscription_id;
	}

	async function listSubscriptionIds(token: string): Promise<Array<string>> {
		const result = await listPushSubscriptions(harness, token);
		return result.subscriptions.map((subscription) => subscription.subscription_id).sort();
	}

	async function pairNewSession(
		approverToken: string,
		approverUserId: string,
		approverOrigin: string,
		initiatorOrigin: string | null = TARGET_ORIGIN,
	) {
		const initiate = createBuilderWithoutAuth<HandoffInitiateResponse>(harness).post('/auth/handoff/initiate');
		if (initiatorOrigin) initiate.header('Origin', initiatorOrigin);
		const initiated = await initiate.body(null).execute();
		await createBuilderWithoutAuth(harness).get(`/auth/handoff/${initiated.code}/info`).execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.header('Origin', approverOrigin)
			.body({code: initiated.code, token: approverToken, user_id: approverUserId})
			.expect(204)
			.execute();
		const completed = await createBuilderWithoutAuth<HandoffStatusResponse>(harness)
			.post(`/auth/handoff/${initiated.code}/status`)
			.body({poll_secret: initiated.poll_secret})
			.execute();
		expect(completed.status).toBe('completed');
		return completed.token!;
	}

	async function withSelfHosted(callback: () => Promise<void>): Promise<void> {
		const config = getConfig();
		const original = config.instance.selfHosted;
		try {
			config.instance.selfHosted = true;
			await callback();
		} finally {
			config.instance.selfHosted = original;
		}
	}

	it('replaces the legacy subscription of the same session when the new origin subscribes', async () => {
		const account = await createTestAccount(harness);
		await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy');
		const target = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target');
		expect(await listSubscriptionIds(account.token)).toEqual([target]);
	});

	it('turns a later legacy subscribe for the replaced session into a no-op', async () => {
		const account = await createTestAccount(harness);
		const target = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target');
		const legacy = await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy');
		expect(legacy).toMatch(/^[a-f0-9]{32}$/);
		expect(legacy).not.toBe(target);
		expect(await listSubscriptionIds(account.token)).toEqual([target]);
	});

	it('does not store a legacy rotation for a replaced session', async () => {
		const account = await createTestAccount(harness);
		await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy-old');
		const target = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target');
		await rotateFrom(
			account.token,
			LEGACY_ORIGIN,
			'https://push.example.com/legacy-old',
			'https://push.example.com/legacy-new',
		);
		expect(await listSubscriptionIds(account.token)).toEqual([target]);
	});

	it('keeps legacy subscriptions working until the new origin subscribes', async () => {
		const account = await createTestAccount(harness);
		const first = await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy-a');
		const second = await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy-b');
		expect(await listSubscriptionIds(account.token)).toEqual([first, second].sort());
	});

	it('leaves subscriptions from other sessions alone', async () => {
		const account = await createTestAccount(harness);
		const other = await loginAccount(harness, account);
		const otherLegacy = await subscribeFrom(other.token, LEGACY_ORIGIN, 'https://push.example.com/other-legacy');
		const target = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target');
		expect(await listSubscriptionIds(account.token)).toEqual([otherLegacy, target].sort());
	});

	it('never removes another new-origin subscription of the same session', async () => {
		const account = await createTestAccount(harness);
		const first = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target-a');
		const second = await subscribeFrom(account.token, 'https://canary.fluxer.com', 'https://push.example.com/target-b');
		expect(await listSubscriptionIds(account.token)).toEqual([first, second].sort());
	});

	it('treats unclassified rows as legacy without ever skipping an unclassified subscribe', async () => {
		const account = await createTestAccount(harness);
		const unknown = await subscribeFrom(account.token, null, 'https://push.example.com/no-origin');
		const target = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target');
		expect(await listSubscriptionIds(account.token)).toEqual([target]);
		const legacyAfter = await subscribeFrom(account.token, null, 'https://push.example.com/no-origin');
		expect(legacyAfter).toBe(unknown);
		expect(await listSubscriptionIds(account.token)).toEqual([unknown, target].sort());
	});

	it('replaces the approving legacy session on the same device once a paired session subscribes', async () => {
		const account = await createTestAccount(harness);
		const approver = await loginAccount(harness, account);
		const approverLegacy = 'https://push.example.com/approver-legacy';
		await subscribeFrom(approver.token, LEGACY_ORIGIN, approverLegacy, {userAgent: IPHONE_UA, installedApp: true});
		const pairedToken = await pairNewSession(approver.token, approver.userId, LEGACY_ORIGIN);
		const paired = await subscribeFrom(pairedToken, TARGET_ORIGIN, 'https://push.example.com/paired', {
			userAgent: IPHONE_UPDATED_UA,
			installedApp: true,
		});
		expect(await listSubscriptionIds(approver.token)).toEqual([paired]);
	});

	it('never silences the approving session for good', async () => {
		const account = await createTestAccount(harness);
		const approver = await loginAccount(harness, account);
		const approverLegacy = 'https://push.example.com/approver-legacy';
		await subscribeFrom(approver.token, LEGACY_ORIGIN, approverLegacy, {userAgent: IPHONE_UA});
		const pairedToken = await pairNewSession(approver.token, approver.userId, LEGACY_ORIGIN);
		const paired = await subscribeFrom(pairedToken, TARGET_ORIGIN, 'https://push.example.com/paired', {
			userAgent: IPHONE_UA,
		});
		const restored = await subscribeFrom(approver.token, LEGACY_ORIGIN, approverLegacy, {userAgent: IPHONE_UA});
		expect(await listSubscriptionIds(approver.token)).toEqual([paired, restored].sort());
	});

	it('leaves the approving session alone when it runs on another device', async () => {
		const account = await createTestAccount(harness);
		const approver = await loginAccount(harness, account);
		const approverLegacy = await subscribeFrom(approver.token, LEGACY_ORIGIN, 'https://push.example.com/desktop', {
			userAgent: DESKTOP_CHROME_UA,
			installedApp: true,
		});
		const pairedToken = await pairNewSession(approver.token, approver.userId, LEGACY_ORIGIN);
		const paired = await subscribeFrom(pairedToken, TARGET_ORIGIN, 'https://push.example.com/phone', {
			userAgent: ANDROID_CHROME_UA,
			installedApp: true,
		});
		expect(await listSubscriptionIds(approver.token)).toEqual([approverLegacy, paired].sort());
		const desktopAgain = await subscribeFrom(approver.token, LEGACY_ORIGIN, 'https://push.example.com/desktop', {
			userAgent: DESKTOP_CHROME_UA,
			installedApp: true,
		});
		expect(desktopAgain).toBe(approverLegacy);
		expect(await listSubscriptionIds(approver.token)).toEqual([approverLegacy, paired].sort());
	});

	it.each([
		{label: 'the approval came from the new origin', approverOrigin: TARGET_ORIGIN, initiatorOrigin: TARGET_ORIGIN},
		{label: 'the new session did not start on the new origin', approverOrigin: LEGACY_ORIGIN, initiatorOrigin: null},
		{
			label: 'the new session started on the old origin',
			approverOrigin: LEGACY_ORIGIN,
			initiatorOrigin: LEGACY_ORIGIN,
		},
	])('does not link sessions when $label', async ({approverOrigin, initiatorOrigin}) => {
		const account = await createTestAccount(harness);
		const approver = await loginAccount(harness, account);
		const approverSubscription = await subscribeFrom(
			approver.token,
			LEGACY_ORIGIN,
			'https://push.example.com/approver-legacy',
			{userAgent: IPHONE_UA},
		);
		const pairedToken = await pairNewSession(approver.token, approver.userId, approverOrigin, initiatorOrigin);
		const paired = await subscribeFrom(pairedToken, TARGET_ORIGIN, 'https://push.example.com/paired', {
			userAgent: IPHONE_UA,
		});
		expect(await listSubscriptionIds(approver.token)).toEqual([approverSubscription, paired].sort());
	});

	it('completes the approval when the predecessor link cannot be written', async () => {
		const account = await createTestAccount(harness);
		const approver = await loginAccount(harness, account);
		const setex = harness.kvProvider.setex.bind(harness.kvProvider);
		vi.spyOn(harness.kvProvider, 'setex').mockImplementation(async (key, ttl, value) => {
			if (key.startsWith('push_session_predecessor:')) throw new Error('kv down');
			return setex(key, ttl, value);
		});
		const pairedToken = await pairNewSession(approver.token, approver.userId, LEGACY_ORIGIN);
		expect(pairedToken).toBeTruthy();
	});

	it('stores a subscribe when the replacement marker cannot be read or written', async () => {
		const account = await createTestAccount(harness);
		const get = harness.kvProvider.get.bind(harness.kvProvider);
		vi.spyOn(harness.kvProvider, 'get').mockImplementation(async (key) => {
			if (key.startsWith('push_origin_replaced:')) throw new Error('kv down');
			return get(key);
		});
		const target = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target');
		const legacy = await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy');
		expect(await listSubscriptionIds(account.token)).toEqual([legacy, target].sort());
	});

	it('keeps an installed legacy app subscribed when only a browser tab moved', async () => {
		const account = await createTestAccount(harness);
		const installed = await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy-app', {
			userAgent: DESKTOP_CHROME_UA,
			installedApp: true,
		});
		const target = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target-tab', {
			userAgent: DESKTOP_CHROME_UA,
		});
		expect(await listSubscriptionIds(account.token)).toEqual([installed, target].sort());
		const rotated = await rotateFrom(
			account.token,
			LEGACY_ORIGIN,
			'https://push.example.com/legacy-app',
			'https://push.example.com/legacy-app-2',
			true,
		);
		expect(await listSubscriptionIds(account.token)).toEqual([rotated, target].sort());
		await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy-tab', {
			userAgent: DESKTOP_CHROME_UA,
		});
		expect(await listSubscriptionIds(account.token)).toEqual([rotated, target].sort());
	});

	it('replaces an installed legacy app once the new app is installed', async () => {
		const account = await createTestAccount(harness);
		await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy-app', {
			userAgent: DESKTOP_CHROME_UA,
			installedApp: true,
		});
		await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target-tab', {
			userAgent: DESKTOP_CHROME_UA,
		});
		const targetApp = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target-app', {
			userAgent: DESKTOP_CHROME_UA,
			installedApp: true,
		});
		const ids = await listSubscriptionIds(account.token);
		expect(ids).toContain(targetApp);
		expect(ids).toHaveLength(2);
		await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy-app', {
			userAgent: DESKTOP_CHROME_UA,
			installedApp: true,
		});
		expect(await listSubscriptionIds(account.token)).toEqual(ids);
	});

	it('does nothing new on a self-hosted instance', async () => {
		await withSelfHosted(async () => {
			const account = await createTestAccount(harness);
			const legacy = await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy');
			const target = await subscribeFrom(account.token, TARGET_ORIGIN, 'https://push.example.com/target');
			const legacyAgain = await subscribeFrom(account.token, LEGACY_ORIGIN, 'https://push.example.com/legacy-2');
			expect(await listSubscriptionIds(account.token)).toEqual([legacy, target, legacyAgain].sort());
		});
	});
});
