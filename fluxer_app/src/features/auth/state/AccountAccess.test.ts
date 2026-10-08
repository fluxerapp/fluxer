// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	HARNESS_API_ENDPOINT,
	installHarnessBootstrap,
	resetScriptedTransport,
	ScriptedXMLHttpRequest,
	scriptedReplies,
	scriptedRequestTokens,
	scriptedRequestUrls,
	serverErrorReply,
	unauthorizedReply,
	userMeReply,
} from '@app/features/auth/state/__fixtures__/AccountSwitchHarness';
import {instanceDiscoveryFixture} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import type {MessageDescriptor} from '@lingui/core';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));

vi.mock('@app/features/gateway/transport/GatewayConnection', () => ({
	default: {
		startSession: () => undefined,
		logout: () => undefined,
		sendInvisiblePresenceForCurrentSession: () => undefined,
	},
}));

vi.mock('@app/features/presence/state/LocalPresence', () => ({
	default: {captureIntent: () => null, restoreIntent: () => undefined},
}));

vi.mock('@app/features/ui/state/LayerManager', () => ({default: {closeAll: () => undefined}}));

vi.mock('@app/features/user/state/UserSettings', () => ({
	default: {handleAccountTransition: () => undefined},
}));

installHarnessBootstrap();

const {AccountAccess, AccountAccessDecision, AccountAccessPhase} = await import(
	'@app/features/auth/state/AccountAccess'
);
const {default: accountStorage} = await import('@app/features/auth/state/AccountStorage');
const {default: RuntimeConfig} = await import('@app/features/app/state/RuntimeConfig');
const {default: SessionManager} = await import('@app/features/platform/state/AuthSession');
const {default: AppStorage} = await import('@app/features/platform/state/PersistentStorage');

const ACCOUNT = {userId: '300', token: 'token-c'};
const ACCOUNT_KEY = `${HARNESS_API_ENDPOINT}::${ACCOUNT.userId}`;
const FOREIGN_ACCOUNT_KEY = `https://foreign.test/api::${ACCOUNT.userId}`;
const RUNTIME = RuntimeConfig.getSnapshot();

function createAccess() {
	return new AccountAccess();
}

beforeEach(async () => {
	vi.stubGlobal('XMLHttpRequest', ScriptedXMLHttpRequest);
	vi.stubGlobal(
		'fetch',
		vi.fn(
			async () =>
				new Response(JSON.stringify(instanceDiscoveryFixture(HARNESS_API_ENDPOINT)), {
					status: 200,
					headers: {'content-type': 'application/json'},
				}),
		),
	);
	await SessionManager.reset();
	AppStorage.clear();
	await accountStorage.deleteAccount(ACCOUNT_KEY);
	await SessionManager.initialize();
	scriptedReplies.push(userMeReply(ACCOUNT.userId));
	await SessionManager.login({token: ACCOUNT.token, userId: ACCOUNT.userId, runtimeSnapshot: RUNTIME});
	resetScriptedTransport();
});

afterEach(() => {
	vi.unstubAllGlobals();
	vi.useRealTimers();
	resetScriptedTransport();
});

describe('AccountAccess phase machine', () => {
	test('a healthy account is allowed and may start a gateway', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.ALLOWED);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.ALLOWED);
		expect(scriptedRequestTokens).toEqual([ACCOUNT.token]);
		expect(scriptedRequestUrls[0]?.startsWith(HARNESS_API_ENDPOINT)).toBe(true);
		expect(scriptedRequestUrls[0]?.endsWith('/users/@me')).toBe(true);
	});

	test('a required action blocks the gateway and is reported as action required', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId, ['REQUIRE_VERIFIED_EMAIL']));
		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.ACTION_REQUIRED);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.ACTION_REQUIRED);
	});

	test('a server that omits required_actions entirely is treated as allowed', async () => {
		const access = createAccess();
		scriptedReplies.push({status: 200, body: {id: ACCOUNT.userId, username: 'legacy'}});
		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.ALLOWED);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.ALLOWED);
	});

	test('an unauthorized response invalidates the account and forgets the record', async () => {
		const access = createAccess();
		scriptedReplies.push(unauthorizedReply());
		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.INVALID);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.UNKNOWN);
		expect(SessionManager.accounts[0]?.isValid).toBe(false);
	});

	test('a mismatched user id invalidates the stored account identity', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply('999'));
		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.INVALID);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.UNKNOWN);
		expect(SessionManager.accounts[0]?.isValid).toBe(false);
	});

	test('an unreachable instance backs off exponentially instead of retrying immediately', async () => {
		vi.useFakeTimers();
		const access = createAccess();
		scriptedReplies.push(serverErrorReply(), serverErrorReply(), serverErrorReply());

		await access.ensureAccountChecked(ACCOUNT_KEY, {force: true, resetFailures: false});
		expect(access.getRetryEligibleAt(ACCOUNT_KEY) - Date.now()).toBe(10_000);

		await access.ensureAccountChecked(ACCOUNT_KEY, {force: true, resetFailures: false});
		expect(access.getRetryEligibleAt(ACCOUNT_KEY) - Date.now()).toBe(20_000);

		await access.ensureAccountChecked(ACCOUNT_KEY, {force: true, resetFailures: false});
		expect(access.getRetryEligibleAt(ACCOUNT_KEY) - Date.now()).toBe(40_000);

		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.UNAVAILABLE);
	});

	test('a repeated check reuses the decision recorded for the same token', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		await access.ensureAccountChecked(ACCOUNT_KEY);
		expect(scriptedRequestTokens).toHaveLength(1);
	});

	test('concurrent checks for one token share a single request', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		const [first, second] = await Promise.all([
			access.ensureAccountChecked(ACCOUNT_KEY),
			access.ensureAccountChecked(ACCOUNT_KEY),
		]);
		expect(first).toBe(AccountAccessDecision.ALLOWED);
		expect(second).toBe(AccountAccessDecision.ALLOWED);
		expect(scriptedRequestTokens).toHaveLength(1);
	});

	test('a re-login under a new token discards the previous decision', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		await SessionManager.login({token: 'rotated-token', userId: ACCOUNT.userId, runtimeSnapshot: RUNTIME});
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		expect(scriptedRequestTokens).toEqual([ACCOUNT.token, 'rotated-token']);
	});

	test('a gateway ready signal clears a recovered restriction', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId, ['REQUIRE_VERIFIED_EMAIL']));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY, {force: true});
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.GATEWAY_STARTING);
		access.markGatewayReady(ACCOUNT_KEY);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.ALLOWED);
	});

	test('a gateway ready signal clears an unavailable record, so unavailable is not terminal', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		access.markGatewayUnavailable(ACCOUNT_KEY, new Error('gateway ready timed out'));
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.UNAVAILABLE);
		access.markGatewayReady(ACCOUNT_KEY);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.ALLOWED);
		expect(access.getUnavailabilityCause(ACCOUNT_KEY).name).toBe('AccountAccessUnrecordedUnavailabilityError');
		expect(access.getCheckedUser(ACCOUNT_KEY)).not.toBeNull();
	});

	test('a gateway ready timeout never downgrades a restricted account out of its required action', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId, ['REQUIRE_VERIFIED_EMAIL']));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		const retryEligibleAt = access.getRetryEligibleAt(ACCOUNT_KEY);

		access.markGatewayUnavailable(ACCOUNT_KEY, new Error('gateway ready timed out'));

		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.ACTION_REQUIRED);
		expect(access.getRetryEligibleAt(ACCOUNT_KEY)).toBe(retryEligibleAt);

		access.markGatewayReady(ACCOUNT_KEY);

		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.ACTION_REQUIRED);
		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.ACTION_REQUIRED);
	});

	test('a gateway ready signal on a never-checked account forces a real check instead of pinning it allowed', async () => {
		const access = createAccess();
		access.markGatewayUnavailable(ACCOUNT_KEY, new Error('gateway ready timed out'));
		access.markGatewayReady(ACCOUNT_KEY);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.UNKNOWN);
		expect(access.getCheckedUser(ACCOUNT_KEY)).toBeNull();

		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.ALLOWED);
		expect(scriptedRequestTokens).toEqual([ACCOUNT.token]);
		expect(access.getCheckedUser(ACCOUNT_KEY)).not.toBeNull();
	});

	test('an unavailable decision is reused only until its own retry deadline passes', async () => {
		vi.useFakeTimers();
		const access = createAccess();
		access.markGatewayUnavailable(ACCOUNT_KEY, new Error('gateway refused the session'));
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.UNAVAILABLE);

		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.UNAVAILABLE);
		expect(scriptedRequestTokens).toHaveLength(0);

		vi.advanceTimersByTime(access.getRetryEligibleAt(ACCOUNT_KEY) - Date.now() + 1);
		scriptedReplies.push(userMeReply(ACCOUNT.userId));

		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.ALLOWED);
		expect(scriptedRequestTokens).toEqual([ACCOUNT.token]);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.ALLOWED);
	});

	test('a stale unavailable decision keeps its backoff instead of restarting it', async () => {
		vi.useFakeTimers();
		const access = createAccess();
		access.markGatewayUnavailable(ACCOUNT_KEY, new Error('gateway refused the session'));
		const firstRetryAt = access.getRetryEligibleAt(ACCOUNT_KEY);

		vi.advanceTimersByTime(firstRetryAt - Date.now() + 1);
		scriptedReplies.push(serverErrorReply());
		await expect(access.ensureAccountChecked(ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.UNAVAILABLE);

		expect(access.getRetryEligibleAt(ACCOUNT_KEY) - Date.now()).toBeGreaterThan(firstRetryAt - Date.now());
	});

	test('forgetting an account drops its record', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		access.forgetAccount(ACCOUNT_KEY);
		expect(access.getPhase(ACCOUNT_KEY)).toBe(AccountAccessPhase.UNKNOWN);
	});
});

describe('AccountAccess account isolation', () => {
	test('a key that names no stored account is never answered by the active account', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));

		await expect(access.ensureAccountChecked(FOREIGN_ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.INVALID);

		expect(scriptedRequestTokens).toEqual([]);
		expect(access.getPhase(FOREIGN_ACCOUNT_KEY)).toBe(AccountAccessPhase.UNKNOWN);
		expect(access.getCheckedUser(FOREIGN_ACCOUNT_KEY)).toBeNull();
	});

	test('a key that names no stored account is refused even after the active account was checked', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		resetScriptedTransport();

		await expect(access.ensureAccountChecked(FOREIGN_ACCOUNT_KEY)).resolves.toBe(AccountAccessDecision.INVALID);

		expect(scriptedRequestTokens).toEqual([]);
		expect(access.getCheckedUser(FOREIGN_ACCOUNT_KEY)).toBeNull();
	});

	test('a reply that lands after the token rotated is discarded instead of recorded', async () => {
		let releaseReply = (): void => undefined;
		let markRequestSent = (): void => undefined;
		const requestSent = new Promise<void>((resolve) => {
			markRequestSent = resolve;
		});
		class DeferredXMLHttpRequest extends ScriptedXMLHttpRequest {
			override send(): void {
				releaseReply = () => super.send();
				markRequestSent();
			}
		}
		vi.stubGlobal('XMLHttpRequest', DeferredXMLHttpRequest);
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));

		const pending = access.ensureAccountChecked(ACCOUNT_KEY);
		await requestSent;
		await SessionManager.login({token: 'rotated-token', userId: ACCOUNT.userId, runtimeSnapshot: RUNTIME});
		releaseReply();

		await expect(pending).resolves.toBe(AccountAccessDecision.UNAVAILABLE);
		expect(access.getCheckedUser(ACCOUNT_KEY)).toBeNull();
		expect(access.getPhase(ACCOUNT_KEY)).not.toBe(AccountAccessPhase.ALLOWED);

		vi.stubGlobal('XMLHttpRequest', ScriptedXMLHttpRequest);
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY);

		expect(scriptedRequestTokens).toEqual([ACCOUNT.token, 'rotated-token']);
		expect(access.getCheckedUser(ACCOUNT_KEY)).not.toBeNull();
	});

	test('a user checked under a superseded token is withheld until the rotated token is checked', async () => {
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		expect(access.getCheckedUser(ACCOUNT_KEY)).not.toBeNull();

		await SessionManager.login({token: 'rotated-token', userId: ACCOUNT.userId, runtimeSnapshot: RUNTIME});

		expect(access.getCheckedUser(ACCOUNT_KEY)).toBeNull();

		scriptedReplies.push(userMeReply(ACCOUNT.userId));
		await access.ensureAccountChecked(ACCOUNT_KEY);

		expect(access.getCheckedUser(ACCOUNT_KEY)).not.toBeNull();
	});
});

describe('AccountAccess re-checks', () => {
	test('a restricted account is never re-fetched on a timer, only when something asks again', async () => {
		vi.useFakeTimers();
		const access = createAccess();
		scriptedReplies.push(userMeReply(ACCOUNT.userId, ['REQUIRE_VERIFIED_EMAIL']));
		await access.ensureAccountChecked(ACCOUNT_KEY);
		expect(scriptedRequestTokens).toHaveLength(1);

		await vi.advanceTimersByTimeAsync(120_000);

		expect(scriptedRequestTokens).toHaveLength(1);
	});
});
