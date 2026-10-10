// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import {
	HARNESS_API_ENDPOINT,
	installHarnessBootstrap,
	resetScriptedTransport,
	ScriptedXMLHttpRequest,
	scriptedReplies,
	scriptedRequestTokens,
	serverErrorReply,
	unauthorizedReply,
	userMeReply,
} from '@app/features/auth/state/__fixtures__/AccountSwitchHarness';
import {instanceDiscoveryFixture} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import type {VoiceAccountExitRequest, VoiceAccountIdentity} from '@app/features/voice/VoiceAccountLifecyclePort';
import type {MessageDescriptor} from '@lingui/core';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));

const mocks = vi.hoisted(() => ({
	foregroundAccountKey: null as string | null,
	isForegroundReady: false,
	waitForForegroundReady: vi.fn<(accountKey: string | null) => Promise<void>>(() => Promise.resolve()),
	beginForegroundPromotion: vi.fn<(accountKey: string) => Promise<void>>(() => Promise.resolve()),
	reuseReadyForegroundSession: vi.fn<(accountKey: string, token: string) => void>(),
	recoverForegroundSession: vi.fn<(accountKey: string) => void>(),
	completeForegroundPromotion: vi.fn<(accountKey: string) => Promise<void>>(() => Promise.resolve()),
	rollbackForegroundPromotion: vi.fn<(accountKey: string) => Promise<void>>(() => Promise.resolve()),
	retireForAccountTransition: vi.fn<(reason: string) => Promise<void>>(() => Promise.resolve()),
	gatewayLogout: vi.fn<() => void>(),
	sendInvisiblePresence: vi.fn<() => void>(),
	replaceWith: vi.fn<(path: string) => void>((path) => {
		mocks.currentPath = path;
		mocks.commitRouterPath(path);
	}),
	readRouterPath: (): string => '',
	commitRouterPath: (_path: string): void => undefined,
	abandonUnreachableLastLocation: vi.fn<() => void>(),
	currentPath: '',
	lastLocation: null as string | null,
	saveLocation: vi.fn<(path: string) => void>(),
	readHydratedAccountKey: (): string | null => null,
	hydrateForegroundStores: (_accountKey: string | null): void => undefined,
	readCurrentAccountKey: (): string | null => null,
	leaveVoiceChannelForAccountExit: vi.fn<(request: VoiceAccountExitRequest) => Promise<void>>(),
	suspendVoiceForAccountRestriction: vi.fn<(account: VoiceAccountIdentity) => Promise<void>>(),
	unregisterAllPushSubscriptions: vi.fn<() => Promise<void>>(),
	registerPushSubscription: vi.fn<() => Promise<string | null>>(),
	isGranted: vi.fn<() => Promise<boolean>>(),
	isInstalledPwa: false,
}));

vi.mock('@app/features/gateway/transport/GatewayConnection', () => ({
	default: {
		get foregroundAccountKey() {
			return mocks.foregroundAccountKey;
		},
		get isReady() {
			return mocks.isForegroundReady;
		},
		waitForForegroundReady: mocks.waitForForegroundReady,
		beginForegroundPromotion: mocks.beginForegroundPromotion,
		completeForegroundPromotion: mocks.completeForegroundPromotion,
		rollbackForegroundPromotion: mocks.rollbackForegroundPromotion,
		reuseReadyForegroundSession: mocks.reuseReadyForegroundSession,
		recoverForegroundSession: mocks.recoverForegroundSession,
		retireForAccountTransition: mocks.retireForAccountTransition,
		logout: mocks.gatewayLogout,
		sendInvisiblePresenceForCurrentSession: mocks.sendInvisiblePresence,
	},
}));

vi.mock('@app/features/navigation/utils/RouterUtils', () => ({
	replaceWith: mocks.replaceWith,
	transitionTo: () => undefined,
	getCurrentPath: () => mocks.currentPath,
	getHistory: () => null,
	history: null,
}));

vi.mock('@app/features/navigation/state/Navigation', async () => {
	const {observable, runInAction} = await import('mobx');
	const routerPath = observable.box('');
	mocks.readRouterPath = () => routerPath.get();
	mocks.commitRouterPath = (path) => runInAction(() => routerPath.set(path));
	return {
		default: {
			get pathname() {
				return mocks.readRouterPath();
			},
		},
	};
});

vi.mock('@app/features/ui/state/Location', () => ({
	default: {getLastLocation: () => mocks.lastLocation, saveLocation: mocks.saveLocation},
}));

vi.mock('@app/features/user/state/Users', async () => {
	const {observable, runInAction} = await import('mobx');
	const hydratedAccountKey = observable.box<string | null>(null);
	mocks.readHydratedAccountKey = () => hydratedAccountKey.get();
	mocks.hydrateForegroundStores = (accountKey) => runInAction(() => hydratedAccountKey.set(accountKey));
	return {
		default: {
			get isCurrentUserHydrated() {
				const accountKey = hydratedAccountKey.get();
				return accountKey !== null && accountKey === mocks.readCurrentAccountKey();
			},
		},
	};
});

vi.mock('@app/features/navigation/utils/ChannelRouteReachability', () => ({
	abandonUnreachableLastLocation: mocks.abandonUnreachableLastLocation,
}));

vi.mock('@app/features/ui/utils/PwaUtils', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/ui/utils/PwaUtils')>()),
	isInstalledPwa: () => mocks.isInstalledPwa,
}));

vi.mock('@app/features/notification/utils/NotificationUtils', () => ({isGranted: mocks.isGranted}));

vi.mock('@app/features/platform/push/PushSubscriptionService', () => ({
	unregisterAllPushSubscriptions: mocks.unregisterAllPushSubscriptions,
	registerPushSubscription: mocks.registerPushSubscription,
}));

vi.mock('@app/features/voice/VoiceAccountLifecycle', () => ({
	leaveVoiceChannelForAccountExit: mocks.leaveVoiceChannelForAccountExit,
	suspendVoiceForAccountRestriction: mocks.suspendVoiceForAccountRestriction,
}));

vi.mock('@app/features/presence/state/LocalPresence', () => ({
	default: {captureIntent: () => null, restoreIntent: () => undefined},
}));

vi.mock('@app/features/ui/state/LayerManager', () => ({default: {closeAll: () => undefined}}));

vi.mock('@app/features/user/state/UserSettings', () => ({
	default: {
		handleAccountTransition: () => undefined,
		captureAccountTransitionCheckpoint: () => ({
			accountEpoch: 0,
			hydrated: false,
			syncedPreferences: {},
			wireSyncedPreferences: {},
			dirtySyncedPreferenceFields: [],
			recentlyAckedSyncedPreferenceFields: [],
			syncConsecutive429s: 0,
			hadPendingFlush: false,
		}),
		restoreAccountTransitionCheckpoint: () => Promise.resolve(),
	},
}));

installHarnessBootstrap();

const {default: RuntimeConfig} = await import('@app/features/app/state/RuntimeConfig');
const {default: AccountAccess, AccountInstanceUnavailableError} = await import(
	'@app/features/auth/state/AccountAccess'
);
const {default: Accounts, AccountReplacementOperationCapacityExceededError} = await import(
	'@app/features/auth/state/Accounts'
);
const {ForegroundGatewayConnectionRecoverableError, ForegroundGatewayRecoveryCause} = await import(
	'@app/features/gateway/transport/ForegroundGatewayConnectionFailure'
);
const {default: accountStorage} = await import('@app/features/auth/state/AccountStorage');
const {default: SessionManager, SessionExpiredError} = await import('@app/features/platform/state/AuthSession');
const {AccountScopedWork} = await import('@app/features/platform/state/AccountScopedWork');
const {default: AppStorage} = await import('@app/features/platform/state/PersistentStorage');
const {AuthSessionStorageKey} = await import('@app/features/platform/state/auth_session/AuthSessionStorage');

mocks.readCurrentAccountKey = () => SessionManager.currentAccountKey;

const ACCOUNT_A = {userId: '100', token: 'token-a'};
const ACCOUNT_B = {userId: '200', token: 'token-b'};
const ACCOUNT_C = {userId: '300', token: 'token-c'};
const BASE_SNAPSHOT: RuntimeConfigSnapshot = RuntimeConfig.getSnapshot();

function accountKeyOf(userId: string): string {
	return `${HARNESS_API_ENDPOINT}::${userId}`;
}

async function flushPushRegistration(): Promise<void> {
	await new Promise<void>((resolve) => setTimeout(resolve, 0));
}

async function seedTwoAccounts(): Promise<void> {
	scriptedReplies.push(userMeReply(ACCOUNT_A.userId));
	await SessionManager.login({token: ACCOUNT_A.token, userId: ACCOUNT_A.userId, runtimeSnapshot: BASE_SNAPSHOT});
	scriptedReplies.push(userMeReply(ACCOUNT_B.userId));
	await SessionManager.login({token: ACCOUNT_B.token, userId: ACCOUNT_B.userId, runtimeSnapshot: BASE_SNAPSHOT});
	scriptedReplies.push(userMeReply(ACCOUNT_A.userId));
	await SessionManager.switchAccount(ACCOUNT_A.userId);
	resetScriptedTransport();
	vi.clearAllMocks();
	mocks.isGranted.mockResolvedValue(false);
	mocks.unregisterAllPushSubscriptions.mockResolvedValue(undefined);
	mocks.registerPushSubscription.mockResolvedValue(null);
	mocks.leaveVoiceChannelForAccountExit.mockResolvedValue(undefined);
	mocks.suspendVoiceForAccountRestriction.mockResolvedValue(undefined);
}

function gatewayOutage(accountKey: string): InstanceType<typeof ForegroundGatewayConnectionRecoverableError> {
	return new ForegroundGatewayConnectionRecoverableError(
		accountKey,
		ForegroundGatewayRecoveryCause.READINESS_TIMEOUT,
		new Error('gateway unreachable'),
	);
}

beforeEach(async () => {
	mocks.currentPath = '';
	mocks.commitRouterPath('');
	mocks.lastLocation = null;
	mocks.replaceWith.mockImplementation((path) => {
		mocks.currentPath = path;
		mocks.commitRouterPath(path);
	});
	mocks.foregroundAccountKey = null;
	mocks.hydrateForegroundStores(null);
	mocks.isForegroundReady = false;
	mocks.isInstalledPwa = false;
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
	mocks.isGranted.mockResolvedValue(false);
	mocks.unregisterAllPushSubscriptions.mockResolvedValue(undefined);
	mocks.registerPushSubscription.mockResolvedValue(null);
	mocks.leaveVoiceChannelForAccountExit.mockResolvedValue(undefined);
	mocks.suspendVoiceForAccountRestriction.mockResolvedValue(undefined);
	RuntimeConfig.applySnapshot(BASE_SNAPSHOT);
	await SessionManager.reset();
	AccountAccess.forgetAccount(accountKeyOf(ACCOUNT_A.userId));
	AccountAccess.forgetAccount(accountKeyOf(ACCOUNT_B.userId));
	AccountAccess.forgetAccount(accountKeyOf(ACCOUNT_C.userId));
	AppStorage.clear();
	await accountStorage.deleteAccount(ACCOUNT_A.userId);
	await accountStorage.deleteAccount(ACCOUNT_B.userId);
	await accountStorage.deleteAccount(ACCOUNT_C.userId);
	await SessionManager.initialize();
	await seedTwoAccounts();
});

afterEach(() => {
	vi.unstubAllGlobals();
	vi.clearAllMocks();
	resetScriptedTransport();
});

describe('Accounts switch transaction', () => {
	test('a successful switch activates the target and starts its gateway session', async () => {
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));
		await Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId), '/channels/@me');
		expect(SessionManager.userId).toBe(ACCOUNT_B.userId);
		expect(SessionManager.token).toBe(ACCOUNT_B.token);
		expect(mocks.leaveVoiceChannelForAccountExit).toHaveBeenCalledWith({
			reason: 'account-switch',
			accountKey: accountKeyOf(ACCOUNT_A.userId),
			userId: ACCOUNT_A.userId,
		});
		expect(mocks.waitForForegroundReady).toHaveBeenLastCalledWith(accountKeyOf(ACCOUNT_B.userId));
		expect(mocks.replaceWith).toHaveBeenCalledWith('/channels/@me');
	});

	test('a failed activation leaves the previous account, credentials and gateway session intact', async () => {
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId));
		mocks.completeForegroundPromotion.mockRejectedValueOnce(new SessionExpiredError());
		await expect(Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId))).rejects.toBeInstanceOf(SessionExpiredError);
		expect(SessionManager.userId).toBe(ACCOUNT_A.userId);
		expect(SessionManager.token).toBe(ACCOUNT_A.token);
		expect(AppStorage.getItem(AuthSessionStorageKey.UserId)).toBe(ACCOUNT_A.userId);
		expect(AppStorage.getItem(AuthSessionStorageKey.Token)).toBe(ACCOUNT_A.token);
		expect(RuntimeConfig.getSnapshot().apiEndpoint).toBe(BASE_SNAPSHOT.apiEndpoint);
		expect(mocks.waitForForegroundReady).toHaveBeenLastCalledWith(accountKeyOf(ACCOUNT_A.userId));
		expect(mocks.replaceWith).not.toHaveBeenCalled();
	});

	test('a rollback that is already on the previous account does not re-fence account scoped work', async () => {
		const runSuspended = vi.spyOn(AccountScopedWork, 'runSuspended');
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId));
		mocks.beginForegroundPromotion.mockRejectedValueOnce(new SessionExpiredError());

		await expect(Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId))).rejects.toBeInstanceOf(SessionExpiredError);

		expect(SessionManager.currentAccountKey).toBe(accountKeyOf(ACCOUNT_A.userId));
		expect(runSuspended).toHaveBeenCalledTimes(1);
		expect(AccountScopedWork.isSuspended).toBe(false);
		runSuspended.mockRestore();
	});

	test('a failed activation restores the runtime snapshot the previous account was using', async () => {
		const foreignSnapshot: RuntimeConfigSnapshot = {...BASE_SNAPSHOT, apiEndpoint: 'https://foreign.test/api'};
		mocks.leaveVoiceChannelForAccountExit.mockImplementation(async () => {
			RuntimeConfig.applySnapshot(foreignSnapshot);
		});
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId));
		await expect(Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId))).rejects.toThrow();
		expect(RuntimeConfig.apiEndpoint).toBe(BASE_SNAPSHOT.apiEndpoint);
		expect(SessionManager.userId).toBe(ACCOUNT_A.userId);
		expect(mocks.waitForForegroundReady).toHaveBeenLastCalledWith(accountKeyOf(ACCOUNT_A.userId));
		expect(Accounts.getAccount(accountKeyOf(ACCOUNT_B.userId))?.isValid).toBe(true);
	});

	test('a rejected pre-flight never touches voice, push or the gateway', async () => {
		mocks.isInstalledPwa = true;
		scriptedReplies.push(unauthorizedReply());
		await expect(Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId))).rejects.toBeInstanceOf(SessionExpiredError);
		expect(mocks.leaveVoiceChannelForAccountExit).not.toHaveBeenCalled();
		expect(mocks.unregisterAllPushSubscriptions).not.toHaveBeenCalled();
		expect(mocks.waitForForegroundReady).not.toHaveBeenCalled();
		expect(SessionManager.userId).toBe(ACCOUNT_A.userId);
		expect(Accounts.getAccount(accountKeyOf(ACCOUNT_B.userId))?.isValid).toBe(false);
	});

	test('a second replacement while one is in flight is refused rather than interleaved', async () => {
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));
		const first = Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId));
		const second = Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId));
		await expect(second).rejects.toBeInstanceOf(AccountReplacementOperationCapacityExceededError);
		await first;
		expect(SessionManager.userId).toBe(ACCOUNT_B.userId);
	});

	test('a switch during a gateway outage commits to the target and keeps reconnecting it', async () => {
		const target = accountKeyOf(ACCOUNT_B.userId);
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));
		mocks.waitForForegroundReady.mockImplementation(async (accountKey) => {
			throw gatewayOutage(accountKey ?? target);
		});

		await Accounts.switchToAccount(target, '/channels/@me');

		expect(SessionManager.currentAccountKey).toBe(target);
		expect(SessionManager.token).toBe(ACCOUNT_B.token);
		expect(mocks.waitForForegroundReady).toHaveBeenCalledTimes(1);
		expect(mocks.waitForForegroundReady).toHaveBeenCalledWith(target);
		expect(mocks.beginForegroundPromotion).not.toHaveBeenCalledWith(accountKeyOf(ACCOUNT_A.userId));
		expect(mocks.recoverForegroundSession).toHaveBeenCalledWith(target);
		expect(mocks.replaceWith).toHaveBeenCalledWith('/channels/@me');
		expect(Accounts.isSwitching).toBe(false);
		mocks.waitForForegroundReady.mockReset();
		mocks.waitForForegroundReady.mockResolvedValue(undefined);
	});

	test('a restore that meets a dead gateway keeps the previous account reconnecting', async () => {
		const previous = accountKeyOf(ACCOUNT_A.userId);
		const target = accountKeyOf(ACCOUNT_B.userId);
		const activationFailure = new Error('foreground gateway was superseded');
		mocks.currentPath = '/channels/111/222';
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));
		mocks.waitForForegroundReady.mockImplementation(async (accountKey) => {
			if (accountKey === target) {
				throw activationFailure;
			}
			throw gatewayOutage(accountKey ?? previous);
		});

		await expect(Accounts.switchToAccount(target, '/channels/@me')).rejects.toBe(activationFailure);

		expect(SessionManager.currentAccountKey).toBe(previous);
		expect(mocks.recoverForegroundSession).toHaveBeenCalledWith(previous);
		expect(mocks.replaceWith).not.toHaveBeenCalled();
		expect(mocks.currentPath).toBe('/channels/111/222');
		mocks.waitForForegroundReady.mockReset();
		mocks.waitForForegroundReady.mockResolvedValue(undefined);
	});

	test('switching to the account that is already current is a no-op', async () => {
		await Accounts.switchToAccount(accountKeyOf(ACCOUNT_A.userId));
		expect(mocks.leaveVoiceChannelForAccountExit).not.toHaveBeenCalled();
		expect(mocks.waitForForegroundReady).not.toHaveBeenCalled();
	});

	test('isSwitching covers the whole transaction, not just the session state', async () => {
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));
		const pending = Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId));
		expect(Accounts.isSwitching).toBe(true);
		expect(Accounts.canSwitchAccounts).toBe(false);
		await pending;
		expect(Accounts.isSwitching).toBe(false);
	});
});

describe('Accounts foreground access checks', () => {
	test('a user-initiated switch re-checks an account parked behind an unavailable backoff', async () => {
		const accountKey = accountKeyOf(ACCOUNT_B.userId);
		AccountAccess.markGatewayUnavailable(accountKey, new Error('gateway refused the session'));
		expect(AccountAccess.getRetryEligibleAt(accountKey)).toBeGreaterThan(Date.now());

		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));
		await Accounts.switchToAccount(accountKey, '/channels/@me');

		expect(scriptedRequestTokens).toContain(ACCOUNT_B.token);
		expect(SessionManager.userId).toBe(ACCOUNT_B.userId);
		expect(mocks.replaceWith).toHaveBeenCalledWith('/channels/@me');
	});

	test('a foreground re-check that fails again keeps the background backoff escalating', async () => {
		const accountKey = accountKeyOf(ACCOUNT_B.userId);
		AccountAccess.markGatewayUnavailable(accountKey, new Error('gateway refused the session'));

		scriptedReplies.push(serverErrorReply());
		await expect(Accounts.switchToAccount(accountKey)).rejects.toBeInstanceOf(AccountInstanceUnavailableError);

		expect(scriptedRequestTokens).toContain(ACCOUNT_B.token);
		expect(AccountAccess.getRetryEligibleAt(accountKey) - Date.now()).toBeGreaterThan(10_000);
		expect(SessionManager.userId).toBe(ACCOUNT_A.userId);
		expect(mocks.leaveVoiceChannelForAccountExit).not.toHaveBeenCalled();
	});
});

describe('Accounts new account activation', () => {
	test('a new account replaces the active one and lands on the requested route', async () => {
		await Accounts.switchToNewAccount({
			userId: ACCOUNT_C.userId,
			token: ACCOUNT_C.token,
			runtimeSnapshot: BASE_SNAPSHOT,
			redirectPath: '/channels/@me',
		});

		expect(SessionManager.userId).toBe(ACCOUNT_C.userId);
		expect(SessionManager.token).toBe(ACCOUNT_C.token);
		expect(SessionManager.currentAccountKey).toBe(accountKeyOf(ACCOUNT_C.userId));
		expect(mocks.leaveVoiceChannelForAccountExit).toHaveBeenCalledWith({
			reason: 'account-switch',
			accountKey: accountKeyOf(ACCOUNT_A.userId),
			userId: ACCOUNT_A.userId,
		});
		expect(mocks.beginForegroundPromotion).toHaveBeenCalledWith(accountKeyOf(ACCOUNT_C.userId));
		expect(mocks.waitForForegroundReady).toHaveBeenLastCalledWith(accountKeyOf(ACCOUNT_C.userId));
		expect(mocks.replaceWith).toHaveBeenCalledWith('/channels/@me');
	});

	test('re-authenticating the active account rotates its token without promoting a gateway again', async () => {
		await Accounts.switchToNewAccount({
			userId: ACCOUNT_A.userId,
			token: 'rotated-token-a',
			runtimeSnapshot: BASE_SNAPSHOT,
			redirectPath: '/channels/@me',
		});

		expect(SessionManager.currentAccountKey).toBe(accountKeyOf(ACCOUNT_A.userId));
		expect(SessionManager.token).toBe('rotated-token-a');
		expect(Accounts.getAccount(accountKeyOf(ACCOUNT_A.userId))?.token).toBe('rotated-token-a');
		expect(mocks.beginForegroundPromotion).not.toHaveBeenCalled();
		expect(mocks.completeForegroundPromotion).not.toHaveBeenCalled();
		expect(mocks.leaveVoiceChannelForAccountExit).not.toHaveBeenCalled();
		expect(mocks.waitForForegroundReady).toHaveBeenLastCalledWith(accountKeyOf(ACCOUNT_A.userId));
		expect(mocks.replaceWith).toHaveBeenCalledWith('/channels/@me');
	});

	test('a rotated token is handed to the foreground gateway that is already ready for the account', async () => {
		mocks.foregroundAccountKey = accountKeyOf(ACCOUNT_A.userId);
		mocks.isForegroundReady = true;

		await Accounts.switchToNewAccount({
			userId: ACCOUNT_A.userId,
			token: 'rotated-token-a',
			runtimeSnapshot: BASE_SNAPSHOT,
		});

		expect(mocks.reuseReadyForegroundSession).toHaveBeenCalledWith(accountKeyOf(ACCOUNT_A.userId), 'rotated-token-a');
		expect(mocks.beginForegroundPromotion).not.toHaveBeenCalled();
	});

	test('a foreground gateway that is not ready yet is left to finish its own handshake', async () => {
		mocks.foregroundAccountKey = accountKeyOf(ACCOUNT_A.userId);
		mocks.isForegroundReady = false;

		await Accounts.switchToNewAccount({
			userId: ACCOUNT_A.userId,
			token: 'rotated-token-a',
			runtimeSnapshot: BASE_SNAPSHOT,
		});

		expect(mocks.reuseReadyForegroundSession).not.toHaveBeenCalled();
		expect(mocks.waitForForegroundReady).toHaveBeenLastCalledWith(accountKeyOf(ACCOUNT_A.userId));
	});

	test('a foreground gateway left attached to another account refuses the re-authentication', async () => {
		mocks.foregroundAccountKey = accountKeyOf(ACCOUNT_B.userId);
		mocks.isForegroundReady = true;

		await expect(
			Accounts.switchToNewAccount({
				userId: ACCOUNT_A.userId,
				token: 'rotated-token-a',
				runtimeSnapshot: BASE_SNAPSHOT,
				redirectPath: '/channels/@me',
			}),
		).rejects.toThrow(
			`Foreground gateway ${accountKeyOf(ACCOUNT_B.userId)} remained attached after activating ${accountKeyOf(ACCOUNT_A.userId)}`,
		);

		expect(mocks.reuseReadyForegroundSession).not.toHaveBeenCalled();
		expect(mocks.waitForForegroundReady).not.toHaveBeenCalled();
		expect(mocks.replaceWith).not.toHaveBeenCalled();
	});
});

describe('Accounts push subscriptions on an installed PWA', () => {
	test('the outgoing account loses its push subscriptions before the incoming one is registered', async () => {
		mocks.isInstalledPwa = true;
		mocks.isGranted.mockResolvedValue(true);
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));

		await Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId), '/channels/@me');
		await flushPushRegistration();

		expect(mocks.unregisterAllPushSubscriptions).toHaveBeenCalledTimes(1);
		expect(mocks.registerPushSubscription).toHaveBeenCalledTimes(1);
		expect(mocks.unregisterAllPushSubscriptions.mock.invocationCallOrder[0]).toBeLessThan(
			mocks.waitForForegroundReady.mock.invocationCallOrder[0],
		);
		expect(mocks.registerPushSubscription.mock.invocationCallOrder[0]).toBeGreaterThan(
			mocks.waitForForegroundReady.mock.invocationCallOrder[0],
		);
	});

	test('a browser that never granted notifications is not asked for a push subscription', async () => {
		mocks.isInstalledPwa = true;
		mocks.isGranted.mockResolvedValue(false);
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));

		await Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId), '/channels/@me');
		await flushPushRegistration();

		expect(mocks.unregisterAllPushSubscriptions).toHaveBeenCalledTimes(1);
		expect(mocks.registerPushSubscription).not.toHaveBeenCalled();
	});

	test('an account that still owes a required action is never re-subscribed to push', async () => {
		mocks.isInstalledPwa = true;
		mocks.isGranted.mockResolvedValue(true);
		scriptedReplies.push(
			userMeReply(ACCOUNT_B.userId, ['REQUIRE_VERIFIED_EMAIL']),
			userMeReply(ACCOUNT_B.userId, ['REQUIRE_VERIFIED_EMAIL']),
		);

		await Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId), '/channels/@me');
		await flushPushRegistration();

		expect(SessionManager.currentAccountKey).toBe(accountKeyOf(ACCOUNT_B.userId));
		expect(AccountAccess.currentPhase).toBe('action_required');
		expect(mocks.unregisterAllPushSubscriptions).toHaveBeenCalledTimes(1);
		expect(mocks.registerPushSubscription).not.toHaveBeenCalled();
	});

	test('a failed activation re-registers push for the account it rolled back to', async () => {
		mocks.isInstalledPwa = true;
		mocks.isGranted.mockResolvedValue(true);
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId));
		mocks.completeForegroundPromotion.mockRejectedValueOnce(new SessionExpiredError());

		await expect(Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId))).rejects.toBeInstanceOf(SessionExpiredError);
		await flushPushRegistration();

		expect(SessionManager.currentAccountKey).toBe(accountKeyOf(ACCOUNT_A.userId));
		expect(mocks.unregisterAllPushSubscriptions).toHaveBeenCalledTimes(1);
		expect(mocks.registerPushSubscription).toHaveBeenCalledTimes(1);
	});

	test('a browser tab that is not an installed PWA never touches push subscriptions', async () => {
		mocks.isGranted.mockResolvedValue(true);
		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));

		await Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId), '/channels/@me');
		await flushPushRegistration();

		expect(mocks.unregisterAllPushSubscriptions).not.toHaveBeenCalled();
		expect(mocks.registerPushSubscription).not.toHaveBeenCalled();
	});
});

describe('Accounts account keys', () => {
	test('accounts are keyed by instance-qualified account key', () => {
		expect([...Accounts.accounts.keys()].sort()).toEqual([
			accountKeyOf(ACCOUNT_A.userId),
			accountKeyOf(ACCOUNT_B.userId),
		]);
	});

	test('removing the current account leaves voice with the account-removed context', async () => {
		await Accounts.removeStoredAccount(accountKeyOf(ACCOUNT_A.userId));
		expect(mocks.leaveVoiceChannelForAccountExit).toHaveBeenCalledWith({
			reason: 'account-removed',
			accountKey: accountKeyOf(ACCOUNT_A.userId),
			userId: ACCOUNT_A.userId,
		});
		expect(Accounts.getAccount(accountKeyOf(ACCOUNT_A.userId))).toBeNull();
	});

	test('removing a background account never touches voice', async () => {
		await Accounts.removeStoredAccount(accountKeyOf(ACCOUNT_B.userId));
		expect(mocks.leaveVoiceChannelForAccountExit).not.toHaveBeenCalled();
		expect(SessionManager.userId).toBe(ACCOUNT_A.userId);
	});
});

describe('Accounts restriction handling', () => {
	test('a restricted account reaches voice only through the lifecycle boundary', async () => {
		await Accounts.suspendVoiceForAccountRestriction(accountKeyOf(ACCOUNT_A.userId));
		expect(mocks.suspendVoiceForAccountRestriction).toHaveBeenCalledWith({
			accountKey: accountKeyOf(ACCOUNT_A.userId),
			userId: ACCOUNT_A.userId,
		});
		expect(mocks.leaveVoiceChannelForAccountExit).not.toHaveBeenCalled();
	});

	test('a restriction reported for a background account never suspends the active account voice session', async () => {
		await Accounts.suspendVoiceForAccountRestriction(accountKeyOf(ACCOUNT_B.userId));

		expect(mocks.suspendVoiceForAccountRestriction).not.toHaveBeenCalled();
		expect(mocks.leaveVoiceChannelForAccountExit).not.toHaveBeenCalled();
	});

	test('a restriction reported for an account on another instance never suspends the active account voice session', async () => {
		await Accounts.suspendVoiceForAccountRestriction(`https://foreign.test/api::${ACCOUNT_A.userId}`);

		expect(mocks.suspendVoiceForAccountRestriction).not.toHaveBeenCalled();
	});
});

describe('Accounts logout', () => {
	test('logout leaves voice, ends the session and navigates to the login page', async () => {
		await Accounts.logout();
		expect(mocks.leaveVoiceChannelForAccountExit).toHaveBeenCalledWith({
			reason: 'logout',
			accountKey: accountKeyOf(ACCOUNT_A.userId),
			userId: ACCOUNT_A.userId,
		});
		expect(SessionManager.userId).toBeNull();
		expect(mocks.replaceWith).toHaveBeenCalledWith('/login');
	});

	test('a stored account left behind by the logout can still be activated', async () => {
		await Accounts.logout();
		expect(SessionManager.currentAccountKey).toBeNull();
		expect(Accounts.canSwitchAccounts).toBe(true);

		scriptedReplies.push(userMeReply(ACCOUNT_B.userId), userMeReply(ACCOUNT_B.userId));
		await Accounts.switchToAccount(accountKeyOf(ACCOUNT_B.userId), '/channels/@me');

		expect(SessionManager.userId).toBe(ACCOUNT_B.userId);
		expect(SessionManager.token).toBe(ACCOUNT_B.token);
		expect(mocks.waitForForegroundReady).toHaveBeenLastCalledWith(accountKeyOf(ACCOUNT_B.userId));
		expect(mocks.replaceWith).toHaveBeenLastCalledWith('/channels/@me');
	});
});
