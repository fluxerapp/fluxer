// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	CommittedRuntimeConfig,
	PreparedRuntimeConfig,
	RuntimeConfigSnapshot,
} from '@app/features/app/state/RuntimeConfig';
import {installHarnessBootstrap} from '@app/features/auth/state/__fixtures__/AccountSwitchHarness';
import type {
	AccountPresenceIntent,
	QualifiedStoredAccount,
	StoredAccount,
	StoredAccountInventory,
	StoredAccountInventoryReplacement,
	StoredAccountList,
	StoredAccountSource,
	UserData,
} from '@app/features/auth/state/AccountStorage';
import {StoredAccountInventoryReplacementConflictError} from '@app/features/auth/state/StoredAccountInventoryContract';
import {
	classifyStoredAccount,
	createStoredAccountInventory,
	inventoryEntriesHaveSameRevision,
	inventoryEntryStorageKey,
	normalizeStoredAccountInventoryReplacement,
} from '@app/features/auth/state/StoredAccountInventoryPolicy';
import type {AuthSessionDependencies, AuthSessionManager} from '@app/features/platform/state/AuthSession';
import type {AuthSessionTransitionContext} from '@app/features/platform/state/auth_session/AuthSessionTransitionGate';
import type {ResetClientStateOptions} from '@app/features/platform/state/ResetClientState';
import {HttpError} from '@app/features/platform/types/EndpointError';
import type {UserSettingsAccountTransitionCheckpoint} from '@app/features/user/state/UserSettings';
import type {MessageDescriptor} from '@lingui/core';
import {autorun, configure} from 'mobx';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const desktopRuntime = vi.hoisted(() => ({required: false}));

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: MessageDescriptor) => descriptor}));
vi.mock('@app/features/app/state/DesktopRuntimeTransaction', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@app/features/app/state/DesktopRuntimeTransaction')>();
	return {...actual, requiresDesktopRuntimeTransaction: () => desktopRuntime.required};
});
vi.mock('@app/features/gateway/transport/GatewayConnection', () => ({
	default: {
		logout: () => undefined,
		startSession: () => undefined,
		sendInvisiblePresenceForCurrentSession: () => undefined,
	},
}));

vi.mock('@app/features/presence/state/LocalPresence', () => ({
	default: {captureIntent: () => null, restoreIntent: () => undefined},
}));

vi.mock('@app/features/ui/state/LayerManager', () => ({default: {closeAll: () => undefined}}));

installHarnessBootstrap();

const {default: RuntimeConfig} = await import('@app/features/app/state/RuntimeConfig');
const {AccountInstanceMismatchError, createSessionManager, SessionExpiredError, SessionState} = await import(
	'@app/features/platform/state/AuthSession'
);
const {AuthSessionStorageKey} = await import('@app/features/platform/state/auth_session/AuthSessionStorage');
const {DurableSessionCredentialMirror} = await import(
	'@app/features/platform/state/auth_session/SessionCredentialMirror'
);
const {GATEWAY_PREBOOT_SESSION_STORAGE_KEY} = await import('@app/features/platform/state/PrebootMirror');
const {default: AppStorage, UNAUTHENTICATED_APP_STORAGE_SCOPE} = await import(
	'@app/features/platform/state/PersistentStorage'
);
const {AccountScopedWork} = await import('@app/features/platform/state/AccountScopedWork');
const {default: DesktopRuntimeTransactions} = await import('@app/features/app/state/DesktopRuntimeTransaction');

const PRIMARY: RuntimeConfigSnapshot = RuntimeConfig.getSnapshot();
const FOREIGN: RuntimeConfigSnapshot = {...PRIMARY, apiEndpoint: 'https://foreign.test/api'};
const BLANK: RuntimeConfigSnapshot = {...PRIMARY, apiEndpoint: ''};

const PRIMARY_KEY = 'https://primary.test/api';
const FOREIGN_KEY = 'https://foreign.test/api';

const NOOP_AUTH_SESSION_TRANSITION: AuthSessionTransitionContext = {
	notifyLogout: async () => undefined,
};

type AccountStorageDep = AuthSessionDependencies['accountStorage'];

interface StashCall {
	userId: string;
	token: string | null;
	userData?: UserData;
	instance?: RuntimeConfigSnapshot;
	presenceIntent?: AccountPresenceIntent | null;
	mirroredUserId: string | null;
}

function instanceKeyOf(instance: RuntimeConfigSnapshot | undefined): string | null {
	if (instance === undefined || instance.apiEndpoint.length === 0) {
		return null;
	}
	return instance.apiEndpoint;
}

class FakeAccountStorage implements AccountStorageDep {
	readonly records = new Map<string, StoredAccount>();
	readonly stashes: Array<StashCall> = [];
	readonly deletes: Array<string> = [];
	source: StoredAccountSource = 'idb';
	listError: Error | null = null;
	stashError: Error | null = null;

	seed(record: Partial<StoredAccount> & {userId: string}): StoredAccount {
		const instance = record.instance;
		const instanceKey = instanceKeyOf(instance);
		const stored: StoredAccount = {
			userId: record.userId,
			token: record.token === undefined ? `token-${record.userId}` : record.token,
			userData: record.userData,
			presenceIntent: record.presenceIntent ?? null,
			localStorageData: {},
			managedStorageData: {},
			lastActive: record.lastActive ?? 1,
			instance,
			isValid: record.isValid,
			storageKey: record.storageKey ?? (instanceKey === null ? undefined : `${instanceKey}::${record.userId}`),
		};
		this.records.set(stored.storageKey ?? stored.userId, stored);
		return stored;
	}

	private findByUserId(userId: string): StoredAccount | undefined {
		for (const record of this.records.values()) {
			if (record.userId === userId) {
				return record;
			}
		}
		return undefined;
	}

	async getAllAccounts(): Promise<StoredAccountList> {
		if (this.listError !== null) {
			throw this.listError;
		}
		return {records: [...this.records.values()].map((record) => ({...record})), source: this.source};
	}

	async getAccountInventory(): Promise<StoredAccountInventory> {
		if (this.listError !== null) {
			throw this.listError;
		}
		return createStoredAccountInventory(
			this.source,
			[...this.records.entries()].map(([storageKey, record]) =>
				classifyStoredAccount({
					value: record,
					authoritativeStorageKey: record.storageKey ?? storageKey,
					source: this.source,
				}),
			),
		);
	}

	async replaceInventoryEntry(request: StoredAccountInventoryReplacement): Promise<QualifiedStoredAccount> {
		const storageKey = inventoryEntryStorageKey(request.expected);
		const current = this.records.get(storageKey);
		if (current === undefined) {
			throw new StoredAccountInventoryReplacementConflictError(storageKey, 'The stored account no longer exists');
		}
		const classified = classifyStoredAccount({
			value: current,
			authoritativeStorageKey: storageKey,
			source: this.source,
		});
		if (
			(classified.kind !== 'ready' && classified.kind !== 'runtime-recovery') ||
			!inventoryEntriesHaveSameRevision(classified, request.expected)
		) {
			throw new StoredAccountInventoryReplacementConflictError(storageKey, 'The stored account changed');
		}
		const replacement = normalizeStoredAccountInventoryReplacement(request);
		this.records.set(storageKey, replacement);
		return replacement;
	}

	async stashAccountData(
		userId: string,
		token: string | null,
		userData?: UserData,
		instance?: RuntimeConfigSnapshot,
		presenceIntent?: AccountPresenceIntent | null,
	): Promise<void> {
		this.stashes.push({
			userId,
			token,
			userData,
			instance,
			presenceIntent,
			mirroredUserId: window.localStorage.getItem(AuthSessionStorageKey.UserId),
		});
		if (this.stashError !== null) {
			throw this.stashError;
		}
		const existing = this.findByUserId(userId);
		this.seed({
			userId,
			token,
			userData: userData ?? existing?.userData,
			presenceIntent: presenceIntent ?? existing?.presenceIntent,
			lastActive: 2,
			instance,
			isValid: existing?.isValid,
		});
	}

	private findByAccountKey(accountKey: string): StoredAccount | undefined {
		return this.records.get(accountKey) ?? this.findByUserId(accountKey);
	}

	async restoreAccountData(accountKey: string): Promise<StoredAccount | null> {
		const record = this.findByAccountKey(accountKey);
		if (record === undefined) {
			return null;
		}
		return {...record};
	}

	async upsertAccount(record: StoredAccount, currentInstance: RuntimeConfigSnapshot): Promise<void> {
		this.seed({...record, instance: record.instance ?? currentInstance});
	}

	async deleteAccount(accountKey: string): Promise<void> {
		this.deletes.push(accountKey);
		const record = this.findByAccountKey(accountKey);
		if (record === undefined) {
			return;
		}
		this.records.delete(record.storageKey ?? record.userId);
	}

	async updateAccountValidity(accountKey: string, isValid: boolean): Promise<void> {
		const record = this.findByAccountKey(accountKey);
		if (record !== undefined) {
			record.isValid = isValid;
		}
	}
}

interface Harness {
	session: AuthSessionManager;
	accountStorage: FakeAccountStorage;
	activatedScopes: Array<string | null>;
	deletedScopes: Array<string>;
	clientStateResets: Array<ResetClientStateOptions>;
	appliedSnapshots: Array<RuntimeConfigSnapshot>;
	gatewayTeardowns: Array<string>;
	validTokens: Map<string, string>;
}

function createHarness(overrides: Partial<AuthSessionDependencies> = {}): Harness {
	const accountStorage = new FakeAccountStorage();
	const activatedScopes: Array<string | null> = [];
	const deletedScopes: Array<string> = [];
	const clientStateResets: Array<ResetClientStateOptions> = [];
	const appliedSnapshots: Array<RuntimeConfigSnapshot> = [];
	const gatewayTeardowns: Array<string> = [];
	const validTokens = new Map<string, string>();
	let currentInstance: RuntimeConfigSnapshot | null = PRIMARY;
	let storageScope = UNAUTHENTICATED_APP_STORAGE_SCOPE;
	let clock = 100;
	const mirrorGatewayPrebootSession = (sessionPresent: boolean): void => {
		if (sessionPresent) {
			window.localStorage.setItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY, '1');
		} else {
			window.localStorage.removeItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY);
		}
	};
	const credentialMirror = new DurableSessionCredentialMirror(AppStorage, mirrorGatewayPrebootSession);
	const userSettingsCheckpoint = Object.freeze({}) as UserSettingsAccountTransitionCheckpoint;
	const prepareRuntimeSnapshot = async (snapshot: RuntimeConfigSnapshot): Promise<PreparedRuntimeConfig> => ({
		snapshot,
		transportApiEndpoint: snapshot.apiEndpoint,
		expectedGeneration: 0,
		desktopRuntime: null,
	});
	const commitRuntimeSnapshot = async (prepared: PreparedRuntimeConfig): Promise<CommittedRuntimeConfig> => ({
		...prepared,
		previousRuntime:
			currentInstance === null ? null : {snapshot: currentInstance, transportApiEndpoint: currentInstance.apiEndpoint},
		activeRuntime: {snapshot: prepared.snapshot, transportApiEndpoint: prepared.transportApiEndpoint},
		desktopRuntime: null,
		publication: {phase: 'committed'},
	});
	const {
		activateStorageScope: activateStorageScopeOverride,
		credentialMirror: credentialMirrorOverride,
		...remainingOverrides
	} = overrides;
	const deps: AuthSessionDependencies = {
		accountStorage,
		credentialMirror: credentialMirrorOverride ?? credentialMirror,
		http: {
			get: async (_url, options) => {
				const headers = options.headers ?? {};
				const token = headers.Authorization ?? '';
				const userId = validTokens.get(token);
				if (userId === undefined) {
					throw new HttpError({method: 'GET', path: '/users/@me', status: 401});
				}
				return {body: {id: userId} as never};
			},
		},
		getRuntimeSnapshot: () => currentInstance,
		resolveRuntimeEndpoint: async (input) => {
			const snapshot = input === FOREIGN_KEY ? FOREIGN : PRIMARY;
			return {snapshot, instanceKey: input, productName: 'Fluxer'};
		},
		getStorageScope: () => storageScope,
		resolveAndPrepareRuntimeSnapshot: prepareRuntimeSnapshot,
		prepareRuntimeSnapshot,
		commitRuntimeSnapshot,
		publishRuntimeSnapshot: (committed) => {
			const snapshot = committed.snapshot;
			appliedSnapshots.push(snapshot);
			currentInstance = snapshot;
		},
		rollbackPublishedRuntime: (committed) => {
			currentInstance = committed.previousRuntime?.snapshot ?? null;
		},
		finalizeRuntimeSnapshot: async () => undefined,
		rollbackRuntimeSnapshot: async () => undefined,
		abortRuntimeSnapshot: async () => undefined,
		deactivateRuntime: async () => {
			currentInstance = null;
		},
		allowsCrossInstanceSwitching: () => false,
		activateStorageScope: async (accountKey) => {
			if (activateStorageScopeOverride === undefined) {
				activatedScopes.push(accountKey);
			} else {
				await activateStorageScopeOverride(accountKey);
			}
			storageScope = accountKey ?? UNAUTHENTICATED_APP_STORAGE_SCOPE;
		},
		deleteAccountStorageScope: async (accountKey) => {
			deletedScopes.push(accountKey);
		},
		resetClientState: async (options) => {
			clientStateResets.push(options);
			window.localStorage.clear();
		},
		closeLayers: () => undefined,
		clearSudoToken: () => undefined,
		sendInvisiblePresence: () => undefined,
		cleanupGatewaySession: (reason) => {
			gatewayTeardowns.push(reason);
		},
		resetSyncedUserSettings: async () => undefined,
		captureSyncedUserSettingsCheckpoint: async () => userSettingsCheckpoint,
		restoreSyncedUserSettingsCheckpoint: async () => undefined,
		accountScopedWork: {
			runSuspended: async (_reason, operation) => operation(NOOP_AUTH_SESSION_TRANSITION),
		},
		captureLocalPresenceIntent: () => null,
		restoreLocalPresenceIntent: () => undefined,
		now: () => {
			clock += 1;
			return clock;
		},
		...remainingOverrides,
	};
	return {
		session: createSessionManager(deps),
		accountStorage,
		activatedScopes,
		deletedScopes,
		clientStateResets,
		appliedSnapshots,
		gatewayTeardowns,
		validTokens,
	};
}

function seedCredentialMirror(accountKey: string | null, userId: string | null, token: string | null): void {
	if (accountKey !== null) {
		window.localStorage.setItem(AuthSessionStorageKey.ActiveAccountKey, accountKey);
	}
	if (userId !== null) {
		window.localStorage.setItem(AuthSessionStorageKey.UserId, userId);
	}
	if (token !== null) {
		window.localStorage.setItem(AuthSessionStorageKey.Token, token);
	}
}

function expectCredentialMirrorIntact(userId: string, token: string): void {
	expect(window.localStorage.getItem(AuthSessionStorageKey.UserId)).toBe(userId);
	expect(window.localStorage.getItem(AuthSessionStorageKey.Token)).toBe(token);
}

beforeEach(() => {
	window.localStorage.clear();
});

afterEach(() => {
	vi.restoreAllMocks();
	window.localStorage.clear();
});

describe('H1 the stored-token equality check', () => {
	test('a record whose token diverged from the mirror no longer clears the session', async () => {
		const harness = createHarness();
		harness.accountStorage.seed({
			userId: '100',
			token: 'stale-record-token',
			instance: PRIMARY,
			userData: {username: 'ada', discriminator: '0001'},
		});
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', 'rotated-token');

		await harness.session.initialize();

		expect(harness.session.isAuthenticated).toBe(true);
		expect(harness.session.userId).toBe('100');
		expect(harness.session.token).toBe('rotated-token');
		expect(harness.session.currentAccount?.userData?.username).toBe('ada');
		expectCredentialMirrorIntact('100', 'rotated-token');
	});

	test('a record with no token is skipped rather than treated as a session', async () => {
		const harness = createHarness();
		harness.accountStorage.seed({userId: '100', token: null, instance: PRIMARY});
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', 'mirror-token');

		await harness.session.initialize();

		expect(harness.session.isAuthenticated).toBe(true);
		expect(harness.session.token).toBe('mirror-token');
		expectCredentialMirrorIntact('100', 'mirror-token');
	});

	test('login persists the active pointer before the account record is stashed', async () => {
		const harness = createHarness();
		harness.accountStorage.stashError = new Error('IndexedDB is gone');

		await expect(harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY})).rejects.toThrow(
			'IndexedDB is gone',
		);

		expectCredentialMirrorIntact('100', 'token-100');
		expect(window.localStorage.getItem(AuthSessionStorageKey.ActiveAccountKey)).toBe(`${PRIMARY_KEY}::100`);
		expect(window.localStorage.getItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY)).toBe('1');
	});
});

describe('H2 the load-time instance filter', () => {
	test('a blank current instance no longer drops every stored account', async () => {
		const harness = createHarness({getRuntimeSnapshot: () => BLANK});
		harness.accountStorage.seed({userId: '100', instance: PRIMARY});
		harness.accountStorage.seed({userId: '200', instance: PRIMARY, lastActive: 5});
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', 'token-100');

		await harness.session.initialize();

		expect(harness.session.accounts).toHaveLength(2);
		expect(harness.session.isAuthenticated).toBe(true);
		expect(harness.session.currentAccountKey).toBe(`${PRIMARY_KEY}::100`);
		expectCredentialMirrorIntact('100', 'token-100');
	});

	test('a foreign-instance row is listed rather than dropped, and refused at switch time', async () => {
		const harness = createHarness();
		harness.accountStorage.seed({userId: '100', instance: PRIMARY});
		harness.accountStorage.seed({userId: '200', instance: FOREIGN});
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', 'token-100');
		harness.validTokens.set('token-200', '200');

		await harness.session.initialize();

		expect(harness.session.accounts.map((account) => account.storageKey).sort()).toEqual([
			`${FOREIGN_KEY}::200`,
			`${PRIMARY_KEY}::100`,
		]);
		await expect(harness.session.switchAccount(`${FOREIGN_KEY}::200`)).rejects.toBeInstanceOf(
			AccountInstanceMismatchError,
		);
		expect(harness.gatewayTeardowns).toHaveLength(0);
		expectCredentialMirrorIntact('100', 'token-100');
	});
});

describe('H3 storage inventory authority', () => {
	test('a missing desktop account is never reconstructed from the raw mirror', async () => {
		const harness = createHarness();
		harness.accountStorage.source = 'desktop';
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', 'token-100');

		await harness.session.initialize();

		expect(harness.session.isAuthenticated).toBe(false);
		expect(harness.session.currentAccountKey).toBeNull();
		expectCredentialMirrorIntact('100', 'token-100');
	});

	test('an unresolved IDB pointer never clears the mirror', async () => {
		const harness = createHarness();
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', null);

		await harness.session.initialize();

		expect(harness.session.isAuthenticated).toBe(false);
		expect(window.localStorage.getItem(AuthSessionStorageKey.UserId)).toBe('100');
		expect(window.localStorage.getItem(AuthSessionStorageKey.ActiveAccountKey)).toBe(`${PRIMARY_KEY}::100`);
	});
});

describe('H4 a throwing boot step', () => {
	test('an account-store read failure is observable and leaves the mirror untouched', async () => {
		const harness = createHarness();
		harness.accountStorage.listError = new Error('IndexedDB open timed out');
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', 'token-100');

		await expect(harness.session.initialize()).rejects.toThrow('IndexedDB open timed out');

		expect(harness.session.state).toBe(SessionState.Error);
		expectCredentialMirrorIntact('100', 'token-100');
	});
});

describe('H5 storage that throws on read', () => {
	test('initialization rejects and deletes nothing when the mirror cannot be read', async () => {
		const harness = createHarness({
			credentialMirror: {
				read: () => {
					throw new Error('SecurityError');
				},
				write: () => undefined,
				persist: async () => undefined,
			},
		});
		harness.accountStorage.seed({userId: '100', instance: PRIMARY});
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', 'token-100');

		await expect(harness.session.initialize()).rejects.toThrow('SecurityError');

		expect(harness.session.state).toBe(SessionState.Error);
		expectCredentialMirrorIntact('100', 'token-100');
	});
});

describe('H6 a corrupt active-account key', () => {
	test('an unparseable key is rejected without redirecting the session', async () => {
		const harness = createHarness();
		harness.accountStorage.seed({userId: '100', instance: PRIMARY});
		seedCredentialMirror(' not-a-key', '100', 'token-100');

		await harness.session.initialize();

		expect(harness.session.currentAccountKey).toBeNull();
		expect(harness.session.isAuthenticated).toBe(false);
		expectCredentialMirrorIntact('100', 'token-100');
	});

	test('a key for an account that is gone never selects the most recently active record', async () => {
		const harness = createHarness();
		harness.accountStorage.seed({userId: '100', instance: PRIMARY, lastActive: 1});
		harness.accountStorage.seed({userId: '200', instance: PRIMARY, lastActive: 9});
		seedCredentialMirror(`${PRIMARY_KEY}::999`, null, null);

		await harness.session.initialize();

		expect(harness.session.currentAccountKey).toBeNull();
		expect(window.localStorage.getItem(AuthSessionStorageKey.ActiveAccountKey)).toBe(`${PRIMARY_KEY}::999`);
	});

	test('a pointer whose account is gone never adopts an unrelated account over the raw mirror', async () => {
		const harness = createHarness();
		harness.accountStorage.seed({userId: '200', token: 'token-200', instance: PRIMARY, lastActive: 9});
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', 'token-100');

		await harness.session.initialize();

		expect(harness.session.userId).toBeNull();
		expect(harness.session.token).toBeNull();
		expect(harness.session.isAuthenticated).toBe(false);
		expectCredentialMirrorIntact('100', 'token-100');
	});

	test('a pointer that resolves to nothing at all still leaves the mirror in place', async () => {
		const harness = createHarness();
		seedCredentialMirror(`${PRIMARY_KEY}::100`, '100', null);

		await harness.session.initialize();

		expect(harness.session.isAuthenticated).toBe(false);
		expect(window.localStorage.getItem(AuthSessionStorageKey.UserId)).toBe('100');
	});

	test('no pointer at all is a clean signed-out boot, not a resurrection of the newest record', async () => {
		const harness = createHarness();
		harness.accountStorage.seed({userId: '100', instance: PRIMARY, lastActive: 9});

		await harness.session.initialize();

		expect(harness.session.isAuthenticated).toBe(false);
		expect(harness.session.accounts).toHaveLength(1);
	});
});

describe('H7 a failing scope activation', () => {
	test('a rejecting scope activation never costs the caller its credentials', async () => {
		const harness = createHarness({
			activateStorageScope: async () => {
				throw new Error('scope activation failed');
			},
		});

		await expect(harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY})).rejects.toThrow(
			'scope activation failed',
		);

		expectCredentialMirrorIntact('100', 'token-100');
		expect(harness.accountStorage.records.get(`${PRIMARY_KEY}::100`)?.token).toBe('token-100');
	});
});

describe('H10 logout', () => {
	test('logout clears the pointer, the preboot marker and the account scope', async () => {
		const harness = createHarness();
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		harness.activatedScopes.length = 0;

		await harness.session.logout();

		expect(window.localStorage.getItem(AuthSessionStorageKey.Token)).toBeNull();
		expect(window.localStorage.getItem(AuthSessionStorageKey.UserId)).toBeNull();
		expect(window.localStorage.getItem(AuthSessionStorageKey.ActiveAccountKey)).toBeNull();
		expect(window.localStorage.getItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY)).toBeNull();
		expect(harness.activatedScopes).toEqual([null]);
		expect(harness.clientStateResets).toEqual([{reason: 'logout', keepDrafts: false}]);
		expect(harness.accountStorage.records.size).toBe(0);
	});

	test('a scope wipe that fails still drops the stored credential', async () => {
		const harness = createHarness({
			resetClientState: async () => {
				throw new Error('storage unavailable');
			},
		});
		harness.validTokens.set('token-100', '100');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});

		await expect(harness.session.logout()).rejects.toThrow(/storage unavailable/u);

		expect(harness.accountStorage.records.size).toBe(0);
		expect(harness.session.getAccount(`${PRIMARY_KEY}::100`)).toBeNull();
	});
});

describe('H11 sticky account validity', () => {
	test('stashing the current account does not resurrect an invalidated account', async () => {
		const harness = createHarness();
		harness.validTokens.set('token-100', '100');
		harness.validTokens.set('token-200', '200');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		await harness.session.login({token: 'token-200', userId: '200', runtimeSnapshot: PRIMARY});
		harness.session.markAccountInvalid(`${PRIMARY_KEY}::200`);

		await harness.session.stashCurrentAccount();

		expect(harness.session.getAccount(`${PRIMARY_KEY}::200`)?.isValid).toBe(false);
		expectCredentialMirrorIntact('200', 'token-200');
	});
});

describe('R14 the switch transaction', () => {
	test('a failed validation is a no-op: the gateway stays up and no scope is activated', async () => {
		const harness = createHarness();
		harness.validTokens.set('token-100', '100');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		harness.accountStorage.seed({userId: '200', token: 'token-200', instance: PRIMARY});
		await harness.session.reset();
		await harness.session.initialize();
		harness.activatedScopes.length = 0;
		harness.appliedSnapshots.length = 0;
		harness.gatewayTeardowns.length = 0;
		harness.accountStorage.stashes.length = 0;

		await expect(harness.session.switchAccount(`${PRIMARY_KEY}::200`)).rejects.toBeInstanceOf(SessionExpiredError);

		expect(harness.gatewayTeardowns).toHaveLength(0);
		expect(harness.activatedScopes).toHaveLength(0);
		expect(harness.accountStorage.stashes).toHaveLength(0);
		expect(harness.session.currentAccountKey).toBe(`${PRIMARY_KEY}::100`);
		expect(harness.session.getAccount(`${PRIMARY_KEY}::200`)?.isValid).toBe(false);
		expectCredentialMirrorIntact('100', 'token-100');
	});

	test('the target keeps its own instance snapshot and its own scope across a cross-instance switch', async () => {
		const harness = createHarness({allowsCrossInstanceSwitching: () => true});
		harness.validTokens.set('token-100', '100');
		harness.validTokens.set('token-200', '200');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		harness.accountStorage.seed({userId: '200', token: 'token-200', instance: FOREIGN});
		await harness.session.reset();
		await harness.session.initialize();
		harness.activatedScopes.length = 0;
		harness.appliedSnapshots.length = 0;

		await harness.session.switchAccount(`${FOREIGN_KEY}::200`);

		expect(harness.accountStorage.records.get(`${FOREIGN_KEY}::200`)?.instance?.apiEndpoint).toBe(FOREIGN.apiEndpoint);
		expect(harness.accountStorage.records.get(`${PRIMARY_KEY}::100`)?.instance?.apiEndpoint).toBe(PRIMARY.apiEndpoint);
		expect(harness.activatedScopes).toEqual([`${FOREIGN_KEY}::200`]);
		expect(harness.appliedSnapshots.map((snapshot) => snapshot.apiEndpoint)).toEqual([FOREIGN.apiEndpoint]);
		expect(harness.session.currentAccountKey).toBe(`${FOREIGN_KEY}::200`);
		expectCredentialMirrorIntact('200', 'token-200');
	});

	test('the target record is stashed only after the mirror already names the target', async () => {
		const harness = createHarness();
		harness.validTokens.set('token-100', '100');
		harness.validTokens.set('token-200', '200');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		harness.accountStorage.seed({userId: '200', token: 'token-200', instance: PRIMARY});
		await harness.session.reset();
		await harness.session.initialize();
		harness.accountStorage.stashes.length = 0;

		await harness.session.switchAccount(`${PRIMARY_KEY}::200`);

		expect(harness.accountStorage.stashes.map((call) => [call.userId, call.mirroredUserId])).toEqual([
			['100', '100'],
			['200', '200'],
		]);
	});

	test('the switch stashes the source account under the source instance', async () => {
		const harness = createHarness({allowsCrossInstanceSwitching: () => true});
		harness.validTokens.set('token-100', '100');
		harness.validTokens.set('token-200', '200');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		harness.accountStorage.seed({userId: '200', token: 'token-200', instance: FOREIGN});
		await harness.session.reset();
		await harness.session.initialize();
		harness.accountStorage.stashes.length = 0;

		await harness.session.switchAccount(`${FOREIGN_KEY}::200`);

		expect(harness.accountStorage.stashes.map((call) => [call.userId, call.instance?.apiEndpoint])).toEqual([
			['100', PRIMARY.apiEndpoint],
			['200', FOREIGN.apiEndpoint],
		]);
	});
});

describe('the active account pointer', () => {
	test('persisting it writes the account key, both raw mirror keys and the preboot marker', async () => {
		const harness = createHarness();

		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});

		expect(window.localStorage.getItem(AuthSessionStorageKey.ActiveAccountKey)).toBe(`${PRIMARY_KEY}::100`);
		expect(window.localStorage.getItem(AuthSessionStorageKey.Token)).toBe('token-100');
		expect(window.localStorage.getItem(AuthSessionStorageKey.UserId)).toBe('100');
		expect(window.localStorage.getItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY)).toBe('1');
	});

	test('a 4004 close clears the pointer and the preboot marker', async () => {
		const harness = createHarness();
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});

		await harness.session.handleConnectionClosed(4004);

		expect(window.localStorage.getItem(AuthSessionStorageKey.Token)).toBeNull();
		expect(window.localStorage.getItem(AuthSessionStorageKey.UserId)).toBeNull();
		expect(window.localStorage.getItem(AuthSessionStorageKey.ActiveAccountKey)).toBeNull();
		expect(window.localStorage.getItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY)).toBeNull();
	});

	test('a token rotation refreshes the mirror without moving the account key', async () => {
		const harness = createHarness();
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});

		await harness.session.setToken('rotated-token');

		expect(window.localStorage.getItem(AuthSessionStorageKey.Token)).toBe('rotated-token');
		expect(window.localStorage.getItem(AuthSessionStorageKey.ActiveAccountKey)).toBe(`${PRIMARY_KEY}::100`);
		expect(window.localStorage.getItem(AuthSessionStorageKey.UserId)).toBe('100');
	});
});

describe('account keys', () => {
	test('two accounts sharing a user id on different instances are both addressable', async () => {
		const harness = createHarness();
		harness.accountStorage.seed({userId: '100', token: 'token-primary', instance: PRIMARY, lastActive: 1});
		harness.accountStorage.seed({userId: '100', token: 'token-foreign', instance: FOREIGN, lastActive: 5});
		seedCredentialMirror(`${FOREIGN_KEY}::100`, '100', 'token-foreign');

		await harness.session.initialize();

		expect(harness.session.accounts).toHaveLength(2);
		expect(harness.session.currentAccountKey).toBe(`${FOREIGN_KEY}::100`);
		expect(harness.session.getAccount(`${PRIMARY_KEY}::100`)?.token).toBe('token-primary');
	});

	test('accounts stay sorted by last activity, newest first', async () => {
		const harness = createHarness();
		harness.accountStorage.seed({userId: '100', instance: PRIMARY, lastActive: 1});
		harness.accountStorage.seed({userId: '200', instance: PRIMARY, lastActive: 9});
		harness.accountStorage.seed({userId: '300', instance: PRIMARY, lastActive: 5});

		await harness.session.initialize();

		expect(harness.session.accounts.map((account) => account.userId)).toEqual(['200', '300', '100']);
	});
});

describe('H12 in-flight work from the outgoing account', () => {
	test('a response resolving after the switch is refused and leaves the mirror naming the incoming account', async () => {
		let releaseResponse = (): void => undefined;
		let responseIsInFlight = false;
		const response = new Promise<void>((resolve) => {
			releaseResponse = resolve;
		});
		const applied: Array<string> = [];
		const harness = createHarness({
			accountScopedWork: {
				runSuspended: (reason, operation) =>
					AccountScopedWork.runSuspended(reason, () => operation(NOOP_AUTH_SESSION_TRANSITION)),
			},
		});
		const unregisterRelease = AccountScopedWork.registerCancellation(() => {
			if (responseIsInFlight) {
				releaseResponse();
			}
		});
		try {
			harness.validTokens.set('token-100', '100');
			harness.validTokens.set('token-200', '200');
			await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
			harness.accountStorage.seed({userId: '200', token: 'token-200', instance: PRIMARY});
			await harness.session.reset();
			await harness.session.initialize();

			const ticket = AccountScopedWork.begin();
			responseIsInFlight = true;
			const outcome = (async () => {
				try {
					await response;
					ticket.assertCurrent();
					applied.push(AppStorage.getItem(AuthSessionStorageKey.UserId) ?? 'none');
					return 'applied';
				} finally {
					ticket.dispose();
				}
			})().catch((error: Error) => error.name);

			await harness.session.switchAccount(`${PRIMARY_KEY}::200`);

			await expect(outcome).resolves.toBe('AbortError');
			expect(applied).toEqual([]);
			expect(ticket.isStale).toBe(true);
			expectCredentialMirrorIntact('200', 'token-200');
		} finally {
			unregisterRelease();
		}
	});
});

describe('account scoped work fences', () => {
	function fenceLog(): {steps: Array<string>; overrides: Partial<AuthSessionDependencies>} {
		const steps: Array<string> = [];
		return {
			steps,
			overrides: {
				accountScopedWork: {
					runSuspended: async (reason, operation) => {
						steps.push(`cancel:${reason}`);
						steps.push('idle');
						try {
							return await operation(NOOP_AUTH_SESSION_TRANSITION);
						} finally {
							steps.push(`resume:${reason}`);
						}
					},
				},
				activateStorageScope: async (accountKey) => {
					steps.push(`scope:${accountKey ?? 'null'}`);
				},
			},
		};
	}

	test('a switch cancels and drains account scoped work before the storage scope swaps', async () => {
		const {steps, overrides} = fenceLog();
		const harness = createHarness(overrides);
		harness.validTokens.set('token-100', '100');
		harness.validTokens.set('token-200', '200');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		harness.accountStorage.seed({userId: '200', token: 'token-200', instance: PRIMARY});
		await harness.session.reset();
		await harness.session.initialize();
		steps.length = 0;

		await harness.session.switchAccount(`${PRIMARY_KEY}::200`);

		expect(steps).toEqual(['cancel:account-switch', 'idle', `scope:${PRIMARY_KEY}::200`, 'resume:account-switch']);
	});

	test('a switch that fails validation neither cancels work nor swaps the scope', async () => {
		const {steps, overrides} = fenceLog();
		const harness = createHarness(overrides);
		harness.validTokens.set('token-100', '100');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		harness.accountStorage.seed({userId: '200', token: 'token-200', instance: PRIMARY});
		await harness.session.reset();
		await harness.session.initialize();
		steps.length = 0;

		await expect(harness.session.switchAccount(`${PRIMARY_KEY}::200`)).rejects.toBeInstanceOf(SessionExpiredError);

		expect(steps).toEqual([]);
	});

	test('logout fences before the client state reset and the scope teardown', async () => {
		const {steps, overrides} = fenceLog();
		const harness = createHarness({
			...overrides,
			resetClientState: async () => {
				steps.push('reset');
			},
		});
		harness.validTokens.set('token-100', '100');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		steps.length = 0;

		await harness.session.logout();

		expect(steps).toEqual(['cancel:logout', 'idle', 'reset', 'scope:null', 'resume:logout']);
	});

	test('removing the current account fences, removing another account does not', async () => {
		const {steps, overrides} = fenceLog();
		const harness = createHarness(overrides);
		harness.validTokens.set('token-100', '100');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		harness.accountStorage.seed({userId: '200', token: 'token-200', instance: PRIMARY});
		await harness.session.reset();
		await harness.session.initialize();
		steps.length = 0;

		await harness.session.removeAccount(`${PRIMARY_KEY}::200`);
		expect(steps).toEqual([]);

		await harness.session.removeAccount(`${PRIMARY_KEY}::100`);
		expect(steps).toEqual(['cancel:logout', 'idle', 'scope:null', 'resume:logout']);
	});

	test('work is resumed even when the switch throws after the point of no return', async () => {
		const {steps, overrides} = fenceLog();
		const harness = createHarness(overrides);
		harness.validTokens.set('token-100', '100');
		harness.validTokens.set('token-200', '200');
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		harness.accountStorage.seed({userId: '200', token: 'token-200', instance: PRIMARY});
		await harness.session.reset();
		await harness.session.initialize();
		harness.accountStorage.records.delete(`${PRIMARY_KEY}::200`);
		steps.length = 0;

		await expect(harness.session.switchAccount(`${PRIMARY_KEY}::200`)).rejects.toThrow();

		expect(steps).toEqual(['cancel:account-switch', 'idle', 'resume:account-switch']);
	});
});

describe('R18 a login that names another instance', () => {
	test('files the account under the named instance, not the one the app is running', async () => {
		const harness = createHarness();

		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: FOREIGN});

		expect(harness.session.currentAccountKey).toBe(`${FOREIGN_KEY}::100`);
		expect(harness.session.currentAccount?.instance).toEqual(FOREIGN);
		expect(harness.accountStorage.stashes.at(-1)?.instance).toEqual(FOREIGN);
		expect(harness.accountStorage.records.get(`${FOREIGN_KEY}::100`)?.instance).toEqual(FOREIGN);
		expect(harness.accountStorage.records.has(`${PRIMARY_KEY}::100`)).toBe(false);
		expect(harness.activatedScopes.at(-1)).toBe(`${FOREIGN_KEY}::100`);
		expect(window.localStorage.getItem(AuthSessionStorageKey.ActiveAccountKey)).toBe(`${FOREIGN_KEY}::100`);
	});

	test('repoints the runtime at the named instance so the session that follows talks to it', async () => {
		const harness = createHarness();

		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: FOREIGN});

		expect(harness.appliedSnapshots).toEqual([FOREIGN]);
	});

	test('the first account activates its explicit runtime after a zero-account boot', async () => {
		const harness = createHarness();

		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});

		expect(harness.session.currentAccountKey).toBe(`${PRIMARY_KEY}::100`);
		expect(harness.accountStorage.stashes.at(-1)?.instance).toEqual(PRIMARY);
		expect(harness.appliedSnapshots).toEqual([PRIMARY]);
	});

	test('a second account on another instance does not collide with the first instance key', async () => {
		const harness = createHarness();
		harness.validTokens.set('token-100', '100');

		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		await harness.session.login({token: 'token-100-elsewhere', userId: '100', runtimeSnapshot: FOREIGN});

		expect(harness.session.accounts.map((account) => account.storageKey).sort()).toEqual([
			`${FOREIGN_KEY}::100`,
			`${PRIMARY_KEY}::100`,
		]);
		expect(harness.session.getAccount(`${PRIMARY_KEY}::100`)?.token).toBe('token-100');
		expect(harness.session.getAccount(`${FOREIGN_KEY}::100`)?.token).toBe('token-100-elsewhere');
	});
});

describe('logging out of the active account while its screens are still mounted', () => {
	const platforms = [
		{name: 'web', desktop: false},
		{name: 'desktop', desktop: true},
	] as const;
	const scenarios = [
		{name: 'with another account present', otherAccount: true},
		{name: 'with no other account', otherAccount: false},
	] as const;

	afterEach(() => {
		configure({disableErrorBoundaries: false});
		desktopRuntime.required = false;
		RuntimeConfig.applySnapshot(PRIMARY);
	});

	function mountAuthenticatedReaders(): {reads: Array<string>; dispose: () => void} {
		const reads: Array<string> = [];
		const dispose = autorun(() => {
			reads.push(
				`${RuntimeConfig.singleCommunityEnabled}:${RuntimeConfig.features.self_hosted}:${RuntimeConfig.getSnapshot().apiEndpoint}`,
			);
		});
		return {reads, dispose};
	}

	async function signIn(desktop: boolean, otherAccount: boolean) {
		const steps: Array<string> = [];
		const harness = createHarness({
			deactivateRuntime: async () => {
				steps.push('deactivate');
				await RuntimeConfig.deactivate();
			},
			closeLayers: () => {
				steps.push('close-layers');
			},
			accountScopedWork: {
				runSuspended: async (_reason, operation) =>
					operation({
						notifyLogout: async () => {
							steps.push('notify-logout');
						},
					}),
			},
		});
		harness.validTokens.set('token-100', '100');
		harness.validTokens.set('token-200', '200');
		if (otherAccount) {
			await harness.session.login({token: 'token-200', userId: '200', runtimeSnapshot: PRIMARY});
		}
		await harness.session.login({token: 'token-100', userId: '100', runtimeSnapshot: PRIMARY});
		desktopRuntime.required = desktop;
		const nativeDeactivate = vi.spyOn(DesktopRuntimeTransactions, 'deactivate').mockResolvedValue(undefined);
		configure({disableErrorBoundaries: true});
		return {harness, steps, nativeDeactivate};
	}

	function expectRuntimeAfterSignOut(desktop: boolean, reads: Array<string>): void {
		expect(reads.at(-1)).toBe(`false:false:${PRIMARY_KEY}`);
		expect(RuntimeConfig.getSnapshotOrNull()?.apiEndpoint ?? null).toBe(desktop ? null : PRIMARY_KEY);
	}

	for (const platform of platforms) {
		for (const scenario of scenarios) {
			test(`${platform.name} sign-out ${scenario.name} completes and the mounted readers keep working`, async () => {
				const {harness, steps, nativeDeactivate} = await signIn(platform.desktop, scenario.otherAccount);
				const {reads, dispose} = mountAuthenticatedReaders();
				try {
					await harness.session.logout();
				} finally {
					dispose();
				}

				expect(harness.session.isAuthenticated).toBe(false);
				expect(harness.session.getAccount(`${PRIMARY_KEY}::100`)).toBeNull();
				expect(harness.session.accounts.map((account) => account.storageKey)).toEqual(
					scenario.otherAccount ? [`${PRIMARY_KEY}::200`] : [],
				);
				expect(steps).toEqual(['deactivate', 'close-layers', 'notify-logout']);
				expect(nativeDeactivate).toHaveBeenCalledTimes(platform.desktop ? 1 : 0);
				expectRuntimeAfterSignOut(platform.desktop, reads);
			});

			test(`${platform.name} removing the current account ${scenario.name} completes`, async () => {
				const {harness, steps} = await signIn(platform.desktop, scenario.otherAccount);
				const {reads, dispose} = mountAuthenticatedReaders();
				try {
					await harness.session.removeAccount(`${PRIMARY_KEY}::100`);
				} finally {
					dispose();
				}

				expect(harness.session.isAuthenticated).toBe(false);
				expect(harness.session.accounts).toHaveLength(scenario.otherAccount ? 1 : 0);
				expect(steps).toEqual(['deactivate', 'close-layers']);
				expectRuntimeAfterSignOut(platform.desktop, reads);
			});

			test(`${platform.name} server revocation ${scenario.name} invalidates without breaking the readers`, async () => {
				const {harness} = await signIn(platform.desktop, scenario.otherAccount);
				const {reads, dispose} = mountAuthenticatedReaders();
				let result: Awaited<ReturnType<AuthSessionManager['handleConnectionClosed']>>;
				try {
					result = await harness.session.handleConnectionClosed(4004);
				} finally {
					dispose();
				}

				expect(result.invalidatedCurrentSession).toBe(true);
				expect(harness.session.getAccount(`${PRIMARY_KEY}::100`)?.isValid).toBe(false);
				expect(harness.session.accounts).toHaveLength(scenario.otherAccount ? 2 : 1);
				expectRuntimeAfterSignOut(platform.desktop, reads);
			});
		}
	}
});
