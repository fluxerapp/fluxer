// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	FIXTURE_CURRENT_INSTANCE_KEY,
	FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	FIXTURE_NOW,
} from '@app/features/auth/state/__fixtures__/AccountRecordFixtures';
import {
	createFakeDesktopStore,
	type FakeDesktopStore,
} from '@app/features/platform/state/__fixtures__/DesktopStoreFixture';
import {
	CONTENT_STORAGE_KEYS,
	GOLDEN_DEPLOYED_STORAGE_ENTRIES,
	GOLDEN_LOCAL_STORAGE_CORPUS,
	OWNED_CONTENT_STORAGE_KEYS,
	SHARED_CONTENT_STORAGE_KEYS,
} from '@app/features/platform/state/__fixtures__/LegacyStorageFixtures';
import {
	readPersistedStoreNames,
	STORES_WITHOUT_DEPLOYED_DATA,
} from '@app/features/platform/state/__fixtures__/PersistedStoreNames';
import {installRuntimeBootstrap} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import type {AppStorageSessionAccount} from '@app/features/platform/state/AppStorageBootstrap';
import {
	AppStorageKey,
	LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY,
	LEGACY_SHARED_CONTENT_REVIEW_KEY,
	LegacySharedContentReviewState,
} from '@app/features/platform/state/AppStorageKeys';
import {APP_STORAGE_INDEXED_DB_NAME} from '@app/features/platform/state/PersistentStorageBackend';
import {getProtectedIndexedDB} from '@app/features/platform/state/ProtectedWebStorage';
import type {ElectronAPI} from '@app/types/electron.d';
import type {DesktopLegacyHarvest} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {
	DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY,
	DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {makeAutoObservable, runInAction} from 'mobx';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

type PersistentStorageModule = typeof import('@app/features/platform/state/PersistentStorage');
type PersistenceModule = typeof import('@app/features/platform/utils/MobXPersistence');

interface LoadedClient {
	readonly storage: PersistentStorageModule;
	readonly persistence: PersistenceModule;
	readonly drafts: PersistedDrafts;
	readonly desktopBackendInstalled: boolean;
}

const ACTIVE_USER_ID = '100000000000000002';
const OTHER_USER_ID = '100000000000000003';
const ACTIVE_SCOPE = `${FIXTURE_CURRENT_INSTANCE_KEY}::${ACTIVE_USER_ID}`;
const OTHER_SCOPE = `${FIXTURE_CURRENT_INSTANCE_KEY}::${OTHER_USER_ID}`;
const ADDED_SINCE_SCOPE = `${FIXTURE_CURRENT_INSTANCE_KEY}::100000000000000009`;
const DEPLOYED_DRAFTS: Readonly<Record<string, string>> = {'110000000000000001': 'unsent message text'};
const GLOBAL_SCOPE = 'global';
const ACTIVE_TOKEN = GOLDEN_LOCAL_STORAGE_CORPUS.find((entry) => entry.key === 'token')!.value;
const LOADS_AFTER_THE_UPGRADE = 3;

const OTHER_ACCOUNT_OWNED_VALUES: Readonly<Record<string, string>> = {
	[AppStorageKey.UI_SIDEBAR_WIDTH]: '331',
	'fluxer:media_player:volume': '0.25',
	'fluxer_scheduled_maintenance_dismissed:4242': '1',
};

const ACTIVE_ACCOUNT_STALE_VALUES: Readonly<Record<string, string>> = {
	[AppStorageKey.UI_SIDEBAR_WIDTH]: '205',
};

const GOLDEN_VALUES: ReadonlyMap<string, string> = new Map(
	GOLDEN_LOCAL_STORAGE_CORPUS.map((entry) => [entry.key, entry.value]),
);

const DEPLOYED_STORE_NAMES: ReadonlyArray<string> = readPersistedStoreNames().filter(
	(name) => !STORES_WITHOUT_DEPLOYED_DATA.has(name),
);

class PersistedDrafts {
	drafts: Record<string, string> = {};
	draftSegments: Record<string, unknown> = {};

	constructor() {
		makeAutoObservable(this);
	}
}

function accountRecord(userId: string, ownedValues: Readonly<Record<string, string>>): Record<string, unknown> {
	const snapshot = {userId, token: `token-${userId}`, ...ownedValues};
	return {
		userId,
		token: userId === ACTIVE_USER_ID ? ACTIVE_TOKEN : `token-${userId}`,
		userData: {id: userId, username: `user-${userId}`, discriminator: '0001', avatar: null},
		localStorageData: snapshot,
		managedStorageData: snapshot,
		lastActive: FIXTURE_NOW - 1_000,
		instance: FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	};
}

function deployedAccountRecords(): Array<Record<string, unknown>> {
	return [
		accountRecord(ACTIVE_USER_ID, ACTIVE_ACCOUNT_STALE_VALUES),
		accountRecord(OTHER_USER_ID, OTHER_ACCOUNT_OWNED_VALUES),
	];
}

function goldenCorpusRecord(): Record<string, string> {
	return Object.fromEntries(GOLDEN_VALUES);
}

function activeSessionAccount(): AppStorageSessionAccount {
	return {
		accountKey: ACTIVE_SCOPE,
		userId: ACTIVE_USER_ID,
		token: ACTIVE_TOKEN,
		instance: FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	};
}

function deleteDatabase(name: string): Promise<void> {
	return new Promise<void>((resolve, reject) => {
		const factory = getProtectedIndexedDB();
		if (factory == null) {
			resolve();
			return;
		}
		const request = factory.deleteDatabase(name);
		request.onsuccess = () => resolve();
		request.onerror = () => reject(request.error ?? new Error(`deleteDatabase ${name} failed`));
		request.onblocked = () => resolve();
	});
}

function seedDeployedWebAccounts(): Promise<void> {
	return new Promise<void>((resolve, reject) => {
		const request = window.indexedDB.open('FluxerAccounts', 2);
		request.onupgradeneeded = () => {
			request.result.createObjectStore('accounts', {keyPath: 'userId'}).createIndex('lastActive', 'lastActive');
		};
		request.onerror = () => reject(request.error ?? new Error('open FluxerAccounts failed'));
		request.onsuccess = () => {
			const database = request.result;
			const transaction = database.transaction('accounts', 'readwrite');
			for (const record of deployedAccountRecords()) {
				transaction.objectStore('accounts').put(record);
			}
			transaction.oncomplete = () => {
				database.close();
				resolve();
			};
			transaction.onerror = () => reject(transaction.error ?? new Error('seeding FluxerAccounts failed'));
		};
	});
}

function seedDeployedLocalStorage(): void {
	for (const [key, value] of GOLDEN_VALUES) {
		window.localStorage.setItem(key, value);
	}
	window.localStorage.setItem('userId', ACTIVE_USER_ID);
}

function installDeployedDesktop(store: FakeDesktopStore): void {
	const harvest: DesktopLegacyHarvest = {
		version: 1,
		origin: 'https://web.canary.fluxer.app',
		capturedAt: FIXTURE_NOW - 10_000,
		localStorage: {...goldenCorpusRecord(), userId: ACTIVE_USER_ID},
		stores: [
			{
				database: 'FluxerAccounts',
				version: 2,
				store: 'accounts',
				records: deployedAccountRecords().map((value) => ({key: null, value})),
			},
		],
		serviceWorkersUnregistered: 0,
		cachesDeleted: 0,
		truncated: [],
	};
	let staged: DesktopLegacyHarvest | null = harvest;
	let replanted = false;
	const api = {
		desktopAccounts: store.accounts,
		desktopStorage: store.storage,
		desktopLegacyHarvest: {
			read: () => Promise.resolve(replanted ? null : staged),
			markReplanted: () => {
				replanted = true;
				return Promise.resolve();
			},
			discard: () => {
				staged = null;
				return Promise.resolve();
			},
		},
	};
	Object.defineProperty(window, 'electron', {value: api as unknown as ElectronAPI, configurable: true, writable: true});
	for (const [key, value] of Object.entries(harvest.localStorage)) {
		window.localStorage.setItem(key, value);
	}
	window.localStorage.setItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY, DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE);
}

async function loadClient(): Promise<LoadedClient> {
	vi.resetModules();
	const storage: PersistentStorageModule = await import('@app/features/platform/state/PersistentStorage');
	const bootstrap = await import('@app/features/platform/state/AppStorageBootstrap');
	const persistence: PersistenceModule = await import('@app/features/platform/utils/MobXPersistence');
	const handle = await bootstrap.startAppStorage({scoped: true});
	const result = await handle.finalizeAfterSessionResolution(activeSessionAccount());
	expect(result.scope).toBe(ACTIVE_SCOPE);
	const drafts = new PersistedDrafts();
	await persistence.makePersistent(drafts, AppStorageKey.MESSAGING_DRAFTS, ['drafts', 'draftSegments']);
	return {storage, persistence, drafts, desktopBackendInstalled: result.desktopBackendInstalled};
}

async function unloadClient(client: LoadedClient): Promise<void> {
	client.persistence.flushPendingPersistWrites();
	await client.storage.flushAppStorageWrites();
	client.persistence.stopPersistent(AppStorageKey.MESSAGING_DRAFTS, client.drafts);
}

function expectActiveAccountState(client: LoadedClient, label: string): void {
	const AppStorage = client.storage.default;
	for (const entry of GOLDEN_DEPLOYED_STORAGE_ENTRIES) {
		if (entry.destination === 'raw' || entry.destination === 'named-account') {
			continue;
		}
		expect(AppStorage.getItem(entry.key), `${label}: ${entry.key}`).toBe(entry.value);
	}
	for (const name of DEPLOYED_STORE_NAMES) {
		expect(AppStorage.getItem(name), `${label}: store ${name}`).toBe(GOLDEN_VALUES.get(name));
	}
	expect(AppStorage.getItem(AppStorageKey.UI_SIDEBAR_WIDTH), label).toBe('286');
	expect(client.drafts.drafts, label).toEqual(DEPLOYED_DRAFTS);
}

function expectOtherAccountState(client: LoadedClient, label: string): void {
	const AppStorage = client.storage.default;
	for (const entry of GOLDEN_DEPLOYED_STORAGE_ENTRIES) {
		if (entry.destination === 'global' || entry.destination === 'every-account') {
			expect(AppStorage.getItem(entry.key), `${label}: ${entry.key}`).toBe(entry.value);
		}
		if (entry.destination === 'named-account') {
			expect(AppStorage.getItem(entry.key), `${label}: ${entry.key}`).toBe(entry.value);
		}
	}
	for (const key of OWNED_CONTENT_STORAGE_KEYS) {
		expect(AppStorage.getItem(key), `${label}: ${key}`).toBe(OTHER_ACCOUNT_OWNED_VALUES[key] ?? null);
	}
	for (const key of SHARED_CONTENT_STORAGE_KEYS) {
		expect(AppStorage.getItem(key), `${label}: ${key}`).toBe(GOLDEN_VALUES.get(key));
	}
	for (const [key, value] of Object.entries(OTHER_ACCOUNT_OWNED_VALUES)) {
		expect(AppStorage.getItem(key), `${label}: ${key}`).toBe(value);
	}
	expect(client.drafts.drafts, label).toEqual(DEPLOYED_DRAFTS);
}

function expectNoLegacyContent(client: LoadedClient, label: string): void {
	for (const key of CONTENT_STORAGE_KEYS) {
		expect(client.storage.default.getItem(key), `${label}: ${key}`).toBeNull();
	}
	expect(client.drafts.drafts, label).toEqual({});
}

async function expectContinuityAcrossLoads(expectDesktopBackend: boolean): Promise<void> {
	for (let load = 1; load <= LOADS_AFTER_THE_UPGRADE; load++) {
		const client = await loadClient();
		expect(client.desktopBackendInstalled, `load ${load}`).toBe(expectDesktopBackend);
		expectActiveAccountState(client, `load ${load}`);

		await client.storage.activateAppStorageScope(OTHER_SCOPE);
		expectOtherAccountState(client, `load ${load} after switching account`);

		await client.storage.activateAppStorageScope(ADDED_SINCE_SCOPE);
		expectNoLegacyContent(client, `load ${load} in an account added since the upgrade`);

		await client.storage.activateAppStorageScope(ACTIVE_SCOPE);
		expectActiveAccountState(client, `load ${load} after switching back`);
		await unloadClient(client);
	}
}

async function expectSharedContentReviewOnce(): Promise<void> {
	const first = await loadClient();
	expect(first.storage.default.getItem(LEGACY_SHARED_CONTENT_REVIEW_KEY)).toBe(LegacySharedContentReviewState.PENDING);
	first.storage.default.setItem(LEGACY_SHARED_CONTENT_REVIEW_KEY, LegacySharedContentReviewState.DONE);
	await first.storage.activateAppStorageScope(ADDED_SINCE_SCOPE);
	expect(first.storage.default.getItem(LEGACY_SHARED_CONTENT_REVIEW_KEY)).toBeNull();
	await first.storage.activateAppStorageScope(OTHER_SCOPE);
	expect(first.storage.default.getItem(LEGACY_SHARED_CONTENT_REVIEW_KEY)).toBe(LegacySharedContentReviewState.PENDING);
	first.storage.default.setItem(LEGACY_SHARED_CONTENT_REVIEW_KEY, LegacySharedContentReviewState.DONE);
	await unloadClient(first);

	const second = await loadClient();
	expect(second.storage.default.getItem(LEGACY_SHARED_CONTENT_REVIEW_KEY)).toBe(LegacySharedContentReviewState.DONE);
	await second.storage.activateAppStorageScope(OTHER_SCOPE);
	expect(second.storage.default.getItem(LEGACY_SHARED_CONTENT_REVIEW_KEY)).toBe(LegacySharedContentReviewState.DONE);
	await second.storage.activateAppStorageScope(ADDED_SINCE_SCOPE);
	expect(second.storage.default.getItem(LEGACY_SHARED_CONTENT_REVIEW_KEY)).toBeNull();
	await unloadClient(second);
}

installRuntimeBootstrap();

beforeEach(async () => {
	Object.defineProperty(navigator, 'locks', {
		value: {request: <T>(_name: string, callback: () => Promise<T>): Promise<T> => callback()},
		configurable: true,
		writable: true,
	});
	window.localStorage.clear();
	Reflect.deleteProperty(window, 'electron');
	for (const name of [APP_STORAGE_INDEXED_DB_NAME, 'FluxerAccounts']) {
		await deleteDatabase(name);
	}
});

afterEach(() => {
	Object.defineProperty(navigator, 'locks', {value: null, configurable: true, writable: true});
	Reflect.deleteProperty(window, 'electron');
});

describe('a deployed profile after the upgrade', () => {
	test('on the web every store reads its own key on each later load and after an account switch', async () => {
		await seedDeployedWebAccounts();
		seedDeployedLocalStorage();

		await expectContinuityAcrossLoads(false);
	});

	test('on desktop the replanted profile reads the same through the desktop store', async () => {
		const store = createFakeDesktopStore();
		installDeployedDesktop(store);

		await expectContinuityAcrossLoads(true);

		const rows = new Map(store.snapshot().entries.map(([scope, key, value]) => [`${scope} ${key}`, value]));
		expect(rows.get(`${ACTIVE_SCOPE} ${AppStorageKey.MESSAGING_DRAFTS}`)).toBe(GOLDEN_VALUES.get('Drafts'));
		expect(rows.get(`${OTHER_SCOPE} ${AppStorageKey.MESSAGING_DRAFTS}`)).toBe(GOLDEN_VALUES.get('Drafts'));
		for (const key of SHARED_CONTENT_STORAGE_KEYS) {
			expect(rows.get(`${OTHER_SCOPE} ${key}`), key).toBe(GOLDEN_VALUES.get(key));
			expect(rows.has(`${ADDED_SINCE_SCOPE} ${key}`), key).toBe(false);
		}
		expect(rows.get(`${OTHER_SCOPE} ${AppStorageKey.UI_SIDEBAR_WIDTH}`)).toBe('331');
		expect(rows.has(`${GLOBAL_SCOPE} ${LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY}`)).toBe(true);
	});

	test('a value changed after the upgrade is what every later load reads, in the account that changed it', async () => {
		await seedDeployedWebAccounts();
		seedDeployedLocalStorage();
		const first = await loadClient();
		runInAction(() => {
			first.drafts.drafts = {'110000000000000001': 'edited after the upgrade'};
		});
		first.storage.default.setItem(AppStorageKey.UI_SIDEBAR_WIDTH, '300');
		await unloadClient(first);

		const second = await loadClient();

		expect(second.drafts.drafts).toEqual({'110000000000000001': 'edited after the upgrade'});
		expect(second.storage.default.getItem(AppStorageKey.UI_SIDEBAR_WIDTH)).toBe('300');
		await second.storage.activateAppStorageScope(OTHER_SCOPE);
		expect(second.drafts.drafts).toEqual(DEPLOYED_DRAFTS);
		expect(second.storage.default.getItem(AppStorageKey.UI_SIDEBAR_WIDTH)).toBe('331');
		await unloadClient(second);
	});

	test('each stored account keeps its own drafts, location and inbox from its first activation on', async () => {
		await seedDeployedWebAccounts();
		seedDeployedLocalStorage();
		const first = await loadClient();
		await first.storage.activateAppStorageScope(OTHER_SCOPE);
		for (const key of ['Drafts', 'Location', 'Inbox', 'SelectedChannel', 'SelectedGuild']) {
			expect(first.storage.default.getItem(key), key).toBe(GOLDEN_VALUES.get(key));
		}
		runInAction(() => {
			first.drafts.drafts = {'110000000000000001': 'written by the second account'};
		});
		first.storage.default.setItem('Location', '{"lastLocation":"/channels/@me/220000000000000002"}');
		await unloadClient(first);

		const second = await loadClient();

		expect(second.drafts.drafts).toEqual(DEPLOYED_DRAFTS);
		expect(second.storage.default.getItem('Location')).toBe(GOLDEN_VALUES.get('Location'));
		await second.storage.activateAppStorageScope(OTHER_SCOPE);
		expect(second.drafts.drafts).toEqual({'110000000000000001': 'written by the second account'});
		expect(second.storage.default.getItem('Location')).toBe('{"lastLocation":"/channels/@me/220000000000000002"}');
		await second.storage.activateAppStorageScope(ADDED_SINCE_SCOPE);
		expectNoLegacyContent(second, 'an account added since the upgrade');
		await unloadClient(second);
	});

	test('on the web every account that received shared content is reviewed, and only once', async () => {
		await seedDeployedWebAccounts();
		seedDeployedLocalStorage();
		await expectSharedContentReviewOnce();
	});

	test('on desktop every account that received shared content is reviewed, and only once', async () => {
		installDeployedDesktop(createFakeDesktopStore());
		await expectSharedContentReviewOnce();
	});
});
