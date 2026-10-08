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
	expectedGoldenStorageRows,
	GOLDEN_LOCAL_STORAGE_CORPUS,
	goldenStorageRowIdentity,
	NOT_MIGRATED_STORAGE_KEYS,
} from '@app/features/platform/state/__fixtures__/LegacyStorageFixtures';
import {installRuntimeBootstrap} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import type {
	AppStorageFinalizationResult,
	AppStorageSessionAccount,
} from '@app/features/platform/state/AppStorageBootstrap';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {
	APP_STORAGE_INDEXED_DB_NAME,
	APP_STORAGE_OBJECT_STORE,
} from '@app/features/platform/state/PersistentStorageBackend';
import {getProtectedIndexedDB} from '@app/features/platform/state/ProtectedWebStorage';
import type {ElectronAPI} from '@app/features/platform/types/Electron';
import type {DesktopLegacyHarvest, DesktopLegacyHarvestStore} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {
	DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY,
	DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {
	DESKTOP_LEGACY_AUTHORITY_MARKER_KEY,
	DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE,
	DESKTOP_LEGACY_IMPORT_MARKER_KEY,
	DesktopLegacyImportPhase,
} from '@fluxer/desktop_ipc/src/StorageContract';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

type BootstrapModule = typeof import('@app/features/platform/state/AppStorageBootstrap');
type PersistentStorageModule = typeof import('@app/features/platform/state/PersistentStorage');

const LEGACY_ORIGIN = 'https://web.canary.fluxer.app';
const ACTIVE_USER_ID = '100000000000000002';
const OTHER_USER_ID = '100000000000000003';
const ACTIVE_SCOPE = `${FIXTURE_CURRENT_INSTANCE_KEY}::${ACTIVE_USER_ID}`;
const OTHER_SCOPE = `${FIXTURE_CURRENT_INSTANCE_KEY}::${OTHER_USER_ID}`;
const UNAUTHENTICATED_SCOPE = 'unauthenticated';
const GLOBAL_SCOPE = 'global';
const ALL_ACCOUNT_SCOPES = [UNAUTHENTICATED_SCOPE, ACTIVE_SCOPE, OTHER_SCOPE];
const SOUND_BYTES = new Uint8Array([7, 11, 13, 17, 19, 23]);
const ACTIVE_TOKEN = GOLDEN_LOCAL_STORAGE_CORPUS.find((entry) => entry.key === 'token')!.value;

const identity = goldenStorageRowIdentity;

function expectedDesktopEntries(): Map<string, string> {
	return expectedGoldenStorageRows({
		global: GLOBAL_SCOPE,
		everyAccount: ALL_ACCOUNT_SCOPES,
		contentAccount: ACTIVE_SCOPE,
		sharedContent: [ACTIVE_SCOPE, OTHER_SCOPE],
		namedAccount: OTHER_SCOPE,
	});
}

function goldenCorpusRecord(): Record<string, string> {
	const record: Record<string, string> = {};
	for (const entry of GOLDEN_LOCAL_STORAGE_CORPUS) {
		record[entry.key] = entry.value;
	}
	return record;
}

function accountRecord(userId: string): Record<string, unknown> {
	return {
		userId,
		token: userId === ACTIVE_USER_ID ? ACTIVE_TOKEN : `token-${userId}`,
		userData: {id: userId, username: `user-${userId}`, discriminator: '0001', avatar: null},
		localStorageData: {userId},
		lastActive: FIXTURE_NOW - 1_000,
		instance: FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	};
}

function activeSessionAccount(): AppStorageSessionAccount {
	return {
		accountKey: ACTIVE_SCOPE,
		userId: ACTIVE_USER_ID,
		token: ACTIVE_TOKEN,
		instance: FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	};
}

async function startAndFinalize(bootstrap: BootstrapModule): Promise<AppStorageFinalizationResult> {
	const handle = await bootstrap.startAppStorage({scoped: true});
	return await handle.finalizeAfterSessionResolution(activeSessionAccount());
}

function installedCanaryStores(): ReadonlyArray<DesktopLegacyHarvestStore> {
	return [
		{
			database: 'FluxerAccounts',
			version: 2,
			store: 'accounts',
			records: [
				{key: null, value: accountRecord(ACTIVE_USER_ID)},
				{key: null, value: accountRecord(OTHER_USER_ID)},
			],
		},
		{
			database: 'FluxerCustomSounds',
			version: 2,
			store: 'customSounds',
			records: [{key: null, value: {soundType: 'message', blob: SOUND_BYTES, edited: true}}],
		},
		{
			database: 'fluxer-theme-library',
			version: 1,
			store: 'themes',
			records: [{key: null, value: {id: 'midnight', name: 'Midnight'}}],
		},
		{
			database: 'FluxerVoiceStats',
			version: 1,
			store: 'stats',
			records: [{key: null, value: {reportId: 'r-1', packetsLost: 3}}],
		},
	];
}

function installedCanaryHarvest(overrides: Partial<DesktopLegacyHarvest> = {}): DesktopLegacyHarvest {
	return {
		version: 1,
		origin: LEGACY_ORIGIN,
		capturedAt: FIXTURE_NOW - 10_000,
		localStorage: goldenCorpusRecord(),
		stores: installedCanaryStores(),
		serviceWorkersUnregistered: 1,
		cachesDeleted: 2,
		truncated: [],
		...overrides,
	};
}

interface HarvestBridgeCalls {
	readonly calls: Array<string>;
}

function installUpgradeBridge(
	store: FakeDesktopStore | null,
	harvest: DesktopLegacyHarvest | null,
	journal: HarvestBridgeCalls,
): void {
	let staged = harvest;
	let replanted = false;
	const api = {
		desktopAccounts: store?.accounts,
		desktopStorage: store?.storage,
		desktopLegacyHarvest:
			harvest === null
				? undefined
				: {
						read: () => {
							journal.calls.push('read');
							return Promise.resolve(replanted ? null : staged);
						},
						markReplanted: () => {
							journal.calls.push('markReplanted');
							replanted = true;
							return Promise.resolve();
						},
						discard: () => {
							journal.calls.push('discard');
							staged = null;
							return Promise.resolve();
						},
					},
	};
	Object.defineProperty(window, 'electron', {
		value: api as unknown as ElectronAPI,
		configurable: true,
		writable: true,
	});
}

function seedRawStorageAsPreloadDoes(harvest: DesktopLegacyHarvest): void {
	for (const [key, value] of Object.entries(harvest.localStorage)) {
		window.localStorage.setItem(key, value);
	}
	window.localStorage.setItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY, DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE);
}

async function loadModules(): Promise<{
	readonly bootstrap: BootstrapModule;
	readonly storage: PersistentStorageModule;
}> {
	vi.resetModules();
	await import('@app/features/platform/state/PersistentStorageBackend');
	await import('@app/features/platform/state/AppStorageBroadcast');
	await import('@app/features/platform/state/PersistentStorageWriteQueue');
	const storage: PersistentStorageModule = await import('@app/features/platform/state/PersistentStorage');
	const bootstrap: BootstrapModule = await import('@app/features/platform/state/AppStorageBootstrap');
	return {bootstrap, storage};
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

function openDatabase(name: string, version: number): Promise<IDBDatabase> {
	return new Promise<IDBDatabase>((resolve, reject) => {
		const request = window.indexedDB.open(name, version);
		request.onsuccess = () => resolve(request.result);
		request.onerror = () => reject(request.error ?? new Error(`open ${name} failed`));
	});
}

function readAll(database: IDBDatabase, store: string): Promise<Array<unknown>> {
	return new Promise<Array<unknown>>((resolve, reject) => {
		const request = database.transaction(store).objectStore(store).getAll();
		request.onsuccess = () => resolve(request.result);
		request.onerror = () => reject(request.error ?? new Error(`getAll ${store} failed`));
	});
}

const WEB_FALLBACK_BOOKKEEPING_KEYS: ReadonlySet<string> = new Set(['fluxer:migration:legacy-app-storage']);

function withoutWebFallbackBookkeeping(
	entries: ReadonlyArray<readonly [string, string, string]>,
): Array<readonly [string, string, string]> {
	return entries.filter(([, key]) => !WEB_FALLBACK_BOOKKEEPING_KEYS.has(key));
}

function desktopEntryMap(store: FakeDesktopStore): Map<string, string> {
	return new Map(store.snapshot().entries.map(([scope, key, value]) => [identity(scope, key), value]));
}

function markerOf(store: FakeDesktopStore, key: string): string | null {
	return new Map(store.snapshot().markers).get(key) ?? null;
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
	for (const name of [
		APP_STORAGE_INDEXED_DB_NAME,
		'FluxerAccounts',
		'FluxerCustomSounds',
		'fluxer-theme-library',
		'FluxerVoiceStats',
	]) {
		await deleteDatabase(name);
	}
});

afterEach(() => {
	Object.defineProperty(navigator, 'locks', {value: null, configurable: true, writable: true});
	Reflect.deleteProperty(window, 'electron');
});

describe('upgrading an installed Fluxer Canary onto the local app scheme', () => {
	test('the whole golden corpus lands in the desktop store under the scope the migration assigns it', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		const journal: HarvestBridgeCalls = {calls: []};
		installUpgradeBridge(store, harvest, journal);
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap} = await loadModules();

		const result = await startAndFinalize(bootstrap);

		expect(result.desktopBackendInstalled).toBe(true);
		expect(result.desktopImport?.status).toBe('imported');
		expect(result.scope).toBe(ACTIVE_SCOPE);

		const entries = desktopEntryMap(store);
		const rewritten: Array<string> = [];
		for (const [destination, value] of expectedDesktopEntries()) {
			expect(entries.has(destination), `lost ${destination}`).toBe(true);
			if (entries.get(destination) !== value) {
				rewritten.push(destination);
			}
		}
		expect(rewritten).toEqual([]);
	});

	test('no deployed key is dropped and none is invented', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap} = await loadModules();

		await startAndFinalize(bootstrap);

		const migrated = new Set(desktopEntryMap(store).keys());
		const expected = expectedDesktopEntries();
		for (const destination of expected.keys()) {
			expect(migrated.has(destination), `missing ${destination}`).toBe(true);
		}
		for (const key of NOT_MIGRATED_STORAGE_KEYS) {
			for (const scope of [GLOBAL_SCOPE, ...ALL_ACCOUNT_SCOPES]) {
				expect(migrated.has(identity(scope, key)), `invented ${identity(scope, key)}`).toBe(false);
			}
		}
	});

	test('both account rows arrive instance-qualified with their tokens intact', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap} = await loadModules();

		await startAndFinalize(bootstrap);

		expect(
			store
				.snapshot()
				.accounts.map(([key]) => key)
				.sort(),
		).toEqual([ACTIVE_SCOPE, OTHER_SCOPE].sort());
		expect(store.readAccount(ACTIVE_SCOPE)).toMatchObject({userId: ACTIVE_USER_ID, storageKey: ACTIVE_SCOPE});
		expect(store.readAccount(OTHER_SCOPE)).toMatchObject({userId: OTHER_USER_ID, storageKey: OTHER_SCOPE});
	});

	test('the live session survives: the pointer, the token and the user id all name the same account', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap} = await loadModules();

		await startAndFinalize(bootstrap);

		const rawToken = harvest.localStorage.token;
		expect(store.readEntry(GLOBAL_SCOPE, AppStorageKey.AUTH_ACCOUNT_KEY)?.value).toBe(ACTIVE_SCOPE);
		expect(store.readAccount(ACTIVE_SCOPE)).toMatchObject({userId: ACTIVE_USER_ID, token: rawToken});
		expect(window.localStorage.getItem('token')).toBe(rawToken);
		expect(window.localStorage.getItem('userId')).toBe(ACTIVE_USER_ID);
	});

	test('the raw session keys stay on the new origin, where the preboot still reads them', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap} = await loadModules();

		await startAndFinalize(bootstrap);

		expect(window.localStorage.getItem('userId')).toBe(ACTIVE_USER_ID);
		expect(window.localStorage.getItem('token')).toBe(harvest.localStorage.token);
	});

	test('every harvested database is replanted onto the new origin, blobs included', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap} = await loadModules();

		await startAndFinalize(bootstrap);

		const accounts = await openDatabase('FluxerAccounts', 2);
		expect((await readAll(accounts, 'accounts')).length).toBe(2);
		accounts.close();

		const sounds = await openDatabase('FluxerCustomSounds', 2);
		const soundRecords = (await readAll(sounds, 'customSounds')) as Array<{blob: unknown; edited: boolean}>;
		sounds.close();
		expect(soundRecords).toHaveLength(1);
		expect(soundRecords[0]?.edited).toBe(true);
		expect(new Uint8Array(soundRecords[0]?.blob as ArrayBufferLike)).toEqual(SOUND_BYTES);

		const themes = await openDatabase('fluxer-theme-library', 1);
		expect(await readAll(themes, 'themes')).toEqual([{id: 'midnight', name: 'Midnight'}]);
		themes.close();

		const stats = await openDatabase('FluxerVoiceStats', 1);
		expect(await readAll(stats, 'stats')).toEqual([{reportId: 'r-1', packetsLost: 3}]);
		stats.close();
	});

	test('the harvest is marked replanted, authority is committed, and only then is it discarded', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		const journal: HarvestBridgeCalls = {calls: []};
		installUpgradeBridge(store, harvest, journal);
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap} = await loadModules();

		await startAndFinalize(bootstrap);

		expect(journal.calls.filter((call) => call !== 'read')).toEqual(['markReplanted', 'discard']);
		expect(markerOf(store, DESKTOP_LEGACY_IMPORT_MARKER_KEY)).toContain(DesktopLegacyImportPhase.DONE);
		expect(markerOf(store, DESKTOP_LEGACY_AUTHORITY_MARKER_KEY)).toBe(DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE);
	});

	test('writes made after boot land in the desktop store, not the web backend', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap, storage} = await loadModules();

		await startAndFinalize(bootstrap);
		storage.default.setItem('UserSettings:syncedPreferencesLocal', 'written-after-upgrade');
		await storage.flushAppStorageWrites();

		expect(store.readEntry(ACTIVE_SCOPE, 'UserSettings:syncedPreferencesLocal')?.value).toBe('written-after-upgrade');
	});
});

describe('the second launch after the upgrade', () => {
	test('re-imports nothing and leaves the store byte-identical', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		const journal: HarvestBridgeCalls = {calls: []};
		installUpgradeBridge(store, harvest, journal);
		seedRawStorageAsPreloadDoes(harvest);

		const first = await loadModules();
		await startAndFinalize(first.bootstrap);
		const afterUpgrade = store.snapshot();

		const second = await loadModules();
		const result = await startAndFinalize(second.bootstrap);

		expect(result.desktopBackendInstalled).toBe(true);
		expect(result.desktopImport?.status).toBe('already-done');
		expect(result.scope).toBe(ACTIVE_SCOPE);
		expect(store.snapshot().accounts).toEqual(afterUpgrade.accounts);
		expect(store.snapshot().entries).toEqual(afterUpgrade.entries);
		expect(markerOf(store, DESKTOP_LEGACY_IMPORT_MARKER_KEY)).toBe(
			new Map(afterUpgrade.markers).get(DESKTOP_LEGACY_IMPORT_MARKER_KEY),
		);
		expect(markerOf(store, DESKTOP_LEGACY_AUTHORITY_MARKER_KEY)).toBe(DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE);
		expect(journal.calls.filter((call) => call === 'markReplanted')).toHaveLength(1);
	});
});

describe('later launches after the harvest was replanted', () => {
	function deleteRecord(database: IDBDatabase, store: string, key: IDBValidKey): Promise<void> {
		return new Promise<void>((resolve, reject) => {
			const transaction = database.transaction(store, 'readwrite');
			transaction.objectStore(store).delete(key);
			transaction.oncomplete = () => resolve();
			transaction.onerror = () => reject(transaction.error ?? new Error(`delete from ${store} failed`));
		});
	}

	async function startSignedOut(bootstrap: BootstrapModule): Promise<void> {
		const handle = await bootstrap.startAppStorage({scoped: true});
		await handle.finalizeAfterSessionResolution(null);
	}

	test('a signed-out install replants once and never restores a record removed since', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		const journal: HarvestBridgeCalls = {calls: []};
		installUpgradeBridge(store, harvest, journal);
		seedRawStorageAsPreloadDoes(harvest);
		window.localStorage.removeItem('token');
		window.localStorage.removeItem('userId');
		const first = await loadModules();
		await startSignedOut(first.bootstrap);
		const stats = await openDatabase('FluxerVoiceStats', 1);
		expect(await readAll(stats, 'stats')).toHaveLength(1);
		await deleteRecord(stats, 'stats', 'r-1');
		stats.close();

		const second = await loadModules();
		await startSignedOut(second.bootstrap);

		const reopened = await openDatabase('FluxerVoiceStats', 1);
		expect(await readAll(reopened, 'stats')).toEqual([]);
		reopened.close();
		expect(journal.calls.filter((call) => call === 'markReplanted')).toHaveLength(1);
		expect(journal.calls).not.toContain('discard');
	});
});

describe('upgrading an install enrolled in the move to the migrated official domain', () => {
	const MIGRATED_ORIGIN = 'https://canary.fluxer.com';
	const OFFICIAL_INSTANCE_KEY = 'https://web.canary.fluxer.app/api';
	const OFFICIAL_INSTANCE_SNAPSHOT = {
		...FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
		apiEndpoint: OFFICIAL_INSTANCE_KEY,
		webAppEndpoint: 'https://web.canary.fluxer.app',
	};
	const OFFICIAL_ACTIVE_SCOPE = `${OFFICIAL_INSTANCE_KEY}::${ACTIVE_USER_ID}`;
	const OFFICIAL_OTHER_SCOPE = `${OFFICIAL_INSTANCE_KEY}::${OTHER_USER_ID}`;

	function migratedDomainAccount(userId: string): Record<string, unknown> {
		return {
			...accountRecord(userId),
			instance: {
				apiEndpoint: `${MIGRATED_ORIGIN}/api`,
				webAppEndpoint: MIGRATED_ORIGIN,
				gatewayEndpoint: 'wss://gateway.fluxer.app',
			},
		};
	}

	function migratedDomainHarvest(): DesktopLegacyHarvest {
		return installedCanaryHarvest({
			origin: MIGRATED_ORIGIN,
			stores: [
				{
					database: 'FluxerAccounts',
					version: 2,
					store: 'accounts',
					records: [
						{key: null, value: migratedDomainAccount(ACTIVE_USER_ID)},
						{key: null, value: migratedDomainAccount(OTHER_USER_ID)},
					],
				},
			],
		});
	}

	test('the accounts stored on the migrated domain are imported once, keyed on the official instance', async () => {
		const store = createFakeDesktopStore();
		const harvest = migratedDomainHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap} = await loadModules();
		const handle = await bootstrap.startAppStorage({scoped: true});

		const result = await handle.finalizeAfterSessionResolution({
			accountKey: OFFICIAL_ACTIVE_SCOPE,
			userId: ACTIVE_USER_ID,
			token: ACTIVE_TOKEN,
			instance: OFFICIAL_INSTANCE_SNAPSHOT,
		});

		expect(result.desktopImport?.status).toBe('imported');
		expect(result.scope).toBe(OFFICIAL_ACTIVE_SCOPE);
		expect(
			store
				.snapshot()
				.accounts.map(([key]) => key)
				.sort(),
		).toEqual([OFFICIAL_ACTIVE_SCOPE, OFFICIAL_OTHER_SCOPE].sort());
		expect(store.readAccount(OFFICIAL_ACTIVE_SCOPE)).toMatchObject({
			userId: ACTIVE_USER_ID,
			token: ACTIVE_TOKEN,
			storageKey: OFFICIAL_ACTIVE_SCOPE,
			instance: {apiEndpoint: OFFICIAL_INSTANCE_KEY},
		});
		expect(store.readEntry(GLOBAL_SCOPE, AppStorageKey.AUTH_ACCOUNT_KEY)?.value).toBe(OFFICIAL_ACTIVE_SCOPE);
		expect(markerOf(store, DESKTOP_LEGACY_AUTHORITY_MARKER_KEY)).toBe(DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE);
	});
});

describe('an upgrade interrupted partway through', () => {
	test('a failed desktop write leaves the web backend serving every value', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap, storage} = await loadModules();
		store.failOnce('storage.import');

		const result = await startAndFinalize(bootstrap);

		expect(result.desktopBackendInstalled).toBe(false);
		expect(result.scope).toBe(ACTIVE_SCOPE);
		expect(storage.default.getItem(AppStorageKey.AUTH_SESSION_USER_ID)).toBe(ACTIVE_USER_ID);
		expect(markerOf(store, DESKTOP_LEGACY_AUTHORITY_MARKER_KEY)).not.toBe(DESKTOP_LEGACY_AUTHORITY_MARKER_VALUE);
	});

	test('the next launch converges on exactly the state an uninterrupted upgrade produces', async () => {
		const reference = createFakeDesktopStore();
		const referenceHarvest = installedCanaryHarvest();
		installUpgradeBridge(reference, referenceHarvest, {calls: []});
		seedRawStorageAsPreloadDoes(referenceHarvest);
		const clean = await loadModules();
		await startAndFinalize(clean.bootstrap);
		const expected = reference.snapshot();

		window.localStorage.clear();
		for (const name of [
			APP_STORAGE_INDEXED_DB_NAME,
			'FluxerAccounts',
			'FluxerCustomSounds',
			'fluxer-theme-library',
			'FluxerVoiceStats',
		]) {
			await deleteDatabase(name);
		}

		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const interrupted = await loadModules();
		store.failOnce('storage.import');
		await startAndFinalize(interrupted.bootstrap);

		const resumed = await loadModules();
		const result = await startAndFinalize(resumed.bootstrap);

		expect(result.desktopBackendInstalled).toBe(true);
		expect(store.snapshot().accounts).toEqual(expected.accounts);
		expect(withoutWebFallbackBookkeeping(store.snapshot().entries)).toEqual(
			withoutWebFallbackBookkeeping(expected.entries),
		);
	});

	test('work done during the fallback session is carried across, not overwritten by the stale snapshot', async () => {
		const store = createFakeDesktopStore();
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(store, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);

		const interrupted = await loadModules();
		store.failOnce('storage.import');
		const failed = await startAndFinalize(interrupted.bootstrap);
		expect(failed.desktopBackendInstalled).toBe(false);
		expect(markerOf(store, DESKTOP_LEGACY_IMPORT_MARKER_KEY)).toContain(DesktopLegacyImportPhase.ENTRIES);

		interrupted.storage.default.setItem('UserSettings:syncedPreferencesLocal', 'written-during-fallback');
		await interrupted.storage.flushAppStorageWrites();

		const resumed = await loadModules();
		const result = await startAndFinalize(resumed.bootstrap);

		expect(result.desktopBackendInstalled).toBe(true);
		expect(store.readEntry(ACTIVE_SCOPE, 'UserSettings:syncedPreferencesLocal')?.value).toBe('written-during-fallback');
	});
});

describe('an upgrade with no desktop store at all', () => {
	test('the web backend still carries the whole corpus onto the new origin', async () => {
		const harvest = installedCanaryHarvest();
		installUpgradeBridge(null, harvest, {calls: []});
		seedRawStorageAsPreloadDoes(harvest);
		const {bootstrap, storage} = await loadModules();

		const result = await startAndFinalize(bootstrap);

		expect(result.desktopBackendInstalled).toBe(false);
		expect(result.scope).toBe(ACTIVE_SCOPE);
		expect(storage.getAppStorageScope()).toBe(ACTIVE_SCOPE);

		const accounts = await openDatabase('FluxerAccounts', 2);
		expect((await readAll(accounts, 'accounts')).length).toBe(2);
		accounts.close();

		const appStorage = await openDatabase(APP_STORAGE_INDEXED_DB_NAME, 1);
		const rows = (await readAll(appStorage, APP_STORAGE_OBJECT_STORE)) as Array<{scope: string; key: string}>;
		appStorage.close();
		const present = new Set(rows.map((row) => identity(row.scope, row.key)));
		for (const destination of expectedDesktopEntries().keys()) {
			expect(present.has(destination), `missing ${destination}`).toBe(true);
		}
	});
});
