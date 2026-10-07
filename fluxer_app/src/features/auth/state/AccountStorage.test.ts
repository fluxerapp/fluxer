// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import {
	FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	FIXTURE_NOW,
	FLUXER_ACCOUNTS_DB_VERSION,
	FLUXER_ACCOUNTS_STORE_NAME,
	openGoldenAccountsDatabase,
} from '@app/features/auth/state/__fixtures__/AccountRecordFixtures';
import type {
	AccountRekeyContext,
	AccountRekeyOutcome,
	KeyedStoredAccount,
	StoredAccount,
} from '@app/features/auth/state/AccountStorage';
import {installRuntimeBootstrap} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import {beforeEach, describe, expect, test, vi} from 'vitest';

const CURRENT_INSTANCE_KEY = 'https://fluxer.app/api';
const MOVED_API_ENDPOINT = 'https://api.fluxer.app';
const FOREIGN_API_ENDPOINT = 'https://self.hosted.example/api';

const DEPLOYED_SWAP_CAPTURED: Record<string, string> = {
	token: 'raw.token',
	userId: '4242',
	runtimeConfig: '{"apiEndpoint":"https://fluxer.app/api"}',
	AccountManager: '{"accounts":[]}',
	mobxTheme: '{"theme":"dark"}',
	'mobx-persist:Keybind': '{"binds":[]}',
	persistedSidebar: '{"width":300}',
	'fluxer:ui:sidebar-width': '300',
	'fluxer:auth:active-account-key': 'https://fluxer.app/api::1',
};

const DEPLOYED_SWAP_IGNORED: Record<string, string> = {
	Theme: '{"type":"dark","__mps__":{"version":1}}',
	Drafts: '{"channels":{},"__mps__":{"version":1}}',
	VoiceSettings: '{"inputVolume":100,"__mps__":{"version":1}}',
	ClientInstallationId: '0123456789abcdef0123456789abcdef',
	theme: 'dark',
	MobXTheme: '{}',
};

type AccountStorageModule = typeof import('@app/features/auth/state/AccountStorage');

function instanceSnapshot(apiEndpoint: string): RuntimeConfigSnapshot {
	return {...FIXTURE_CURRENT_INSTANCE_SNAPSHOT, apiEndpoint, apiPublicEndpoint: apiEndpoint};
}

function storedRecord(userId: string, overrides: Partial<StoredAccount> = {}): StoredAccount {
	return {
		userId,
		token: `token.${userId}`,
		userData: {username: `user${userId}`, discriminator: '0001'},
		presenceIntent: null,
		localStorageData: {},
		managedStorageData: {},
		lastActive: FIXTURE_NOW - 1_000,
		instance: FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
		...overrides,
	};
}

function rekeyContext(
	userId: string,
	instance: RuntimeConfigSnapshot = FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
): AccountRekeyContext {
	return {target: {userId, token: `token.${userId}`, instance}, now: FIXTURE_NOW};
}

async function loadAccountStorage(): Promise<AccountStorageModule> {
	return import('@app/features/auth/state/AccountStorage');
}

async function seed(records: ReadonlyArray<StoredAccount>): Promise<void> {
	const database = await openGoldenAccountsDatabase(window.indexedDB);
	await new Promise<void>((resolve, reject) => {
		const transaction = database.transaction([FLUXER_ACCOUNTS_STORE_NAME], 'readwrite');
		const store = transaction.objectStore(FLUXER_ACCOUNTS_STORE_NAME);
		for (const record of records) {
			store.put(record);
		}
		transaction.oncomplete = () => resolve();
		transaction.onerror = () => reject(transaction.error ?? new Error('IndexedDB seed failed'));
	});
	database.close();
}

async function readRaw(userId: string): Promise<StoredAccount | null> {
	const database = await openGoldenAccountsDatabase(window.indexedDB);
	const record = await new Promise<StoredAccount | null>((resolve, reject) => {
		const request = database
			.transaction([FLUXER_ACCOUNTS_STORE_NAME], 'readonly')
			.objectStore(FLUXER_ACCOUNTS_STORE_NAME)
			.get(userId);
		request.onsuccess = () => resolve((request.result as StoredAccount | undefined) ?? null);
		request.onerror = () => reject(request.error ?? new Error('IndexedDB read failed'));
	});
	database.close();
	return record;
}

function readRawLocalStorage(): Record<string, string> {
	const entries: Record<string, string> = {};
	for (let i = 0; i < window.localStorage.length; i++) {
		const key = window.localStorage.key(i);
		if (key == null) {
			continue;
		}
		entries[key] = window.localStorage.getItem(key) ?? '';
	}
	return entries;
}

function seedRawLocalStorage(entries: Record<string, string>): void {
	for (const [key, value] of Object.entries(entries)) {
		window.localStorage.setItem(key, value);
	}
}

function installIndexedDB(factory: IDBFactory | undefined): void {
	Object.defineProperty(window, 'indexedDB', {value: factory, configurable: true, writable: true});
}

beforeEach(() => {
	vi.resetModules();
	installIndexedDB(new IDBFactory());
	installRuntimeBootstrap();
	window.localStorage.clear();
});

describe('reading a deployed v2 database', () => {
	test('three v2 records stay at version 2 and read back with a derived storageKey', async () => {
		await seed([storedRecord('1'), storedRecord('2'), storedRecord('3')]);
		const {default: accountStorage} = await loadAccountStorage();
		await accountStorage.init();

		const {records, source} = await accountStorage.getAllAccounts();

		expect(source).toBe('idb');
		expect(records.map((record) => record.storageKey).sort()).toEqual([
			`${CURRENT_INSTANCE_KEY}::1`,
			`${CURRENT_INSTANCE_KEY}::2`,
			`${CURRENT_INSTANCE_KEY}::3`,
		]);
		expect(records.map((record) => record.token).sort()).toEqual(['token.1', 'token.2', 'token.3']);

		const database = await openGoldenAccountsDatabase(window.indexedDB);
		expect(database.version).toBe(FLUXER_ACCOUNTS_DB_VERSION);
		expect(database.transaction([FLUXER_ACCOUNTS_STORE_NAME]).objectStore(FLUXER_ACCOUNTS_STORE_NAME).keyPath).toBe(
			'userId',
		);
		database.close();
	});

	test('a record with no instance is listed and left unkeyed rather than healed by a read', async () => {
		await seed([storedRecord('1'), storedRecord('2', {instance: undefined})]);
		const {default: accountStorage} = await loadAccountStorage();

		const {records} = await accountStorage.getAllAccounts();

		expect(records).toHaveLength(2);
		const orphan = records.find((record) => record.userId === '2');
		expect(orphan?.token).toBe('token.2');
		expect(orphan?.storageKey).toBeUndefined();
		expect(await readRaw('2')).not.toBeNull();
	});

	test('a persisted storageKey survives a read even when the record instance became unkeyable', async () => {
		await seed([
			storedRecord('1', {
				storageKey: `${CURRENT_INSTANCE_KEY}::1`,
				instance: instanceSnapshot('https://operator:hunter2@fluxer.app/api?tenant=1'),
			}),
		]);
		const {default: accountStorage} = await loadAccountStorage();

		const {records} = await accountStorage.getAllAccounts();

		expect(records[0].storageKey).toBe(`${CURRENT_INSTANCE_KEY}::1`);
	});

	test('a persisted storageKey naming another user is unavailable rather than redirected', async () => {
		await seed([storedRecord('1', {storageKey: `${CURRENT_INSTANCE_KEY}::9`})]);
		const {default: accountStorage} = await loadAccountStorage();

		const inventory = await accountStorage.getAccountInventory();

		expect(inventory.readyEntries).toHaveLength(0);
		expect(inventory.unavailableRecords).toHaveLength(1);
	});

	test('getAllAccounts falls back to an empty list when IndexedDB is unavailable', async () => {
		installIndexedDB(undefined);
		const {default: accountStorage} = await loadAccountStorage();

		await expect(accountStorage.init()).resolves.toBeUndefined();
		await expect(accountStorage.getAllAccounts()).resolves.toEqual({records: [], source: 'idb'});
	});
});

describe('unusable IndexedDB', () => {
	function installThrowingIndexedDB(): void {
		const factory = new IDBFactory();
		factory.open = () => {
			throw new DOMException('denied', 'SecurityError');
		};
		installIndexedDB(factory);
	}

	test('an open that throws serves accounts from the fallback and keeps them across a reload', async () => {
		installThrowingIndexedDB();
		const first = await loadAccountStorage();

		await first.default.stashAccountData('1', 'token.1', undefined, FIXTURE_CURRENT_INSTANCE_SNAPSHOT);
		expect((await first.default.restoreAccountData(`${CURRENT_INSTANCE_KEY}::1`))?.token).toBe('token.1');

		vi.resetModules();
		installThrowingIndexedDB();
		const second = await loadAccountStorage();
		const {records} = await second.default.getAllAccounts();

		expect(records.map((record) => [record.userId, record.storageKey])).toEqual([['1', `${CURRENT_INSTANCE_KEY}::1`]]);

		await second.default.deleteAccount(`${CURRENT_INSTANCE_KEY}::1`);
		expect((await second.default.getAllAccounts()).records).toHaveLength(0);
	});

	test('the fallback still refuses a cross-instance overwrite', async () => {
		installThrowingIndexedDB();
		const {default: accountStorage, CrossInstanceAccountCollisionError} = await loadAccountStorage();
		await accountStorage.upsertAccount(storedRecord('1'), FIXTURE_CURRENT_INSTANCE_SNAPSHOT);

		await expect(
			accountStorage.upsertAccount(
				storedRecord('1', {instance: instanceSnapshot(FOREIGN_API_ENDPOINT)}),
				FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
			),
		).rejects.toBeInstanceOf(CrossInstanceAccountCollisionError);
	});

	test('accounts stay in memory when localStorage refuses the fallback copy', async () => {
		installThrowingIndexedDB();
		vi.spyOn(Storage.prototype, 'setItem').mockImplementation(() => {
			throw new DOMException('full', 'QuotaExceededError');
		});
		const {default: accountStorage} = await loadAccountStorage();

		await accountStorage.upsertAccount(storedRecord('1'), FIXTURE_CURRENT_INSTANCE_SNAPSHOT);

		expect((await accountStorage.getAllAccounts()).records).toHaveLength(1);
		vi.restoreAllMocks();
	});

	test('a transaction failure on an open database is still refused', async () => {
		await seed([storedRecord('1')]);
		const {default: accountStorage, BrowserAccountStorageUnavailableError} = await loadAccountStorage();
		await accountStorage.init();
		vi.spyOn(IDBDatabase.prototype, 'transaction').mockImplementation(() => {
			throw new DOMException('broken', 'UnknownError');
		});

		await expect(accountStorage.getAllAccounts()).rejects.toBeInstanceOf(BrowserAccountStorageUnavailableError);
		vi.restoreAllMocks();
	});
});

describe('stashAccountData', () => {
	test('isValid: false survives a stash of the same token', async () => {
		await seed([storedRecord('1', {isValid: false})]);
		const {default: accountStorage} = await loadAccountStorage();

		await accountStorage.stashAccountData('1', 'token.1', undefined, FIXTURE_CURRENT_INSTANCE_SNAPSHOT);

		const stored = await readRaw('1');
		expect(stored?.isValid).toBe(false);
		expect(stored?.userData?.username).toBe('user1');
	});

	test('a stash with a new token after invalidation persists the account as valid', async () => {
		await seed([storedRecord('1', {isValid: false})]);
		const {default: accountStorage} = await loadAccountStorage();

		await accountStorage.stashAccountData('1', 'token.relogin', undefined, FIXTURE_CURRENT_INSTANCE_SNAPSHOT);

		const stored = await readRaw('1');
		expect(stored?.isValid).toBe(true);
		expect(stored?.token).toBe('token.relogin');
		expect((await accountStorage.restoreAccountData(`${CURRENT_INSTANCE_KEY}::1`))?.isValid).toBe(true);
	});

	test('a stash rewrites the storage key under the instance it was handed', async () => {
		await seed([storedRecord('1', {storageKey: `${CURRENT_INSTANCE_KEY}::1`})]);
		const {default: accountStorage} = await loadAccountStorage();

		await accountStorage.stashAccountData('1', 'token.1', undefined, instanceSnapshot(MOVED_API_ENDPOINT));

		expect((await readRaw('1'))?.storageKey).toBe(`${MOVED_API_ENDPOINT}::1`);
	});

	test('a stash with no instance keeps the storage key the record already had', async () => {
		await seed([storedRecord('1', {storageKey: `${CURRENT_INSTANCE_KEY}::1`, instance: undefined})]);
		const {default: accountStorage} = await loadAccountStorage();

		await accountStorage.stashAccountData('1', 'token.1');

		expect((await readRaw('1'))?.storageKey).toBe(`${CURRENT_INSTANCE_KEY}::1`);
	});

	test('community and services survive the stored-instance clone', async () => {
		const {default: accountStorage} = await loadAccountStorage();
		const instance: RuntimeConfigSnapshot = {
			...FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
			community: {
				single_community: true,
				single_community_guild_id: '77',
				direct_messages_disabled: true,
				guild_create_access: true,
			},
			services: {gif_enabled: false, youtube_enabled: true, bluesky_enabled: true},
		};

		await accountStorage.stashAccountData('1', 'token.1', undefined, instance);

		const stored = await readRaw('1');
		expect(stored?.instance?.community).toEqual(instance.community);
		expect(stored?.instance?.services).toEqual(instance.services);
	});
});

describe('migrateAccountStorageKeys', () => {
	test('heals an instance-less record once, persists it, and rewrites nothing on a second run', async () => {
		await seed([storedRecord('1'), storedRecord('2', {instance: undefined})]);
		const {default: accountStorage} = await loadAccountStorage();

		const first: AccountRekeyOutcome = await accountStorage.migrateAccountStorageKeys(rekeyContext('2'));

		expect(first.status).toBe('complete');
		expect(first.source).toBe('idb');
		expect(first.written).toBe(1);
		expect(first.qualifiedRecords.map((record: KeyedStoredAccount) => record.storageKey).sort()).toEqual([
			`${CURRENT_INSTANCE_KEY}::1`,
			`${CURRENT_INSTANCE_KEY}::2`,
		]);

		const healed = await readRaw('2');
		expect(healed?.storageKey).toBe(`${CURRENT_INSTANCE_KEY}::2`);
		expect(healed?.instance?.apiEndpoint).toBe(CURRENT_INSTANCE_KEY);
		expect(healed?.token).toBe('token.2');

		const second = await accountStorage.migrateAccountStorageKeys(rekeyContext('2'));
		expect(second.status).toBe('complete');
		expect(second.written).toBe(0);
	});

	test('a record keyed from its own foreign instance is kept and never re-keyed onto the current one', async () => {
		await seed([storedRecord('1', {instance: instanceSnapshot(FOREIGN_API_ENDPOINT)})]);
		const {default: accountStorage} = await loadAccountStorage();

		const outcome = await accountStorage.migrateAccountStorageKeys(rekeyContext('1'));

		expect(outcome.written).toBe(0);
		expect(outcome.qualifiedRecords[0].storageKey).toBe(`${FOREIGN_API_ENDPOINT}::1`);
	});

	test('a secondary account with only a legacy instance on the current instance is healed at once', async () => {
		const legacyInstance = {apiEndpoint: CURRENT_INSTANCE_KEY} as RuntimeConfigSnapshot;
		await seed([storedRecord('1'), storedRecord('2', {instance: legacyInstance})]);
		const {default: accountStorage} = await loadAccountStorage();

		const first = await accountStorage.migrateAccountStorageKeys(rekeyContext('1'));

		expect(first.status).toBe('complete');
		expect(first.deferredRecords).toHaveLength(0);
		expect(first.written).toBe(1);
		const healed = await readRaw('2');
		expect(healed?.storageKey).toBe(`${CURRENT_INSTANCE_KEY}::2`);
		expect(healed?.instance).toEqual(FIXTURE_CURRENT_INSTANCE_SNAPSHOT);
		expect(healed?.token).toBe('token.2');
		expect((await accountStorage.getAccountInventory()).runtimeRecoveryCandidates).toHaveLength(0);

		const second = await accountStorage.migrateAccountStorageKeys(rekeyContext('1'));
		expect(second.status).toBe('complete');
		expect(second.written).toBe(0);
	});

	test('a secondary account with a legacy instance elsewhere stays deferred with its record intact', async () => {
		const legacyInstance = {apiEndpoint: FOREIGN_API_ENDPOINT} as RuntimeConfigSnapshot;
		await seed([storedRecord('1'), storedRecord('2', {instance: legacyInstance})]);
		const {default: accountStorage} = await loadAccountStorage();

		const outcome = await accountStorage.migrateAccountStorageKeys(rekeyContext('1'));

		expect(outcome.status).toBe('deferred');
		expect(outcome.deferredRecords).toHaveLength(1);
		expect(outcome.written).toBe(0);
		expect((await readRaw('2'))?.instance).toEqual(legacyInstance);
	});

	test('an active credential for another account does not heal the record', async () => {
		await seed([storedRecord('2', {instance: undefined})]);
		const {default: accountStorage} = await loadAccountStorage();

		const outcome = await accountStorage.migrateAccountStorageKeys(rekeyContext('1'));

		expect(outcome.status).toBe('deferred');
		expect(outcome.written).toBe(0);
		expect(outcome.qualifiedRecords).toHaveLength(0);
		expect(outcome.deferredRecords).toHaveLength(1);
		expect(await readRaw('2')).not.toBeNull();
		expect((await accountStorage.getAllAccounts()).records).toHaveLength(1);
	});

	test('an unkeyable current instance heals nothing', async () => {
		await seed([storedRecord('2', {instance: undefined})]);
		const {default: accountStorage} = await loadAccountStorage();

		const outcome = await accountStorage.migrateAccountStorageKeys(
			rekeyContext('2', instanceSnapshot('https://operator:hunter2@fluxer.app/api?tenant=1')),
		);

		expect(outcome.written).toBe(0);
		expect((await readRaw('2'))?.storageKey).toBeUndefined();
	});

	test('an unavailable IndexedDB migrates the fallback copy instead of failing', async () => {
		installIndexedDB(undefined);
		const {default: accountStorage} = await loadAccountStorage();
		await accountStorage.importAccounts([storedRecord('1', {instance: undefined})]);

		const outcome = await accountStorage.migrateAccountStorageKeys(rekeyContext('1'));

		expect(outcome.written).toBe(1);
		expect((await accountStorage.getAllAccounts()).records[0]?.storageKey).toBe(`${CURRENT_INSTANCE_KEY}::1`);
	});
});

describe('upsertAccount', () => {
	test('a cross-instance userId collision is refused and leaves the stored record intact', async () => {
		await seed([storedRecord('1')]);
		const {default: accountStorage, CrossInstanceAccountCollisionError} = await loadAccountStorage();

		await expect(
			accountStorage.upsertAccount(
				storedRecord('1', {token: 'token.foreign', instance: instanceSnapshot(FOREIGN_API_ENDPOINT)}),
				FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
			),
		).rejects.toBeInstanceOf(CrossInstanceAccountCollisionError);

		const stored = await readRaw('1');
		expect(stored?.token).toBe('token.1');
		expect(stored?.instance?.apiEndpoint).toBe(CURRENT_INSTANCE_KEY);
	});

	test('an instance that moved its own apiEndpoint re-keys the record instead of being refused', async () => {
		await seed([storedRecord('1', {storageKey: `${CURRENT_INSTANCE_KEY}::1`})]);
		const {default: accountStorage} = await loadAccountStorage();
		const moved = instanceSnapshot(MOVED_API_ENDPOINT);

		await accountStorage.upsertAccount(storedRecord('1', {instance: moved}), moved);

		expect((await readRaw('1'))?.storageKey).toBe(`${MOVED_API_ENDPOINT}::1`);
	});

	test('a first write for an unknown userId on a foreign instance is accepted', async () => {
		const {default: accountStorage} = await loadAccountStorage();
		const foreign = instanceSnapshot(FOREIGN_API_ENDPOINT);

		await accountStorage.upsertAccount(storedRecord('1', {instance: foreign}), FIXTURE_CURRENT_INSTANCE_SNAPSHOT);

		expect((await readRaw('1'))?.storageKey).toBe(`${FOREIGN_API_ENDPOINT}::1`);
	});
});

describe('deleteAccount', () => {
	test('an instance-qualified key evicts the row it names', async () => {
		await seed([storedRecord('1')]);
		const {default: accountStorage} = await loadAccountStorage();

		await accountStorage.deleteAccount(`${CURRENT_INSTANCE_KEY}::1`);

		expect(await readRaw('1')).toBeNull();
	});

	test('a key qualified by another instance evicts nothing', async () => {
		await seed([storedRecord('1')]);
		const {default: accountStorage} = await loadAccountStorage();

		await accountStorage.deleteAccount(`${FOREIGN_API_ENDPOINT}::1`);

		expect((await readRaw('1'))?.token).toBe('token.1');
	});

	test('a key qualified by another instance leaves the account readable', async () => {
		await seed([storedRecord('1')]);
		const {default: accountStorage} = await loadAccountStorage();

		await accountStorage.deleteAccount(`${FOREIGN_API_ENDPOINT}::1`);

		const {records} = await accountStorage.getAllAccounts();
		expect(records.map((record) => record.storageKey)).toEqual([`${CURRENT_INSTANCE_KEY}::1`]);
	});
});

describe('restoreAccountData', () => {
	test('a restore rewrites no raw localStorage key', async () => {
		await seed([
			storedRecord('1', {
				localStorageData: {token: 'stashed.token', 'fluxer:ui:sidebar-width': '999', 'fluxer:ui:only-in-record': 'x'},
				managedStorageData: {token: 'stashed.token', 'fluxer:ui:sidebar-width': '999', 'fluxer:ui:only-in-record': 'x'},
			}),
		]);
		const {default: accountStorage} = await loadAccountStorage();
		seedRawLocalStorage({...DEPLOYED_SWAP_CAPTURED, ...DEPLOYED_SWAP_IGNORED});
		const before = readRawLocalStorage();

		await accountStorage.restoreAccountData('1');

		expect(readRawLocalStorage()).toEqual(before);
		expect(window.localStorage.getItem('fluxer:ui:only-in-record')).toBeNull();
	});

	test('a restore returns the stored snapshot instead of applying it', async () => {
		const snapshot = {'fluxer:ui:sidebar-width': '999'};
		await seed([storedRecord('1', {localStorageData: snapshot, managedStorageData: snapshot})]);
		const {default: accountStorage} = await loadAccountStorage();
		seedRawLocalStorage({'fluxer:ui:sidebar-width': '300'});

		const restored = await accountStorage.restoreAccountData('1');

		expect(restored?.token).toBe('token.1');
		expect(restored?.localStorageData).toEqual(snapshot);
		expect(window.localStorage.getItem('fluxer:ui:sidebar-width')).toBe('300');
	});

	test('a restore still refreshes lastActive', async () => {
		await seed([storedRecord('1')]);
		const {default: accountStorage} = await loadAccountStorage();
		const before = (await readRaw('1'))?.lastActive ?? 0;

		await accountStorage.restoreAccountData('1');

		expect((await readRaw('1'))?.lastActive ?? 0).toBeGreaterThan(before);
	});

	test('a foreign-instance record restores and touches no raw localStorage', async () => {
		await seed([storedRecord('1', {instance: instanceSnapshot(FOREIGN_API_ENDPOINT)})]);
		const {default: accountStorage} = await loadAccountStorage();
		seedRawLocalStorage(DEPLOYED_SWAP_CAPTURED);
		const before = readRawLocalStorage();

		expect((await accountStorage.restoreAccountData('1'))?.token).toBe('token.1');

		expect(readRawLocalStorage()).toEqual(before);
	});
});

describe('the per-account storage snapshot', () => {
	test('a stash captures nothing, because no build here reads the snapshot back', async () => {
		const {default: accountStorage} = await loadAccountStorage();
		seedRawLocalStorage({...DEPLOYED_SWAP_CAPTURED, ...DEPLOYED_SWAP_IGNORED});

		await accountStorage.stashAccountData('1', 'token.1', undefined, FIXTURE_CURRENT_INSTANCE_SNAPSHOT);

		const stored = await readRaw('1');
		expect(stored?.managedStorageData).toEqual({});
		expect(stored?.localStorageData).toEqual({});
	});

	test('a snapshot a deployed build already wrote is preserved rather than destroyed', async () => {
		await seed([
			storedRecord('1', {token: 'token.0', localStorageData: DEPLOYED_SWAP_CAPTURED, managedStorageData: undefined}),
		]);
		const {default: accountStorage} = await loadAccountStorage();

		await accountStorage.stashAccountData('1', 'token.1', undefined, FIXTURE_CURRENT_INSTANCE_SNAPSHOT);

		const stored = await readRaw('1');
		expect(stored?.managedStorageData).toEqual(DEPLOYED_SWAP_CAPTURED);
		expect(stored?.token).toBe('token.1');
	});
});
