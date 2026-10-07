// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	resolveDiscoveryApiEndpoint,
	runtimeInstanceKey,
	storedInstanceKey,
} from '@app/features/app/state/InstanceSnapshotStore';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/RuntimeConfig';
import {
	FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	FIXTURE_NOW,
	FLUXER_ACCOUNTS_STORE_NAME,
	openGoldenAccountsDatabase,
} from '@app/features/auth/state/__fixtures__/AccountRecordFixtures';
import {planAccountRekey} from '@app/features/auth/state/AccountStorageContract';
import {classifyStoredAccount} from '@app/features/auth/state/StoredAccountInventoryPolicy';
import {AuthSessionStoredAccountResolver} from '@app/features/platform/state/auth_session/AuthSessionStoredAccountResolver';
import {beforeEach, describe, expect, test, vi} from 'vitest';

type AccountStorageModule = typeof import('@app/features/auth/state/AccountStorage');

const MIGRATED_ORIGIN = 'https://canary.fluxer.com';
const LEGACY_WEB_ORIGIN = 'https://web.canary.fluxer.app';
const OFFICIAL_INSTANCE_KEY = `${LEGACY_WEB_ORIGIN}/api`;
const USER_ID = '100000000000000002';
const OTHER_USER_ID = '100000000000000003';

function instanceSnapshot(origin: string): RuntimeConfigSnapshot {
	return {
		...FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
		apiEndpoint: `${origin}/api`,
		apiPublicEndpoint: 'https://api.canary.fluxer.app',
		webAppEndpoint: origin,
	};
}

function oldClientRecord(origin: string, userId: string): Record<string, unknown> {
	return {
		userId,
		token: `token.${userId}`,
		userData: {username: `user${userId}`, discriminator: '0001'},
		presenceIntent: null,
		localStorageData: {},
		managedStorageData: {},
		lastActive: FIXTURE_NOW - 1_000,
		instance: {
			apiEndpoint: `${origin}/api`,
			webAppEndpoint: origin,
			gatewayEndpoint: 'wss://gateway.fluxer.app',
		},
		isValid: true,
	};
}

function newClientRecord(origin: string, userId: string): Record<string, unknown> {
	return {
		...oldClientRecord(origin, userId),
		instance: instanceSnapshot(origin),
		storageKey: `${origin}/api::${userId}`,
	};
}

function classifyBrowserRecord(value: unknown) {
	return classifyStoredAccount({value, authoritativeStorageKey: null, source: 'idb'});
}

function setDocumentUrl(url: string): void {
	(window as unknown as {happyDOM: {setURL(url: string): void}}).happyDOM.setURL(url);
}

async function seed(records: ReadonlyArray<Record<string, unknown>>): Promise<void> {
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

async function readAllRaw(): Promise<Array<Record<string, unknown>>> {
	const database = await openGoldenAccountsDatabase(window.indexedDB);
	const records = await new Promise<Array<Record<string, unknown>>>((resolve, reject) => {
		const request = database
			.transaction([FLUXER_ACCOUNTS_STORE_NAME], 'readonly')
			.objectStore(FLUXER_ACCOUNTS_STORE_NAME)
			.getAll();
		request.onsuccess = () => resolve(request.result as Array<Record<string, unknown>>);
		request.onerror = () => reject(request.error ?? new Error('IndexedDB read failed'));
	});
	database.close();
	return records;
}

function createResolver(accountStorage: AccountStorageModule['default'], inputs: Array<string>) {
	return new AuthSessionStoredAccountResolver({
		accountStorage,
		resolveRuntimeEndpoint: async (input) => {
			inputs.push(input);
			const snapshot = instanceSnapshot(new URL(resolveDiscoveryApiEndpoint(input)).origin);
			const instanceKey = runtimeInstanceKey(snapshot);
			if (instanceKey === null) {
				throw new Error(`No instance key for ${input}`);
			}
			return {snapshot, instanceKey, productName: 'Fluxer'};
		},
	});
}

beforeEach(() => {
	vi.resetModules();
	Object.defineProperty(window, 'indexedDB', {value: new IDBFactory(), configurable: true, writable: true});
	window.localStorage.clear();
	setDocumentUrl('fluxer-app://app/channels/@me');
});

describe('classifying an account stored on the migrated official domain', () => {
	test('an old client record is a recovery candidate for the official instance', () => {
		const classified = classifyBrowserRecord(oldClientRecord(MIGRATED_ORIGIN, USER_ID));

		expect(classified.kind).toBe('runtime-recovery');
		expect(classified).toMatchObject({
			instanceKey: OFFICIAL_INSTANCE_KEY,
			storageKey: `${OFFICIAL_INSTANCE_KEY}::${USER_ID}`,
		});
	});

	test('the same user harvested from either official origin is one account', () => {
		const migrated = classifyBrowserRecord(oldClientRecord(MIGRATED_ORIGIN, USER_ID));
		const legacy = classifyBrowserRecord(oldClientRecord(LEGACY_WEB_ORIGIN, USER_ID));

		expect(migrated).toMatchObject({kind: 'runtime-recovery', storageKey: `${OFFICIAL_INSTANCE_KEY}::${USER_ID}`});
		expect(legacy).toMatchObject({kind: 'runtime-recovery', storageKey: `${OFFICIAL_INSTANCE_KEY}::${USER_ID}`});
	});

	test('a record the new web client keyed on the migrated domain is recovered under the official instance', () => {
		const classified = classifyBrowserRecord(newClientRecord(MIGRATED_ORIGIN, USER_ID));

		expect(classified).toMatchObject({
			kind: 'runtime-recovery',
			instanceKey: OFFICIAL_INSTANCE_KEY,
			storageKey: `${OFFICIAL_INSTANCE_KEY}::${USER_ID}`,
		});
	});

	test('a record keyed on the official instance stays ready', () => {
		const classified = classifyBrowserRecord(newClientRecord(LEGACY_WEB_ORIGIN, USER_ID));

		expect(classified).toMatchObject({kind: 'ready', record: {storageKey: `${OFFICIAL_INSTANCE_KEY}::${USER_ID}`}});
	});

	test('a desktop row whose authoritative key names the migrated domain is not silently rekeyed', () => {
		const classified = classifyStoredAccount({
			value: newClientRecord(MIGRATED_ORIGIN, USER_ID),
			authoritativeStorageKey: `${MIGRATED_ORIGIN}/api::${USER_ID}`,
			source: 'desktop',
		});

		expect(classified.kind).toBe('unavailable');
	});

	test('an account on another instance keeps its own identity', () => {
		const classified = classifyBrowserRecord(oldClientRecord('https://chat.example.test', USER_ID));

		expect(classified).toMatchObject({
			kind: 'runtime-recovery',
			instanceKey: 'https://chat.example.test/api',
			storageKey: `https://chat.example.test/api::${USER_ID}`,
		});
	});
});

describe('a web user on the migrated official domain', () => {
	test.each(['https://fluxer.com', MIGRATED_ORIGIN])(
		'on %s the stored account keeps the same-origin instance it was stored with',
		(origin) => {
			setDocumentUrl(`${origin}/channels/@me`);

			expect(storedInstanceKey(`${origin}/api`)).toBe(`${origin}/api`);
			expect(classifyBrowserRecord(oldClientRecord(origin, USER_ID))).toMatchObject({
				kind: 'runtime-recovery',
				instanceKey: `${origin}/api`,
				storageKey: `${origin}/api::${USER_ID}`,
			});
			expect(classifyBrowserRecord(newClientRecord(origin, USER_ID))).toMatchObject({
				kind: 'ready',
				record: {storageKey: `${origin}/api::${USER_ID}`, instance: {apiEndpoint: `${origin}/api`}},
			});
		},
	);
});

describe('restoring the session of an account harvested from the migrated official domain', () => {
	test.each([
		['an old client record', oldClientRecord],
		['a record keyed by the new web client', newClientRecord],
	])('%s signs in on the official instance and is stored once', async (_label, buildRecord) => {
		await seed([buildRecord(MIGRATED_ORIGIN, USER_ID)]);
		const {default: accountStorage} = await import('@app/features/auth/state/AccountStorage');
		await accountStorage.init();
		const inputs: Array<string> = [];

		const restoration = await createResolver(accountStorage, inputs).resolveActiveRestoration({
			storageKey: null,
			userId: USER_ID,
			token: `token.${USER_ID}`,
		});

		expect(inputs).toEqual([OFFICIAL_INSTANCE_KEY]);
		expect(restoration.kind).toBe('resolved');
		expect(restoration).toMatchObject({
			accountKey: `${OFFICIAL_INSTANCE_KEY}::${USER_ID}`,
			record: {
				userId: USER_ID,
				token: `token.${USER_ID}`,
				storageKey: `${OFFICIAL_INSTANCE_KEY}::${USER_ID}`,
				instance: {apiEndpoint: OFFICIAL_INSTANCE_KEY, webAppEndpoint: LEGACY_WEB_ORIGIN},
			},
		});
		const stored = await readAllRaw();
		expect(stored).toHaveLength(1);
		expect(stored[0]).toMatchObject({
			userId: USER_ID,
			storageKey: `${OFFICIAL_INSTANCE_KEY}::${USER_ID}`,
			instance: {apiEndpoint: OFFICIAL_INSTANCE_KEY, webAppEndpoint: LEGACY_WEB_ORIGIN},
		});
		const inventory = await accountStorage.getAccountInventory();
		expect(inventory.readyEntries.map((entry) => entry.record.storageKey)).toEqual([
			`${OFFICIAL_INSTANCE_KEY}::${USER_ID}`,
		]);
		expect(inventory.runtimeRecoveryCandidates).toHaveLength(0);
		expect(inventory.unavailableRecords).toHaveLength(0);
	});

	test('every harvested account is keyed once under the official instance', async () => {
		await seed([oldClientRecord(MIGRATED_ORIGIN, USER_ID), oldClientRecord(MIGRATED_ORIGIN, OTHER_USER_ID)]);
		const {default: accountStorage} = await import('@app/features/auth/state/AccountStorage');
		await accountStorage.init();
		const restoration = await createResolver(accountStorage, []).resolveActiveRestoration({
			storageKey: null,
			userId: USER_ID,
			token: `token.${USER_ID}`,
		});
		if (restoration.kind !== 'resolved') {
			throw new Error(`Expected a resolved restoration, got ${restoration.kind}`);
		}

		const {records} = await accountStorage.getAllAccounts();
		const plan = planAccountRekey(records, {
			target: {userId: USER_ID, token: `token.${USER_ID}`, instance: restoration.record.instance},
			now: FIXTURE_NOW,
		});

		expect(records.map((record) => record.storageKey).sort()).toEqual([
			`${OFFICIAL_INSTANCE_KEY}::${USER_ID}`,
			`${OFFICIAL_INSTANCE_KEY}::${OTHER_USER_ID}`,
		]);
		expect(plan.qualifiedRecords.map((record) => record.storageKey)).toContain(`${OFFICIAL_INSTANCE_KEY}::${USER_ID}`);
		expect(
			[...plan.qualifiedRecords, ...plan.deferredRecords].filter((record) => record.userId === USER_ID),
		).toHaveLength(1);
	});
});
