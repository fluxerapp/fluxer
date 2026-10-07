// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	FIXTURE_CURRENT_INSTANCE_KEY,
	FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
	FIXTURE_NOW,
	FLUXER_ACCOUNTS_V2_RECORD_CLASSES,
} from '@app/features/auth/state/__fixtures__/AccountRecordFixtures';
import type {
	AccountRekeyOutcome,
	AccountRekeyTarget,
	KeyedStoredAccount,
} from '@app/features/auth/state/AccountStorage';
import {
	CONTENT_STORAGE_KEYS,
	expectedGoldenStorageRows,
	GOLDEN_DEPLOYED_STORAGE_ENTRIES,
	GOLDEN_LOCAL_STORAGE_CORPUS,
	GOLDEN_UNKNOWN_STORAGE_ENTRIES,
	goldenStorageRowIdentity,
	NOT_MIGRATED_STORAGE_KEYS,
	OWNED_CONTENT_STORAGE_KEYS,
	readGoldenLocalStorageSnapshot,
	SHARED_CONTENT_STORAGE_KEYS,
	seedGoldenLocalStorage,
} from '@app/features/platform/state/__fixtures__/LegacyStorageFixtures';
import {installRuntimeBootstrap} from '@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture';
import type {AppStorageWrite, PersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

type MigrationModule = typeof import('@app/features/platform/state/LegacyAppStorageMigration');
type BackendModule = typeof import('@app/features/platform/state/PersistentStorageBackend');

const ACTIVE_USER_ID = '100000000000000002';
const OTHER_USER_ID = '100000000000000003';
const THIRD_USER_ID = '100000000000000007';
const ACTIVE_SCOPE = `${FIXTURE_CURRENT_INSTANCE_KEY}::${ACTIVE_USER_ID}`;
const OTHER_SCOPE = `${FIXTURE_CURRENT_INSTANCE_KEY}::${OTHER_USER_ID}`;
const THIRD_SCOPE = `${FIXTURE_CURRENT_INSTANCE_KEY}::${THIRD_USER_ID}`;
const UNAUTHENTICATED_SCOPE = 'unauthenticated';
const GLOBAL_SCOPE = 'global';
const SHARED_CONTENT_REVIEW_KEY = 'fluxer:migration:shared-content-review';
const ALL_ACCOUNT_SCOPES = [UNAUTHENTICATED_SCOPE, ACTIVE_SCOPE, OTHER_SCOPE];
const DEPLOYED_ACCOUNT_SCOPES = [ACTIVE_SCOPE, OTHER_SCOPE];
const ACTIVE_TOKEN = GOLDEN_LOCAL_STORAGE_CORPUS.find((entry) => entry.key === 'token')!.value;
const CURRENT_ACCOUNT: AccountRekeyTarget = {
	userId: ACTIVE_USER_ID,
	token: ACTIVE_TOKEN,
	instance: FIXTURE_CURRENT_INSTANCE_SNAPSHOT,
};

const KEYED_ACCOUNTS: ReadonlyArray<KeyedStoredAccount> = (() => {
	const recordClass = FLUXER_ACCOUNTS_V2_RECORD_CLASSES.find((candidate) => candidate.id === 'same-instance-pair')!;
	return recordClass.records.map((record, index) => ({
		...record,
		token: record.userId === ACTIVE_USER_ID ? ACTIVE_TOKEN : record.token,
		storageKey: recordClass.expectedStorageKeys[index],
	}));
})();

const THREE_KEYED_ACCOUNTS: ReadonlyArray<KeyedStoredAccount> = [
	...KEYED_ACCOUNTS,
	{...KEYED_ACCOUNTS[1]!, userId: THIRD_USER_ID, token: `token.${THIRD_USER_ID}`, storageKey: THIRD_SCOPE},
];

const identity = goldenStorageRowIdentity;

function expectedPlanEntries(): Map<string, string> {
	const rows = expectedGoldenStorageRows({
		global: GLOBAL_SCOPE,
		everyAccount: ALL_ACCOUNT_SCOPES,
		contentAccount: ACTIVE_SCOPE,
		sharedContent: DEPLOYED_ACCOUNT_SCOPES,
		namedAccount: OTHER_SCOPE,
	});
	for (const scope of DEPLOYED_ACCOUNT_SCOPES) {
		rows.set(identity(scope, SHARED_CONTENT_REVIEW_KEY), 'pending');
	}
	return rows;
}

function completeMarker(marker: {
	readonly status: 'complete' | 'partial';
	readonly pendingScopes: ReadonlyArray<string>;
	readonly deferredAccounts?: number;
	readonly sharedContentScopes?: ReadonlyArray<string>;
	readonly deferredUserIds?: ReadonlyArray<string>;
}): string {
	return JSON.stringify({
		version: 1,
		status: marker.status,
		pendingScopes: marker.pendingScopes,
		deferredAccounts: marker.deferredAccounts ?? 0,
		contentScope: ACTIVE_SCOPE,
		sharedContentScopes: marker.sharedContentScopes ?? DEPLOYED_ACCOUNT_SCOPES,
		deferredUserIds: marker.deferredUserIds ?? [],
	});
}

function planEntries(writes: ReadonlyArray<AppStorageWrite>): Map<string, string> {
	return new Map(writes.map((write) => [identity(write.scope, write.key), write.value]));
}

async function loadModules(): Promise<{migration: MigrationModule; backendModule: BackendModule}> {
	const backendModule = await import('@app/features/platform/state/PersistentStorageBackend');
	const migration = await import('@app/features/platform/state/LegacyAppStorageMigration');
	window.localStorage.clear();
	seedGoldenLocalStorage(window.localStorage);
	window.localStorage.setItem('userId', ACTIVE_USER_ID);
	return {migration, backendModule};
}

function accountSource(outcome: Partial<AccountRekeyOutcome> = {}): {
	migrateAccountStorageKeys: () => Promise<AccountRekeyOutcome>;
} {
	return {
		migrateAccountStorageKeys: () =>
			Promise.resolve({
				status: 'complete',
				source: 'idb',
				qualifiedRecords: [...KEYED_ACCOUNTS],
				deferredRecords: [],
				written: 0,
				...outcome,
			}),
	};
}

async function readBackend(backend: PersistentStorageBackend): Promise<Map<string, string>> {
	const entries = new Map<string, string>();
	for (const scope of [GLOBAL_SCOPE, ...ALL_ACCOUNT_SCOPES]) {
		for (const [key, entry] of await backend.load(scope)) {
			entries.set(identity(scope, key), entry.value);
		}
	}
	return entries;
}

function installLockManager(): {readonly names: Array<string>} {
	const names: Array<string> = [];
	let tail: Promise<unknown> = Promise.resolve();
	const manager = {
		request<T>(name: string, callback: () => Promise<T>): Promise<T> {
			names.push(name);
			const run = tail.then(() => callback());
			tail = run.then(
				() => undefined,
				() => undefined,
			);
			return run;
		},
	};
	Object.defineProperty(navigator, 'locks', {value: manager, configurable: true, writable: true});
	return {names};
}

beforeEach(() => {
	vi.resetModules();
	Object.defineProperty(window, 'indexedDB', {value: new IDBFactory(), configurable: true, writable: true});
	installLockManager();
	installRuntimeBootstrap();
	window.localStorage.clear();
});

afterEach(() => {
	Object.defineProperty(navigator, 'locks', {value: null, configurable: true, writable: true});
	window.localStorage.clear();
});

describe('the migration plan', () => {
	test('pins the names item 34 must clear and item 40 must invalidate', async () => {
		const {migration} = await loadModules();

		expect(migration.LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY).toBe('fluxer:migration:legacy-app-storage');
		expect(migration.LEGACY_APP_STORAGE_UNCLASSIFIED_KEYS_KEY).toBe('fluxer:migration:unclassified-legacy-keys');
		expect(migration.LEGACY_APP_STORAGE_MIGRATION_VERSION).toBe(1);
		expect(Object.values(migration.LegacyAppStorageMigrationStatus)).toEqual([
			'migrated',
			'partial',
			'already-done',
			'skipped',
			'failed',
		]);
		expect(Object.values(migration.LegacyAppStorageMigrationSkipReason)).toEqual([
			'memory-backend',
			'no-local-storage',
			'no-instance-key',
			'accounts-not-authoritative',
			'scope-not-pending',
		]);
	});

	test('matches the golden corpus destination for destination and byte for byte', async () => {
		const {migration} = await loadModules();

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts: KEYED_ACCOUNTS,
			contentScope: ACTIVE_SCOPE,
			sharedContentScopes: DEPLOYED_ACCOUNT_SCOPES,
		});

		expect(plan.scopes).toEqual(ALL_ACCOUNT_SCOPES);
		expect(plan.contentScope).toBe(ACTIVE_SCOPE);
		expect(planEntries(plan.writes)).toEqual(expectedPlanEntries());
		expect(plan.writes.every((write) => write.ifAbsent === true)).toBe(true);
	});

	test('every deployed corpus key is resolved by the key map or explicitly not migrated', async () => {
		const {migration} = await loadModules();
		const {isNotMigratedLegacyKey, resolveLegacyAppStorageKey} = await import(
			'@app/features/platform/state/LegacyAppStorageKeyMap'
		);

		for (const entry of GOLDEN_DEPLOYED_STORAGE_ENTRIES) {
			const covered = resolveLegacyAppStorageKey(entry.key) != null || isNotMigratedLegacyKey(entry.key);
			expect(covered, `${entry.key} resolves to nothing`).toBe(true);
		}

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts: KEYED_ACCOUNTS,
			contentScope: ACTIVE_SCOPE,
			sharedContentScopes: DEPLOYED_ACCOUNT_SCOPES,
		});
		expect([...plan.unclassifiedKeys].sort()).toEqual(GOLDEN_UNKNOWN_STORAGE_ENTRIES.map((entry) => entry.key).sort());
	});

	test('content the deployed build swapped per account lands in the active scope only, content it shared lands in every deployed account, and the synced-preference wire keys land nowhere', async () => {
		const {migration} = await loadModules();

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts: KEYED_ACCOUNTS,
			contentScope: ACTIVE_SCOPE,
			sharedContentScopes: DEPLOYED_ACCOUNT_SCOPES,
		});
		const scopesByKey = new Map<string, Array<string>>();
		for (const write of plan.writes) {
			scopesByKey.set(write.key, [...(scopesByKey.get(write.key) ?? []), write.scope]);
		}

		for (const legacyKey of OWNED_CONTENT_STORAGE_KEYS) {
			expect(scopesByKey.get(legacyKey), legacyKey).toEqual([ACTIVE_SCOPE]);
		}
		for (const legacyKey of SHARED_CONTENT_STORAGE_KEYS) {
			expect(scopesByKey.get(legacyKey)?.sort(), legacyKey).toEqual(DEPLOYED_ACCOUNT_SCOPES);
		}
		for (const legacyKey of NOT_MIGRATED_STORAGE_KEYS) {
			expect(scopesByKey.has(legacyKey), legacyKey).toBe(false);
		}
		for (const rawKey of ['token', 'userId', 'runtimeConfig', 'AccountManager']) {
			expect(scopesByKey.has(rawKey), rawKey).toBe(false);
		}
		expect(scopesByKey.get('UserSettings:syncedPreferencesLocal')?.sort()).toEqual([...ALL_ACCOUNT_SCOPES].sort());
		expect(scopesByKey.get('Theme')).toEqual([GLOBAL_SCOPE]);
	});

	test('a per-user neko key lands in that account scope only, under the name the reader asks for', async () => {
		const {migration} = await loadModules();

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts: KEYED_ACCOUNTS,
			contentScope: ACTIVE_SCOPE,
			sharedContentScopes: DEPLOYED_ACCOUNT_SCOPES,
		});
		const entries = planEntries(plan.writes);

		const perUserKey = `Accessibility:showNeko:${OTHER_USER_ID}`;
		expect(entries.get(identity(OTHER_SCOPE, perUserKey))).toBe('false');
		expect(entries.has(identity(ACTIVE_SCOPE, perUserKey))).toBe(false);
		expect(entries.has(identity(UNAUTHENTICATED_SCOPE, perUserKey))).toBe(false);
		for (const scope of ALL_ACCOUNT_SCOPES) {
			expect(entries.get(identity(scope, 'Accessibility:showNeko')), scope).toBe('true');
		}
	});

	test('a suffixed key naming an unknown account is skipped rather than routed to the active scope', async () => {
		const {migration} = await loadModules();
		window.localStorage.setItem('Accessibility:showNeko:999000000000000999', 'false');

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts: KEYED_ACCOUNTS,
			contentScope: ACTIVE_SCOPE,
			sharedContentScopes: DEPLOYED_ACCOUNT_SCOPES,
		});
		const entries = planEntries(plan.writes);

		expect(entries.get(identity(ACTIVE_SCOPE, 'Accessibility:showNeko'))).toBe('true');
		expect([...entries.keys()].filter((key) => key.includes('999000000000000999'))).toEqual([]);
		expect(window.localStorage.getItem('Accessibility:showNeko:999000000000000999')).toBe('false');
	});

	test('an active scope outside the account list degrades to the unauthenticated scope', async () => {
		const {migration} = await loadModules();

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts: KEYED_ACCOUNTS,
			contentScope: `${FIXTURE_CURRENT_INSTANCE_KEY}::404000000000000404`,
			sharedContentScopes: [],
		});

		expect(plan.contentScope).toBe(UNAUTHENTICATED_SCOPE);
		expect(planEntries(plan.writes).get(identity(UNAUTHENTICATED_SCOPE, 'Drafts'))).toBe(
			window.localStorage.getItem('Drafts'),
		);
	});

	test('an account the deployed build swapped storage for keeps its own snapshot, and the live values stay with the active account', async () => {
		const {migration} = await loadModules();
		const ownedByOther = {
			token: `token.${OTHER_USER_ID}`,
			userId: OTHER_USER_ID,
			'fluxer:ui:sidebar-width': '331',
			'fluxer_scheduled_maintenance_dismissed:4242': '1',
			'fluxer:experimental:unshipped-toggle': 'off',
			'fluxer:media_caps:v2': '{"other":true}',
			'fluxer:domain-migration': '{"other":true}',
		};
		const accounts = KEYED_ACCOUNTS.map((account) => ({
			...account,
			localStorageData: account.userId === OTHER_USER_ID ? ownedByOther : {'fluxer:ui:sidebar-width': '205'},
			managedStorageData: account.userId === OTHER_USER_ID ? ownedByOther : {'fluxer:ui:sidebar-width': '205'},
		}));

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts,
			contentScope: ACTIVE_SCOPE,
			sharedContentScopes: DEPLOYED_ACCOUNT_SCOPES,
		});
		const entries = planEntries(plan.writes);

		expect(entries.get(identity(ACTIVE_SCOPE, 'fluxer:ui:sidebar-width'))).toBe('286');
		expect(entries.get(identity(OTHER_SCOPE, 'fluxer:ui:sidebar-width'))).toBe('331');
		expect(entries.has(identity(UNAUTHENTICATED_SCOPE, 'fluxer:ui:sidebar-width'))).toBe(false);
		expect(entries.get(identity(OTHER_SCOPE, 'fluxer_scheduled_maintenance_dismissed:4242'))).toBe('1');
		expect(entries.has(identity(ACTIVE_SCOPE, 'fluxer_scheduled_maintenance_dismissed:4242'))).toBe(false);
		expect(entries.get(identity(OTHER_SCOPE, 'fluxer:experimental:unshipped-toggle'))).toBe('off');
		expect(entries.get(identity(GLOBAL_SCOPE, 'fluxer:experimental:unshipped-toggle'))).toBe('on');
		expect(entries.get(identity(GLOBAL_SCOPE, 'fluxer:media_caps:v2'))).toBe(
			window.localStorage.getItem('fluxer:media_caps:v2'),
		);
		for (const neverCarried of ['token', 'userId', 'fluxer:media_caps:v2', 'fluxer:domain-migration']) {
			expect(entries.has(identity(OTHER_SCOPE, neverCarried)), neverCarried).toBe(false);
		}
	});

	test('shared content reaches only the accounts the deployed build stored, never one added since', async () => {
		const {migration} = await loadModules();

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts: THREE_KEYED_ACCOUNTS,
			contentScope: ACTIVE_SCOPE,
			sharedContentScopes: DEPLOYED_ACCOUNT_SCOPES,
		});
		const entries = planEntries(plan.writes);

		for (const contentKey of CONTENT_STORAGE_KEYS) {
			expect(entries.has(identity(THIRD_SCOPE, contentKey)), contentKey).toBe(false);
			expect(entries.has(identity(UNAUTHENTICATED_SCOPE, contentKey)), contentKey).toBe(false);
		}
		for (const contentKey of SHARED_CONTENT_STORAGE_KEYS) {
			expect(entries.get(identity(OTHER_SCOPE, contentKey)), contentKey).toBe(window.localStorage.getItem(contentKey));
		}
		expect(entries.get(identity(THIRD_SCOPE, 'Notification'))).toBe(window.localStorage.getItem('Notification'));
	});

	test('with no content scope nothing an account owns is planted anywhere', async () => {
		const {migration} = await loadModules();

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts: KEYED_ACCOUNTS,
			contentScope: null,
			sharedContentScopes: [],
		});

		expect(plan.contentScope).toBeNull();
		for (const contentKey of CONTENT_STORAGE_KEYS) {
			expect(
				plan.writes.some((write) => write.key === contentKey),
				contentKey,
			).toBe(false);
		}
	});

	test('the raw mirror written by the scoped storage layer is never captured', async () => {
		const {migration} = await loadModules();
		window.localStorage.setItem(`fluxer:app-storage-mirror:${OTHER_SCOPE}::Drafts`, 'mirrored');

		const plan = migration.buildLegacyAppStorageMigrationPlan({
			storage: window.localStorage,
			accounts: KEYED_ACCOUNTS,
			contentScope: ACTIVE_SCOPE,
			sharedContentScopes: DEPLOYED_ACCOUNT_SCOPES,
		});

		expect(plan.writes.some((write) => write.value === 'mirrored')).toBe(false);
		expect(plan.unclassifiedKeys.some((key) => key.startsWith('fluxer:app-storage-mirror:'))).toBe(false);
	});
});

describe('the module graph', () => {
	async function staticImportClosure(entry: string): Promise<Array<string>> {
		const {readFileSync, existsSync} = await import('node:fs');
		const {dirname, join, relative, resolve} = await import('node:path');
		const {fileURLToPath} = await import('node:url');
		const sourceRoot = resolve(dirname(fileURLToPath(import.meta.url)), '../../..');

		const resolveSpecifier = (specifier: string): string | null => {
			if (!specifier.startsWith('@app/')) {
				return null;
			}
			const base = join(sourceRoot, specifier.slice('@app/'.length));
			return [`${base}.ts`, `${base}.tsx`].find((candidate) => existsSync(candidate)) ?? null;
		};

		const visited = new Set<string>();
		const walk = (file: string): void => {
			if (visited.has(file)) {
				return;
			}
			visited.add(file);
			const pattern = /^import\s+([\s\S]*?)from\s+'([^']+)'/gm;
			const source = readFileSync(file, 'utf8');
			let match = pattern.exec(source);
			while (match != null) {
				const resolved = /^\s*type\s/.test(match[1]) ? null : resolveSpecifier(match[2]);
				if (resolved != null) {
					walk(resolved);
				}
				match = pattern.exec(source);
			}
		};
		walk(join(sourceRoot, entry));
		return [...visited].map((file) => relative(sourceRoot, file));
	}

	test('never reaches RuntimeConfig, whose constructor throws on an older server', async () => {
		const closure = await staticImportClosure('features/platform/state/LegacyAppStorageMigration.ts');

		expect(closure).not.toContain('features/app/state/RuntimeConfig.ts');
	});

	test('is never imported by the scoped storage module, which would close a boot-path cycle', async () => {
		const closure = await staticImportClosure('features/platform/state/PersistentStorage.ts');

		expect(closure).not.toContain('features/platform/state/LegacyAppStorageMigration.ts');
	});
});

describe('running the migration', () => {
	test('writes the whole plan and the marker, and never deletes a legacy entry', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const corpusBefore = readGoldenLocalStorageSnapshot(window.localStorage);

		const result = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(result.status).toBe('migrated');
		expect(result.contentScope).toBe(ACTIVE_SCOPE);
		expect(result.scopes).toEqual(ALL_ACCOUNT_SCOPES);
		expect(result.pendingScopes).toEqual([]);

		const stored = await readBackend(backend);
		const marker = stored.get(identity(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage'));
		expect(marker).toBe(completeMarker({status: 'complete', pendingScopes: []}));
		stored.delete(identity(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage'));
		expect(stored).toEqual(expectedPlanEntries());
		expect(readGoldenLocalStorageSnapshot(window.localStorage)).toEqual(corpusBefore);
	});

	test('three runs are idempotent and a value the user changed between runs survives', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const request = {
			backend,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		};

		expect((await migration.runLegacyAppStorageMigration(request)).status).toBe('migrated');
		const afterFirstRun = await readBackend(backend);

		await backend.set(GLOBAL_SCOPE, 'Theme', '{"localTheme":"light"}');
		await backend.delete(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage');
		expect((await migration.runLegacyAppStorageMigration(request)).status).toBe('migrated');

		await backend.delete(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage');
		expect((await migration.runLegacyAppStorageMigration(request)).status).toBe('migrated');

		const afterThirdRun = await readBackend(backend);
		expect(afterThirdRun.get(identity(GLOBAL_SCOPE, 'Theme'))).toBe('{"localTheme":"light"}');
		afterFirstRun.set(identity(GLOBAL_SCOPE, 'Theme'), '{"localTheme":"light"}');
		expect(afterThirdRun).toEqual(afterFirstRun);
	});

	test('a marker already at the current version stops the migration before it reads localStorage', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		await backend.set(
			GLOBAL_SCOPE,
			'fluxer:migration:legacy-app-storage',
			completeMarker({status: 'complete', pendingScopes: []}),
		);
		let consulted = false;

		const result = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: {
				migrateAccountStorageKeys: () => {
					consulted = true;
					return accountSource().migrateAccountStorageKeys();
				},
			},
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(result.status).toBe('already-done');
		expect(consulted).toBe(false);
		expect((await readBackend(backend)).size).toBe(1);
	});

	test('a marker written while accounts were still deferred does not stop the next run', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		await backend.set(
			GLOBAL_SCOPE,
			'fluxer:migration:legacy-app-storage',
			completeMarker({status: 'partial', pendingScopes: [], deferredAccounts: 1}),
		);

		const result = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(result.status).toBe('migrated');
		const stored = await readBackend(backend);
		expect(stored.get(identity(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage'))).toBe(
			completeMarker({status: 'complete', pendingScopes: []}),
		);
		expect(stored.get(identity(OTHER_SCOPE, 'UserSettings:syncedPreferencesLocal'))).toBe(
			window.localStorage.getItem('UserSettings:syncedPreferencesLocal'),
		);
	});

	test('a run after a deferred account qualifies gives it the shared content and leaves the swapped content with its owner', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const activeOnly = KEYED_ACCOUNTS.filter((account) => account.userId === ACTIVE_USER_ID);
		const deferred = KEYED_ACCOUNTS.filter((account) => account.userId !== ACTIVE_USER_ID);
		const first = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource({status: 'deferred', qualifiedRecords: [...activeOnly], deferredRecords: [...deferred]}),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});
		expect(first.status).toBe('partial');
		expect(first.deferredAccounts).toBe(1);
		expect((await backend.get(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage'))?.value).toBe(
			completeMarker({
				status: 'partial',
				pendingScopes: [],
				deferredAccounts: 1,
				sharedContentScopes: [ACTIVE_SCOPE],
				deferredUserIds: [OTHER_USER_ID],
			}),
		);
		const other = KEYED_ACCOUNTS.find((account) => account.userId === OTHER_USER_ID)!;

		const second = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource(),
			currentAccount: {userId: OTHER_USER_ID, token: other.token!, instance: FIXTURE_CURRENT_INSTANCE_SNAPSHOT},
			now: FIXTURE_NOW,
		});

		expect(second.status).toBe('migrated');
		expect(second.contentScope).toBe(ACTIVE_SCOPE);
		const stored = await readBackend(backend);
		for (const contentKey of CONTENT_STORAGE_KEYS) {
			expect(stored.get(identity(ACTIVE_SCOPE, contentKey)), contentKey).toBe(window.localStorage.getItem(contentKey));
		}
		for (const contentKey of OWNED_CONTENT_STORAGE_KEYS) {
			expect(stored.has(identity(OTHER_SCOPE, contentKey)), contentKey).toBe(false);
		}
		for (const contentKey of SHARED_CONTENT_STORAGE_KEYS) {
			expect(stored.get(identity(OTHER_SCOPE, contentKey)), contentKey).toBe(window.localStorage.getItem(contentKey));
		}
		expect(stored.get(identity(OTHER_SCOPE, 'Notification'))).toBe(window.localStorage.getItem('Notification'));
		expect(stored.get(identity(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage'))).toBe(
			completeMarker({status: 'complete', pendingScopes: []}),
		);
	});

	test('an account added after the upgrade never receives legacy content when a later run completes the migration', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const activeOnly = KEYED_ACCOUNTS.filter((account) => account.userId === ACTIVE_USER_ID);
		const deferred = KEYED_ACCOUNTS.filter((account) => account.userId !== ACTIVE_USER_ID);
		const addedSince = THREE_KEYED_ACCOUNTS.filter((account) => account.userId === THIRD_USER_ID);
		await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource({status: 'deferred', qualifiedRecords: [...activeOnly], deferredRecords: [...deferred]}),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		const stillDeferred = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource({
				status: 'deferred',
				qualifiedRecords: [...activeOnly, ...addedSince],
				deferredRecords: [...deferred],
			}),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});
		expect(stillDeferred.status).toBe('partial');

		const completed = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource({qualifiedRecords: [...THREE_KEYED_ACCOUNTS]}),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(completed.status).toBe('migrated');
		for (const contentKey of CONTENT_STORAGE_KEYS) {
			expect(await backend.get(THIRD_SCOPE, contentKey), contentKey).toBeNull();
		}
		for (const contentKey of SHARED_CONTENT_STORAGE_KEYS) {
			expect((await backend.get(OTHER_SCOPE, contentKey))?.value, contentKey).toBe(
				window.localStorage.getItem(contentKey),
			);
		}
		expect((await backend.get(THIRD_SCOPE, 'Notification'))?.value).toBe(window.localStorage.getItem('Notification'));
		expect((await backend.get(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage'))?.value).toBe(
			completeMarker({status: 'complete', pendingScopes: []}),
		);
	});

	test('a memory backend is skipped and never marked', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(null);

		const result = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(result).toMatchObject({status: 'skipped', reason: 'memory-backend'});
		expect(await backend.get(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage')).toBeNull();
		expect((await readBackend(backend)).size).toBe(0);
	});

	test('a non-authoritative account list is skipped and never marked', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);

		const result = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource({status: 'skipped', source: 'desktop', qualifiedRecords: []}),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(result).toMatchObject({status: 'skipped', reason: 'accounts-not-authoritative'});
		expect((await readBackend(backend)).size).toBe(0);
	});

	test('a failed account re-key is skipped rather than fanned out into unauthenticated alone', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);

		const result = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource({status: 'failed', qualifiedRecords: []}),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(result).toMatchObject({status: 'skipped', reason: 'accounts-not-authoritative'});
		expect((await readBackend(backend)).size).toBe(0);
	});

	test('an unusable active instance writes nothing and activates nothing', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const unkeyable = {...FIXTURE_CURRENT_INSTANCE_SNAPSHOT, apiEndpoint: 'https://user:pw@fluxer.app/api?tenant=1'};

		const result = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource(),
			currentAccount: {...CURRENT_ACCOUNT, instance: unkeyable},
			now: FIXTURE_NOW,
		});

		expect(result).toMatchObject({status: 'skipped', reason: 'no-instance-key'});
		expect(result.contentScope).toBe(UNAUTHENTICATED_SCOPE);
		expect(result.scopes).toEqual([]);
		expect((await readBackend(backend)).size).toBe(0);
	});

	test('a throwing backend leaves no marker and does not reject', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const failing: PersistentStorageBackend = {
			...backend,
			setMany: () => Promise.reject(new backendModule.AppStorageOperationError('write entries')),
		};

		const result = await migration.runLegacyAppStorageMigration({
			backend: failing,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(result.status).toBe('failed');
		expect(await backend.get(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage')).toBeNull();
		expect((await readBackend(backend)).size).toBe(0);
	});

	test('exactly one tab writes the plan when the migration lock serialises them', async () => {
		const {migration, backendModule} = await loadModules();
		const lock = installLockManager();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		let setManyCalls = 0;
		const counting: PersistentStorageBackend = {
			...backend,
			setMany: (writes) => {
				setManyCalls += 1;
				return backend.setMany(writes);
			},
		};
		const request = {
			backend: counting,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		};

		const [first, second] = await Promise.all([
			migration.runLegacyAppStorageMigration(request),
			migration.runLegacyAppStorageMigration(request),
		]);

		expect(lock.names).toEqual(['fluxer:app-storage:migration', 'fluxer:app-storage:migration']);
		expect([first.status, second.status]).toEqual(['migrated', 'already-done']);
		expect(setManyCalls).toBe(1);
	});
});

describe('quota degradation and first-touch completion', () => {
	const limits = {maxTotalBytes: 1, maxEntries: 1};

	test('a plan over the caps seeds the active scope and records the rest as pending', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);

		const result = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
			limits,
		});

		expect(result.status).toBe('partial');
		expect(result.pendingScopes).toEqual([OTHER_SCOPE]);
		const stored = await readBackend(backend);
		expect(stored.get(identity(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage'))).toBe(
			completeMarker({status: 'partial', pendingScopes: [OTHER_SCOPE]}),
		);
		expect([...stored.keys()].some((key) => key.startsWith(OTHER_SCOPE))).toBe(false);
		expect(stored.get(identity(ACTIVE_SCOPE, 'Drafts'))).toBe(window.localStorage.getItem('Drafts'));
	});

	test('a plan over the caps still seeds the global scope, which no pending-scope pass revisits', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);

		const result = await migration.runLegacyAppStorageMigration({
			backend,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
			limits,
		});

		expect(result.pendingScopes).toEqual([OTHER_SCOPE]);
		const stored = await readBackend(backend);
		expect(stored.get(identity(GLOBAL_SCOPE, 'Theme'))).toBe(window.localStorage.getItem('Theme'));
		expect(stored.get(identity(GLOBAL_SCOPE, 'fluxer:migration:unclassified-legacy-keys'))).toBe(
			JSON.stringify(GOLDEN_UNKNOWN_STORAGE_ENTRIES.map((entry) => entry.key).sort()),
		);
	});

	test('a QuotaExceededError from the transaction degrades to the same partial plan', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		let attempts = 0;
		const quotaBound: PersistentStorageBackend = {
			...backend,
			setMany: (writes) => {
				attempts += 1;
				if (attempts === 1) {
					return Promise.reject(new backendModule.AppStorageQuotaExceededError('write entries'));
				}
				return backend.setMany(writes);
			},
		};

		const result = await migration.runLegacyAppStorageMigration({
			backend: quotaBound,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(attempts).toBe(2);
		expect(result.status).toBe('partial');
		expect(result.pendingScopes).toEqual([OTHER_SCOPE]);
	});

	test('a persistent QuotaExceededError leaves no marker at all', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const quotaBound: PersistentStorageBackend = {
			...backend,
			setMany: () => Promise.reject(new backendModule.AppStorageQuotaExceededError('write entries')),
		};

		const result = await migration.runLegacyAppStorageMigration({
			backend: quotaBound,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		});

		expect(result.status).toBe('failed');
		expect(await backend.get(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage')).toBeNull();
	});

	test('first touch completes a pending scope write-if-absent with the shared content and none another account owns', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const request = {
			backend,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
			limits,
		};
		await migration.runLegacyAppStorageMigration(request);
		await backend.set(OTHER_SCOPE, 'Accessibility:motion', '{"userChanged":true}');

		const completed = await migration.completePendingLegacyAppStorageScope(request, OTHER_SCOPE);

		expect(completed.status).toBe('migrated');
		expect(completed.pendingScopes).toEqual([]);
		const stored = await readBackend(backend);
		expect(stored.get(identity(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage'))).toBe(
			completeMarker({status: 'complete', pendingScopes: []}),
		);
		expect(stored.get(identity(OTHER_SCOPE, 'Accessibility:motion'))).toBe('{"userChanged":true}');
		expect(stored.get(identity(OTHER_SCOPE, `Accessibility:showNeko:${OTHER_USER_ID}`))).toBe('false');
		expect(stored.get(identity(OTHER_SCOPE, 'UserSettings:syncedPreferencesLocal'))).toBe(
			window.localStorage.getItem('UserSettings:syncedPreferencesLocal'),
		);
		for (const contentKey of OWNED_CONTENT_STORAGE_KEYS) {
			expect(stored.has(identity(OTHER_SCOPE, contentKey)), contentKey).toBe(false);
		}
		for (const contentKey of SHARED_CONTENT_STORAGE_KEYS) {
			expect(stored.get(identity(OTHER_SCOPE, contentKey)), contentKey).toBe(window.localStorage.getItem(contentKey));
		}
	});

	test('completing one pending scope leaves every other pending scope waiting for its own first touch', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const request = {
			backend,
			accounts: accountSource({qualifiedRecords: [...THREE_KEYED_ACCOUNTS]}),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
			limits,
		};
		const seeded = await migration.runLegacyAppStorageMigration(request);
		expect(seeded.pendingScopes).toEqual([OTHER_SCOPE, THIRD_SCOPE]);

		const first = await migration.completePendingLegacyAppStorageScope(request, OTHER_SCOPE);

		expect(first.status).toBe('partial');
		expect(first.pendingScopes).toEqual([THIRD_SCOPE]);
		expect((await backend.get(GLOBAL_SCOPE, 'fluxer:migration:legacy-app-storage'))?.value).toBe(
			completeMarker({
				status: 'partial',
				pendingScopes: [THIRD_SCOPE],
				sharedContentScopes: [ACTIVE_SCOPE, OTHER_SCOPE, THIRD_SCOPE],
			}),
		);

		const second = await migration.completePendingLegacyAppStorageScope(request, THIRD_SCOPE);

		expect(second.status).toBe('migrated');
		expect(second.pendingScopes).toEqual([]);
		expect((await backend.get(THIRD_SCOPE, 'UserSettings:syncedPreferencesLocal'))?.value).toBe(
			window.localStorage.getItem('UserSettings:syncedPreferencesLocal'),
		);
	});

	test('completing a scope that is not pending writes nothing', async () => {
		const {migration, backendModule} = await loadModules();
		const backend = backendModule.createPersistentStorageBackend(window.indexedDB);
		const request = {
			backend,
			accounts: accountSource(),
			currentAccount: CURRENT_ACCOUNT,
			now: FIXTURE_NOW,
		};
		await migration.runLegacyAppStorageMigration(request);
		const before = await readBackend(backend);

		const completed = await migration.completePendingLegacyAppStorageScope(request, OTHER_SCOPE);

		expect(completed).toMatchObject({status: 'skipped', reason: 'scope-not-pending'});
		expect(await readBackend(backend)).toEqual(before);
	});
});
