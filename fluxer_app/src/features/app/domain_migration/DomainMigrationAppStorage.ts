// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type DomainMigrationAppStorageEntry,
	isExportableLocalStorageKey,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import type {RuntimeConfigSnapshot} from '@app/features/app/state/InstanceSnapshotStore';
import type {StoredAccount} from '@app/features/auth/state/AccountStorage';
import {accountStorageKey, deriveAccountStorageKey} from '@app/features/auth/state/AccountStorageKey';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {LEGACY_APP_STORAGE_KEY_MAP} from '@app/features/platform/state/LegacyAppStorageKeyMap';
import {
	LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY,
	LEGACY_APP_STORAGE_UNCLASSIFIED_KEYS_KEY,
} from '@app/features/platform/state/LegacyAppStorageMigration';
import {GLOBAL_APP_STORAGE_SCOPE} from '@app/features/platform/state/PersistentStorage';
import type {AppStorageWrite, PersistentStorageBackend} from '@app/features/platform/state/PersistentStorageBackend';

export interface DomainMigrationAppStorageExport {
	app_storage: Array<DomainMigrationAppStorageEntry>;
	local_storage: Record<string, string>;
}

const DENIED_APP_STORAGE_KEYS: ReadonlySet<string> = new Set([
	LEGACY_APP_STORAGE_MIGRATION_MARKER_KEY,
	LEGACY_APP_STORAGE_UNCLASSIFIED_KEYS_KEY,
]);

export function isExportableAppStorageKey(key: string): boolean {
	return !DENIED_APP_STORAGE_KEYS.has(key) && isExportableLocalStorageKey(key);
}

function parseNameList(value: string | undefined): Array<string> {
	if (value === undefined) {
		return [];
	}
	try {
		const parsed: unknown = JSON.parse(value);
		return Array.isArray(parsed) ? parsed.filter((name): name is string => typeof name === 'string') : [];
	} catch {
		return [];
	}
}

function accountResetTombstones(keptValue: string | undefined): Array<string> {
	if (keptValue === undefined) {
		return [];
	}
	const kept = new Set(parseNameList(keptValue));
	const tombstones: Array<string> = [];
	for (const row of LEGACY_APP_STORAGE_KEY_MAP) {
		if (row.kind !== 'exact' || row.scope !== 'account') {
			continue;
		}
		if (!kept.has(row.key)) {
			tombstones.push(row.key);
		}
	}
	return tombstones;
}

function deletedRawNames(entries: ReadonlyMap<string, string>): Array<string> {
	const names = [
		...parseNameList(entries.get(AppStorageKey.DELETED_KEYS)),
		...accountResetTombstones(entries.get(AppStorageKey.ACCOUNT_RESET_KEPT_KEYS)),
	];
	return names.filter((name) => !entries.has(name));
}

async function loadScope(backend: PersistentStorageBackend, scope: string): Promise<Map<string, string>> {
	const values = new Map<string, string>();
	for (const [key, entry] of await backend.load(scope)) {
		values.set(key, entry.value);
	}
	return values;
}

export async function collectExportableAppStorage(
	backend: PersistentStorageBackend,
	localStorage: Record<string, string>,
	accounts: ReadonlyArray<StoredAccount>,
	activeUserId: string | null,
): Promise<DomainMigrationAppStorageExport> {
	const scopes = new Map<string | null, string>([[null, GLOBAL_APP_STORAGE_SCOPE]]);
	for (const account of accounts) {
		const scope = deriveAccountStorageKey(account);
		if (scope !== null && !scopes.has(account.userId)) {
			scopes.set(account.userId, scope);
		}
	}
	const appStorage: Array<DomainMigrationAppStorageEntry> = [];
	const exportedLocalStorage = {...localStorage};
	for (const [userId, scope] of scopes) {
		const entries = await loadScope(backend, scope);
		for (const [key, value] of entries) {
			if (isExportableAppStorageKey(key)) {
				appStorage.push({user_id: userId, key, value});
			}
		}
		if (userId === null || userId === activeUserId) {
			for (const name of deletedRawNames(entries)) {
				delete exportedLocalStorage[name];
			}
		}
	}
	return {app_storage: appStorage, local_storage: exportedLocalStorage};
}

export async function writeImportedAppStorage(
	backend: PersistentStorageBackend,
	entries: ReadonlyArray<DomainMigrationAppStorageEntry>,
	accounts: ReadonlyArray<StoredAccount>,
	instance: RuntimeConfigSnapshot,
): Promise<void> {
	const scopes = new Map<string, string>();
	for (const account of accounts) {
		const scope = accountStorageKey(account.userId, instance);
		if (scope !== null) {
			scopes.set(account.userId, scope);
		}
	}
	const writes: Array<AppStorageWrite> = [];
	for (const entry of entries) {
		if (!isExportableAppStorageKey(entry.key)) {
			continue;
		}
		const scope = entry.user_id === null ? GLOBAL_APP_STORAGE_SCOPE : scopes.get(entry.user_id);
		if (scope !== undefined) {
			writes.push({scope, key: entry.key, value: entry.value});
		}
	}
	if (writes.length > 0) {
		await backend.setMany(writes);
	}
}
