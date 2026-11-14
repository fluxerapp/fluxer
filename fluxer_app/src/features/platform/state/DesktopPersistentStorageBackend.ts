// SPDX-License-Identifier: AGPL-3.0-or-later

import {type DesktopStoreAccessOptions, resolveDesktopStoreAPI} from '@app/features/platform/state/DesktopStoreAccess';
import {
	type AppStorageEntry,
	AppStorageOperationError,
	type AppStorageStamp,
	type AppStorageWrite,
	type PersistentStorageBackend,
	PersistentStorageBackendKind,
} from '@app/features/platform/state/PersistentStorageBackend';
import type {DesktopStorageAPI, DesktopStorageEntry} from '@fluxer/desktop_ipc/src/StorageContract';

const DESKTOP_STORAGE_METHODS = [
	'getStatus',
	'load',
	'get',
	'set',
	'delete',
	'clearAllForScope',
	'clearAllExcept',
	'setMany',
	'import',
	'getMarker',
	'setMarker',
] as const satisfies ReadonlyArray<keyof DesktopStorageAPI>;

export function getDesktopStorageAPI(options?: DesktopStoreAccessOptions): DesktopStorageAPI | null {
	return resolveDesktopStoreAPI((electron) => electron.desktopStorage, DESKTOP_STORAGE_METHODS, options);
}

function toStamp(updatedAt: number): AppStorageStamp {
	return {wall: Number.isFinite(updatedAt) ? updatedAt : 0, seq: 0};
}

function toEntry(entry: DesktopStorageEntry): AppStorageEntry {
	return {value: entry.value, updatedAt: toStamp(entry.updatedAt)};
}

async function run<Result>(operation: string, work: () => Promise<Result>): Promise<Result> {
	try {
		return await work();
	} catch (error) {
		throw new AppStorageOperationError(operation, {cause: error});
	}
}

export function createDesktopPersistentStorageBackend(api: DesktopStorageAPI): PersistentStorageBackend {
	return {
		kind: PersistentStorageBackendKind.DESKTOP,

		async load(scope) {
			const entries = await run('load scope', () => api.load(scope));
			return new Map(entries.map((entry) => [entry.key, toEntry(entry)]));
		},

		async get(scope, key) {
			const entry = await run('read value', () => api.get(scope, key));
			return entry == null ? null : toEntry(entry);
		},

		set(scope, key, value) {
			return run('write value', () => api.set(scope, key, value));
		},

		delete(scope, key) {
			return run('delete value', () => api.delete(scope, key));
		},

		clearAllForScope(scope) {
			return run('clear scope', () => api.clearAllForScope(scope));
		},

		clearAllExcept(keysToKeep) {
			return run('clear entries', () => api.clearAllExcept([...keysToKeep]));
		},

		setMany(writes) {
			return run('write entries', () =>
				api.setMany(
					writes.map((write: AppStorageWrite) => ({
						scope: write.scope,
						key: write.key,
						value: write.value,
						ifAbsent: write.ifAbsent === true,
					})),
				),
			);
		},
	};
}
