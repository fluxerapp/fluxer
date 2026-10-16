// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	DesktopAccountCompareAndSwapRequest,
	DesktopAccountImportReport,
	DesktopAccountImportRequest,
	DesktopAccountPruneReport,
	DesktopAccountPruneRequest,
	DesktopAccountRecord,
	DesktopAccountStorageAPI,
} from '@fluxer/desktop_ipc/src/AccountContract';
import {DESKTOP_ACCOUNT_CHANNELS} from '@fluxer/desktop_ipc/src/AccountContract';
import type {
	DesktopKnownInstanceRecord,
	DesktopKnownInstanceStorageAPI,
} from '@fluxer/desktop_ipc/src/KnownInstanceContract';
import {DESKTOP_KNOWN_INSTANCE_CHANNELS} from '@fluxer/desktop_ipc/src/KnownInstanceContract';
import type {
	DesktopStorageAPI,
	DesktopStorageEntry,
	DesktopStorageImportReport,
	DesktopStorageImportRequest,
	DesktopStorageWrite,
	DesktopStoreStatus,
} from '@fluxer/desktop_ipc/src/StorageContract';
import {DESKTOP_STORAGE_CHANNELS} from '@fluxer/desktop_ipc/src/StorageContract';

interface DesktopStorageInvoker {
	invoke: (channel: string, ...args: Array<unknown>) => Promise<unknown>;
}

interface DesktopStoragePreloadAPI {
	readonly desktopAccounts: DesktopAccountStorageAPI;
	readonly desktopStorage: DesktopStorageAPI;
	readonly desktopKnownInstances: DesktopKnownInstanceStorageAPI;
}

export function createDesktopStoragePreloadAPI(renderer: DesktopStorageInvoker): DesktopStoragePreloadAPI {
	const invoke = <T>(channel: string, ...args: Array<unknown>): Promise<T> =>
		renderer.invoke(channel, ...args) as Promise<T>;
	return Object.freeze({
		desktopAccounts: Object.freeze<DesktopAccountStorageAPI>({
			getAll: () => invoke<Array<DesktopAccountRecord>>(DESKTOP_ACCOUNT_CHANNELS.getAll),
			get: (storageKey: string) => invoke<DesktopAccountRecord | null>(DESKTOP_ACCOUNT_CHANNELS.get, storageKey),
			upsert: (account: DesktopAccountRecord) => invoke<void>(DESKTOP_ACCOUNT_CHANNELS.upsert, account),
			compareAndSwap: (request: DesktopAccountCompareAndSwapRequest) =>
				invoke<boolean>(DESKTOP_ACCOUNT_CHANNELS.compareAndSwap, request),
			delete: (storageKey: string) => invoke<void>(DESKTOP_ACCOUNT_CHANNELS.delete, storageKey),
			import: (request: DesktopAccountImportRequest) =>
				invoke<DesktopAccountImportReport>(DESKTOP_ACCOUNT_CHANNELS.import, request),
			prune: (request: DesktopAccountPruneRequest) =>
				invoke<DesktopAccountPruneReport>(DESKTOP_ACCOUNT_CHANNELS.prune, request),
		}),
		desktopStorage: Object.freeze<DesktopStorageAPI>({
			getStatus: () => invoke<DesktopStoreStatus>(DESKTOP_STORAGE_CHANNELS.getStatus),
			load: (scope: string) => invoke<Array<DesktopStorageEntry>>(DESKTOP_STORAGE_CHANNELS.load, scope),
			get: (scope: string, key: string) => invoke<DesktopStorageEntry | null>(DESKTOP_STORAGE_CHANNELS.get, scope, key),
			set: (scope: string, key: string, value: string) => invoke<void>(DESKTOP_STORAGE_CHANNELS.set, scope, key, value),
			delete: (scope: string, key: string) => invoke<void>(DESKTOP_STORAGE_CHANNELS.delete, scope, key),
			clearAllForScope: (scope: string) => invoke<void>(DESKTOP_STORAGE_CHANNELS.clearAllForScope, scope),
			clearAllExcept: (keysToKeep: ReadonlyArray<string>) =>
				invoke<void>(DESKTOP_STORAGE_CHANNELS.clearAllExcept, [...keysToKeep]),
			setMany: (writes: ReadonlyArray<DesktopStorageWrite>) =>
				invoke<void>(DESKTOP_STORAGE_CHANNELS.setMany, [...writes]),
			import: (request: DesktopStorageImportRequest) =>
				invoke<DesktopStorageImportReport>(DESKTOP_STORAGE_CHANNELS.import, {
					entries: [...request.entries],
					marker: request.marker ?? null,
				}),
			getMarker: (key: string) => invoke<string | null>(DESKTOP_STORAGE_CHANNELS.getMarker, key),
			setMarker: (key: string, value: string) => invoke<void>(DESKTOP_STORAGE_CHANNELS.setMarker, key, value),
		}),
		desktopKnownInstances: Object.freeze<DesktopKnownInstanceStorageAPI>({
			getAll: () => invoke<Array<DesktopKnownInstanceRecord>>(DESKTOP_KNOWN_INSTANCE_CHANNELS.getAll),
			upsert: (instance: DesktopKnownInstanceRecord) => invoke<void>(DESKTOP_KNOWN_INSTANCE_CHANNELS.upsert, instance),
			delete: (instanceKey: string) => invoke<void>(DESKTOP_KNOWN_INSTANCE_CHANNELS.delete, instanceKey),
		}),
	});
}
