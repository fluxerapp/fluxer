// SPDX-License-Identifier: AGPL-3.0-or-later

import {notifyAccountEngineAccountsChanged} from '@electron/main/AccountEngineHost';
import {retainBootStateForAccounts} from '@electron/main/BootState';
import type {DesktopAppStorage} from '@electron/main/DesktopAppStorage';
import {DESKTOP_ACCOUNT_CHANNELS} from '@fluxer/desktop_ipc/src/AccountContract';
import {DESKTOP_KNOWN_INSTANCE_CHANNELS} from '@fluxer/desktop_ipc/src/KnownInstanceContract';
import {DESKTOP_STORAGE_CHANNELS} from '@fluxer/desktop_ipc/src/StorageContract';

type DesktopAppStorageIpcRoutes = Readonly<Record<string, (...args: Array<unknown>) => Promise<unknown>>>;

async function afterAccountWrite<T>(write: Promise<T>, onAccountsChanged: () => void): Promise<T> {
	const result = await write;
	onAccountsChanged();
	return result;
}

async function afterAccountRemoval<T>(
	write: Promise<T>,
	storage: DesktopAppStorage,
	onAccountsChanged: () => void,
): Promise<T> {
	const result = await afterAccountWrite(write, onAccountsChanged);
	await retainBootStateForStoredAccounts(storage);
	return result;
}

export async function retainBootStateForStoredAccounts(storage: DesktopAppStorage): Promise<void> {
	const status = await storage.getStoreStatus();
	if (!status.available) {
		return;
	}
	const accounts = await storage.getAllAccounts().catch(() => null);
	if (accounts !== null) {
		await retainBootStateForAccounts(accounts.map((account) => account.storageKey));
	}
}

export function createDesktopAppStorageIpcRoutes(
	storage: DesktopAppStorage,
	onAccountsChanged: () => void = notifyAccountEngineAccountsChanged,
): DesktopAppStorageIpcRoutes {
	return Object.freeze({
		[DESKTOP_ACCOUNT_CHANNELS.getAll]: () => storage.getAllAccounts(),
		[DESKTOP_ACCOUNT_CHANNELS.get]: (storageKey: unknown) => storage.getAccount(storageKey),
		[DESKTOP_ACCOUNT_CHANNELS.upsert]: (account: unknown) =>
			afterAccountWrite(storage.upsertAccount(account), onAccountsChanged),
		[DESKTOP_ACCOUNT_CHANNELS.compareAndSwap]: (request: unknown) =>
			afterAccountWrite(storage.compareAndSwapAccount(request), onAccountsChanged),
		[DESKTOP_ACCOUNT_CHANNELS.delete]: (storageKey: unknown) =>
			afterAccountRemoval(storage.deleteAccount(storageKey), storage, onAccountsChanged),
		[DESKTOP_ACCOUNT_CHANNELS.import]: (request: unknown) =>
			afterAccountRemoval(storage.importAccounts(request), storage, onAccountsChanged),
		[DESKTOP_ACCOUNT_CHANNELS.prune]: (request: unknown) =>
			afterAccountRemoval(storage.pruneAccounts(request), storage, onAccountsChanged),
		[DESKTOP_STORAGE_CHANNELS.getStatus]: () => storage.getStoreStatus(),
		[DESKTOP_STORAGE_CHANNELS.load]: (scope: unknown) => storage.loadEntries(scope),
		[DESKTOP_STORAGE_CHANNELS.get]: (scope: unknown, key: unknown) => storage.getEntry(scope, key),
		[DESKTOP_STORAGE_CHANNELS.set]: (scope: unknown, key: unknown, value: unknown) =>
			storage.setEntry(scope, key, value),
		[DESKTOP_STORAGE_CHANNELS.delete]: (scope: unknown, key: unknown) => storage.deleteEntry(scope, key),
		[DESKTOP_STORAGE_CHANNELS.clearAllForScope]: (scope: unknown) => storage.clearAllForScope(scope),
		[DESKTOP_STORAGE_CHANNELS.clearAllExcept]: (keysToKeep: unknown) => storage.clearAllExcept(keysToKeep),
		[DESKTOP_STORAGE_CHANNELS.setMany]: (writes: unknown) => storage.setManyEntries(writes),
		[DESKTOP_STORAGE_CHANNELS.import]: (request: unknown) => storage.importEntries(request),
		[DESKTOP_STORAGE_CHANNELS.getMarker]: (key: unknown) => storage.getMarker(key),
		[DESKTOP_STORAGE_CHANNELS.setMarker]: (key: unknown, value: unknown) =>
			afterAccountWrite(storage.setMarker(key, value), onAccountsChanged),
		[DESKTOP_KNOWN_INSTANCE_CHANNELS.getAll]: () => storage.getAllKnownInstances(),
		[DESKTOP_KNOWN_INSTANCE_CHANNELS.upsert]: (instance: unknown) => storage.upsertKnownInstance(instance),
		[DESKTOP_KNOWN_INSTANCE_CHANNELS.delete]: (instanceKey: unknown) => storage.deleteKnownInstance(instanceKey),
	});
}
