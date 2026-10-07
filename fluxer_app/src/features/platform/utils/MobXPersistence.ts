// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage, {
	type AppStorageScopeChangeEvent,
	onBeforeAppStorageScopeChange,
	subscribeAppStorageScope,
} from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {compareStructural, runInAction, toJS} from 'mobx';
import {
	configurePersistable,
	hydrateStore,
	makePersistable,
	pausePersisting,
	startPersisting,
	stopPersisting,
} from 'mobx-persist-store';

const logger = new Logger('MobXPersistence');

interface PersistedStoreState {
	readonly store: object;
	readonly properties: ReadonlyArray<string>;
	readonly resetToDefaults: () => void;
	readonly stopSync?: () => void;
	readonly onRehydrated?: (event: AppStorageScopeChangeEvent | null) => void;
}

const persistedStates = new Map<string, PersistedStoreState>();
const getStorage = () => {
	return AppStorage;
};

const PERSIST_WRITE_DELAY_MS = 500;
const pendingPersistFlushes = new Set<() => void>();

function createPersistScheduler(delayMs: number): (callback: () => void) => void {
	if (delayMs <= 0) {
		return (callback) => {
			callback();
		};
	}
	return (callback) => {
		let settled = false;
		const flush = () => {
			if (settled) {
				return;
			}
			settled = true;
			pendingPersistFlushes.delete(flush);
			clearTimeout(timer);
			callback();
		};
		const timer = setTimeout(flush, delayMs);
		pendingPersistFlushes.add(flush);
	};
}

export function flushPendingPersistWrites(): void {
	for (const flush of [...pendingPersistFlushes]) {
		try {
			flush();
		} catch (error) {
			logger.error('Failed to flush a pending persistent store write:', error);
		}
	}
}

function clonePersistedValue<V>(value: V): V {
	return structuredClone(toJS(value));
}

function captureStoreDefaults(
	store: object,
	properties: ReadonlyArray<string>,
): {properties: ReadonlyArray<string>; resetToDefaults: () => void} {
	const target = store as Record<string, unknown>;
	const defaults: Array<readonly [string, unknown]> = [];
	for (const property of properties) {
		try {
			defaults.push([property, clonePersistedValue(target[property])]);
		} catch (error) {
			logger.error(`Cannot capture the default value of persisted property ${property}:`, error);
		}
	}
	return {
		properties: defaults.map(([property]) => property),
		resetToDefaults: () => {
			runInAction(() => {
				for (const [property, value] of defaults) {
					if (compareStructural(toJS(target[property]), value)) {
						continue;
					}
					target[property] = clonePersistedValue(value);
				}
			});
		},
	};
}

async function rehydratePersistedStore(
	storageKey: string,
	state: PersistedStoreState,
	event: AppStorageScopeChangeEvent | null,
): Promise<void> {
	if (persistedStates.get(storageKey) !== state) {
		return;
	}
	pausePersisting(state.store);
	try {
		state.resetToDefaults();
		await hydrateStore(state.store);
	} catch (error) {
		logger.error(`Store ${storageKey} stays paused, it could not be rehydrated for the active storage scope:`, error);
		return;
	}
	if (persistedStates.get(storageKey) === state) {
		startPersisting(state.store);
		state.onRehydrated?.(event);
	}
}

async function rehydratePersistedStores(event: AppStorageScopeChangeEvent | null): Promise<void> {
	const states = [...persistedStates];
	if (states.length === 0) {
		return;
	}
	logger.debug(`Rehydrating ${states.length} persisted stores for the active storage scope.`);
	for (const [storageKey, state] of states) {
		await rehydratePersistedStore(storageKey, state, event);
	}
}

let rehydrationTail: Promise<void> = Promise.resolve();

export function rehydrateAllPersistentStores(event: AppStorageScopeChangeEvent | null = null): Promise<void> {
	const run = () => rehydratePersistedStores(event);
	rehydrationTail = rehydrationTail.then(run, run);
	return rehydrationTail;
}

if (typeof window !== 'undefined' && typeof window.addEventListener === 'function') {
	window.addEventListener('pagehide', flushPendingPersistWrites);
	window.addEventListener('beforeunload', flushPendingPersistWrites);
}

onBeforeAppStorageScopeChange(flushPendingPersistWrites);
subscribeAppStorageScope(rehydrateAllPersistentStores);

configurePersistable(
	{
		storage: getStorage(),
		expireIn: undefined,
		removeOnExpiration: false,
		stringify: true,
		debugMode: false,
	},
	{
		fireImmediately: false,
		scheduler: createPersistScheduler(PERSIST_WRITE_DELAY_MS),
	},
);

const hydrationPromises = new Map<string, Promise<void>>();

export function awaitHydration(storageKey: string): Promise<void> {
	return hydrationPromises.get(storageKey) ?? Promise.resolve();
}

export async function makePersistent<T extends object>(
	store: T,
	storageKey: string,
	properties: Array<keyof T>,
	options?: {
		expireIn?: number;
		removeOnExpiration?: boolean;
		version?: number;
		syncAcrossTabs?: boolean;
		writeDelayMs?: number;
		onRehydrated?: (event: AppStorageScopeChangeEvent | null) => void;
	},
): Promise<void> {
	try {
		if (persistedStates.has(storageKey)) {
			logger.debug(`Store ${storageKey} is already being persisted, skipping...`);
			return;
		}
		const defaults = captureStoreDefaults(store, properties as Array<keyof T & string>);
		const hydrationPromise = makePersistable(
			store,
			{
				name: storageKey,
				properties: properties as Array<keyof T & string>,
				storage: getStorage(),
				expireIn: options?.expireIn,
				removeOnExpiration: options?.removeOnExpiration,
				stringify: true,
				version: options?.version ?? 1,
			},
			options?.writeDelayMs === undefined ? undefined : {scheduler: createPersistScheduler(options.writeDelayMs)},
		).then(() => undefined);
		hydrationPromises.set(storageKey, hydrationPromise);
		await hydrationPromise;
		let stopSync: (() => void) | undefined;
		if (options?.syncAcrossTabs) {
			let hydrationQueue = Promise.resolve();
			stopSync = AppStorage.subscribe(
				() => {
					hydrationQueue = hydrationQueue
						.catch(() => undefined)
						.then(async () => {
							logger.debug(`Rehydrating store ${storageKey} after external storage change.`);
							await hydrateStore(store);
						})
						.catch((error) => {
							logger.error(`Failed to rehydrate store ${storageKey} after external storage change:`, error);
						});
				},
				{
					key: storageKey,
					source: 'external',
				},
			);
		}
		persistedStates.set(storageKey, {
			store,
			properties: defaults.properties,
			resetToDefaults: defaults.resetToDefaults,
			stopSync,
			onRehydrated: options?.onRehydrated,
		});
		logger.debug(`Store ${storageKey} hydrated from AppStorage and is now persisting.`);
	} catch (error) {
		logger.error(`Failed to hydrate store ${storageKey}:`, error);
	}
}

export function stopPersistent(storageKey: string, store: object): void {
	try {
		const persistedState = persistedStates.get(storageKey);
		persistedState?.stopSync?.();
		stopPersisting(store);
		persistedStates.delete(storageKey);
		logger.debug(`Stopped persisting store: ${storageKey}`);
	} catch (error) {
		logger.error(`Failed to stop persisting store ${storageKey}:`, error);
	}
}
