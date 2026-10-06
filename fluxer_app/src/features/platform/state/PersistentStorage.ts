// SPDX-License-Identifier: AGPL-3.0-or-later

import {DOMAIN_MIGRATION_STORAGE_KEY_PREFIXES} from '@app/features/app/domain_migration/DomainMigrationCore';
import {getProtectedLocalStorage, getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';

export type StorageChangeSource = 'local' | 'external';

export interface StorageChangeEvent {
	key: string | null;
	oldValue: string | null;
	newValue: string | null;
	source: StorageChangeSource;
	storageType: 'local' | 'session' | 'memory';
}

type StorageChangeListener = (event: StorageChangeEvent) => void;

interface StorageSubscriptionOptions {
	key?: string;
	source?: StorageChangeSource | 'any';
}

interface EnhancedStorage {
	getItem(key: string): string | null;
	setItem(key: string, value: string): void;
	removeItem(key: string): void;
	clear(): void;
	clearExcept(keysToKeep: ReadonlyArray<string>, prefixesToKeep?: ReadonlyArray<string>): void;
	key(index: number): string | null;
	readonly length: number;
	getJSON<T>(key: string, defaultValue?: T): T | null;
	setJSON<T>(key: string, value: T): void;
	keys(): Array<string>;
	subscribe(listener: StorageChangeListener, options?: StorageSubscriptionOptions): () => void;
}

function createStorage(storageType: 'local' | 'session' | 'memory' = 'local'): EnhancedStorage {
	let baseStorage: Storage | null = null;
	let resolvedStorageType: 'local' | 'session' | 'memory' = storageType;
	if (storageType === 'local' || storageType === 'session') {
		try {
			baseStorage = storageType === 'local' ? getProtectedLocalStorage() : getProtectedSessionStorage();
			if (baseStorage == null) {
				throw new Error(`${storageType} storage unavailable`);
			}
			baseStorage.setItem('__test__', '1');
			baseStorage.removeItem('__test__');
		} catch (_e) {
			baseStorage = null;
		}
	}
	if (baseStorage == null) {
		resolvedStorageType = 'memory';
		const memoryCache: Record<string, string> = {};
		baseStorage = {
			getItem: (key) => (key in memoryCache ? memoryCache[key] : null),
			setItem: (key, value) => {
				memoryCache[key] = String(value);
			},
			removeItem: (key) => {
				delete memoryCache[key];
			},
			clear: () => {
				Object.keys(memoryCache).forEach((k) => {
					delete memoryCache[k];
				});
			},
			key: (index) => {
				const keys = Object.keys(memoryCache);
				return index >= 0 && index < keys.length ? keys[index] : null;
			},
			get length() {
				return Object.keys(memoryCache).length;
			},
		};
	}
	const listeners = new Set<{
		listener: StorageChangeListener;
		options: StorageSubscriptionOptions;
	}>();
	const shouldNotifyListener = (
		event: StorageChangeEvent,
		subscriptionOptions: StorageSubscriptionOptions | undefined,
	): boolean => {
		if (!subscriptionOptions) {
			return true;
		}
		if (subscriptionOptions.key && event.key !== null && subscriptionOptions.key !== event.key) {
			return false;
		}
		if (subscriptionOptions.key && event.key === null) {
			return true;
		}
		if (
			subscriptionOptions.source &&
			subscriptionOptions.source !== 'any' &&
			subscriptionOptions.source !== event.source
		) {
			return false;
		}
		return true;
	};
	const notifyListeners = (event: Omit<StorageChangeEvent, 'storageType'>): void => {
		const fullEvent: StorageChangeEvent = {
			...event,
			storageType: resolvedStorageType,
		};
		listeners.forEach(({listener, options}) => {
			if (!shouldNotifyListener(fullEvent, options)) {
				return;
			}
			listener(fullEvent);
		});
	};
	const getAllKeys = (): Array<string> => {
		const result: Array<string> = [];
		for (let i = 0; i < baseStorage!.length; i++) {
			const key = baseStorage!.key(i);
			if (key !== null) {
				result.push(key);
			}
		}
		return result;
	};
	const setItemInternal = (key: string, value: string): void => {
		const nextValue = String(value);
		const oldValue = baseStorage!.getItem(key);
		if (oldValue === nextValue) {
			return;
		}
		baseStorage!.setItem(key, nextValue);
		notifyListeners({
			key,
			oldValue,
			newValue: nextValue,
			source: 'local',
		});
	};
	const removeItemInternal = (key: string): void => {
		const oldValue = baseStorage!.getItem(key);
		if (oldValue === null) {
			return;
		}
		baseStorage!.removeItem(key);
		notifyListeners({
			key,
			oldValue,
			newValue: null,
			source: 'local',
		});
	};
	const clearInternal = (): void => {
		const existingEntries = getAllKeys()
			.map((key) => [key, baseStorage!.getItem(key)] as const)
			.filter((entry): entry is readonly [string, string] => entry[1] !== null);
		if (existingEntries.length === 0) {
			return;
		}
		baseStorage!.clear();
		existingEntries.forEach(([key, oldValue]) => {
			notifyListeners({
				key,
				oldValue,
				newValue: null,
				source: 'local',
			});
		});
	};
	if (typeof window !== 'undefined' && resolvedStorageType !== 'memory') {
		window.addEventListener('storage', (event: StorageEvent) => {
			if (event.storageArea !== baseStorage) {
				return;
			}
			notifyListeners({
				key: event.key,
				oldValue: event.oldValue,
				newValue: event.newValue,
				source: 'external',
			});
		});
	}
	const storage = {} as EnhancedStorage;
	Object.defineProperties(storage, {
		getItem: {
			value: (key: string) => baseStorage!.getItem(key),
			writable: false,
			enumerable: false,
		},
		setItem: {
			value: setItemInternal,
			writable: false,
			enumerable: false,
		},
		removeItem: {
			value: removeItemInternal,
			writable: false,
			enumerable: false,
		},
		clear: {
			value: clearInternal,
			writable: false,
			enumerable: false,
		},
		clearExcept: {
			value: (keysToKeep: ReadonlyArray<string>, prefixesToKeep: ReadonlyArray<string> = []) => {
				if (keysToKeep.length === 0 && prefixesToKeep.length === 0) {
					clearInternal();
					return;
				}
				const keepSet = new Set(keysToKeep);
				const removedEntries = getAllKeys()
					.filter((key) => !keepSet.has(key) && !prefixesToKeep.some((prefix) => key.startsWith(prefix)))
					.map((key) => [key, baseStorage!.getItem(key)] as const)
					.filter((entry): entry is readonly [string, string] => entry[1] !== null);
				if (removedEntries.length === 0) {
					return;
				}
				removedEntries.forEach(([key]) => {
					baseStorage!.removeItem(key);
				});
				removedEntries.forEach(([key, oldValue]) => {
					notifyListeners({
						key,
						oldValue,
						newValue: null,
						source: 'local',
					});
				});
			},
			writable: false,
			enumerable: false,
		},
		key: {
			value: (index: number) => baseStorage!.key(index),
			writable: false,
			enumerable: false,
		},
		length: {
			get: () => baseStorage!.length,
			enumerable: false,
		},
		getJSON: {
			value: <T>(key: string, defaultValue?: T): T | null => {
				const item = baseStorage!.getItem(key);
				if (item === null) return defaultValue === undefined ? null : defaultValue;
				try {
					return JSON.parse(item);
				} catch (e) {
					console.warn(`[AppStorage] Failed to parse JSON for key "${key}":`, e);
					return defaultValue === undefined ? null : defaultValue;
				}
			},
			writable: false,
			enumerable: false,
		},
		setJSON: {
			value: <T>(key: string, value: T) => {
				if (value === storage) {
					throw new Error('Cannot store the storage object itself');
				}
				try {
					const serialized = JSON.stringify(value);
					setItemInternal(key, serialized);
				} catch (e) {
					throw new Error(`Failed to store value for key "${key}": ${e}`);
				}
			},
			writable: false,
			enumerable: false,
		},
		keys: {
			value: getAllKeys,
			writable: false,
			enumerable: false,
		},
		subscribe: {
			value: (listener: StorageChangeListener, options: StorageSubscriptionOptions = {}) => {
				const subscription = {listener, options};
				listeners.add(subscription);
				return () => {
					listeners.delete(subscription);
				};
			},
			writable: false,
			enumerable: false,
		},
	});
	return storage;
}

const AppStorage = createStorage('local');

export default AppStorage;

export const PRESERVED_RESET_STORAGE_KEYS = ['Drafts'] as const;

export const PRESERVED_RESET_STORAGE_KEY_PREFIXES = DOMAIN_MIGRATION_STORAGE_KEY_PREFIXES;
