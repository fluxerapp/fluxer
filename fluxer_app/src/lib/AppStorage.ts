/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

interface EnhancedStorage {
	getItem(key: string): string | null;
	setItem(key: string, value: string): void;
	removeItem(key: string): void;
	clear(): void;
	key(index: number): string | null;
	readonly length: number;

	getJSON<T>(key: string, defaultValue?: T): T | null;
	setJSON<T>(key: string, value: T): void;
	keys(): Array<string>;
}

function createStorage(storageType: 'local' | 'session' | 'memory' = 'local'): EnhancedStorage {
	let baseStorage: Storage | null = null;

	if (storageType === 'local' || storageType === 'session') {
		try {
			baseStorage = storageType === 'local' ? localStorage : sessionStorage;
			baseStorage.setItem('__test__', '1');
			baseStorage.removeItem('__test__');
		} catch (_e) {
			baseStorage = null;
		}
	}

	if (baseStorage == null) {
		const memoryStore: Record<string, string> = {};

		baseStorage = {
			getItem: (key) => (key in memoryStore ? memoryStore[key] : null),
			setItem: (key, value) => {
				memoryStore[key] = String(value);
			},
			removeItem: (key) => {
				delete memoryStore[key];
			},
			clear: () => {
				Object.keys(memoryStore).forEach((k) => {
					delete memoryStore[k];
				});
			},
			key: (index) => {
				const keys = Object.keys(memoryStore);
				return index >= 0 && index < keys.length ? keys[index] : null;
			},
			get length() {
				return Object.keys(memoryStore).length;
			},
		};
	}

	const storage: EnhancedStorage = Object.create(null);

	Object.defineProperties(storage, {
		getItem: {
			value: (key: string) => baseStorage!.getItem(key),
			writable: false,
			enumerable: false,
		},
		setItem: {
			value: (key: string, value: string) => baseStorage!.setItem(key, value),
			writable: false,
			enumerable: false,
		},
		removeItem: {
			value: (key: string) => baseStorage!.removeItem(key),
			writable: false,
			enumerable: false,
		},
		clear: {
			value: () => baseStorage!.clear(),
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
					console.warn(`Failed to parse JSON for key "${key}":`, e);
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
					baseStorage!.setItem(key, serialized);
				} catch (e) {
					throw new Error(`Failed to store value for key "${key}": ${e}`);
				}
			},
			writable: false,
			enumerable: false,
		},
		keys: {
			value: (): Array<string> => {
				const result: Array<string> = [];
				for (let i = 0; i < baseStorage!.length; i++) {
					const key = baseStorage!.key(i);
					if (key !== null) {
						result.push(key);
					}
				}
				return result;
			},
			writable: false,
			enumerable: false,
		},
	});

	return storage;
}

const AppStorage = createStorage('local');
export default AppStorage;
