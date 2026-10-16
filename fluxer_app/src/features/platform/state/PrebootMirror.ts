// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {
	GATEWAY_PREBOOT_SESSION_PRESENT,
	GATEWAY_PREBOOT_SESSION_STORAGE_KEY,
	SHELL_HINT_STORAGE_KEY,
	ZOOM_PREBOOT_MIRROR_STORAGE_KEY,
} from '@fluxer/desktop_ipc/src/StorageContract';

export const AUTH_SHELL_HINT_STORAGE_KEY = 'fluxer:ui:auth-shell-hint';
export const PREBOOT_NETWORK_HINT_STORAGE_KEY = 'fluxer:preboot:network';
export {GATEWAY_PREBOOT_SESSION_STORAGE_KEY, SHELL_HINT_STORAGE_KEY, ZOOM_PREBOOT_MIRROR_STORAGE_KEY};

const PREBOOT_STORAGE_PROBE_KEY = 'fluxer:preboot:storage-probe';

export const PREBOOT_MIRROR_KEYS: ReadonlyArray<string> = Object.freeze([
	AppStorageKey.AUTH_ACCOUNT_KEY,
	AppStorageKey.AUTH_SESSION_TOKEN,
	AppStorageKey.AUTH_SESSION_USER_ID,
	AppStorageKey.THEME_PREBOOT_MIRROR,
	ZOOM_PREBOOT_MIRROR_STORAGE_KEY,
	GATEWAY_PREBOOT_SESSION_STORAGE_KEY,
	SHELL_HINT_STORAGE_KEY,
	AUTH_SHELL_HINT_STORAGE_KEY,
	PREBOOT_NETWORK_HINT_STORAGE_KEY,
]);

type RawStorageKind = 'local' | 'session' | 'memory';

interface RawStorageBinding {
	readonly storage: Storage;
	readonly storageType: RawStorageKind;
}

function createMemoryStorage(): Storage {
	const memoryCache: Record<string, string> = {};
	return {
		getItem: (key) => (key in memoryCache ? memoryCache[key] : null),
		setItem: (key, value) => {
			memoryCache[key] = String(value);
		},
		removeItem: (key) => {
			delete memoryCache[key];
		},
		clear: () => {
			for (const key of Object.keys(memoryCache)) {
				delete memoryCache[key];
			}
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

function createRawStorageBinding(): RawStorageBinding {
	if (typeof window === 'undefined') {
		return {storage: createMemoryStorage(), storageType: 'memory'};
	}
	try {
		const storage = getProtectedLocalStorage();
		if (storage != null) {
			const previousValue = storage.getItem(PREBOOT_STORAGE_PROBE_KEY);
			storage.setItem(PREBOOT_STORAGE_PROBE_KEY, '1');
			if (previousValue === null) {
				storage.removeItem(PREBOOT_STORAGE_PROBE_KEY);
			} else {
				storage.setItem(PREBOOT_STORAGE_PROBE_KEY, previousValue);
			}
			return {storage, storageType: 'local'};
		}
		console.warn('[PrebootMirror] Local storage is unavailable, using an in-memory preboot mirror');
	} catch (error) {
		console.warn('[PrebootMirror] Local storage probe failed, using an in-memory preboot mirror', error);
	}
	return {storage: createMemoryStorage(), storageType: 'memory'};
}

const rawBinding = createRawStorageBinding();

export function rawStorageKind(): RawStorageKind {
	return rawBinding.storageType;
}

export function isRawStorageArea(area: Storage | null): boolean {
	return area === rawBinding.storage;
}

export function readRawStorageItem(name: string): string | null {
	try {
		return rawBinding.storage.getItem(name);
	} catch (error) {
		console.warn('[PrebootMirror] Failed to read raw storage', error);
		return null;
	}
}

export function writeRawStorageItem(name: string, value: string | null): void {
	try {
		if (value === null) {
			rawBinding.storage.removeItem(name);
			return;
		}
		rawBinding.storage.setItem(name, value);
	} catch (error) {
		console.warn('[PrebootMirror] Failed to write raw storage', error);
	}
}

export function rawStorageLength(): number {
	try {
		return rawBinding.storage.length;
	} catch (error) {
		console.warn('[PrebootMirror] Failed to size raw storage', error);
		return 0;
	}
}

export function rawStorageKeyAt(index: number): string | null {
	try {
		return rawBinding.storage.key(index);
	} catch (error) {
		console.warn('[PrebootMirror] Failed to index raw storage', error);
		return null;
	}
}

export function rawStorageKeys(): Array<string> {
	const keys: Array<string> = [];
	const length = rawStorageLength();
	for (let index = 0; index < length; index++) {
		const key = rawStorageKeyAt(index);
		if (key !== null) {
			keys.push(key);
		}
	}
	return keys;
}

export function mirrorGatewayPrebootSession(sessionPresent: boolean): void {
	writeRawStorageItem(GATEWAY_PREBOOT_SESSION_STORAGE_KEY, sessionPresent ? GATEWAY_PREBOOT_SESSION_PRESENT : null);
	if (!sessionPresent) {
		writeRawStorageItem(SHELL_HINT_STORAGE_KEY, null);
		writeRawStorageItem(PREBOOT_NETWORK_HINT_STORAGE_KEY, null);
	}
}
