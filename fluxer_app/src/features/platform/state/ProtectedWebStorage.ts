// SPDX-License-Identifier: AGPL-3.0-or-later

function captureStorageBackend<T>(readStorage: () => T): T | null {
	if (typeof window === 'undefined') {
		return null;
	}
	try {
		return readStorage();
	} catch {
		return null;
	}
}

const protectedLocalStorage = captureStorageBackend(() => window.localStorage);
const protectedSessionStorage = captureStorageBackend(() => window.sessionStorage);
const protectedIndexedDB = captureStorageBackend(() => window.indexedDB);
const protectedCacheStorage = captureStorageBackend(() => window.caches);

let browserStorageAccessProtected = false;

function getProtectedOrCurrentBackend<T>(capturedBackend: T | null, readStorage: () => T): T | null {
	if (capturedBackend || browserStorageAccessProtected) {
		return capturedBackend;
	}
	return captureStorageBackend(readStorage);
}

export function getProtectedLocalStorage(): Storage | null {
	return getProtectedOrCurrentBackend(protectedLocalStorage, () => window.localStorage);
}

export function getProtectedSessionStorage(): Storage | null {
	return getProtectedOrCurrentBackend(protectedSessionStorage, () => window.sessionStorage);
}

export function getProtectedIndexedDB(): IDBFactory | null {
	return getProtectedOrCurrentBackend(protectedIndexedDB, () => window.indexedDB);
}

export function getProtectedCacheStorage(): CacheStorage | null {
	return getProtectedOrCurrentBackend(protectedCacheStorage, () => window.caches);
}

function deleteWindowStorageProperty(property: 'localStorage' | 'sessionStorage' | 'indexedDB' | 'caches'): void {
	try {
		delete (window as Partial<Window>)[property];
	} catch {}
}

export function installBrowserStorageAccessProtection(): void {
	if (browserStorageAccessProtected || !import.meta.env.PROD || typeof window === 'undefined') {
		return;
	}
	browserStorageAccessProtected = true;
	deleteWindowStorageProperty('localStorage');
	deleteWindowStorageProperty('sessionStorage');
	deleteWindowStorageProperty('indexedDB');
	deleteWindowStorageProperty('caches');
}

installBrowserStorageAccessProtection();
