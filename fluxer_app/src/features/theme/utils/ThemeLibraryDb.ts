// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedIndexedDB} from '@app/features/platform/state/ProtectedWebStorage';
import type {
	ThemeLibraryAsset,
	ThemeLibraryLocalFileReference,
	ThemeLibraryTheme,
} from '@app/features/theme/state/ThemeLibrary';

const DB_NAME = 'fluxer-theme-library';
const DB_VERSION = 1;
const THEMES_STORE = 'themes';
const ASSETS_STORE = 'assets';
const LOCAL_FILES_STORE = 'localFiles';
const META_STORE = 'meta';
const ENABLED_THEME_IDS_KEY = 'enabledThemeIds';
const browserIndexedDB = getProtectedIndexedDB();

type ThemeLibraryTableName = typeof THEMES_STORE | typeof ASSETS_STORE | typeof LOCAL_FILES_STORE | typeof META_STORE;

let openPromise: Promise<IDBDatabase> | null = null;

const THEME_SOURCES = new Set<string>(['quick_css', 'css_file', 'desktop_directory', 'shared_theme', 'import']);

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

function isStringArray(value: unknown): value is Array<string> {
	return Array.isArray(value) && value.every((item) => typeof item === 'string');
}

function isThemeLibraryTheme(value: unknown): value is ThemeLibraryTheme {
	return (
		isRecord(value) &&
		typeof value.id === 'string' &&
		typeof value.name === 'string' &&
		typeof value.description === 'string' &&
		typeof value.author === 'string' &&
		typeof value.version === 'string' &&
		isStringArray(value.tags) &&
		typeof value.css === 'string' &&
		typeof value.fileName === 'string' &&
		typeof value.source === 'string' &&
		THEME_SOURCES.has(value.source) &&
		typeof value.createdAt === 'number' &&
		typeof value.updatedAt === 'number'
	);
}

function isThemeLibraryAsset(value: unknown): value is ThemeLibraryAsset {
	return (
		isRecord(value) &&
		typeof value.id === 'string' &&
		typeof value.name === 'string' &&
		typeof value.mimeType === 'string' &&
		typeof value.size === 'number' &&
		(value.data === undefined || value.data instanceof Blob) &&
		(value.desktopPath === undefined || typeof value.desktopPath === 'string') &&
		typeof value.createdAt === 'number' &&
		typeof value.updatedAt === 'number'
	);
}

function isThemeLibraryLocalFileReference(value: unknown): value is ThemeLibraryLocalFileReference {
	return (
		isRecord(value) &&
		typeof value.id === 'string' &&
		typeof value.name === 'string' &&
		typeof value.path === 'string' &&
		typeof value.mimeType === 'string' &&
		typeof value.size === 'number' &&
		typeof value.createdAt === 'number' &&
		typeof value.updatedAt === 'number'
	);
}

function requestToPromise<T>(request: IDBRequest<T>): Promise<T> {
	return new Promise((resolve, reject) => {
		request.onsuccess = () => resolve(request.result);
		request.onerror = () => reject(request.error ?? new Error('IndexedDB request failed'));
	});
}

function transactionDone(transaction: IDBTransaction): Promise<void> {
	return new Promise((resolve, reject) => {
		transaction.oncomplete = () => resolve();
		transaction.onerror = () => reject(transaction.error ?? new Error('IndexedDB transaction failed'));
		transaction.onabort = () => reject(transaction.error ?? new Error('IndexedDB transaction aborted'));
	});
}

function getDatabase(): Promise<IDBDatabase> {
	if (openPromise) {
		return openPromise;
	}
	const promise = new Promise<IDBDatabase>((resolve, reject) => {
		if (!browserIndexedDB) {
			reject(new Error('IndexedDB unavailable'));
			return;
		}
		const request = browserIndexedDB.open(DB_NAME, DB_VERSION);
		request.onupgradeneeded = () => {
			const db = request.result;
			if (!db.objectStoreNames.contains(THEMES_STORE)) {
				db.createObjectStore(THEMES_STORE, {keyPath: 'id'});
			}
			if (!db.objectStoreNames.contains(ASSETS_STORE)) {
				db.createObjectStore(ASSETS_STORE, {keyPath: 'id'});
			}
			if (!db.objectStoreNames.contains(LOCAL_FILES_STORE)) {
				db.createObjectStore(LOCAL_FILES_STORE, {keyPath: 'id'});
			}
			if (!db.objectStoreNames.contains(META_STORE)) {
				db.createObjectStore(META_STORE);
			}
		};
		request.onsuccess = () => {
			const db = request.result;
			db.onversionchange = () => {
				db.close();
				openPromise = null;
			};
			resolve(db);
		};
		request.onerror = () => reject(request.error ?? new Error('Failed to open theme library database'));
		request.onblocked = () => reject(new Error('Theme library database upgrade is blocked by another window'));
	});
	promise.catch(() => {
		if (openPromise === promise) {
			openPromise = null;
		}
	});
	openPromise = promise;
	return promise;
}

async function withReadonlyDb<T>(
	name: ThemeLibraryTableName,
	operation: (store: IDBObjectStore) => Promise<T>,
): Promise<T> {
	const db = await getDatabase();
	const transaction = db.transaction(name, 'readonly');
	const store = transaction.objectStore(name);
	const done = transactionDone(transaction);
	const result = await operation(store);
	await done;
	return result;
}

async function withReadwriteDb<T>(name: ThemeLibraryTableName, operation: (store: IDBObjectStore) => T): Promise<T> {
	const db = await getDatabase();
	const transaction = db.transaction(name, 'readwrite');
	const done = transactionDone(transaction);
	const result = operation(transaction.objectStore(name));
	await done;
	return result;
}

export async function listThemeLibraryThemes(): Promise<Array<ThemeLibraryTheme>> {
	const result = await withReadonlyDb(THEMES_STORE, (store) => requestToPromise(store.getAll()));
	return result.filter(isThemeLibraryTheme);
}

export async function saveThemeLibraryTheme(theme: ThemeLibraryTheme): Promise<void> {
	await withReadwriteDb(THEMES_STORE, (store) => {
		store.put(theme);
	});
}

export async function deleteThemeLibraryTheme(id: string): Promise<void> {
	await withReadwriteDb(THEMES_STORE, (store) => {
		store.delete(id);
	});
}

export async function clearThemeLibraryThemes(): Promise<void> {
	await withReadwriteDb(THEMES_STORE, (store) => {
		store.clear();
	});
}

export async function listThemeLibraryAssets(): Promise<Array<ThemeLibraryAsset>> {
	const result = await withReadonlyDb(ASSETS_STORE, (store) => requestToPromise(store.getAll()));
	return result.filter(isThemeLibraryAsset);
}

export async function saveThemeLibraryAsset(asset: ThemeLibraryAsset): Promise<void> {
	await withReadwriteDb(ASSETS_STORE, (store) => {
		store.put(asset);
	});
}

export async function deleteThemeLibraryAsset(id: string): Promise<void> {
	await withReadwriteDb(ASSETS_STORE, (store) => {
		store.delete(id);
	});
}

export async function clearThemeLibraryAssets(): Promise<void> {
	await withReadwriteDb(ASSETS_STORE, (store) => {
		store.clear();
	});
}

export async function listThemeLibraryLocalFiles(): Promise<Array<ThemeLibraryLocalFileReference>> {
	const result = await withReadonlyDb(LOCAL_FILES_STORE, (store) => requestToPromise(store.getAll()));
	return result.filter(isThemeLibraryLocalFileReference);
}

export async function saveThemeLibraryLocalFile(file: ThemeLibraryLocalFileReference): Promise<void> {
	await withReadwriteDb(LOCAL_FILES_STORE, (store) => {
		store.put(file);
	});
}

export async function deleteThemeLibraryLocalFile(id: string): Promise<void> {
	await withReadwriteDb(LOCAL_FILES_STORE, (store) => {
		store.delete(id);
	});
}

export async function clearThemeLibraryLocalFiles(): Promise<void> {
	await withReadwriteDb(LOCAL_FILES_STORE, (store) => {
		store.clear();
	});
}

export async function getEnabledThemeIds(): Promise<Array<string>> {
	const result = await withReadonlyDb(META_STORE, (store) => requestToPromise(store.get(ENABLED_THEME_IDS_KEY)));
	return Array.isArray(result) ? result.filter((value): value is string => typeof value === 'string') : [];
}

export async function setEnabledThemeIds(themeIds: ReadonlyArray<string>): Promise<void> {
	await withReadwriteDb(META_STORE, (store) => {
		store.put([...themeIds], ENABLED_THEME_IDS_KEY);
	});
}

export async function clearThemeLibraryMeta(): Promise<void> {
	await withReadwriteDb(META_STORE, (store) => {
		store.clear();
	});
}
