// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY,
	DESKTOP_LEGACY_HARVEST_CHANNELS,
	DESKTOP_LEGACY_HARVEST_SENTINEL_PATH,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {ipcRenderer} from 'electron';

const LEGACY_HARVEST_LOOPBACK_HOSTS: ReadonlySet<string> = new Set(['127.0.0.1', '[::1]', 'localhost']);
const LEGACY_CACHE_NAME_PREFIX = 'fluxer-';
const DATABASE_OPEN_TIMEOUT_MS = 15_000;
const MEDIA_DEVICE_TIMEOUT_MS = 5_000;
const MAX_BLOB_BYTES = 32 * 1024 * 1024;
const MAX_TOTAL_BLOB_BYTES = 256 * 1024 * 1024;

const LEGACY_DATABASE_NAMES: ReadonlyArray<string> = [
	'FluxerAccounts',
	'FluxerCustomSounds',
	'FluxerVoiceStats',
	'fluxer-theme-library',
	'fluxer-app-storage',
];

const CRITICAL_LEGACY_DATABASE_NAMES: ReadonlySet<string> = new Set(['FluxerAccounts']);

class LegacyHarvestTimeoutError extends Error {
	public constructor(operation: string, timeoutMs: number) {
		super(`Legacy harvest step timed out after ${timeoutMs}ms: ${operation}`);
		this.name = 'LegacyHarvestTimeoutError';
	}
}

class LegacyHarvestStoreReadError extends Error {
	public constructor(database: string, store: string, reason: string) {
		super(`Legacy harvest could not read ${database}/${store}: ${reason}`);
		this.name = 'LegacyHarvestStoreReadError';
	}
}

interface LegacyHarvestBlobPayload {
	readonly blobId: string;
	readonly bytes: Uint8Array;
}

interface LegacyHarvestBlobReference {
	readonly blobId: string;
	readonly type: string;
	readonly size: number;
}

interface LegacyHarvestStorePayload {
	readonly database: string;
	readonly version: number;
	readonly store: string;
	readonly records: Array<{readonly key: unknown; readonly value: unknown}>;
}

interface LegacyHarvestMediaDevicePayload {
	readonly deviceId: string;
	readonly kind: string;
	readonly label: string;
	readonly groupId: string;
}

interface LegacyHarvestPayload {
	readonly localStorage: Record<string, string>;
	readonly stores: Array<LegacyHarvestStorePayload>;
	readonly mediaDevices: Array<LegacyHarvestMediaDevicePayload>;
	readonly blobs: Array<LegacyHarvestBlobPayload>;
	readonly serviceWorkersUnregistered: number;
	readonly cachesDeleted: number;
	readonly truncated: Array<string>;
}

interface LegacyHarvestContext {
	readonly blobs: Array<LegacyHarvestBlobPayload>;
	readonly truncated: Array<string>;
	readonly criticalFailures: Array<string>;
	totalBlobBytes: number;
}

function describeError(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

function withTimeout<T>(promise: Promise<T>, timeoutMs: number, operation: string): Promise<T> {
	return new Promise<T>((resolve, reject) => {
		const timer = window.setTimeout(() => reject(new LegacyHarvestTimeoutError(operation, timeoutMs)), timeoutMs);
		promise.then(
			(value) => {
				window.clearTimeout(timer);
				resolve(value);
			},
			(error: unknown) => {
				window.clearTimeout(timer);
				reject(error);
			},
		);
	});
}

function createBlobId(): string {
	return crypto.randomUUID().replaceAll('-', '');
}

async function encodeBlob(blob: Blob, context: LegacyHarvestContext, label: string): Promise<unknown> {
	if (blob.size > MAX_BLOB_BYTES || context.totalBlobBytes + blob.size > MAX_TOTAL_BLOB_BYTES) {
		context.truncated.push(label);
		return null;
	}
	const bytes = new Uint8Array(await blob.arrayBuffer());
	const blobId = createBlobId();
	context.totalBlobBytes += bytes.byteLength;
	context.blobs.push({blobId, bytes});
	const reference: LegacyHarvestBlobReference = {blobId, type: blob.type, size: bytes.byteLength};
	return {[DESKTOP_LEGACY_HARVEST_BLOB_REF_KEY]: reference};
}

async function encodeValue(
	value: unknown,
	context: LegacyHarvestContext,
	label: string,
	seen: WeakSet<object>,
): Promise<unknown> {
	if (value === null || typeof value !== 'object') return value;
	if (value instanceof Blob) return encodeBlob(value, context, label);
	if (value instanceof Date) return value;
	if (value instanceof ArrayBuffer || ArrayBuffer.isView(value)) {
		context.truncated.push(label);
		return value;
	}
	if (seen.has(value)) return null;
	seen.add(value);
	try {
		if (Array.isArray(value)) {
			const items: Array<unknown> = [];
			for (let index = 0; index < value.length; index += 1) {
				items.push(await encodeValue(value[index], context, `${label}[${index}]`, seen));
			}
			return items;
		}
		if (value instanceof Map) {
			context.truncated.push(label);
			const entries: Array<[unknown, unknown]> = [];
			for (const [key, item] of value) {
				entries.push([key, await encodeValue(item, context, `${label}.${String(key)}`, seen)]);
			}
			return new Map(entries);
		}
		if (value instanceof Set) {
			context.truncated.push(label);
			const items: Array<unknown> = [];
			for (const item of value) {
				items.push(await encodeValue(item, context, `${label}.*`, seen));
			}
			return new Set(items);
		}
		const encoded: Record<string, unknown> = {};
		for (const [key, item] of Object.entries(value)) {
			encoded[key] = await encodeValue(item, context, `${label}.${key}`, seen);
		}
		return encoded;
	} finally {
		seen.delete(value);
	}
}

function harvestLocalStorage(criticalFailures: Array<string>): Record<string, string> {
	const entries: Record<string, string> = {};
	try {
		const storage = window.localStorage;
		for (let index = 0; index < storage.length; index += 1) {
			const key = storage.key(index);
			if (key === null) continue;
			const value = storage.getItem(key);
			if (value === null) continue;
			entries[key] = value;
		}
	} catch (error) {
		criticalFailures.push(`localStorage: ${describeError(error)}`);
	}
	return entries;
}

function openLegacyDatabase(name: string): Promise<IDBDatabase | null> {
	return new Promise<IDBDatabase | null>((resolve, reject) => {
		let settled = false;
		const request = window.indexedDB.open(name);
		const timer = window.setTimeout(() => {
			if (settled) return;
			settled = true;
			reject(new LegacyHarvestTimeoutError(`opening ${name}`, DATABASE_OPEN_TIMEOUT_MS));
		}, DATABASE_OPEN_TIMEOUT_MS);
		request.onupgradeneeded = () => {
			request.transaction?.abort();
		};
		request.onsuccess = () => {
			if (settled) {
				request.result.close();
				return;
			}
			settled = true;
			window.clearTimeout(timer);
			resolve(request.result);
		};
		request.onerror = () => {
			if (settled) return;
			settled = true;
			window.clearTimeout(timer);
			resolve(null);
		};
	});
}

function readAllEntries(store: IDBObjectStore, outOfLineKeys: boolean): Promise<Array<{key: unknown; value: unknown}>> {
	return new Promise((resolve, reject) => {
		const entries: Array<{key: unknown; value: unknown}> = [];
		const request = store.openCursor();
		request.onsuccess = () => {
			const cursor = request.result;
			if (cursor === null) {
				resolve(entries);
				return;
			}
			entries.push({key: outOfLineKeys ? cursor.key : null, value: cursor.value});
			cursor.continue();
		};
		request.onerror = () => {
			reject(
				new LegacyHarvestStoreReadError(
					store.transaction.db.name,
					store.name,
					describeError(request.error ?? 'cursor failed'),
				),
			);
		};
	});
}

async function harvestStore(
	database: IDBDatabase,
	storeName: string,
	context: LegacyHarvestContext,
): Promise<LegacyHarvestStorePayload> {
	const transaction = database.transaction(storeName, 'readonly');
	const store = transaction.objectStore(storeName);
	const entries = await readAllEntries(store, store.keyPath === null);
	const records: Array<{key: unknown; value: unknown}> = [];
	for (let index = 0; index < entries.length; index += 1) {
		const entry = entries[index];
		const label = `${database.name}/${storeName}/${index}`;
		records.push({key: entry.key, value: await encodeValue(entry.value, context, label, new WeakSet())});
	}
	return {database: database.name, version: database.version, store: storeName, records};
}

async function harvestDatabase(name: string, context: LegacyHarvestContext): Promise<Array<LegacyHarvestStorePayload>> {
	const database = await openLegacyDatabase(name);
	if (database === null) return [];
	try {
		const stores: Array<LegacyHarvestStorePayload> = [];
		for (const storeName of Array.from(database.objectStoreNames)) {
			try {
				stores.push(await harvestStore(database, storeName, context));
			} catch (error) {
				if (CRITICAL_LEGACY_DATABASE_NAMES.has(name)) {
					context.criticalFailures.push(`${name}/${storeName}: ${describeError(error)}`);
				} else {
					context.truncated.push(`${name}/${storeName}`);
				}
			}
		}
		return stores;
	} finally {
		database.close();
	}
}

async function harvestMediaDevices(truncated: Array<string>): Promise<Array<LegacyHarvestMediaDevicePayload>> {
	try {
		const devices = await withTimeout(
			navigator.mediaDevices.enumerateDevices(),
			MEDIA_DEVICE_TIMEOUT_MS,
			'enumerateDevices',
		);
		return devices.map((device) => ({
			deviceId: device.deviceId,
			kind: device.kind,
			label: device.label,
			groupId: device.groupId,
		}));
	} catch {
		truncated.push('mediaDevices');
		return [];
	}
}

async function purgeLegacyServiceWorkers(truncated: Array<string>): Promise<number> {
	try {
		const registrations = await navigator.serviceWorker.getRegistrations();
		let unregistered = 0;
		for (const registration of registrations) {
			if (await registration.unregister()) unregistered += 1;
		}
		return unregistered;
	} catch {
		truncated.push('serviceWorkers');
		return 0;
	}
}

async function purgeLegacyCaches(truncated: Array<string>): Promise<number> {
	try {
		const names = await caches.keys();
		let deleted = 0;
		for (const name of names) {
			if (!name.startsWith(LEGACY_CACHE_NAME_PREFIX)) continue;
			if (await caches.delete(name)) deleted += 1;
		}
		return deleted;
	} catch {
		truncated.push('caches');
		return 0;
	}
}

async function runLegacyHarvest(): Promise<LegacyHarvestPayload> {
	const context: LegacyHarvestContext = {blobs: [], truncated: [], criticalFailures: [], totalBlobBytes: 0};
	const harvestedLocalStorage = harvestLocalStorage(context.criticalFailures);
	const stores: Array<LegacyHarvestStorePayload> = [];
	for (const name of LEGACY_DATABASE_NAMES) {
		try {
			stores.push(...(await harvestDatabase(name, context)));
		} catch (error) {
			if (CRITICAL_LEGACY_DATABASE_NAMES.has(name)) {
				context.criticalFailures.push(`${name}: ${describeError(error)}`);
			} else {
				context.truncated.push(name);
			}
		}
	}
	const mediaDevices = await harvestMediaDevices(context.truncated);
	const serviceWorkersUnregistered = await purgeLegacyServiceWorkers(context.truncated);
	const cachesDeleted = await purgeLegacyCaches(context.truncated);
	if (context.criticalFailures.length > 0) {
		throw new Error(`Legacy harvest could not read ${context.criticalFailures.join('; ')}`);
	}
	return {
		localStorage: harvestedLocalStorage,
		stores,
		mediaDevices,
		blobs: context.blobs,
		serviceWorkersUnregistered,
		cachesDeleted,
		truncated: context.truncated,
	};
}

function isTrustworthyHarvestOrigin(): boolean {
	if (window.location.protocol === 'https:') return true;
	if (window.location.protocol !== 'http:') return false;
	return LEGACY_HARVEST_LOOPBACK_HOSTS.has(window.location.hostname);
}

function isLegacyHarvestDocument(): boolean {
	try {
		return isTrustworthyHarvestOrigin() && window.location.pathname === DESKTOP_LEGACY_HARVEST_SENTINEL_PATH;
	} catch {
		return false;
	}
}

function submitLegacyHarvest(): void {
	runLegacyHarvest()
		.then(
			(payload) => ipcRenderer.invoke(DESKTOP_LEGACY_HARVEST_CHANNELS.submit, {ok: true, error: null, payload}),
			(error: unknown) =>
				ipcRenderer.invoke(DESKTOP_LEGACY_HARVEST_CHANNELS.submit, {
					ok: false,
					error: describeError(error),
					payload: null,
				}),
		)
		.catch(() => undefined);
}

if (isLegacyHarvestDocument()) {
	submitLegacyHarvest();
}
