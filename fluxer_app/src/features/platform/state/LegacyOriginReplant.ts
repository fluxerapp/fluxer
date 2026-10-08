// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import {
	createLegacyHarvestValueDecoder,
	type LegacyHarvestValueDecoder,
} from '@app/features/platform/state/LegacyHarvestValueDecoder';
import {GLOBAL_APP_STORAGE_SCOPE} from '@app/features/platform/state/PersistentStorage';
import {
	APP_STORAGE_INDEXED_DB_NAME,
	APP_STORAGE_INDEXED_DB_VERSION,
	APP_STORAGE_OBJECT_STORE,
	APP_STORAGE_SCOPE_INDEX,
	appStorageRecordId,
	getPersistentStorageBackend,
} from '@app/features/platform/state/PersistentStorageBackend';
import {getProtectedIndexedDB, getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	DesktopLegacyHarvest,
	DesktopLegacyHarvestRecord,
	DesktopLegacyHarvestStore,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';
import {
	DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY,
	DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE,
} from '@fluxer/desktop_ipc/src/LegacyHarvestContract';

const REPLANT_DATABASE_TIMEOUT_MS = 15_000;
const MEDIA_DEVICE_TIMEOUT_MS = 5_000;

const VOICE_SETTINGS_DEVICE_FIELDS: ReadonlyArray<string> = Object.freeze([
	'inputDeviceId',
	'outputDeviceId',
	'videoDeviceId',
]);

const PSEUDO_DEVICE_IDS: ReadonlySet<string> = new Set(['', 'default', 'communications']);

const logger = new Logger('LegacyOriginReplant');

interface ReplantStoreIndexSchema {
	readonly name: string;
	readonly keyPath: string;
}

interface ReplantStoreSchema {
	readonly name: string;
	readonly keyPath: string | null;
	readonly indexes?: ReadonlyArray<ReplantStoreIndexSchema>;
}

interface ReplantDatabaseSchema {
	readonly name: string;
	readonly version: number;
	readonly stores: ReadonlyArray<ReplantStoreSchema>;
}

interface HarvestedMediaDevice {
	readonly deviceId: string;
	readonly kind: string;
	readonly label: string;
}

const REPLANT_DATABASES: ReadonlyArray<ReplantDatabaseSchema> = Object.freeze([
	{
		name: 'FluxerAccounts',
		version: 2,
		stores: [{name: 'accounts', keyPath: 'userId', indexes: [{name: 'lastActive', keyPath: 'lastActive'}]}],
	},
	{
		name: 'FluxerCustomSounds',
		version: 2,
		stores: [
			{name: 'customSounds', keyPath: 'soundType'},
			{name: 'entranceSound', keyPath: null},
		],
	},
	{
		name: 'fluxer-theme-library',
		version: 1,
		stores: [
			{name: 'themes', keyPath: 'id'},
			{name: 'assets', keyPath: 'id'},
			{name: 'localFiles', keyPath: 'id'},
			{name: 'meta', keyPath: null},
		],
	},
	{
		name: 'FluxerVoiceStats',
		version: 1,
		stores: [{name: 'stats', keyPath: 'reportId'}],
	},
	{
		name: APP_STORAGE_INDEXED_DB_NAME,
		version: APP_STORAGE_INDEXED_DB_VERSION,
		stores: [
			{
				name: APP_STORAGE_OBJECT_STORE,
				keyPath: 'id',
				indexes: [{name: APP_STORAGE_SCOPE_INDEX, keyPath: 'scope'}],
			},
		],
	},
]);

class LegacyReplantUnavailableError extends Error {
	public constructor() {
		super('IndexedDB is unavailable, so the legacy origin cannot be replanted');
		this.name = 'LegacyReplantUnavailableError';
	}
}

class LegacyReplantDatabaseError extends Error {
	public constructor(database: string, options?: ErrorOptions) {
		super(`The legacy replant could not open ${database}`, options);
		this.name = 'LegacyReplantDatabaseError';
	}
}

class LegacyReplantBlockedError extends Error {
	public constructor(database: string) {
		super(`The legacy replant of ${database} is blocked by another open connection`);
		this.name = 'LegacyReplantBlockedError';
	}
}

class LegacyReplantStoreError extends Error {
	public constructor(database: string, store: string, options?: ErrorOptions) {
		super(`The legacy replant could not write ${database}/${store}`, options);
		this.name = 'LegacyReplantStoreError';
	}
}

class LegacyReplantTimeoutError extends Error {
	public constructor(database: string, timeoutMs: number) {
		super(`The legacy replant of ${database} timed out after ${timeoutMs}ms`);
		this.name = 'LegacyReplantTimeoutError';
	}
}

class LegacyReplantInvalidRecordError extends Error {
	public constructor(database: string, store: string, index: number, reason: string) {
		super(`The legacy replant found an invalid record in ${database}/${store} at index ${index}: ${reason}`);
		this.name = 'LegacyReplantInvalidRecordError';
	}
}

function abortReplantTransaction(transaction: IDBTransaction | null | undefined, context: string): void {
	if (transaction == null) {
		return;
	}
	try {
		transaction.abort();
	} catch (error) {
		logger.warn(`The legacy replant could not abort ${context}`, error);
	}
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function toValidKey(value: unknown): IDBValidKey | null {
	if (typeof value === 'string') {
		return value;
	}
	if (typeof value === 'number' && Number.isFinite(value)) {
		return value;
	}
	if (value instanceof Date && Number.isFinite(value.getTime())) {
		return value;
	}
	if (value instanceof ArrayBuffer) {
		return value;
	}
	if (ArrayBuffer.isView(value)) {
		const buffer = new ArrayBuffer(value.byteLength);
		new Uint8Array(buffer).set(new Uint8Array(value.buffer, value.byteOffset, value.byteLength));
		return buffer;
	}
	if (!Array.isArray(value)) {
		return null;
	}
	const parts: Array<IDBValidKey> = [];
	for (const item of value) {
		const part = toValidKey(item);
		if (part === null) {
			return null;
		}
		parts.push(part);
	}
	return parts;
}

function keyIdentity(key: IDBValidKey): string {
	if (typeof key === 'string') {
		return `s:${JSON.stringify(key)}`;
	}
	if (typeof key === 'number') {
		return `n:${key}`;
	}
	if (key instanceof Date) {
		return `d:${key.getTime()}`;
	}
	if (key instanceof ArrayBuffer) {
		return `b:${Array.from(new Uint8Array(key), (byte) => byte.toString(16).padStart(2, '0')).join('')}`;
	}
	if (Array.isArray(key)) {
		return `a:${JSON.stringify(key.map(keyIdentity))}`;
	}
	return `x:${String(key)}`;
}

function inlineKeyOf(value: unknown, keyPath: string): IDBValidKey | null {
	if (!isPlainObject(value)) {
		return null;
	}
	return toValidKey(value[keyPath]);
}

function validAppStorageValue(value: unknown): boolean {
	if (!isPlainObject(value)) {
		return false;
	}
	const {key, scope, updatedAt, value: storedValue} = value;
	if (typeof scope !== 'string' || typeof key !== 'string' || typeof storedValue !== 'string') {
		return false;
	}
	if (!Object.hasOwn(value, 'id') || value.id !== appStorageRecordId(scope, key)) {
		return false;
	}
	if (updatedAt == null) {
		return true;
	}
	if (!isPlainObject(updatedAt)) {
		return false;
	}
	return (
		typeof updatedAt.wall === 'number' &&
		Number.isFinite(updatedAt.wall) &&
		typeof updatedAt.seq === 'number' &&
		Number.isFinite(updatedAt.seq)
	);
}

function openReplantDatabase(factory: IDBFactory, schema: ReplantDatabaseSchema): Promise<IDBDatabase> {
	return new Promise<IDBDatabase>((resolve, reject) => {
		let request: IDBOpenDBRequest;
		try {
			request = factory.open(schema.name, schema.version);
		} catch (error) {
			reject(new LegacyReplantDatabaseError(schema.name, {cause: error}));
			return;
		}
		let settled = false;
		const timer = globalThis.setTimeout(() => {
			if (settled) return;
			settled = true;
			abortReplantTransaction(request.transaction, `${schema.name} after its open timed out`);
			reject(new LegacyReplantTimeoutError(schema.name, REPLANT_DATABASE_TIMEOUT_MS));
		}, REPLANT_DATABASE_TIMEOUT_MS);
		const finish = (action: () => void): void => {
			if (settled) return;
			settled = true;
			globalThis.clearTimeout(timer);
			action();
		};
		request.onupgradeneeded = () => {
			if (settled) {
				abortReplantTransaction(request.transaction, `${schema.name} after its open had already settled`);
				return;
			}
			const database = request.result;
			for (const store of schema.stores) {
				if (database.objectStoreNames.contains(store.name)) {
					continue;
				}
				let created: IDBObjectStore;
				if (store.keyPath === null) {
					created = database.createObjectStore(store.name);
				} else {
					created = database.createObjectStore(store.name, {keyPath: store.keyPath});
				}
				for (const index of store.indexes ?? []) {
					created.createIndex(index.name, index.keyPath);
				}
			}
		};
		request.onsuccess = () => {
			if (settled) {
				request.result.close();
				return;
			}
			finish(() => resolve(request.result));
		};
		request.onerror = () => finish(() => reject(new LegacyReplantDatabaseError(schema.name, {cause: request.error})));
		request.onblocked = () => finish(() => reject(new LegacyReplantBlockedError(schema.name)));
	});
}

function replantStoreRecords(
	database: IDBDatabase,
	schema: ReplantStoreSchema,
	records: ReadonlyArray<DesktopLegacyHarvestRecord>,
	decode: LegacyHarvestValueDecoder,
): Promise<number> {
	return new Promise<number>((resolve, reject) => {
		let settled = false;
		let planted = 0;
		let transaction: IDBTransaction;
		try {
			transaction = database.transaction(schema.name, 'readwrite');
		} catch (error) {
			reject(new LegacyReplantStoreError(database.name, schema.name, {cause: error}));
			return;
		}
		const timer = globalThis.setTimeout(() => {
			if (settled) return;
			settled = true;
			abortReplantTransaction(transaction, `${database.name}/${schema.name} after its write timed out`);
			reject(new LegacyReplantTimeoutError(database.name, REPLANT_DATABASE_TIMEOUT_MS));
		}, REPLANT_DATABASE_TIMEOUT_MS);
		const finish = (action: () => void): void => {
			if (settled) return;
			settled = true;
			globalThis.clearTimeout(timer);
			action();
		};
		transaction.oncomplete = () => finish(() => resolve(planted));
		transaction.onabort = () =>
			finish(() => reject(new LegacyReplantStoreError(database.name, schema.name, {cause: transaction.error})));
		try {
			const store = transaction.objectStore(schema.name);
			const keysRequest = store.getAllKeys();
			keysRequest.onsuccess = () => {
				if (settled) return;
				try {
					const existing = new Set(keysRequest.result.map(keyIdentity));
					const incoming = new Set<string>();
					for (const [index, record] of records.entries()) {
						const value = decode(record.value);
						if (database.name === APP_STORAGE_INDEXED_DB_NAME && schema.name === APP_STORAGE_OBJECT_STORE) {
							if (!validAppStorageValue(value)) {
								throw new LegacyReplantInvalidRecordError(
									database.name,
									schema.name,
									index,
									'the app-storage row has an invalid shape',
								);
							}
						}
						const key = schema.keyPath === null ? toValidKey(decode(record.key)) : inlineKeyOf(value, schema.keyPath);
						if (key === null) {
							logger.warn(
								'The legacy replant is dropping a record with an unusable key',
								database.name,
								schema.name,
								index,
							);
							continue;
						}
						const identity = keyIdentity(key);
						if (incoming.has(identity)) {
							throw new LegacyReplantInvalidRecordError(database.name, schema.name, index, 'duplicate record key');
						}
						incoming.add(identity);
						if (existing.has(identity)) {
							continue;
						}
						if (schema.keyPath === null) {
							store.put(value, key);
						} else {
							store.put(value);
						}
						planted += 1;
					}
				} catch (error) {
					abortReplantTransaction(transaction, `${database.name}/${schema.name} after a record failed`);
					finish(() => reject(error));
				}
			};
		} catch (error) {
			abortReplantTransaction(transaction, `${database.name}/${schema.name} after its write failed to start`);
			finish(() => reject(new LegacyReplantStoreError(database.name, schema.name, {cause: error})));
		}
	});
}

function harvestedStoresFor(harvest: DesktopLegacyHarvest, database: string): ReadonlyArray<DesktopLegacyHarvestStore> {
	return harvest.stores.filter((store) => store.database === database);
}

async function replantDatabase(
	factory: IDBFactory,
	schema: ReplantDatabaseSchema,
	stores: ReadonlyArray<DesktopLegacyHarvestStore>,
	decode: LegacyHarvestValueDecoder,
): Promise<void> {
	for (const harvested of stores) {
		if (harvested.version > schema.version) {
			throw new LegacyReplantInvalidRecordError(
				schema.name,
				harvested.store,
				-1,
				`the harvested database version ${harvested.version} is newer than the supported version ${schema.version}`,
			);
		}
		if (schema.stores.some((storeSchema) => storeSchema.name === harvested.store)) {
			continue;
		}
		logger.warn(
			'The legacy harvest carries an unknown store that will not be replanted',
			schema.name,
			harvested.store,
			harvested.records.length,
		);
	}
	const database = await openReplantDatabase(factory, schema);
	try {
		for (const storeSchema of schema.stores) {
			const harvested = stores.find((store) => store.store === storeSchema.name);
			if (harvested == null || harvested.records.length === 0) {
				continue;
			}
			const planted = await replantStoreRecords(database, storeSchema, harvested.records, decode);
			if (planted > 0) {
				logger.info(`Replanted ${planted} record(s) into ${schema.name}/${storeSchema.name}`);
			}
		}
	} finally {
		database.close();
	}
}

export async function replantLegacyOriginDatabases(harvest: DesktopLegacyHarvest): Promise<void> {
	const factory = getProtectedIndexedDB();
	if (factory === null) {
		throw new LegacyReplantUnavailableError();
	}
	const decode = createLegacyHarvestValueDecoder();
	const knownDatabases = new Set(REPLANT_DATABASES.map((schema) => schema.name));
	for (const store of harvest.stores) {
		if (!knownDatabases.has(store.database) && store.records.length > 0) {
			throw new LegacyReplantInvalidRecordError(
				store.database,
				store.store,
				-1,
				'the harvested database is not supported by the replant schema',
			);
		}
	}
	for (const schema of REPLANT_DATABASES) {
		const stores = harvestedStoresFor(harvest, schema.name);
		if (stores.length === 0) {
			continue;
		}
		await replantDatabase(factory, schema, stores, decode);
	}
}

function readHarvestedMediaDevices(harvest: DesktopLegacyHarvest): Array<HarvestedMediaDevice> {
	const devices: Array<HarvestedMediaDevice> = [];
	for (const entry of harvest.mediaDevices ?? []) {
		if (!isPlainObject(entry)) {
			continue;
		}
		const {deviceId, kind, label} = entry;
		if (typeof deviceId !== 'string' || typeof kind !== 'string' || typeof label !== 'string') {
			continue;
		}
		devices.push({deviceId, kind, label});
	}
	return devices;
}

async function enumerateCurrentDevices(): Promise<Array<MediaDeviceInfo>> {
	const mediaDevices = globalThis.navigator?.mediaDevices;
	if (mediaDevices == null || typeof mediaDevices.enumerateDevices !== 'function') {
		return [];
	}
	try {
		return await new Promise<Array<MediaDeviceInfo>>((resolve, reject) => {
			const timer = globalThis.setTimeout(
				() => reject(new LegacyReplantTimeoutError('mediaDevices', MEDIA_DEVICE_TIMEOUT_MS)),
				MEDIA_DEVICE_TIMEOUT_MS,
			);
			Promise.resolve()
				.then(() => mediaDevices.enumerateDevices())
				.then(
					(value) => {
						globalThis.clearTimeout(timer);
						resolve(value);
					},
					(error: unknown) => {
						globalThis.clearTimeout(timer);
						reject(error);
					},
				);
		});
	} catch (error) {
		logger.warn('The current media devices could not be enumerated, keeping the harvested selections', error);
		return [];
	}
}

function deviceIdentity(kind: string, label: string): string {
	return JSON.stringify([kind, label]);
}

function collectDeviceIdsByIdentity(
	devices: ReadonlyArray<HarvestedMediaDevice | MediaDeviceInfo>,
): Map<string, Set<string>> {
	const byIdentity = new Map<string, Set<string>>();
	for (const device of devices) {
		if (device.label.length === 0 || PSEUDO_DEVICE_IDS.has(device.deviceId)) {
			continue;
		}
		const identity = deviceIdentity(device.kind, device.label);
		const ids = byIdentity.get(identity) ?? new Set<string>();
		ids.add(device.deviceId);
		byIdentity.set(identity, ids);
	}
	return byIdentity;
}

function buildDeviceIdMapping(
	harvested: ReadonlyArray<HarvestedMediaDevice>,
	current: ReadonlyArray<MediaDeviceInfo>,
): Map<string, string> {
	const harvestedByIdentity = collectDeviceIdsByIdentity(harvested);
	const currentByIdentity = collectDeviceIdsByIdentity(current);
	const mapping = new Map<string, string>();
	const ambiguousHarvestedIds = new Set<string>();
	for (const [identity, harvestedIds] of harvestedByIdentity) {
		const currentIds = currentByIdentity.get(identity);
		if (harvestedIds.size !== 1 || currentIds?.size !== 1) {
			continue;
		}
		const harvestedId = harvestedIds.values().next().value;
		const replacement = currentIds.values().next().value;
		if (harvestedId === undefined || replacement === undefined || replacement === harvestedId) {
			continue;
		}
		if (ambiguousHarvestedIds.has(harvestedId)) {
			continue;
		}
		const existing = mapping.get(harvestedId);
		if (existing !== undefined && existing !== replacement) {
			mapping.delete(harvestedId);
			ambiguousHarvestedIds.add(harvestedId);
			continue;
		}
		mapping.set(harvestedId, replacement);
	}
	return mapping;
}

function remapVoiceSettings(value: string, mapping: ReadonlyMap<string, string>): string | null {
	let parsed: unknown;
	try {
		parsed = JSON.parse(value);
	} catch {
		return null;
	}
	if (!isPlainObject(parsed)) {
		return null;
	}
	let changed = false;
	for (const field of VOICE_SETTINGS_DEVICE_FIELDS) {
		const current = parsed[field];
		if (typeof current !== 'string') {
			continue;
		}
		const replacement = mapping.get(current);
		if (replacement == null) {
			continue;
		}
		parsed[field] = replacement;
		changed = true;
	}
	return changed ? JSON.stringify(parsed) : null;
}

function readHarvestedVoiceSettings(harvest: DesktopLegacyHarvest): string | null {
	const value: unknown = harvest.localStorage[AppStorageKey.VOICE_SETTINGS];
	return typeof value === 'string' ? value : null;
}

export async function replantLegacyMediaDeviceSelections(harvest: DesktopLegacyHarvest): Promise<void> {
	const harvested = readHarvestedMediaDevices(harvest);
	if (harvested.length === 0) {
		return;
	}
	const mapping = buildDeviceIdMapping(harvested, await enumerateCurrentDevices());
	if (mapping.size === 0) {
		return;
	}
	const backend = getPersistentStorageBackend();
	const stored = await backend.get(GLOBAL_APP_STORAGE_SCOPE, AppStorageKey.VOICE_SETTINGS);
	const current = stored === null ? readHarvestedVoiceSettings(harvest) : stored.value;
	if (current === null) {
		return;
	}
	const remapped = remapVoiceSettings(current, mapping);
	if (remapped === null) {
		return;
	}
	await backend.set(GLOBAL_APP_STORAGE_SCOPE, AppStorageKey.VOICE_SETTINGS, remapped);
	logger.info('Remapped the persisted voice device selections');
}

function isLegacyRawStorageSeedComplete(): boolean {
	return (
		getProtectedLocalStorage()?.getItem(DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_KEY) ===
		DESKTOP_LEGACY_RAW_STORAGE_SEED_MARKER_VALUE
	);
}

export async function markLegacyReplantComplete(): Promise<void> {
	const harvestAPI = globalThis.window?.electron?.desktopLegacyHarvest;
	if (harvestAPI == null) {
		return;
	}
	if (!isLegacyRawStorageSeedComplete()) {
		logger.warn('The legacy raw localStorage seed is incomplete, leaving the harvest staged for the next launch');
		return;
	}
	await harvestAPI.markReplanted();
}
