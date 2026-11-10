// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BOOT_FRAME_AAD_PREFIX,
	BOOT_STATE_AAD_PREFIX,
	BOOT_STATE_ACCOUNT_ID_BYTES,
	BOOT_STATE_ACCOUNT_ID_PREFIX,
	BOOT_STATE_COMPRESSION,
	BOOT_STATE_DATABASE_NAME,
	BOOT_STATE_DATABASE_VERSION,
	BOOT_STATE_KEY_STORE,
	BOOT_STATE_MARKER_STORAGE_KEY,
	BOOT_STATE_MAX_ACCOUNTS,
	BOOT_STATE_MAX_AGE_MS,
	BOOT_STATE_PREBOOT_GLOBAL,
	BOOT_STATE_STATE_STORE,
} from '@app/features/boot_state/BootStateBrowserFormat';
import {readRawStorageItem, writeRawStorageItem} from '@app/features/platform/state/PrebootMirror';
import {getProtectedIndexedDB} from '@app/features/platform/state/ProtectedWebStorage';
import {
	BOOT_STATE_MAX_PAYLOAD_BYTES,
	type DesktopBootStateFrameRequest,
	type DesktopBootStateFrameResult,
	type DesktopBootStateReadResult,
	type DesktopBootStateWriteRequest,
	type DesktopBootStateWriteResult,
	readBootStatePayloadFormat,
} from '@fluxer/desktop_ipc/src/BootStateContract';

type PrebootReadResult = DesktopBootStateReadResult | {readonly status: 'invalid'};

interface PrebootRead {
	readonly accountKey: string;
	readonly result: Promise<PrebootReadResult>;
}

interface SealedBytes {
	readonly iv: Uint8Array;
	readonly ciphertext: ArrayBuffer;
}

interface StoredFrame extends SealedBytes {
	readonly id: string;
	readonly build: string;
}

interface StoredState extends SealedBytes {
	readonly format: string;
	readonly writtenAt: number;
	readonly frame?: StoredFrame;
}

interface CapturedFrame {
	readonly id: string;
	readonly json: string;
}

const FRAME_BUILD = import.meta.env.BUILD_VERSION ?? 'dev';

interface AccountMarker {
	readonly f: string;
	readonly a: ReadonlyArray<string>;
}

const encoder = new TextEncoder();

function takePrebootRead(): PrebootRead | null {
	const holder = window as unknown as Record<string, PrebootRead | undefined>;
	const read = holder[BOOT_STATE_PREBOOT_GLOBAL] ?? null;
	delete holder[BOOT_STATE_PREBOOT_GLOBAL];
	return read;
}

function settle<T>(request: IDBRequest<T>): Promise<T> {
	return new Promise((resolve, reject) => {
		request.onsuccess = () => resolve(request.result);
		request.onerror = () => reject(request.error);
	});
}

function completion(transaction: IDBTransaction): Promise<void> {
	return new Promise((resolve, reject) => {
		transaction.oncomplete = () => resolve();
		transaction.onerror = () => reject(transaction.error);
		transaction.onabort = () => reject(transaction.error);
	});
}

async function deriveAccountId(accountKey: string): Promise<string> {
	const digest = await crypto.subtle.digest('SHA-256', encoder.encode(BOOT_STATE_ACCOUNT_ID_PREFIX + accountKey));
	return Array.from(new Uint8Array(digest, 0, BOOT_STATE_ACCOUNT_ID_BYTES), (byte) =>
		byte.toString(16).padStart(2, '0'),
	).join('');
}

function additionalData(prefix: string, version: string, accountId: string): Uint8Array<ArrayBuffer> {
	return encoder.encode(`${prefix}|${version}|${accountId}`);
}

async function seal(key: CryptoKey, payload: Uint8Array, aad: Uint8Array<ArrayBuffer>): Promise<SealedBytes> {
	const iv = crypto.getRandomValues(new Uint8Array(12));
	const ciphertext = await crypto.subtle.encrypt(
		{name: 'AES-GCM', iv, additionalData: aad},
		key,
		await compress(payload),
	);
	return {iv, ciphertext};
}

function randomFrameId(): string {
	return Array.from(crypto.getRandomValues(new Uint8Array(16)), (byte) => byte.toString(16).padStart(2, '0')).join('');
}

async function compress(payload: Uint8Array): Promise<ArrayBuffer> {
	const stream = new Blob([payload as Uint8Array<ArrayBuffer>])
		.stream()
		.pipeThrough(new CompressionStream(BOOT_STATE_COMPRESSION));
	return new Response(stream).arrayBuffer();
}

function readMarker(): AccountMarker | null {
	try {
		const parsed = JSON.parse(readRawStorageItem(BOOT_STATE_MARKER_STORAGE_KEY) ?? 'null') as AccountMarker | null;
		return parsed !== null && typeof parsed.f === 'string' && Array.isArray(parsed.a) ? parsed : null;
	} catch {
		return null;
	}
}

function writeMarker(marker: AccountMarker | null): void {
	writeRawStorageItem(
		BOOT_STATE_MARKER_STORAGE_KEY,
		marker === null || marker.a.length === 0 ? null : JSON.stringify(marker),
	);
}

function markWritten(accountKey: string, format: string): void {
	const marker = readMarker();
	const others = marker === null || marker.f !== format ? [] : marker.a.filter((key) => key !== accountKey);
	writeMarker({f: format, a: [...others, accountKey].slice(-BOOT_STATE_MAX_ACCOUNTS)});
}

function unmark(accountKey: string): void {
	const marker = readMarker();
	if (marker !== null) {
		writeMarker({f: marker.f, a: marker.a.filter((key) => key !== accountKey)});
	}
}

async function markedAccountIds(): Promise<Map<string, string>> {
	const marker = readMarker();
	const keys = marker?.a ?? [];
	const ids = await Promise.all(keys.map((accountKey) => deriveAccountId(accountKey)));
	return new Map(ids.map((accountId, index) => [accountId, keys[index]]));
}

export function createWebBootStateStore(): BootStateWebStore | null {
	const indexedDB = getProtectedIndexedDB();
	if (
		indexedDB === null ||
		typeof crypto === 'undefined' ||
		crypto.subtle === undefined ||
		typeof CompressionStream !== 'function' ||
		typeof DecompressionStream !== 'function'
	) {
		return null;
	}
	return new BootStateWebStore(indexedDB);
}

export class BootStateWebStore {
	private readonly indexedDB: IDBFactory;
	private readonly tokens = new Map<string, string>();
	private readonly queues = new Map<string, Promise<unknown>>();
	private readonly frames = new Map<string, CapturedFrame>();
	private readonly prebootRead = takePrebootRead();
	private database: Promise<IDBDatabase> | null = null;

	constructor(indexedDB: IDBFactory) {
		this.indexedDB = indexedDB;
	}

	async read(accountKey: string): Promise<DesktopBootStateReadResult> {
		const preboot = this.prebootRead;
		if (preboot === null || preboot.accountKey !== accountKey) {
			return {status: 'miss'};
		}
		const result = await preboot.result;
		if (result.status === 'invalid') {
			await this.enqueue(accountKey, () => this.deleteAccount(accountKey));
			return {status: 'miss'};
		}
		return result;
	}

	async captureFrame(request: DesktopBootStateFrameRequest): Promise<DesktopBootStateFrameResult> {
		const [{captureDomBootFrame}, {default: StreamerMode}] = await Promise.all([
			import('@app/features/boot_state/BootDomFrame'),
			import('@app/features/streamer_mode/state/StreamerMode'),
		]);
		if (this.tokens.get(request.accountKey) !== request.writeToken) {
			return {status: 'revoked'};
		}
		if (StreamerMode.enabled) return {status: 'skipped'};
		const json = await captureDomBootFrame(request.systemDark);
		if (this.tokens.get(request.accountKey) !== request.writeToken) {
			return {status: 'revoked'};
		}
		if (json === null || StreamerMode.enabled || encoder.encode(json).byteLength > BOOT_STATE_MAX_PAYLOAD_BYTES) {
			return {status: 'skipped'};
		}
		const frame: CapturedFrame = {id: randomFrameId(), json};
		this.frames.set(request.accountKey, frame);
		return {status: 'captured', frameId: frame.id};
	}

	async issueWriteToken(accountKey: string): Promise<string | null> {
		const token = crypto.randomUUID();
		this.tokens.set(accountKey, token);
		return token;
	}

	write(request: DesktopBootStateWriteRequest): Promise<DesktopBootStateWriteResult> {
		const {accountKey, writeToken, payload, frameId} = request;
		const current = () => this.tokens.get(accountKey) === writeToken;
		return this.enqueue(accountKey, async (): Promise<DesktopBootStateWriteResult> => {
			if (!current()) {
				return 'revoked';
			}
			if (payload.byteLength > BOOT_STATE_MAX_PAYLOAD_BYTES) {
				return 'too-large';
			}
			const format = readBootStatePayloadFormat(payload);
			if (format === null) {
				return 'failed';
			}
			const [accountId, db] = await Promise.all([deriveAccountId(accountKey), this.open()]);
			const key = await this.accountCryptoKey(db, accountId);
			const [sealed, frame] = await Promise.all([
				seal(key, payload, additionalData(BOOT_STATE_AAD_PREFIX, format, accountId)),
				this.sealFrame(db, key, accountKey, accountId, frameId),
			]);
			if (!current()) {
				return 'revoked';
			}
			const record: StoredState = {format, writtenAt: Date.now(), ...sealed, ...(frame === null ? {} : {frame})};
			const transaction = db.transaction(BOOT_STATE_STATE_STORE, 'readwrite');
			transaction.objectStore(BOOT_STATE_STATE_STORE).put(record, accountId);
			await completion(transaction);
			markWritten(accountKey, format);
			await this.evictStale(db);
			return 'written';
		}).catch(() => 'failed' as const);
	}

	clear(accountKey: string): Promise<void> {
		this.tokens.delete(accountKey);
		this.frames.delete(accountKey);
		return this.enqueue(accountKey, () => this.deleteAccount(accountKey));
	}

	async wipeAll(): Promise<void> {
		this.tokens.clear();
		this.frames.clear();
		await Promise.allSettled(this.queues.values());
		writeMarker(null);
		const open = this.database;
		this.database = null;
		if (open !== null) {
			(await open.catch(() => null))?.close();
		}
		await settle(this.indexedDB.deleteDatabase(BOOT_STATE_DATABASE_NAME));
	}

	private async deleteAccount(accountKey: string): Promise<void> {
		unmark(accountKey);
		const [accountId, db] = await Promise.all([deriveAccountId(accountKey), this.open()]);
		await this.deleteAccounts(db, [accountId]);
	}

	private async sealFrame(
		db: IDBDatabase,
		key: CryptoKey,
		accountKey: string,
		accountId: string,
		frameId: string | null,
	): Promise<StoredFrame | null> {
		if (frameId === null) {
			return null;
		}
		const captured = this.frames.get(accountKey);
		if (captured?.id === frameId) {
			const sealed = await seal(
				key,
				encoder.encode(captured.json),
				additionalData(BOOT_FRAME_AAD_PREFIX, FRAME_BUILD, accountId),
			);
			return {id: frameId, build: FRAME_BUILD, ...sealed};
		}
		const stored = (await settle(
			db.transaction(BOOT_STATE_STATE_STORE, 'readonly').objectStore(BOOT_STATE_STATE_STORE).get(accountId),
		)) as StoredState | undefined;
		return stored?.frame?.id === frameId && stored.frame.build === FRAME_BUILD ? stored.frame : null;
	}

	private enqueue<T>(accountKey: string, task: () => Promise<T>): Promise<T> {
		const previous = this.queues.get(accountKey) ?? Promise.resolve();
		const run = previous.catch(() => undefined).then(task);
		const tail = run.catch(() => undefined);
		this.queues.set(accountKey, tail);
		void tail.then(() => {
			if (this.queues.get(accountKey) === tail) {
				this.queues.delete(accountKey);
			}
		});
		return run;
	}

	private open(): Promise<IDBDatabase> {
		if (this.database === null) {
			const request = this.indexedDB.open(BOOT_STATE_DATABASE_NAME, BOOT_STATE_DATABASE_VERSION);
			request.onupgradeneeded = () => {
				const db = request.result;
				for (const store of [BOOT_STATE_KEY_STORE, BOOT_STATE_STATE_STORE]) {
					if (!db.objectStoreNames.contains(store)) {
						db.createObjectStore(store);
					}
				}
			};
			const database = settle(request).then((db) => {
				db.onversionchange = () => {
					db.close();
					if (this.database === database) {
						this.database = null;
					}
				};
				return db;
			});
			database.catch(() => {
				if (this.database === database) {
					this.database = null;
				}
			});
			this.database = database;
		}
		return this.database;
	}

	private async accountCryptoKey(db: IDBDatabase, accountId: string): Promise<CryptoKey> {
		const existing = await settle(
			db.transaction(BOOT_STATE_KEY_STORE, 'readonly').objectStore(BOOT_STATE_KEY_STORE).get(accountId),
		);
		if (existing instanceof CryptoKey) {
			return existing;
		}
		const key = await crypto.subtle.generateKey({name: 'AES-GCM', length: 256}, false, ['encrypt', 'decrypt']);
		const transaction = db.transaction(BOOT_STATE_KEY_STORE, 'readwrite');
		transaction.objectStore(BOOT_STATE_KEY_STORE).put(key, accountId);
		await completion(transaction);
		return key;
	}

	private async deleteAccounts(db: IDBDatabase, accountIds: ReadonlyArray<string>): Promise<void> {
		if (accountIds.length === 0) {
			return;
		}
		const transaction = db.transaction([BOOT_STATE_KEY_STORE, BOOT_STATE_STATE_STORE], 'readwrite');
		for (const accountId of accountIds) {
			transaction.objectStore(BOOT_STATE_STATE_STORE).delete(accountId);
			transaction.objectStore(BOOT_STATE_KEY_STORE).delete(accountId);
		}
		await completion(transaction);
	}

	private async evictStale(db: IDBDatabase): Promise<void> {
		const transaction = db.transaction([BOOT_STATE_KEY_STORE, BOOT_STATE_STATE_STORE], 'readonly');
		const states = transaction.objectStore(BOOT_STATE_STATE_STORE);
		const [stateIds, records, keyIds, marked] = await Promise.all([
			settle(states.getAllKeys()),
			settle(states.getAll() as IDBRequest<Array<StoredState>>),
			settle(transaction.objectStore(BOOT_STATE_KEY_STORE).getAllKeys()),
			markedAccountIds(),
		]);
		const now = Date.now();
		const kept = stateIds
			.map((id, index) => ({id: String(id), writtenAt: records[index]?.writtenAt ?? 0}))
			.filter((entry) => marked.has(entry.id) && now - entry.writtenAt <= BOOT_STATE_MAX_AGE_MS)
			.sort((a, b) => b.writtenAt - a.writtenAt)
			.slice(0, BOOT_STATE_MAX_ACCOUNTS)
			.map((entry) => entry.id);
		const keep = new Set(kept);
		const evicted = new Set([...stateIds, ...keyIds].map(String).filter((id) => !keep.has(id)));
		await this.deleteAccounts(db, Array.from(evicted));
		const marker = readMarker();
		if (marker !== null) {
			writeMarker({f: marker.f, a: [...kept].reverse().flatMap((id) => marked.get(id) ?? [])});
		}
	}
}
