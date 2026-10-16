// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedIndexedDB} from '@app/features/platform/state/ProtectedWebStorage';

export const PersistentStorageBackendKind = Object.freeze({
	INDEXED_DB: 'indexeddb',
	MEMORY: 'memory',
	DESKTOP: 'desktop',
} as const);

export type PersistentStorageBackendKind =
	(typeof PersistentStorageBackendKind)[keyof typeof PersistentStorageBackendKind];

export interface AppStorageStamp {
	readonly wall: number;
	readonly seq: number;
}

export interface AppStorageEntry {
	readonly value: string;
	readonly updatedAt: AppStorageStamp;
}

export interface AppStorageWrite {
	readonly scope: string;
	readonly key: string;
	readonly value: string;
	readonly ifAbsent?: boolean;
}

export interface PersistentStorageBackend {
	readonly kind: PersistentStorageBackendKind;
	load(scope: string): Promise<Map<string, AppStorageEntry>>;
	get(scope: string, key: string): Promise<AppStorageEntry | null>;
	set(scope: string, key: string, value: string): Promise<void>;
	delete(scope: string, key: string): Promise<void>;
	clearAllForScope(scope: string): Promise<void>;
	clearAllExcept(keysToKeep: ReadonlySet<string>): Promise<void>;
	setMany(writes: ReadonlyArray<AppStorageWrite>): Promise<void>;
}

export const APP_STORAGE_INDEXED_DB_NAME = 'fluxer-app-storage';
export const APP_STORAGE_INDEXED_DB_VERSION = 1;
export const APP_STORAGE_OBJECT_STORE = 'entries';
export const APP_STORAGE_SCOPE_INDEX = 'scope';

const APP_STORAGE_OPERATION_TIMEOUT_MS = 5_000;
const APP_STORAGE_OPEN_BLOCKED_WAIT_MS = 500;
const APP_STORAGE_OPEN_ATTEMPTS = 3;

export class AppStorageQuotaExceededError extends Error {
	public constructor(operation: string, options?: ErrorOptions) {
		super(`AppStorage ${operation} exceeded the available storage quota`, options);
		this.name = 'AppStorageQuotaExceededError';
	}
}

export class AppStorageBlockedError extends Error {
	public constructor(operation: string, options?: ErrorOptions) {
		super(`AppStorage ${operation} is blocked by another open connection`, options);
		this.name = 'AppStorageBlockedError';
	}
}

export class AppStorageOperationError extends Error {
	public constructor(operation: string, options?: ErrorOptions) {
		super(`AppStorage ${operation} failed`, options);
		this.name = 'AppStorageOperationError';
	}
}

export class AppStorageOpenTimeoutError extends AppStorageOperationError {
	public constructor() {
		super('open');
		this.name = 'AppStorageOpenTimeoutError';
	}
}

function isQuotaFailure(error: unknown): boolean {
	if (error == null || typeof error !== 'object') {
		return false;
	}
	const candidate = error as {name?: unknown; code?: unknown};
	return candidate.name === 'QuotaExceededError' || candidate.code === 22;
}

function toBackendError(error: unknown, operation: string): Error {
	if (
		error instanceof AppStorageQuotaExceededError ||
		error instanceof AppStorageBlockedError ||
		error instanceof AppStorageOperationError
	) {
		return error;
	}
	if (isQuotaFailure(error)) {
		return new AppStorageQuotaExceededError(operation, {cause: error});
	}
	return new AppStorageOperationError(operation, {cause: error});
}

const EPOCH_STAMP: AppStorageStamp = Object.freeze({wall: 0, seq: 0});

let stampWall = 0;
let stampSequence = 0;

function nextStamp(): AppStorageStamp {
	const now = Date.now();
	stampWall = now > stampWall ? now : stampWall;
	stampSequence += 1;
	return {wall: stampWall, seq: stampSequence};
}

interface AppStorageRecord {
	readonly id: string;
	readonly scope: string;
	readonly key: string;
	readonly value: string;
	readonly updatedAt: AppStorageStamp;
}

interface ReadRecord {
	readonly key: string;
	readonly entry: AppStorageEntry;
}

export function appStorageRecordId(scope: string, key: string): string {
	return JSON.stringify([scope, key]);
}

function buildRecord(scope: string, key: string, value: string): AppStorageRecord {
	return {id: appStorageRecordId(scope, key), scope, key, value, updatedAt: nextStamp()};
}

function readStamp(value: unknown): AppStorageStamp {
	if (value == null || typeof value !== 'object') {
		return EPOCH_STAMP;
	}
	const {wall, seq} = value as {wall?: unknown; seq?: unknown};
	if (typeof wall !== 'number' || !Number.isFinite(wall)) {
		return EPOCH_STAMP;
	}
	if (typeof seq !== 'number' || !Number.isFinite(seq)) {
		return EPOCH_STAMP;
	}
	return {wall, seq};
}

function readRecord(value: unknown): ReadRecord | null {
	if (value == null || typeof value !== 'object') {
		return null;
	}
	const {key, value: storedValue, updatedAt} = value as {key?: unknown; value?: unknown; updatedAt?: unknown};
	if (typeof key !== 'string' || typeof storedValue !== 'string') {
		return null;
	}
	return {key, entry: {value: storedValue, updatedAt: readStamp(updatedAt)}};
}

interface TransactionControl<Result> {
	readonly store: IDBObjectStore;
	setResult(result: Result): void;
	fail(error: unknown): void;
}

interface TransactionRequest<Result> {
	readonly database: IDBDatabase;
	readonly mode: IDBTransactionMode;
	readonly operation: string;
	start(control: TransactionControl<Result>): void;
}

function runTransaction<Result>({database, mode, operation, start}: TransactionRequest<Result>): Promise<Result> {
	return new Promise<Result>((resolve, reject) => {
		let transaction: IDBTransaction;
		try {
			transaction = database.transaction(APP_STORAGE_OBJECT_STORE, mode);
		} catch (error) {
			reject(toBackendError(error, operation));
			return;
		}

		let settled = false;
		let failure: unknown = null;
		let hasResult = false;
		let result: Result;
		let timeoutId: ReturnType<typeof globalThis.setTimeout> | null = null;

		const settle = (): void => {
			if (settled) {
				return;
			}
			settled = true;
			if (timeoutId != null) {
				globalThis.clearTimeout(timeoutId);
				timeoutId = null;
			}
			if (failure != null) {
				reject(toBackendError(failure, operation));
				return;
			}
			if (!hasResult) {
				reject(new AppStorageOperationError(operation, {cause: transaction.error}));
				return;
			}
			resolve(result);
		};

		const fail = (error: unknown): void => {
			failure ??= error ?? new AppStorageOperationError(operation);
			try {
				transaction.abort();
			} catch {
				settle();
			}
		};

		transaction.oncomplete = settle;
		transaction.onabort = () => {
			failure ??= transaction.error ?? new AppStorageOperationError(operation);
			settle();
		};

		try {
			start({
				store: transaction.objectStore(APP_STORAGE_OBJECT_STORE),
				setResult: (value) => {
					result = value;
					hasResult = true;
				},
				fail,
			});
		} catch (error) {
			fail(error);
		}

		if (!settled) {
			timeoutId = globalThis.setTimeout(
				() => fail(new AppStorageOperationError(operation)),
				APP_STORAGE_OPERATION_TIMEOUT_MS,
			);
		}
	});
}

class IndexedDBConnection {
	private database: IDBDatabase | null = null;
	private pending: Promise<IDBDatabase> | null = null;

	public constructor(private readonly factory: IDBFactory) {}

	public open(): Promise<IDBDatabase> {
		if (this.database != null) {
			return Promise.resolve(this.database);
		}
		if (this.pending != null) {
			return this.pending;
		}
		const pending = this.openWithRetry().finally(() => {
			if (this.pending === pending) {
				this.pending = null;
			}
		});
		this.pending = pending;
		return pending;
	}

	private async openWithRetry(): Promise<IDBDatabase> {
		let blocked: AppStorageBlockedError | null = null;
		for (let attempt = 0; attempt < APP_STORAGE_OPEN_ATTEMPTS; attempt += 1) {
			try {
				const database = await this.openOnce();
				this.install(database);
				return database;
			} catch (error) {
				if (!(error instanceof AppStorageBlockedError)) {
					throw error;
				}
				blocked = error;
			}
		}
		throw blocked ?? new AppStorageBlockedError('open');
	}

	private openOnce(): Promise<IDBDatabase> {
		return new Promise<IDBDatabase>((resolve, reject) => {
			let request: IDBOpenDBRequest;
			try {
				request = this.factory.open(APP_STORAGE_INDEXED_DB_NAME, APP_STORAGE_INDEXED_DB_VERSION);
			} catch (error) {
				reject(toBackendError(error, 'open'));
				return;
			}

			let settled = false;
			let blocked = false;
			let timeoutId: ReturnType<typeof globalThis.setTimeout> = globalThis.setTimeout(
				() => finishReject(new AppStorageOpenTimeoutError()),
				APP_STORAGE_OPERATION_TIMEOUT_MS,
			);

			function finishResolve(database: IDBDatabase): void {
				if (settled) {
					database.close();
					return;
				}
				settled = true;
				globalThis.clearTimeout(timeoutId);
				resolve(database);
			}

			function finishReject(error: Error): void {
				if (settled) {
					return;
				}
				settled = true;
				globalThis.clearTimeout(timeoutId);
				reject(error);
			}

			request.onblocked = () => {
				blocked = true;
				globalThis.clearTimeout(timeoutId);
				timeoutId = globalThis.setTimeout(
					() => finishReject(new AppStorageBlockedError('open')),
					APP_STORAGE_OPEN_BLOCKED_WAIT_MS,
				);
			};
			request.onupgradeneeded = () => {
				const database = request.result;
				if (database.objectStoreNames.contains(APP_STORAGE_OBJECT_STORE)) {
					return;
				}
				const store = database.createObjectStore(APP_STORAGE_OBJECT_STORE, {keyPath: 'id'});
				store.createIndex(APP_STORAGE_SCOPE_INDEX, 'scope', {unique: false});
			};
			request.onsuccess = () => finishResolve(request.result);
			request.onerror = () => {
				const error = request.error;
				finishReject(blocked ? new AppStorageBlockedError('open', {cause: error}) : toBackendError(error, 'open'));
			};
		});
	}

	private install(database: IDBDatabase): void {
		this.database = database;
		database.onversionchange = () => {
			database.close();
			this.forget(database);
		};
		database.onclose = () => this.forget(database);
	}

	private forget(database: IDBDatabase): void {
		if (this.database === database) {
			this.database = null;
		}
	}
}

function createIndexedDBBackend(connection: IndexedDBConnection): PersistentStorageBackend {
	const run = async <Result>(
		mode: IDBTransactionMode,
		operation: string,
		start: (control: TransactionControl<Result>) => void,
	): Promise<Result> => {
		const database = await connection.open();
		return runTransaction<Result>({database, mode, operation, start});
	};

	const put = (control: TransactionControl<void>, record: AppStorageRecord): void => {
		const request = control.store.put(record);
		request.onerror = () => control.fail(request.error);
	};

	return {
		kind: PersistentStorageBackendKind.INDEXED_DB,

		load(scope) {
			return run<Map<string, AppStorageEntry>>('readonly', 'load scope', (control) => {
				const entries = new Map<string, AppStorageEntry>();
				const request = control.store.index(APP_STORAGE_SCOPE_INDEX).openCursor(IDBKeyRange.only(scope));
				request.onsuccess = () => {
					const cursor = request.result;
					if (cursor == null) {
						control.setResult(entries);
						return;
					}
					const record = readRecord(cursor.value);
					if (record != null) {
						entries.set(record.key, record.entry);
					}
					cursor.continue();
				};
				request.onerror = () => control.fail(request.error);
			});
		},

		get(scope, key) {
			return run<AppStorageEntry | null>('readonly', 'read value', (control) => {
				const request = control.store.get(appStorageRecordId(scope, key));
				request.onsuccess = () => {
					const record = readRecord(request.result);
					control.setResult(record == null ? null : record.entry);
				};
				request.onerror = () => control.fail(request.error);
			});
		},

		set(scope, key, value) {
			return run<void>('readwrite', 'write value', (control) => {
				put(control, buildRecord(scope, key, value));
				control.setResult(undefined);
			});
		},

		setMany(writes) {
			return run<void>('readwrite', 'write entries', (control) => {
				for (const write of writes) {
					const record = buildRecord(write.scope, write.key, write.value);
					if (write.ifAbsent !== true) {
						put(control, record);
						continue;
					}
					const existing = control.store.get(record.id);
					existing.onsuccess = () => {
						if (existing.result != null) {
							return;
						}
						put(control, record);
					};
					existing.onerror = () => control.fail(existing.error);
				}
				control.setResult(undefined);
			});
		},

		delete(scope, key) {
			return run<void>('readwrite', 'delete value', (control) => {
				const request = control.store.delete(appStorageRecordId(scope, key));
				request.onerror = () => control.fail(request.error);
				control.setResult(undefined);
			});
		},

		clearAllForScope(scope) {
			return run<void>('readwrite', 'clear scope', (control) => {
				const request = control.store.index(APP_STORAGE_SCOPE_INDEX).openKeyCursor(IDBKeyRange.only(scope));
				request.onsuccess = () => {
					const cursor = request.result;
					if (cursor == null) {
						control.setResult(undefined);
						return;
					}
					const removal = control.store.delete(cursor.primaryKey);
					removal.onerror = () => control.fail(removal.error);
					cursor.continue();
				};
				request.onerror = () => control.fail(request.error);
			});
		},

		clearAllExcept(keysToKeep) {
			return run<void>('readwrite', 'clear entries', (control) => {
				const request = control.store.openCursor();
				request.onsuccess = () => {
					const cursor = request.result;
					if (cursor == null) {
						control.setResult(undefined);
						return;
					}
					const record = readRecord(cursor.value);
					if (record == null || !keysToKeep.has(record.key)) {
						cursor.delete();
					}
					cursor.continue();
				};
				request.onerror = () => control.fail(request.error);
			});
		},
	};
}

function createMemoryBackend(): PersistentStorageBackend {
	const scopes = new Map<string, Map<string, AppStorageEntry>>();

	const writeEntry = ({scope, key, value, ifAbsent}: AppStorageWrite): void => {
		let entries = scopes.get(scope);
		if (entries == null) {
			entries = new Map<string, AppStorageEntry>();
			scopes.set(scope, entries);
		}
		if (ifAbsent === true && entries.has(key)) {
			return;
		}
		entries.set(key, {value, updatedAt: nextStamp()});
	};

	const dropEmptyScope = (scope: string): void => {
		if (scopes.get(scope)?.size === 0) {
			scopes.delete(scope);
		}
	};

	return {
		kind: PersistentStorageBackendKind.MEMORY,

		load(scope) {
			return Promise.resolve(new Map(scopes.get(scope) ?? []));
		},

		get(scope, key) {
			return Promise.resolve(scopes.get(scope)?.get(key) ?? null);
		},

		set(scope, key, value) {
			writeEntry({scope, key, value});
			return Promise.resolve();
		},

		setMany(writes) {
			for (const write of writes) {
				writeEntry(write);
			}
			return Promise.resolve();
		},

		delete(scope, key) {
			scopes.get(scope)?.delete(key);
			dropEmptyScope(scope);
			return Promise.resolve();
		},

		clearAllForScope(scope) {
			scopes.delete(scope);
			return Promise.resolve();
		},

		clearAllExcept(keysToKeep) {
			for (const [scope, entries] of scopes) {
				for (const key of [...entries.keys()]) {
					if (!keysToKeep.has(key)) {
						entries.delete(key);
					}
				}
				dropEmptyScope(scope);
			}
			return Promise.resolve();
		},
	};
}

function createIndexedDBBackendWithMemoryFallback(factory: IDBFactory): PersistentStorageBackend {
	const connection = new IndexedDBConnection(factory);
	const indexedDB = createIndexedDBBackend(connection);
	let memory: PersistentStorageBackend | null = null;
	let opened = false;

	const resolve = async (): Promise<PersistentStorageBackend> => {
		if (memory !== null) {
			return memory;
		}
		if (opened) {
			return indexedDB;
		}
		try {
			await connection.open();
			opened = true;
			return indexedDB;
		} catch (error) {
			if (error instanceof AppStorageBlockedError || error instanceof AppStorageOpenTimeoutError) {
				throw error;
			}
			if (memory === null) {
				console.warn('[AppStorage] IndexedDB could not be opened, keeping app storage in memory for this tab', error);
				memory = createMemoryBackend();
			}
			return memory;
		}
	};

	return {
		get kind() {
			return memory === null ? PersistentStorageBackendKind.INDEXED_DB : PersistentStorageBackendKind.MEMORY;
		},
		load: async (scope) => (await resolve()).load(scope),
		get: async (scope, key) => (await resolve()).get(scope, key),
		set: async (scope, key, value) => (await resolve()).set(scope, key, value),
		delete: async (scope, key) => (await resolve()).delete(scope, key),
		clearAllForScope: async (scope) => (await resolve()).clearAllForScope(scope),
		clearAllExcept: async (keysToKeep) => (await resolve()).clearAllExcept(keysToKeep),
		setMany: async (writes) => (await resolve()).setMany(writes),
	};
}

export function createPersistentStorageBackend(factory: IDBFactory | null): PersistentStorageBackend {
	if (factory == null) {
		return createMemoryBackend();
	}
	return createIndexedDBBackendWithMemoryFallback(factory);
}

let sharedBackend: PersistentStorageBackend | null = null;

export function getPersistentStorageBackend(): PersistentStorageBackend {
	sharedBackend ??= createPersistentStorageBackend(getProtectedIndexedDB());
	return sharedBackend;
}

export function installPersistentStorageBackend(backend: PersistentStorageBackend): void {
	sharedBackend = backend;
}
