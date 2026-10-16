// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	assertAccountWritePreservesInstance,
	type BrowserAccountStorageOperation,
	BrowserAccountStorageTimeoutError,
	BrowserAccountStorageUnavailableError,
	CrossInstanceAccountCollisionError,
	normalizeStoredAccount,
	type QualifiedStoredAccount,
	recordMatchesAccountKey,
	type StoredAccount,
} from '@app/features/auth/state/AccountStorageContract';
import {accountKeyUserId} from '@app/features/auth/state/AccountStorageKey';
import {BrowserAccountFallbackStore} from '@app/features/auth/state/BrowserAccountFallbackStore';
import {
	type ReplaceableStoredAccountEntry,
	type StoredAccountInventory,
	type StoredAccountInventoryReplacement,
	StoredAccountInventoryReplacementConflictError,
} from '@app/features/auth/state/StoredAccountInventoryContract';
import {
	classifyStoredAccount,
	createStoredAccountInventory,
	inventoryEntriesHaveSameRevision,
	inventoryEntryStorageKey,
	normalizeStoredAccountInventoryReplacement,
	storedAccountRecordFromClassifiedEntry,
	storedAccountRecordsFromInventory,
} from '@app/features/auth/state/StoredAccountInventoryPolicy';
import {getProtectedIndexedDB} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('AccountStorage');
const DATABASE_NAME = 'FluxerAccounts';
const DATABASE_VERSION = 2;
const STORE_NAME = 'accounts';
const REQUEST_TIMEOUT_MS = 5000;
const BATCH_TIMEOUT_MS = 15000;

interface AccountTransaction<Result> {
	readonly store: IDBObjectStore;
	isActive(): boolean;
	setResult(result: Result): void;
	fail(error: unknown): void;
}

function abortAccountStorageTransaction(transaction: IDBTransaction | null | undefined, context: string): void {
	if (transaction == null) {
		return;
	}
	try {
		transaction.abort();
	} catch (error) {
		logger.warn(`Failed to abort the IndexedDB account transaction ${context}`, error);
	}
}

function closeAccountStorageDatabase(database: IDBDatabase, context: string): void {
	try {
		database.close();
	} catch (error) {
		logger.warn(`Failed to close the IndexedDB account database ${context}`, error);
	}
}

export class BrowserAccountStorageRepository {
	private readonly indexedDB = getProtectedIndexedDB();
	private database: IDBDatabase | null = null;
	private opening: Promise<IDBDatabase> | null = null;
	private fallback: BrowserAccountFallbackStore | null = null;
	private databaseEverOpened = false;

	async init(): Promise<void> {
		await this.openStore();
	}

	async list(): Promise<Array<StoredAccount>> {
		return storedAccountRecordsFromInventory(await this.listInventory());
	}

	async listInventory(): Promise<StoredAccountInventory> {
		return createStoredAccountInventory(
			'idb',
			(await this.listRaw()).map((value) =>
				classifyStoredAccount({value, authoritativeStorageKey: null, source: 'idb'}),
			),
		);
	}

	private async listRaw(): Promise<Array<unknown>> {
		const database = await this.openStore();
		if (database instanceof BrowserAccountFallbackStore) {
			return database.list();
		}
		return await this.runTransaction<Array<unknown>>(
			database,
			'readonly',
			REQUEST_TIMEOUT_MS,
			'list',
			'list accounts',
			(transaction) => {
				const request = transaction.store.getAll();
				request.onsuccess = () => {
					if (!transaction.isActive()) {
						return;
					}
					const result: unknown = request.result;
					if (!Array.isArray(result)) {
						transaction.fail(new Error('IndexedDB getAll returned a non-array result'));
						return;
					}
					transaction.setResult(result);
				};
				request.onerror = () => transaction.fail(request.error ?? new Error('IndexedDB getAll failed'));
			},
		);
	}

	async read(accountKey: string): Promise<StoredAccount | null> {
		const userId = accountKeyUserId(accountKey);
		const database = await this.openStore();
		const value =
			database instanceof BrowserAccountFallbackStore ? database.get(userId) : await this.readRaw(database, userId);
		if (value === null) {
			return null;
		}
		const record = storedAccountRecordFromClassifiedEntry(
			classifyStoredAccount({value, authoritativeStorageKey: null, source: 'idb'}),
		);
		return record !== null && recordMatchesAccountKey(record, accountKey) ? record : null;
	}

	private readRaw(database: IDBDatabase, userId: string): Promise<unknown | null> {
		return this.runTransaction<unknown | null>(
			database,
			'readonly',
			REQUEST_TIMEOUT_MS,
			'read',
			'read account',
			(transaction) => {
				const request = transaction.store.get(userId);
				request.onsuccess = () => {
					if (transaction.isActive()) {
						transaction.setResult(request.result ?? null);
					}
				};
				request.onerror = () => transaction.fail(request.error ?? new Error('IndexedDB get failed'));
			},
		);
	}

	async replaceInventoryEntry(request: StoredAccountInventoryReplacement): Promise<QualifiedStoredAccount> {
		const replacement = normalizeStoredAccountInventoryReplacement(request);
		const storageKey = inventoryEntryStorageKey(request.expected);
		const userId = accountKeyUserId(storageKey);
		const database = await this.openStore();
		if (database instanceof BrowserAccountFallbackStore) {
			this.assertReplaceable(database.get(userId), storageKey, request.expected);
			database.put(replacement);
			return replacement;
		}
		return await this.runTransaction<QualifiedStoredAccount>(
			database,
			'readwrite',
			REQUEST_TIMEOUT_MS,
			'write',
			'replace account inventory entry',
			(transaction) => {
				const read = transaction.store.get(userId);
				read.onsuccess = () => {
					if (!transaction.isActive()) {
						return;
					}
					try {
						this.assertReplaceable(read.result, storageKey, request.expected);
						const write = transaction.store.put(replacement);
						write.onerror = () => transaction.fail(write.error ?? new Error('IndexedDB replacement put failed'));
						transaction.setResult(replacement);
					} catch (error) {
						transaction.fail(error);
					}
				};
				read.onerror = () => transaction.fail(read.error ?? new Error('IndexedDB replacement read failed'));
			},
		);
	}

	private assertReplaceable(
		currentValue: unknown,
		storageKey: string,
		expected: StoredAccountInventoryReplacement['expected'],
	): void {
		const current = this.replaceableEntry(currentValue, storageKey);
		if (current === null || !inventoryEntriesHaveSameRevision(current, expected)) {
			throw new StoredAccountInventoryReplacementConflictError(
				storageKey,
				`Stored account ${storageKey} changed before its inventory replacement`,
			);
		}
	}

	private replaceableEntry(value: unknown, storageKey: string): ReplaceableStoredAccountEntry | null {
		if (value === undefined || value === null) {
			return null;
		}
		const classified = classifyStoredAccount({value, authoritativeStorageKey: null, source: 'idb'});
		if (classified.kind !== 'ready' && classified.kind !== 'runtime-recovery') {
			return null;
		}
		return inventoryEntryStorageKey(classified) === storageKey ? classified : null;
	}

	async putPreservingInstance(record: StoredAccount): Promise<void> {
		await this.write(record, 'preserve-instance');
	}

	async putReplacingInstance(record: StoredAccount): Promise<void> {
		await this.write(record, 'replace-instance');
	}

	async delete(accountKey: string): Promise<void> {
		const userId = accountKeyUserId(accountKey);
		const database = await this.openStore();
		if (database instanceof BrowserAccountFallbackStore) {
			const existing = database.get(userId) as StoredAccount | null;
			if (existing !== null && recordMatchesAccountKey(existing, accountKey)) {
				database.delete(userId);
			}
			return;
		}
		await this.runTransaction<void>(
			database,
			'readwrite',
			REQUEST_TIMEOUT_MS,
			'delete',
			'delete account',
			(transaction) => {
				const request = transaction.store.get(userId);
				request.onsuccess = () => {
					if (!transaction.isActive()) {
						return;
					}
					try {
						const existing = (request.result as StoredAccount | undefined) ?? null;
						if (existing !== null && recordMatchesAccountKey(existing, accountKey)) {
							const deletion = transaction.store.delete(userId);
							deletion.onerror = () => transaction.fail(deletion.error ?? new Error('IndexedDB delete failed'));
						}
						transaction.setResult(undefined);
					} catch (error) {
						transaction.fail(error);
					}
				};
				request.onerror = () => transaction.fail(request.error ?? new Error('IndexedDB get before delete failed'));
			},
		);
	}

	async writeMigration(records: ReadonlyArray<StoredAccount>): Promise<void> {
		const normalized = records.map(normalizeStoredAccount);
		const database = await this.openStore();
		if (database instanceof BrowserAccountFallbackStore) {
			database.putMany(normalized);
			return;
		}
		await this.runTransaction<void>(
			database,
			'readwrite',
			BATCH_TIMEOUT_MS,
			'migrate',
			'migrate accounts',
			(transaction) => {
				for (const record of normalized) {
					if (!transaction.isActive()) {
						return;
					}
					const write = transaction.store.put(record);
					write.onerror = () => transaction.fail(write.error ?? new Error('IndexedDB put batch failed'));
				}
				transaction.setResult(undefined);
			},
		);
	}

	private async write(record: StoredAccount, mode: 'preserve-instance' | 'replace-instance'): Promise<void> {
		const normalized = normalizeStoredAccount(record);
		const database = await this.openStore();
		if (database instanceof BrowserAccountFallbackStore) {
			if (mode === 'preserve-instance') {
				assertAccountWritePreservesInstance(database.get(normalized.userId) as StoredAccount | null, normalized);
			}
			database.put(normalized);
			return;
		}
		await this.runTransaction<void>(
			database,
			'readwrite',
			REQUEST_TIMEOUT_MS,
			'write',
			'write account',
			(transaction) => {
				const request = transaction.store.get(normalized.userId);
				request.onsuccess = () => {
					if (!transaction.isActive()) {
						return;
					}
					try {
						if (mode === 'preserve-instance') {
							assertAccountWritePreservesInstance((request.result as StoredAccount | undefined) ?? null, normalized);
						}
						const write = transaction.store.put(normalized);
						write.onerror = () => transaction.fail(write.error ?? new Error('IndexedDB put failed'));
						transaction.setResult(undefined);
					} catch (error) {
						transaction.fail(error);
					}
				};
				request.onerror = () => transaction.fail(request.error ?? new Error('IndexedDB get before put failed'));
			},
		);
	}

	private async openStore(): Promise<IDBDatabase | BrowserAccountFallbackStore> {
		if (this.fallback !== null) {
			return this.fallback;
		}
		try {
			return await this.ensureDatabase();
		} catch (error) {
			if (
				!(error instanceof BrowserAccountStorageUnavailableError) ||
				error.operation !== 'open' ||
				error instanceof BrowserAccountStorageTimeoutError ||
				this.databaseEverOpened
			) {
				throw error;
			}
			if (this.fallback === null) {
				logger.warn('IndexedDB init failed, using in-memory fallback', error);
				this.fallback = new BrowserAccountFallbackStore();
			}
			return this.fallback;
		}
	}

	private async ensureDatabase(): Promise<IDBDatabase> {
		if (this.database !== null) {
			return this.database;
		}
		if (this.indexedDB === null) {
			throw new BrowserAccountStorageUnavailableError('open', 'IndexedDB is unavailable for browser account storage');
		}
		const opening = this.opening ?? this.openDatabase(this.indexedDB);
		this.opening = opening;
		try {
			return await opening;
		} finally {
			if (this.opening === opening) {
				this.opening = null;
			}
		}
	}

	private openDatabase(indexedDB: IDBFactory): Promise<IDBDatabase> {
		return new Promise<IDBDatabase>((resolve, reject) => {
			let request: IDBOpenDBRequest | null = null;
			let settled = false;
			let timer: number | null = null;
			const clearTimer = (): void => {
				if (timer !== null) {
					window.clearTimeout(timer);
					timer = null;
				}
			};
			const fail = (error: unknown): void => {
				if (settled) {
					return;
				}
				settled = true;
				clearTimer();
				abortAccountStorageTransaction(request?.transaction, 'after opening failed');
				reject(this.storageError('open', 'open the IndexedDB account database', error));
			};
			const finish = (database: IDBDatabase): void => {
				if (settled) {
					closeAccountStorageDatabase(database, 'after opening had already settled');
					return;
				}
				settled = true;
				clearTimer();
				database.onversionchange = () => this.invalidateDatabase(database);
				this.database = database;
				this.databaseEverOpened = true;
				resolve(database);
			};
			timer = window.setTimeout(
				() =>
					fail(
						new BrowserAccountStorageTimeoutError('open', 'opening the IndexedDB account database', REQUEST_TIMEOUT_MS),
					),
				REQUEST_TIMEOUT_MS,
			);

			try {
				const openRequest = indexedDB.open(DATABASE_NAME, DATABASE_VERSION);
				request = openRequest;
				openRequest.onerror = () => fail(openRequest.error ?? new Error('IndexedDB open error'));
				openRequest.onblocked = () =>
					fail(
						new BrowserAccountStorageUnavailableError(
							'open',
							'The IndexedDB account database upgrade is blocked by another connection',
						),
					);
				openRequest.onupgradeneeded = () => {
					if (settled) {
						abortAccountStorageTransaction(openRequest.transaction, 'after opening had already settled');
						return;
					}
					try {
						const database = openRequest.result;
						if (!database.objectStoreNames.contains(STORE_NAME)) {
							database.createObjectStore(STORE_NAME, {keyPath: 'userId'}).createIndex('lastActive', 'lastActive');
							logger.debug('Created IndexedDB object store for accounts');
						}
					} catch (error) {
						fail(error);
					}
				};
				openRequest.onsuccess = () => finish(openRequest.result);
			} catch (error) {
				fail(error);
			}
		});
	}

	private runTransaction<Result>(
		database: IDBDatabase,
		mode: IDBTransactionMode,
		timeoutMs: number,
		operation: BrowserAccountStorageOperation,
		label: string,
		start: (transaction: AccountTransaction<Result>) => void,
	): Promise<Result> {
		return new Promise<Result>((resolve, reject) => {
			let transaction: IDBTransaction;
			let store: IDBObjectStore;
			try {
				transaction = database.transaction([STORE_NAME], mode);
				store = transaction.objectStore(STORE_NAME);
			} catch (error) {
				if (hasErrorName(error, 'InvalidStateError')) {
					this.invalidateDatabase(database);
				}
				reject(this.storageError(operation, label, error));
				return;
			}

			let settled = false;
			let hasResult = false;
			let result!: Result;
			let timer: number | null = null;
			const clearTimer = (): void => {
				if (timer !== null) {
					window.clearTimeout(timer);
					timer = null;
				}
			};
			const fail = (error: unknown): void => {
				if (settled) {
					return;
				}
				settled = true;
				clearTimer();
				if (hasErrorName(error, 'InvalidStateError')) {
					this.invalidateDatabase(database);
				}
				abortAccountStorageTransaction(transaction, `after ${label} failed`);
				reject(this.storageError(operation, label, error));
			};
			const finish = (): void => {
				if (settled) {
					return;
				}
				settled = true;
				clearTimer();
				if (!hasResult) {
					reject(
						new BrowserAccountStorageUnavailableError(
							operation,
							`Browser account storage completed ${label} without a result`,
						),
					);
					return;
				}
				resolve(result);
			};
			const control: AccountTransaction<Result> = {
				store,
				isActive: () => !settled,
				setResult: (value) => {
					if (!settled) {
						result = value;
						hasResult = true;
					}
				},
				fail,
			};
			transaction.oncomplete = finish;
			transaction.onabort = () => fail(transaction.error ?? new Error(`IndexedDB ${label} aborted`));
			transaction.onerror = () => fail(transaction.error ?? new Error(`IndexedDB ${label} failed`));
			try {
				start(control);
			} catch (error) {
				fail(error);
			}
			if (!settled) {
				timer = window.setTimeout(
					() => fail(new BrowserAccountStorageTimeoutError(operation, label, timeoutMs)),
					timeoutMs,
				);
			}
		});
	}

	private invalidateDatabase(database: IDBDatabase): void {
		if (this.database === database) {
			this.database = null;
		}
		closeAccountStorageDatabase(database, 'after it became invalid');
	}

	private storageError(operation: BrowserAccountStorageOperation, label: string, error: unknown): Error {
		if (
			error instanceof BrowserAccountStorageUnavailableError ||
			error instanceof CrossInstanceAccountCollisionError ||
			error instanceof StoredAccountInventoryReplacementConflictError
		) {
			return error;
		}
		return new BrowserAccountStorageUnavailableError(operation, `Browser account storage could not ${label}`, {
			cause: error,
		});
	}
}

function hasErrorName(error: unknown, expectedName: string): boolean {
	if (error === null || typeof error !== 'object') {
		return false;
	}
	return (error as {name?: unknown}).name === expectedName;
}
