// SPDX-License-Identifier: AGPL-3.0-or-later

import type {SnapshotEntity, SnapshotRowOp} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {
	StateSnapshotCapture,
	StateSnapshotEntries,
	StateSnapshotEntry,
	StateSyncCursor,
} from '@app/features/gateway/snapshot/SnapshotTypes';

export const MAX_SNAPSHOT_ACCOUNTS = 7;
export const MAX_SNAPSHOT_ACCOUNT_ENTRIES = 100_000;
export const MAX_SNAPSHOT_ACCOUNT_BYTES = 128 * 1024 * 1024;
export const MAX_SNAPSHOT_TOTAL_ENTRIES = MAX_SNAPSHOT_ACCOUNT_ENTRIES * MAX_SNAPSHOT_ACCOUNTS;
export const MAX_SNAPSHOT_TOTAL_BYTES = MAX_SNAPSHOT_ACCOUNT_BYTES * MAX_SNAPSHOT_ACCOUNTS;
export const MAX_SNAPSHOT_VALUE_BYTES = 256 * 1024;
export const MAX_SNAPSHOT_KEY_BYTES = 4096;
export const MAX_SNAPSHOT_STORAGE_KEY_BYTES = 4096;
export const MAX_SNAPSHOT_CURSOR_SESSION_ID_BYTES = 4096;
export const MAX_SNAPSHOT_EVENT_OPERATIONS = 100_000;

interface StoredSnapshotRow {
	readonly value: string;
	readonly byteSize: number;
}

interface SerializedSnapshotValue {
	readonly serialized: string;
	readonly byteSize: number;
}

interface SnapshotReplacementInput {
	readonly key: string;
	readonly value: unknown;
}

interface SnapshotAccountState {
	readonly storageKey: string;
	readonly entities: Map<SnapshotEntity, Map<string, StoredSnapshotRow>>;
	entryCount: number;
	byteSize: number;
	cursor: StateSyncCursor | null;
	cursorByteSize: number;
}

export class SnapshotCapacityError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'SnapshotCapacityError';
	}
}

export class SnapshotDataError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'SnapshotDataError';
	}
}

export function snapshotStringByteSize(value: string): number {
	let byteSize = 0;
	let index = 0;
	while (index < value.length) {
		const codeUnit = value.charCodeAt(index);
		if (codeUnit <= 0x7f) {
			byteSize += 1;
			index += 1;
		} else if (codeUnit <= 0x7ff) {
			byteSize += 2;
			index += 1;
		} else if (codeUnit >= 0xd800 && codeUnit <= 0xdbff) {
			const nextCodeUnit = value.charCodeAt(index + 1);
			if (nextCodeUnit >= 0xdc00 && nextCodeUnit <= 0xdfff) {
				byteSize += 4;
				index += 2;
			} else {
				byteSize += 3;
				index += 1;
			}
		} else {
			byteSize += 3;
			index += 1;
		}
	}
	return byteSize;
}

function assertNonEmptyBoundedString(value: string, label: string, maxBytes: number): number {
	if (value.length === 0) {
		throw new SnapshotDataError(`${label} must be non-empty`);
	}
	const byteSize = snapshotStringByteSize(value);
	if (byteSize > maxBytes) {
		throw new SnapshotCapacityError(`${label} exceeded ${maxBytes} bytes`);
	}
	return byteSize;
}

function serializedSnapshotValue(value: unknown): SerializedSnapshotValue {
	const serialized = JSON.stringify(value);
	if (serialized == null) {
		throw new SnapshotDataError('Snapshot value serialization returned undefined');
	}
	const byteSize = snapshotStringByteSize(serialized);
	if (byteSize > MAX_SNAPSHOT_VALUE_BYTES) {
		throw new SnapshotCapacityError(`Snapshot value exceeded ${MAX_SNAPSHOT_VALUE_BYTES} bytes`);
	}
	return {serialized, byteSize};
}

function snapshotRowByteSize(entity: SnapshotEntity, key: string, valueByteSize: number): number {
	return snapshotStringByteSize(entity) + snapshotStringByteSize(key) + valueByteSize;
}

function assertBoundedKeyPrefix(keyPrefix: string): void {
	if (snapshotStringByteSize(keyPrefix) > MAX_SNAPSHOT_KEY_BYTES) {
		throw new SnapshotCapacityError(`Snapshot key prefix exceeded ${MAX_SNAPSHOT_KEY_BYTES} bytes`);
	}
}

export function snapshotOperationEntryCount(op: SnapshotRowOp): number {
	if (op.kind === 'replaceEntity') {
		return op.entries.length;
	}
	if (op.kind === 'upsert') {
		return 1;
	}
	return 0;
}

export function assertSnapshotEntriesCapacity(entries: StateSnapshotEntries): void {
	let entryCount = 0;
	let byteSize = 0;
	for (const [entity, rows] of Object.entries(entries)) {
		assertNonEmptyBoundedString(entity, 'Snapshot entity', 64);
		for (const [key, value] of Object.entries(rows)) {
			assertNonEmptyBoundedString(key, 'Snapshot row key', MAX_SNAPSHOT_KEY_BYTES);
			const valueByteSize = snapshotStringByteSize(value);
			if (valueByteSize > MAX_SNAPSHOT_VALUE_BYTES) {
				throw new SnapshotCapacityError(`Snapshot value exceeded ${MAX_SNAPSHOT_VALUE_BYTES} bytes`);
			}
			entryCount += 1;
			byteSize += snapshotStringByteSize(entity) + snapshotStringByteSize(key) + valueByteSize;
			if (entryCount > MAX_SNAPSHOT_ACCOUNT_ENTRIES) {
				throw new SnapshotCapacityError(`Snapshot entries exceeded ${MAX_SNAPSHOT_ACCOUNT_ENTRIES} rows`);
			}
			if (byteSize > MAX_SNAPSHOT_ACCOUNT_BYTES) {
				throw new SnapshotCapacityError(`Snapshot entries exceeded ${MAX_SNAPSHOT_ACCOUNT_BYTES} bytes`);
			}
		}
	}
}

export class InMemorySnapshotStore {
	private readonly accounts = new Map<string, SnapshotAccountState>();
	private totalEntryCount = 0;
	private totalByteSize = 0;

	apply(storageKey: string, op: SnapshotRowOp): void {
		const account = this.ensureAccount(storageKey);
		switch (op.kind) {
			case 'upsert':
				this.applyUpsert(account, op.entity, op.key, serializedSnapshotValue(op.value));
				return;
			case 'delete':
				this.applyDelete(account, op.entity, op.key);
				return;
			case 'deleteByPrefix':
				this.applyDeleteByPrefix(account, op.entity, op.keyPrefix);
				return;
			case 'replaceEntity': {
				if (op.entries.length > MAX_SNAPSHOT_ACCOUNT_ENTRIES) {
					throw new SnapshotCapacityError(`Snapshot replacement exceeded ${MAX_SNAPSHOT_ACCOUNT_ENTRIES} entries`);
				}
				this.applyReplaceEntity(account, op.entity, op.entries);
				return;
			}
		}
	}

	writeCursor(storageKey: string, cursor: StateSyncCursor): void {
		const account = this.ensureAccount(storageKey);
		let sessionIdByteSize = 0;
		if (cursor.sessionId != null) {
			sessionIdByteSize = assertNonEmptyBoundedString(
				cursor.sessionId,
				'Snapshot cursor session id',
				MAX_SNAPSHOT_CURSOR_SESSION_ID_BYTES,
			);
		}
		this.assertNullableNonNegativeSafeInteger(cursor.schemaEpoch, 'Snapshot cursor schema epoch');
		if (!Number.isFinite(cursor.updatedAt) || cursor.updatedAt < 0) {
			throw new SnapshotDataError('Snapshot cursor updatedAt must be a non-negative finite number');
		}
		const cursorByteSize = sessionIdByteSize + 32;
		const nextAccountBytes = account.byteSize - account.cursorByteSize + cursorByteSize;
		const nextTotalBytes = this.totalByteSize - account.cursorByteSize + cursorByteSize;
		this.assertCapacity(account.entryCount, nextAccountBytes, this.totalEntryCount, nextTotalBytes);
		account.byteSize = nextAccountBytes;
		this.totalByteSize = nextTotalBytes;
		account.cursorByteSize = cursorByteSize;
		account.cursor = {...cursor};
	}

	readCursor(storageKey: string): StateSyncCursor | null {
		const cursor = this.accounts.get(storageKey)?.cursor;
		return cursor == null ? null : {...cursor};
	}

	get(storageKey: string, entity: SnapshotEntity, key: string): string | null {
		return this.accounts.get(storageKey)?.entities.get(entity)?.get(key)?.value ?? null;
	}

	*entriesByPrefix(
		storageKey: string,
		entity: SnapshotEntity,
		keyPrefix: string,
	): IterableIterator<StateSnapshotEntry> {
		assertBoundedKeyPrefix(keyPrefix);
		const rows = this.accounts.get(storageKey)?.entities.get(entity);
		if (rows == null) {
			return;
		}
		for (const [key, row] of rows) {
			if (key.startsWith(keyPrefix)) {
				yield {key, value: row.value};
			}
		}
	}

	getAll(storageKey: string): StateSnapshotEntries | null {
		const account = this.accounts.get(storageKey);
		if (account == null || account.cursor == null) {
			return null;
		}
		return this.copyEntries(account);
	}

	capture(storageKey: string): StateSnapshotCapture | null {
		const account = this.accounts.get(storageKey);
		if (account == null || account.cursor == null) {
			return null;
		}
		return {entries: this.copyEntries(account), cursor: {...account.cursor}};
	}

	private copyEntries(account: SnapshotAccountState): StateSnapshotEntries {
		const entries: StateSnapshotEntries = {};
		for (const [entity, rows] of account.entities) {
			const entityEntries: Record<string, string> = {};
			for (const [key, row] of rows) {
				entityEntries[key] = row.value;
			}
			entries[entity] = entityEntries;
		}
		return entries;
	}

	has(storageKey: string): boolean {
		return this.accounts.get(storageKey)?.cursor != null;
	}

	evict(storageKey: string): void {
		const account = this.accounts.get(storageKey);
		if (account == null) {
			return;
		}
		this.accounts.delete(storageKey);
		this.totalEntryCount -= account.entryCount;
		this.totalByteSize -= account.byteSize;
		this.assertAccounting();
	}

	prune(knownStorageKeys: ReadonlyArray<string>): void {
		for (const storageKey of Array.from(this.accounts.keys())) {
			if (!knownStorageKeys.includes(storageKey)) {
				this.evict(storageKey);
			}
		}
	}

	private ensureAccount(storageKey: string): SnapshotAccountState {
		const existing = this.accounts.get(storageKey);
		if (existing != null) {
			return existing;
		}
		const storageKeyByteSize = assertNonEmptyBoundedString(
			storageKey,
			'Snapshot storage key',
			MAX_SNAPSHOT_STORAGE_KEY_BYTES,
		);
		if (this.accounts.size >= MAX_SNAPSHOT_ACCOUNTS) {
			throw new SnapshotCapacityError(`Snapshot store exceeded ${MAX_SNAPSHOT_ACCOUNTS} accounts`);
		}
		if (this.totalByteSize + storageKeyByteSize > MAX_SNAPSHOT_TOTAL_BYTES) {
			throw new SnapshotCapacityError(`Snapshot store exceeded ${MAX_SNAPSHOT_TOTAL_BYTES} bytes`);
		}
		const account: SnapshotAccountState = {
			storageKey,
			entities: new Map(),
			entryCount: 0,
			byteSize: storageKeyByteSize,
			cursor: null,
			cursorByteSize: 0,
		};
		this.accounts.set(storageKey, account);
		this.totalByteSize += storageKeyByteSize;
		return account;
	}

	private applyUpsert(
		account: SnapshotAccountState,
		entity: SnapshotEntity,
		key: string,
		value: SerializedSnapshotValue,
	): void {
		assertNonEmptyBoundedString(entity, 'Snapshot entity', 64);
		assertNonEmptyBoundedString(key, 'Snapshot row key', MAX_SNAPSHOT_KEY_BYTES);
		const byteSize = snapshotRowByteSize(entity, key, value.byteSize);
		const rows = account.entities.get(entity);
		const previous = rows?.get(key);
		const entryDelta = previous == null ? 1 : 0;
		const byteDelta = byteSize - (previous?.byteSize ?? 0);
		this.assertDelta(account, entryDelta, byteDelta);
		let mutableRows = rows;
		if (mutableRows == null) {
			mutableRows = new Map();
			account.entities.set(entity, mutableRows);
		}
		mutableRows.set(key, {value: value.serialized, byteSize});
		this.commitDelta(account, entryDelta, byteDelta);
	}

	private applyDelete(account: SnapshotAccountState, entity: SnapshotEntity, key: string): void {
		const rows = account.entities.get(entity);
		const previous = rows?.get(key);
		if (rows == null || previous == null) {
			return;
		}
		rows.delete(key);
		if (rows.size === 0) {
			account.entities.delete(entity);
		}
		this.commitDelta(account, -1, -previous.byteSize);
	}

	private applyDeleteByPrefix(account: SnapshotAccountState, entity: SnapshotEntity, keyPrefix: string): void {
		assertBoundedKeyPrefix(keyPrefix);
		const rows = account.entities.get(entity);
		if (rows == null) {
			return;
		}
		let removedEntries = 0;
		let removedBytes = 0;
		for (const [key, row] of rows) {
			if (!key.startsWith(keyPrefix)) {
				continue;
			}
			rows.delete(key);
			removedEntries += 1;
			removedBytes += row.byteSize;
		}
		if (rows.size === 0) {
			account.entities.delete(entity);
		}
		this.commitDelta(account, -removedEntries, -removedBytes);
	}

	private applyReplaceEntity(
		account: SnapshotAccountState,
		entity: SnapshotEntity,
		entries: ReadonlyArray<SnapshotReplacementInput>,
	): void {
		assertNonEmptyBoundedString(entity, 'Snapshot entity', 64);
		const replacement = new Map<string, StoredSnapshotRow>();
		let replacementBytes = 0;
		for (const entry of entries) {
			assertNonEmptyBoundedString(entry.key, 'Snapshot row key', MAX_SNAPSHOT_KEY_BYTES);
			if (replacement.has(entry.key)) {
				throw new SnapshotDataError(`Snapshot replacement contains duplicate key ${entry.key}`);
			}
			const serializedValue = serializedSnapshotValue(entry.value);
			const byteSize = snapshotRowByteSize(entity, entry.key, serializedValue.byteSize);
			replacementBytes += byteSize;
			if (replacementBytes > MAX_SNAPSHOT_ACCOUNT_BYTES) {
				throw new SnapshotCapacityError(`Snapshot replacement exceeded ${MAX_SNAPSHOT_ACCOUNT_BYTES} bytes`);
			}
			replacement.set(entry.key, {value: serializedValue.serialized, byteSize});
		}
		const previous = account.entities.get(entity);
		let previousBytes = 0;
		if (previous != null) {
			for (const row of previous.values()) {
				previousBytes += row.byteSize;
			}
		}
		const entryDelta = replacement.size - (previous?.size ?? 0);
		const byteDelta = replacementBytes - previousBytes;
		this.assertDelta(account, entryDelta, byteDelta);
		if (replacement.size === 0) {
			account.entities.delete(entity);
		} else {
			account.entities.set(entity, replacement);
		}
		this.commitDelta(account, entryDelta, byteDelta);
	}

	private assertDelta(account: SnapshotAccountState, entryDelta: number, byteDelta: number): void {
		this.assertCapacity(
			account.entryCount + entryDelta,
			account.byteSize + byteDelta,
			this.totalEntryCount + entryDelta,
			this.totalByteSize + byteDelta,
		);
	}

	private assertCapacity(
		accountEntryCount: number,
		accountByteSize: number,
		totalEntryCount: number,
		totalByteSize: number,
	): void {
		if (accountEntryCount > MAX_SNAPSHOT_ACCOUNT_ENTRIES) {
			throw new SnapshotCapacityError(`Snapshot account exceeded ${MAX_SNAPSHOT_ACCOUNT_ENTRIES} entries`);
		}
		if (accountByteSize > MAX_SNAPSHOT_ACCOUNT_BYTES) {
			throw new SnapshotCapacityError(`Snapshot account exceeded ${MAX_SNAPSHOT_ACCOUNT_BYTES} bytes`);
		}
		if (totalEntryCount > MAX_SNAPSHOT_TOTAL_ENTRIES) {
			throw new SnapshotCapacityError(`Snapshot store exceeded ${MAX_SNAPSHOT_TOTAL_ENTRIES} entries`);
		}
		if (totalByteSize > MAX_SNAPSHOT_TOTAL_BYTES) {
			throw new SnapshotCapacityError(`Snapshot store exceeded ${MAX_SNAPSHOT_TOTAL_BYTES} bytes`);
		}
		if (accountEntryCount < 0 || accountByteSize < 0 || totalEntryCount < 0 || totalByteSize < 0) {
			throw new Error('Snapshot capacity accounting became negative');
		}
	}

	private commitDelta(account: SnapshotAccountState, entryDelta: number, byteDelta: number): void {
		account.entryCount += entryDelta;
		account.byteSize += byteDelta;
		this.totalEntryCount += entryDelta;
		this.totalByteSize += byteDelta;
		this.assertAccounting();
	}

	private assertNullableNonNegativeSafeInteger(value: number | null, label: string): void {
		if (value != null && (!Number.isSafeInteger(value) || value < 0)) {
			throw new SnapshotDataError(`${label} must be a non-negative safe integer or null`);
		}
	}

	private assertAccounting(): void {
		if (this.totalEntryCount < 0 || this.totalByteSize < 0) {
			throw new Error('Snapshot store accounting invariant violated');
		}
	}
}
