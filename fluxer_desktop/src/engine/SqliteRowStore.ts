// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CACHE_ACCOUNT_MAX_ROWS,
	CACHE_ACCOUNT_MAX_VALUE_BYTES,
	type CacheDatabase,
} from '@electron/engine/CacheDatabase';
import type {
	AccountEngineSnapshotCapture,
	AccountEngineSnapshotCursor,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import type {AccountEngineRowOp, AccountEngineRowStore} from '@fluxer/desktop_ipc/src/AccountEngineCoreContract';

const ROW_STORE_VALUE_MAX_BYTES = 256 * 1024;

class RowStoreCapacityError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'RowStoreCapacityError';
	}
}

function valueByteLength(value: string): number {
	const bytes = Buffer.byteLength(value, 'utf8');
	if (bytes > ROW_STORE_VALUE_MAX_BYTES) {
		throw new RowStoreCapacityError(`Snapshot value exceeded ${ROW_STORE_VALUE_MAX_BYTES} bytes`);
	}
	return bytes;
}

function serializeValue(value: unknown): {value: string; bytes: number} {
	const serialized = JSON.stringify(value);
	if (serialized === undefined) {
		throw new Error('Snapshot value serialization returned undefined');
	}
	return {value: serialized, bytes: valueByteLength(serialized)};
}

export class SqliteRowStore implements AccountEngineRowStore {
	private readonly db: CacheDatabase;
	private readonly maxRows: number;
	private readonly maxBytes: number;

	constructor(db: CacheDatabase, limits: {maxRows?: number; maxBytes?: number} = {}) {
		this.db = db;
		this.maxRows = limits.maxRows ?? CACHE_ACCOUNT_MAX_ROWS;
		this.maxBytes = limits.maxBytes ?? CACHE_ACCOUNT_MAX_VALUE_BYTES;
	}

	has(storageKey: string): boolean {
		return this.db.readCursor(storageKey) !== null;
	}

	get(storageKey: string, entity: string, key: string): string | null {
		return this.db.getRow(storageKey, entity, key);
	}

	entriesByPrefix(storageKey: string, entity: string, prefix: string): Iterable<{key: string; value: string}> {
		return this.db.rowsByPrefix(storageKey, entity, prefix);
	}

	apply(storageKey: string, op: AccountEngineRowOp): void {
		switch (op.kind) {
			case 'upsert': {
				const {value, bytes} = serializeValue(op.value);
				this.assertCapacity(storageKey, 1, bytes);
				this.db.upsertRow(storageKey, op.entity, op.key, value);
				return;
			}
			case 'delete':
				this.db.deleteRow(storageKey, op.entity, op.key);
				return;
			case 'deleteByPrefix':
				this.db.deleteRowsByPrefix(storageKey, op.entity, op.keyPrefix);
				return;
			case 'replaceEntity': {
				const seen = new Set<string>();
				const entries: Array<readonly [string, string]> = [];
				let bytes = 0;
				for (const entry of op.entries) {
					if (seen.has(entry.key)) {
						throw new Error(`Snapshot replacement contains duplicate key ${entry.key}`);
					}
					seen.add(entry.key);
					const serialized = serializeValue(entry.value);
					bytes += serialized.bytes;
					entries.push([entry.key, serialized.value]);
				}
				this.assertCapacity(storageKey, entries.length, bytes);
				this.db.replaceEntity(storageKey, op.entity, entries);
				return;
			}
		}
	}

	writeCursor(storageKey: string, cursor: AccountEngineSnapshotCursor): void {
		this.db.writeCursor(storageKey, cursor);
	}

	readCursor(storageKey: string): AccountEngineSnapshotCursor | null {
		return this.db.readCursor(storageKey);
	}

	capture(storageKey: string): AccountEngineSnapshotCapture | null {
		const cursor = this.db.readCursor(storageKey);
		if (cursor === null) {
			return null;
		}
		return {entries: this.db.captureAccount(storageKey), cursor};
	}

	replaceAccount(storageKey: string, capture: AccountEngineSnapshotCapture): void {
		let rows = 0;
		let bytes = 0;
		const entities: Array<readonly [string, Array<readonly [string, string]>]> = [];
		for (const [entity, values] of Object.entries(capture.entries)) {
			const entries: Array<readonly [string, string]> = [];
			for (const [key, value] of Object.entries(values)) {
				if (typeof value !== 'string') {
					throw new Error(`Snapshot row ${entity}/${key} is not serialized`);
				}
				bytes += valueByteLength(value);
				rows += 1;
				entries.push([key, value]);
			}
			entities.push([entity, entries]);
		}
		if (rows > this.maxRows) {
			throw new RowStoreCapacityError(`The cache for ${storageKey} exceeded ${this.maxRows} rows`);
		}
		if (bytes > this.maxBytes) {
			throw new RowStoreCapacityError(`The cache for ${storageKey} exceeded ${this.maxBytes} value bytes`);
		}
		this.db.deleteAccountSnapshot(storageKey);
		for (const [entity, entries] of entities) {
			this.db.replaceEntity(storageKey, entity, entries);
		}
		this.db.writeCursor(storageKey, capture.cursor);
	}

	evict(storageKey: string): void {
		this.db.deleteAccountSnapshot(storageKey);
	}

	private assertCapacity(storageKey: string, addedRows: number, addedBytes: number): void {
		const usage = this.db.usageFor(storageKey);
		if (usage.rows + addedRows > this.maxRows) {
			throw new RowStoreCapacityError(`The cache for ${storageKey} exceeded ${this.maxRows} rows`);
		}
		if (usage.bytes + addedBytes > this.maxBytes) {
			throw new RowStoreCapacityError(`The cache for ${storageKey} exceeded ${this.maxBytes} value bytes`);
		}
	}
}
