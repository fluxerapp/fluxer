// SPDX-License-Identifier: AGPL-3.0-or-later

import * as fs from 'node:fs';
import {DatabaseSync, type SQLOutputValue, type StatementSync} from 'node:sqlite';
import type {EngineClock, EngineTimer} from '@electron/engine/EngineClock';
import type {
	AccountEngineSnapshotCursor,
	AccountEngineSnapshotEntries,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import {ACCOUNT_ENGINE_CORE_SCHEMA_EPOCH_OFFSET} from '@fluxer/desktop_ipc/src/AccountEngineCoreContract';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';

const ENGINE_CACHE_EPOCH = 1;
const CACHE_COMMIT_INTERVAL_MS = 250;
const CACHE_COMMIT_MAX_OPS = 4000;
export const CACHE_ACCOUNT_MAX_ROWS = 2_000_000;
export const CACHE_ACCOUNT_MAX_VALUE_BYTES = 1024 * 1024 * 1024;
const MENTION_PRUNE_SLACK = 50;
const SQLITE_PRIMARY_RESULT_MASK = 0xff;
const SQLITE_CORRUPT = 11;
const SQLITE_NOTADB = 26;

const SCHEMA = `
CREATE TABLE IF NOT EXISTS meta(key TEXT PRIMARY KEY, value TEXT);
CREATE TABLE IF NOT EXISTS rows(account_key TEXT, entity TEXT, key TEXT, value TEXT,
	PRIMARY KEY(account_key, entity, key)) WITHOUT ROWID;
CREATE TABLE IF NOT EXISTS cursors(account_key TEXT PRIMARY KEY, session_id TEXT, schema_epoch INTEGER, updated_at INTEGER);
CREATE TABLE IF NOT EXISTS summaries(account_key TEXT PRIMARY KEY, json TEXT, updated_at INTEGER);
CREATE TABLE IF NOT EXISTS mentions(account_key TEXT, message_id TEXT, created_at INTEGER, json TEXT,
	PRIMARY KEY(account_key, message_id)) WITHOUT ROWID;
CREATE INDEX IF NOT EXISTS mentions_by_time ON mentions(account_key, created_at DESC, message_id DESC);
`;

interface CacheAccountUsage {
	rows: number;
	bytes: number;
}

interface CacheOpenReport {
	readonly reset: 'none' | 'epoch' | 'corrupt';
	readonly memoryFallback: string | null;
}

function prefixUpperBound(prefix: string): string | null {
	const codePoints = Array.from(prefix);
	const last = codePoints.pop()?.codePointAt(0);
	if (last === undefined || last >= 0x10ffff) {
		return null;
	}
	return codePoints.join('') + String.fromCodePoint(last + 1);
}

function removeDatabaseFiles(path: string): void {
	for (const suffix of ['', '-wal', '-shm']) {
		fs.rmSync(`${path}${suffix}`, {force: true});
	}
}

function closeQuietly(db: DatabaseSync | null): void {
	try {
		db?.close();
	} catch {}
}

function readStoredEpoch(db: DatabaseSync): string | null {
	const row = db.prepare('SELECT value FROM meta WHERE key = ?').get('schema_epoch') as
		| {value: string | null}
		| undefined;
	return row?.value ?? null;
}

function writeStoredEpoch(db: DatabaseSync, schemaEpoch: string): void {
	db.prepare('INSERT OR REPLACE INTO meta(key, value) VALUES (?, ?)').run('schema_epoch', schemaEpoch);
}

function openMemoryDatabase(schemaEpoch: string): DatabaseSync {
	const db = new DatabaseSync(':memory:');
	db.exec(SCHEMA);
	writeStoredEpoch(db, schemaEpoch);
	return db;
}

function ensurePrivateFile(path: string): void {
	const handle = fs.openSync(path, 'a', 0o600);
	fs.closeSync(handle);
	try {
		fs.chmodSync(path, 0o600);
	} catch {}
}

interface Statements {
	readonly getMeta: StatementSync;
	readonly setMeta: StatementSync;
	readonly getRow: StatementSync;
	readonly rowLength: StatementSync;
	readonly upsertRow: StatementSync;
	readonly deleteRow: StatementSync;
	readonly rowsByEntity: StatementSync;
	readonly rowsByRange: StatementSync;
	readonly rowsFrom: StatementSync;
	readonly deleteEntity: StatementSync;
	readonly entityUsage: StatementSync;
	readonly allRows: StatementSync;
	readonly deleteAccountRows: StatementSync;
	readonly getCursor: StatementSync;
	readonly setCursor: StatementSync;
	readonly deleteCursor: StatementSync;
	readonly setSummary: StatementSync;
	readonly allSummaries: StatementSync;
	readonly deleteSummary: StatementSync;
	readonly insertMention: StatementSync;
	readonly pruneMentions: StatementSync;
	readonly listMentions: StatementSync;
	readonly deleteMentions: StatementSync;
	readonly countMentions: StatementSync;
	readonly accountUsage: StatementSync;
}

export class CacheDatabase {
	private readonly path: string;
	private readonly clock: EngineClock;
	private db: DatabaseSync;
	private statements: Statements;
	private readonly usage = new Map<string, CacheAccountUsage>();
	private readonly mentionCounts = new Map<string, number>();
	private inTransaction = false;
	private pendingOps = 0;
	private commitTimer: EngineTimer | null = null;
	private closed = false;
	readonly openReport: CacheOpenReport;

	constructor(path: string, snapshotSchemaEpoch: number, clock: EngineClock) {
		this.path = path;
		this.clock = clock;
		const schemaEpoch = String(ENGINE_CACHE_EPOCH * ACCOUNT_ENGINE_CORE_SCHEMA_EPOCH_OFFSET + snapshotSchemaEpoch);
		try {
			const {db, reset} = this.openFile(schemaEpoch);
			this.db = db;
			this.openReport = {reset, memoryFallback: null};
		} catch (error) {
			this.db = openMemoryDatabase(schemaEpoch);
			this.openReport = {reset: 'none', memoryFallback: error instanceof Error ? error.message : String(error)};
		}
		this.statements = this.prepareStatements();
	}

	getMeta(key: string): string | null {
		const row = this.statements.getMeta.get(key) as {value: string | null} | undefined;
		return row?.value ?? null;
	}

	setMeta(key: string, value: string): void {
		this.write(() => this.statements.setMeta.run(key, value));
	}

	usageFor(accountKey: string): CacheAccountUsage {
		const usage = this.mutableUsage(accountKey);
		return {rows: usage.rows, bytes: usage.bytes};
	}

	getRow(accountKey: string, entity: string, key: string): string | null {
		const row = this.statements.getRow.get(accountKey, entity, key) as {value: string} | undefined;
		return row?.value ?? null;
	}

	rowsByPrefix(accountKey: string, entity: string, prefix: string): Array<{key: string; value: string}> {
		const upper = prefixUpperBound(prefix);
		let rows: Array<Record<string, SQLOutputValue>>;
		if (prefix.length === 0) {
			rows = this.statements.rowsByEntity.all(accountKey, entity);
		} else if (upper === null) {
			rows = this.statements.rowsFrom.all(accountKey, entity, prefix);
		} else {
			rows = this.statements.rowsByRange.all(accountKey, entity, prefix, upper);
		}
		const result: Array<{key: string; value: string}> = [];
		for (const row of rows as Array<{key: string; value: string}>) {
			if (prefix.length === 0 || row.key.startsWith(prefix)) {
				result.push({key: row.key, value: row.value});
			}
		}
		return result;
	}

	upsertRow(accountKey: string, entity: string, key: string, value: string): void {
		const previous = this.statements.rowLength.get(accountKey, entity, key) as {size: number} | undefined;
		const usage = this.mutableUsage(accountKey);
		this.write(() => this.statements.upsertRow.run(accountKey, entity, key, value));
		if (previous === undefined) {
			usage.rows += 1;
			usage.bytes += Buffer.byteLength(value, 'utf8');
		} else {
			usage.bytes += Buffer.byteLength(value, 'utf8') - previous.size;
		}
	}

	deleteRow(accountKey: string, entity: string, key: string): void {
		const previous = this.statements.rowLength.get(accountKey, entity, key) as {size: number} | undefined;
		if (previous === undefined) {
			return;
		}
		const usage = this.mutableUsage(accountKey);
		this.write(() => this.statements.deleteRow.run(accountKey, entity, key));
		usage.rows -= 1;
		usage.bytes -= previous.size;
	}

	deleteRowsByPrefix(accountKey: string, entity: string, prefix: string): void {
		for (const row of this.rowsByPrefix(accountKey, entity, prefix)) {
			this.deleteRow(accountKey, entity, row.key);
		}
	}

	replaceEntity(accountKey: string, entity: string, entries: ReadonlyArray<readonly [string, string]>): void {
		const previous = this.statements.entityUsage.get(accountKey, entity) as {rows: number; bytes: number | null};
		const usage = this.mutableUsage(accountKey);
		this.write(() => {
			this.statements.deleteEntity.run(accountKey, entity);
			for (const [key, value] of entries) {
				this.statements.upsertRow.run(accountKey, entity, key, value);
			}
		}, entries.length + 1);
		usage.rows += entries.length - previous.rows;
		let bytes = 0;
		for (const [, value] of entries) {
			bytes += Buffer.byteLength(value, 'utf8');
		}
		usage.bytes += bytes - (previous.bytes ?? 0);
	}

	readCursor(accountKey: string): AccountEngineSnapshotCursor | null {
		const row = this.statements.getCursor.get(accountKey) as
			| {session_id: string | null; schema_epoch: number | null; updated_at: number}
			| undefined;
		if (row === undefined) {
			return null;
		}
		return {sessionId: row.session_id, schemaEpoch: row.schema_epoch, updatedAt: row.updated_at};
	}

	writeCursor(accountKey: string, cursor: AccountEngineSnapshotCursor): void {
		this.write(() =>
			this.statements.setCursor.run(accountKey, cursor.sessionId, cursor.schemaEpoch, Math.floor(cursor.updatedAt)),
		);
	}

	captureAccount(accountKey: string): AccountEngineSnapshotEntries {
		const entries: AccountEngineSnapshotEntries = {};
		for (const row of this.statements.allRows.iterate(accountKey) as Iterable<{
			entity: string;
			key: string;
			value: string;
		}>) {
			let entity = entries[row.entity];
			if (entity === undefined) {
				entity = {};
				entries[row.entity] = entity;
			}
			entity[row.key] = row.value;
		}
		return entries;
	}

	deleteAccountSnapshot(accountKey: string): void {
		this.write(() => {
			this.statements.deleteAccountRows.run(accountKey);
			this.statements.deleteCursor.run(accountKey);
		}, 2);
		this.usage.delete(accountKey);
	}

	purgeAccount(accountKey: string): void {
		this.write(() => {
			this.statements.deleteAccountRows.run(accountKey);
			this.statements.deleteCursor.run(accountKey);
			this.statements.deleteSummary.run(accountKey);
			this.statements.deleteMentions.run(accountKey);
		}, 4);
		this.usage.delete(accountKey);
		this.mentionCounts.delete(accountKey);
		this.commit();
		try {
			this.db.exec('PRAGMA wal_checkpoint(TRUNCATE)');
		} catch {}
	}

	writeSummary(accountKey: string, json: string): void {
		this.write(() => this.statements.setSummary.run(accountKey, json, this.clock.now()));
	}

	readSummaries(): Map<string, string> {
		const summaries = new Map<string, string>();
		for (const row of this.statements.allSummaries.all() as Array<{account_key: string; json: string}>) {
			summaries.set(row.account_key, row.json);
		}
		return summaries;
	}

	insertMention(accountKey: string, messageId: string, createdAt: number, json: string, keep: number): void {
		this.write(() => this.statements.insertMention.run(accountKey, messageId, createdAt, json));
		const count = (this.mentionCounts.get(accountKey) ?? this.countMentions(accountKey) - 1) + 1;
		if (count <= keep + MENTION_PRUNE_SLACK) {
			this.mentionCounts.set(accountKey, count);
			return;
		}
		this.write(() => this.statements.pruneMentions.run(accountKey, accountKey, keep));
		this.mentionCounts.set(accountKey, keep);
	}

	listMentions(accountKey: string, limit: number): Array<string> {
		return (this.statements.listMentions.all(accountKey, limit) as Array<{json: string}>).map((row) => row.json);
	}

	private countMentions(accountKey: string): number {
		return (this.statements.countMentions.get(accountKey) as {count: number}).count;
	}

	knownAccountKeys(): Set<string> {
		const keys = new Set<string>();
		for (const query of [
			'SELECT account_key FROM cursors',
			'SELECT account_key FROM summaries',
			'SELECT DISTINCT account_key FROM mentions',
			'SELECT DISTINCT account_key FROM rows',
		]) {
			for (const row of this.db.prepare(query).all() as Array<{account_key: string}>) {
				keys.add(row.account_key);
			}
		}
		return keys;
	}

	commit(): void {
		if (this.commitTimer !== null) {
			this.clock.clearTimeout(this.commitTimer);
			this.commitTimer = null;
		}
		this.pendingOps = 0;
		if (!this.inTransaction) {
			return;
		}
		this.inTransaction = false;
		this.db.exec('COMMIT');
	}

	close(): void {
		if (this.closed) {
			return;
		}
		this.commit();
		this.closed = true;
		try {
			this.db.exec('PRAGMA wal_checkpoint(TRUNCATE)');
		} catch {}
		this.db.close();
	}

	private write<T>(run: () => T, ops = 1): T {
		if (this.closed) {
			throw new Error('The account engine cache is closed');
		}
		if (!this.inTransaction) {
			this.db.exec('BEGIN');
			this.inTransaction = true;
		}
		const result = run();
		this.pendingOps += ops;
		if (this.pendingOps >= CACHE_COMMIT_MAX_OPS) {
			this.commit();
		} else if (this.commitTimer === null) {
			this.commitTimer = this.clock.setTimeout(() => {
				this.commitTimer = null;
				this.commit();
			}, CACHE_COMMIT_INTERVAL_MS);
		}
		return result;
	}

	private mutableUsage(accountKey: string): CacheAccountUsage {
		let usage = this.usage.get(accountKey);
		if (usage === undefined) {
			const stored = this.statements.accountUsage.get(accountKey) as {rows: number; bytes: number | null};
			usage = {rows: stored.rows, bytes: stored.bytes ?? 0};
			this.usage.set(accountKey, usage);
		}
		return usage;
	}

	private openFile(schemaEpoch: string): {db: DatabaseSync; reset: CacheOpenReport['reset']} {
		let reset: CacheOpenReport['reset'] = 'none';
		let db = this.tryOpen();
		if (db === null) {
			reset = 'corrupt';
			removeDatabaseFiles(this.path);
			db = this.tryOpen();
			if (db === null) {
				throw new Error(`The account engine cache at ${this.path} cannot be opened`);
			}
		}
		try {
			const storedEpoch = readStoredEpoch(db);
			if (storedEpoch !== schemaEpoch) {
				if (storedEpoch !== null) {
					reset = 'epoch';
					db.close();
					removeDatabaseFiles(this.path);
					db = this.tryOpen();
					if (db === null) {
						throw new Error(`The account engine cache at ${this.path} cannot be recreated`);
					}
				}
				writeStoredEpoch(db, schemaEpoch);
			}
		} catch (error) {
			closeQuietly(db);
			throw error;
		}
		return {db, reset};
	}

	private tryOpen(): DatabaseSync | null {
		let db: DatabaseSync | null = null;
		try {
			ensurePrivateFile(this.path);
			db = new DatabaseSync(this.path);
			db.exec('PRAGMA journal_mode=WAL');
			db.exec('PRAGMA synchronous=NORMAL');
			db.exec('PRAGMA cache_size=-65536');
			db.exec('PRAGMA busy_timeout=2000');
			db.exec('PRAGMA secure_delete=ON');
			const check = db.prepare('PRAGMA quick_check').all() as Array<Record<string, unknown>>;
			const verdict = check.length === 1 ? Object.values(check[0])[0] : null;
			if (verdict !== 'ok') {
				db.close();
				return null;
			}
			db.exec(SCHEMA);
			return db;
		} catch (error) {
			closeQuietly(db);
			if (!isRecord(error) || error.code !== 'ERR_SQLITE_ERROR' || typeof error.errcode !== 'number') {
				throw error;
			}
			const primaryCode = error.errcode & SQLITE_PRIMARY_RESULT_MASK;
			if (primaryCode !== SQLITE_CORRUPT && primaryCode !== SQLITE_NOTADB) {
				throw error;
			}
			return null;
		}
	}

	private prepareStatements(): Statements {
		const db = this.db;
		return {
			getMeta: db.prepare('SELECT value FROM meta WHERE key = ?'),
			setMeta: db.prepare('INSERT OR REPLACE INTO meta(key, value) VALUES (?, ?)'),
			getRow: db.prepare('SELECT value FROM rows WHERE account_key = ? AND entity = ? AND key = ?'),
			rowLength: db.prepare(
				'SELECT octet_length(value) AS size FROM rows WHERE account_key = ? AND entity = ? AND key = ?',
			),
			upsertRow: db.prepare('INSERT OR REPLACE INTO rows(account_key, entity, key, value) VALUES (?, ?, ?, ?)'),
			deleteRow: db.prepare('DELETE FROM rows WHERE account_key = ? AND entity = ? AND key = ?'),
			rowsByEntity: db.prepare('SELECT key, value FROM rows WHERE account_key = ? AND entity = ?'),
			rowsByRange: db.prepare(
				'SELECT key, value FROM rows WHERE account_key = ? AND entity = ? AND key >= ? AND key < ?',
			),
			rowsFrom: db.prepare('SELECT key, value FROM rows WHERE account_key = ? AND entity = ? AND key >= ?'),
			deleteEntity: db.prepare('DELETE FROM rows WHERE account_key = ? AND entity = ?'),
			entityUsage: db.prepare(
				'SELECT count(*) AS rows, total(octet_length(value)) AS bytes FROM rows WHERE account_key = ? AND entity = ?',
			),
			allRows: db.prepare('SELECT entity, key, value FROM rows WHERE account_key = ?'),
			deleteAccountRows: db.prepare('DELETE FROM rows WHERE account_key = ?'),
			getCursor: db.prepare('SELECT session_id, schema_epoch, updated_at FROM cursors WHERE account_key = ?'),
			setCursor: db.prepare(
				'INSERT OR REPLACE INTO cursors(account_key, session_id, schema_epoch, updated_at) VALUES (?, ?, ?, ?)',
			),
			deleteCursor: db.prepare('DELETE FROM cursors WHERE account_key = ?'),
			setSummary: db.prepare('INSERT OR REPLACE INTO summaries(account_key, json, updated_at) VALUES (?, ?, ?)'),
			allSummaries: db.prepare('SELECT account_key, json FROM summaries'),
			deleteSummary: db.prepare('DELETE FROM summaries WHERE account_key = ?'),
			insertMention: db.prepare(
				'INSERT OR REPLACE INTO mentions(account_key, message_id, created_at, json) VALUES (?, ?, ?, ?)',
			),
			pruneMentions: db.prepare(
				'DELETE FROM mentions WHERE account_key = ? AND message_id NOT IN (SELECT message_id FROM mentions WHERE account_key = ? ORDER BY created_at DESC, message_id DESC LIMIT ?)',
			),
			listMentions: db.prepare(
				'SELECT json FROM mentions WHERE account_key = ? ORDER BY created_at DESC, message_id DESC LIMIT ?',
			),
			deleteMentions: db.prepare('DELETE FROM mentions WHERE account_key = ?'),
			countMentions: db.prepare('SELECT count(*) AS count FROM mentions WHERE account_key = ?'),
			accountUsage: db.prepare(
				'SELECT count(*) AS rows, total(octet_length(value)) AS bytes FROM rows WHERE account_key = ?',
			),
		};
	}
}
