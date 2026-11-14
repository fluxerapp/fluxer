// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {
	ensurePostgresKVStoreSchema,
	POSTGRES_KV_TABLES,
	postgresKVLiveFilter,
} from '@pkgs/kv_client/src/PostgresKVSchema';
import type Redis from 'ioredis';
import pg from 'pg';

export type KVTransferKind = 'string' | 'hash' | 'set' | 'zset' | 'list';

const KV_TRANSFER_KINDS: ReadonlyArray<KVTransferKind> = ['string', 'hash', 'set', 'zset', 'list'];

export type KVTransferItems =
	| {kind: 'string'; values: Array<Buffer>}
	| {kind: 'list'; values: Array<Buffer>}
	| {kind: 'set'; values: Array<Buffer>}
	| {kind: 'hash'; values: Array<[Buffer, Buffer]>}
	| {kind: 'zset'; values: Array<[Buffer, number]>};

export interface KVTransferChunk {
	key: Buffer;
	ttlMs: number | null;
	first: boolean;
	last: boolean;
	items: KVTransferItems;
}

export interface KVTransferSide {
	readonly name: string;
	unsupportedKeys(limit: number): Promise<Array<string>>;
	isEmpty(): Promise<boolean>;
	clear(): Promise<void>;
	read(batchSize: number): AsyncIterable<KVTransferChunk>;
	write(chunks: Array<KVTransferChunk>): Promise<void>;
	skipped(): number;
}

export interface KVTransferKindDigest {
	kind: KVTransferKind;
	keys: number;
	sha256: string;
}

export interface KVTransferResult {
	copied: number;
	verified: boolean;
	source: Array<KVTransferKindDigest>;
	target: Array<KVTransferKindDigest>;
	skipped: number;
	ttlMismatches: Array<string>;
}

export interface KVTransferOptions {
	batchSize: number;
	verifyOnly: boolean;
	replace: boolean;
	ttlToleranceMs: number;
	onProgress?: (message: string) => void;
}

export class KVTransferError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'KVTransferError';
	}
}

const MODULUS = 1n << 256n;
const REPORTED_KEY_LIMIT = 100;

const VALKEY_TYPES: Record<string, KVTransferKind> = {
	string: 'string',
	hash: 'hash',
	set: 'set',
	zset: 'zset',
	list: 'list',
};

function printable(key: Buffer): string {
	return key.toString('utf8');
}

function lengthPrefixed(...parts: Array<Buffer>): Buffer {
	const out: Array<Buffer> = [];
	for (const part of parts) {
		const length = Buffer.alloc(4);
		length.writeUInt32BE(part.length);
		out.push(length, part);
	}
	return Buffer.concat(out);
}

function scoreBytes(score: number): Buffer {
	const bytes = Buffer.alloc(8);
	bytes.writeDoubleBE(score);
	return bytes;
}

function indexBytes(index: number): Buffer {
	const bytes = Buffer.alloc(8);
	bytes.writeBigUInt64BE(BigInt(index));
	return bytes;
}

function parseScore(raw: Buffer): number {
	const text = raw.toString('utf8').toLowerCase();
	if (text === 'inf' || text === '+inf') return Number.POSITIVE_INFINITY;
	if (text === '-inf') return Number.NEGATIVE_INFINITY;
	return Number(text);
}

class Digest {
	readonly keys = new Map<KVTransferKind, number>();
	readonly sums = new Map<KVTransferKind, bigint>();
	readonly expiries = new Map<string, number | null>();
	private listIndex = 0;

	add(chunk: KVTransferChunk, nowMs: number): void {
		const kind = chunk.items.kind;
		if (chunk.first) {
			this.keys.set(kind, (this.keys.get(kind) ?? 0) + 1);
			this.expiries.set(chunk.key.toString('latin1'), chunk.ttlMs === null ? null : nowMs + chunk.ttlMs);
			this.listIndex = 0;
		}
		let sum = this.sums.get(kind) ?? 0n;
		const tag = Buffer.from(kind);
		const add = (payload: Buffer): void => {
			const hash = createHash('sha256')
				.update(lengthPrefixed(tag, chunk.key, payload))
				.digest('hex');
			sum = (sum + BigInt(`0x${hash}`)) % MODULUS;
		};
		switch (chunk.items.kind) {
			case 'string':
			case 'set':
				for (const value of chunk.items.values) add(lengthPrefixed(value));
				break;
			case 'list':
				for (const value of chunk.items.values) add(lengthPrefixed(indexBytes(this.listIndex++), value));
				break;
			case 'hash':
				for (const [field, value] of chunk.items.values) add(lengthPrefixed(field, value));
				break;
			case 'zset':
				for (const [member, score] of chunk.items.values) add(lengthPrefixed(member, scoreBytes(score)));
				break;
		}
		this.sums.set(kind, sum);
	}

	report(): Array<KVTransferKindDigest> {
		return KV_TRANSFER_KINDS.map((kind) => ({
			kind,
			keys: this.keys.get(kind) ?? 0,
			sha256: (this.sums.get(kind) ?? 0n).toString(16).padStart(64, '0'),
		}));
	}
}

async function digestSide(side: KVTransferSide, batchSize: number): Promise<Digest> {
	const digest = new Digest();
	for await (const chunk of side.read(batchSize)) digest.add(chunk, Date.now());
	return digest;
}

export async function transferKV(
	source: KVTransferSide,
	target: KVTransferSide,
	options: KVTransferOptions,
): Promise<KVTransferResult> {
	const progress = options.onProgress ?? (() => {});
	const unsupported = await source.unsupportedKeys(REPORTED_KEY_LIMIT);
	if (unsupported.length > 0) {
		throw new KVTransferError(
			`${source.name} holds keys of a type the importer cannot copy: ${unsupported.map((key) => JSON.stringify(key)).join(', ')}`,
		);
	}
	let copied = 0;
	if (!options.verifyOnly) {
		if (options.replace) {
			progress(`emptying ${target.name}`);
			await target.clear();
		} else if (!(await target.isEmpty())) {
			throw new KVTransferError(`${target.name} already holds key-value data. Pass --replace to empty it first`);
		}
		let batch: Array<KVTransferChunk> = [];
		let items = 0;
		for await (const chunk of source.read(options.batchSize)) {
			batch.push(chunk);
			if (chunk.first) copied++;
			items += Math.max(1, chunk.items.values.length);
			if (items >= options.batchSize) {
				await target.write(batch);
				batch = [];
				items = 0;
				progress(`copied ${copied} keys`);
			}
		}
		if (batch.length > 0) await target.write(batch);
		progress(`copied ${copied} keys from ${source.name} to ${target.name}`);
	}
	const sourceDigest = await digestSide(source, options.batchSize);
	const targetDigest = await digestSide(target, options.batchSize);
	const ttlMismatches: Array<string> = [];
	for (const [key, expiry] of sourceDigest.expiries) {
		const other = targetDigest.expiries.get(key);
		if (other === undefined) continue;
		const differs =
			expiry === null || other === null ? expiry !== other : Math.abs(expiry - other) > options.ttlToleranceMs;
		if (differs && ttlMismatches.length < REPORTED_KEY_LIMIT) {
			ttlMismatches.push(printable(Buffer.from(key, 'latin1')));
		}
	}
	const sourceReport = sourceDigest.report();
	const targetReport = targetDigest.report();
	const verified =
		ttlMismatches.length === 0 &&
		sourceReport.every(
			(entry, index) => entry.keys === targetReport[index].keys && entry.sha256 === targetReport[index].sha256,
		);
	return {copied, verified, source: sourceReport, target: targetReport, skipped: source.skipped(), ttlMismatches};
}

export class ValkeyTransferSide implements KVTransferSide {
	readonly name = 'Valkey';
	private skippedKeys = 0;

	constructor(
		private readonly redis: Redis,
		private readonly skippedPrefixes: ReadonlyArray<string>,
	) {}

	skipped(): number {
		return this.skippedKeys;
	}

	async isEmpty(): Promise<boolean> {
		return (await this.redis.dbsize()) === 0;
	}

	async clear(): Promise<void> {
		await this.redis.flushdb();
	}

	async unsupportedKeys(limit: number): Promise<Array<string>> {
		const found: Array<string> = [];
		for await (const keys of this.scanKeys(1000)) {
			const types = await this.types(keys);
			keys.forEach((key, index) => {
				if (types[index] !== 'none' && VALKEY_TYPES[types[index]] === undefined && found.length < limit) {
					found.push(printable(key));
				}
			});
			if (found.length >= limit) break;
		}
		return found;
	}

	private async *scanKeys(count: number): AsyncGenerator<Array<Buffer>> {
		const seen = new Set<string>();
		let cursor = '0';
		do {
			const [next, keys] = await this.redis.scanBuffer(cursor, 'COUNT', count);
			cursor = next.toString();
			const fresh = keys.filter((key) => {
				const id = key.toString('latin1');
				if (seen.has(id)) return false;
				seen.add(id);
				return true;
			});
			if (fresh.length > 0) yield fresh;
		} while (cursor !== '0');
	}

	private async types(keys: Array<Buffer>): Promise<Array<string>> {
		const pipeline = this.redis.pipeline();
		for (const key of keys) pipeline.type(key);
		const results = (await pipeline.exec()) ?? [];
		return results.map(([error, value]) => {
			if (error) throw error;
			return String(value);
		});
	}

	async *read(batchSize: number): AsyncGenerator<KVTransferChunk> {
		this.skippedKeys = 0;
		for await (const scanned of this.scanKeys(batchSize)) {
			const keys = scanned.filter((key) => {
				const skip = this.skippedPrefixes.some((prefix) => key.toString('latin1').startsWith(prefix));
				if (skip) this.skippedKeys++;
				return !skip;
			});
			if (keys.length === 0) continue;
			const pipeline = this.redis.pipeline();
			for (const key of keys) pipeline.type(key).pttl(key);
			const results = (await pipeline.exec()) ?? [];
			for (const [index, key] of keys.entries()) {
				const [typeError, type] = results[index * 2];
				const [ttlError, pttl] = results[index * 2 + 1];
				if (typeError) throw typeError;
				if (ttlError) throw ttlError;
				const kind = VALKEY_TYPES[String(type)];
				const ttl = Number(pttl);
				if (kind === undefined || ttl === -2) continue;
				const ttlMs = ttl < 0 ? null : Math.max(1, ttl);
				yield* this.readKey(key, kind, ttlMs, batchSize);
			}
		}
	}

	private async *readKey(
		key: Buffer,
		kind: KVTransferKind,
		ttlMs: number | null,
		batchSize: number,
	): AsyncGenerator<KVTransferChunk> {
		if (kind === 'string') {
			const value = await this.redis.getBuffer(key);
			if (value !== null) yield {key, ttlMs, first: true, last: true, items: {kind, values: [value]}};
			return;
		}
		let first = true;
		if (kind === 'list') {
			const length = await this.redis.llen(key);
			for (let start = 0; start < length; start += batchSize) {
				const values = await this.redis.lrangeBuffer(key, start, start + batchSize - 1);
				yield {key, ttlMs, first, last: false, items: {kind, values}};
				first = false;
			}
		} else {
			const seen = new Set<string>();
			let cursor = '0';
			do {
				const [next, raw] = await this.scanMembers(kind, key, cursor, batchSize);
				cursor = next.toString();
				const step = kind === 'set' ? 1 : 2;
				const fresh: Array<number> = [];
				for (let index = 0; index < raw.length; index += step) {
					const id = raw[index].toString('latin1');
					if (seen.has(id)) continue;
					seen.add(id);
					fresh.push(index);
				}
				if (fresh.length === 0) continue;
				const items = valkeyItems(kind, raw, fresh);
				yield {key, ttlMs, first, last: false, items};
				first = false;
			} while (cursor !== '0');
		}
		if (!first) yield {key, ttlMs, first: false, last: true, items: emptyItems(kind)};
	}

	private async scanMembers(
		kind: 'hash' | 'set' | 'zset',
		key: Buffer,
		cursor: string,
		count: number,
	): Promise<[Buffer, Array<Buffer>]> {
		switch (kind) {
			case 'hash':
				return await this.redis.hscanBuffer(key, cursor, 'COUNT', count);
			case 'set':
				return await this.redis.sscanBuffer(key, cursor, 'COUNT', count);
			case 'zset':
				return await this.redis.zscanBuffer(key, cursor, 'COUNT', count);
		}
	}

	async write(chunks: Array<KVTransferChunk>): Promise<void> {
		const pipeline = this.redis.pipeline();
		for (const chunk of chunks) {
			const {key, items} = chunk;
			if (chunk.first) pipeline.del(key);
			switch (items.kind) {
				case 'string':
					if (chunk.ttlMs === null) pipeline.set(key, items.values[0]);
					else pipeline.set(key, items.values[0], 'PX', chunk.ttlMs);
					break;
				case 'list':
					if (items.values.length > 0) pipeline.rpush(key, ...items.values);
					break;
				case 'set':
					if (items.values.length > 0) pipeline.sadd(key, ...items.values);
					break;
				case 'hash':
					if (items.values.length > 0) pipeline.hset(key, ...items.values.flat());
					break;
				case 'zset':
					if (items.values.length > 0) {
						pipeline.zadd(key, ...items.values.flatMap(([member, score]) => [scoreArgument(score), member]));
					}
					break;
			}
			if (chunk.last && chunk.ttlMs !== null && items.kind !== 'string') pipeline.pexpire(key, chunk.ttlMs);
		}
		for (const [error] of (await pipeline.exec()) ?? []) {
			if (error) throw error;
		}
	}
}

function scoreArgument(score: number): string {
	if (score === Number.POSITIVE_INFINITY) return '+inf';
	if (score === Number.NEGATIVE_INFINITY) return '-inf';
	return String(score);
}

function valkeyItems(kind: 'hash' | 'set' | 'zset', raw: Array<Buffer>, fresh: Array<number>): KVTransferItems {
	switch (kind) {
		case 'hash':
			return {kind, values: fresh.map((index): [Buffer, Buffer] => [raw[index], raw[index + 1]])};
		case 'set':
			return {kind, values: fresh.map((index) => raw[index])};
		case 'zset':
			return {kind, values: fresh.map((index): [Buffer, number] => [raw[index], parseScore(raw[index + 1])])};
	}
}

function postgresItems(kind: 'hash' | 'set' | 'zset', rows: Array<Record<string, unknown>>): KVTransferItems {
	switch (kind) {
		case 'hash':
			return {kind, values: rows.map((row): [Buffer, Buffer] => [row.field as Buffer, row.value as Buffer])};
		case 'set':
			return {kind, values: rows.map((row) => row.member as Buffer)};
		case 'zset':
			return {kind, values: rows.map((row): [Buffer, number] => [row.member as Buffer, Number(row.score)])};
	}
}

function emptyItems(kind: KVTransferKind): KVTransferItems {
	return {kind, values: []} as KVTransferItems;
}

const T = POSTGRES_KV_TABLES;
const LIVE = postgresKVLiveFilter();
const DATA_TABLES = [T.key, T.hash, T.set, T.zset, T.list];
const MEMBER_TABLES: Record<'hash' | 'set' | 'zset', {table: string; kind: string; columns: string; order: string}> = {
	hash: {table: T.hash, kind: 'h', columns: 'field, value', order: 'field'},
	set: {table: T.set, kind: 'e', columns: 'member', order: 'member'},
	zset: {table: T.zset, kind: 'z', columns: 'member, score', order: 'member'},
};
const TTL_MS = `CASE WHEN expires_at IS NULL THEN NULL ELSE greatest(1, floor(extract(epoch FROM (expires_at - now())) * 1000))::bigint END`;

export class PostgresTransferSide implements KVTransferSide {
	readonly name = 'Postgres';
	private readonly listOffsets = new Map<string, number>();

	constructor(private readonly pool: pg.Pool) {}

	static connect(config: pg.PoolConfig): PostgresTransferSide {
		return new PostgresTransferSide(new pg.Pool(config));
	}

	skipped(): number {
		return 0;
	}

	async close(): Promise<void> {
		await this.pool.end();
	}

	async prepare(): Promise<void> {
		const client = await this.pool.connect();
		try {
			await ensurePostgresKVStoreSchema(client);
		} finally {
			client.release();
		}
	}

	async unsupportedKeys(): Promise<Array<string>> {
		return [];
	}

	async isEmpty(): Promise<boolean> {
		for (const table of DATA_TABLES) {
			const {rows} = await this.pool.query(`SELECT EXISTS (SELECT 1 FROM ${table}) AS present`);
			if (rows[0].present === true) return false;
		}
		return true;
	}

	async clear(): Promise<void> {
		await this.pool.query(`TRUNCATE ${[...DATA_TABLES, T.bucket].join(', ')}`);
	}

	async *read(batchSize: number): AsyncGenerator<KVTransferChunk> {
		let after: Buffer = Buffer.alloc(0);
		while (true) {
			const {rows} = await this.pool.query(
				`SELECT key, kind, value, list_head, list_tail, ${TTL_MS} AS ttl_ms FROM ${T.key}
				WHERE key > $1 AND kind IN ('s', 'l') AND ${LIVE} ORDER BY key LIMIT $2`,
				[after, batchSize],
			);
			for (const row of rows) {
				const key = row.key as Buffer;
				const ttlMs = row.ttl_ms === null ? null : Number(row.ttl_ms);
				if (row.kind === 's') {
					yield {key, ttlMs, first: true, last: true, items: {kind: 'string', values: [row.value as Buffer]}};
					continue;
				}
				yield* this.readList(key, ttlMs, Number(row.list_head), Number(row.list_tail), batchSize);
			}
			if (rows.length < batchSize) break;
			after = rows[rows.length - 1].key as Buffer;
		}
		for (const kind of ['hash', 'set', 'zset'] as const) yield* this.readMembers(kind, batchSize);
	}

	private async *readList(
		key: Buffer,
		ttlMs: number | null,
		head: number,
		tail: number,
		batchSize: number,
	): AsyncGenerator<KVTransferChunk> {
		let first = true;
		for (let from = head; from < tail; from += batchSize) {
			const {rows} = await this.pool.query(
				`SELECT value FROM ${T.list} WHERE key = $1 AND seq >= $2 AND seq < $3 ORDER BY seq`,
				[key, from, Math.min(tail, from + batchSize)],
			);
			yield {key, ttlMs, first, last: false, items: {kind: 'list', values: rows.map((row) => row.value as Buffer)}};
			first = false;
		}
		if (!first) yield {key, ttlMs, first: false, last: true, items: {kind: 'list', values: []}};
	}

	private async *readMembers(kind: 'hash' | 'set' | 'zset', batchSize: number): AsyncGenerator<KVTransferChunk> {
		const spec = MEMBER_TABLES[kind];
		let after: Buffer = Buffer.alloc(0);
		while (true) {
			const {rows} = await this.pool.query(
				`SELECT m.key, ${TTL_MS.replaceAll('expires_at', 'k.expires_at')} AS ttl_ms,
					(k.expires_at IS NOT NULL AND k.expires_at <= now()) AS expired
				FROM (SELECT DISTINCT key FROM ${spec.table} WHERE key > $1 ORDER BY key LIMIT $2) m
				LEFT JOIN ${T.key} k ON k.key = m.key ORDER BY m.key`,
				[after, batchSize],
			);
			for (const row of rows) {
				if (row.expired === true) continue;
				const key = row.key as Buffer;
				const ttlMs = row.ttl_ms === null ? null : Number(row.ttl_ms);
				let first = true;
				let cursor: Buffer | null = null;
				while (true) {
					const page = await this.pool.query(
						`SELECT ${spec.columns} FROM ${spec.table} WHERE key = $1 AND ($2::bytea IS NULL OR ${spec.order} > $2)
						ORDER BY ${spec.order} LIMIT $3`,
						[key, cursor, batchSize],
					);
					if (page.rows.length === 0) break;
					const items = postgresItems(kind, page.rows);
					yield {key, ttlMs, first, last: false, items};
					first = false;
					const lastRow = page.rows[page.rows.length - 1];
					cursor = (kind === 'hash' ? lastRow.field : lastRow.member) as Buffer;
					if (page.rows.length < batchSize) break;
				}
				if (!first) yield {key, ttlMs, first: false, last: true, items: emptyItems(kind)};
			}
			if (rows.length < batchSize) break;
			after = rows[rows.length - 1].key as Buffer;
		}
	}

	async write(chunks: Array<KVTransferChunk>): Promise<void> {
		const keyRows: Array<{key: Buffer; kind: string; value: Buffer | null; ttlMs: number | null; tail: number | null}> =
			[];
		const hashRows: Array<[Buffer, Buffer, Buffer]> = [];
		const setRows: Array<[Buffer, Buffer]> = [];
		const zsetRows: Array<[Buffer, Buffer, number]> = [];
		const listRows: Array<[Buffer, number, Buffer]> = [];
		for (const chunk of chunks) {
			const {key, items} = chunk;
			const id = key.toString('latin1');
			switch (items.kind) {
				case 'string':
					keyRows.push({key, kind: 's', value: items.values[0], ttlMs: chunk.ttlMs, tail: null});
					break;
				case 'list': {
					let offset = chunk.first ? 0 : (this.listOffsets.get(id) ?? 0);
					for (const value of items.values) listRows.push([key, offset++, value]);
					this.listOffsets.set(id, offset);
					if (chunk.last) {
						keyRows.push({key, kind: 'l', value: null, ttlMs: chunk.ttlMs, tail: offset});
						this.listOffsets.delete(id);
					}
					break;
				}
				case 'hash':
					for (const [field, value] of items.values) hashRows.push([key, field, value]);
					break;
				case 'set':
					for (const member of items.values) setRows.push([key, member]);
					break;
				case 'zset':
					for (const [member, score] of items.values) zsetRows.push([key, member, score]);
					break;
			}
			if (chunk.last && chunk.ttlMs !== null && items.kind !== 'string' && items.kind !== 'list') {
				keyRows.push({key, kind: MEMBER_TABLES[items.kind].kind, value: null, ttlMs: chunk.ttlMs, tail: null});
			}
		}
		const client = await this.pool.connect();
		try {
			await client.query('BEGIN');
			if (hashRows.length > 0) {
				await client.query(
					`INSERT INTO ${T.hash} (key, field, value) SELECT * FROM unnest($1::bytea[], $2::bytea[], $3::bytea[])
					ON CONFLICT (key, field) DO UPDATE SET value = EXCLUDED.value`,
					[hashRows.map((row) => row[0]), hashRows.map((row) => row[1]), hashRows.map((row) => row[2])],
				);
			}
			if (setRows.length > 0) {
				await client.query(
					`INSERT INTO ${T.set} (key, member) SELECT * FROM unnest($1::bytea[], $2::bytea[]) ON CONFLICT DO NOTHING`,
					[setRows.map((row) => row[0]), setRows.map((row) => row[1])],
				);
			}
			if (zsetRows.length > 0) {
				await client.query(
					`INSERT INTO ${T.zset} (key, member, score) SELECT * FROM unnest($1::bytea[], $2::bytea[], $3::float8[])
					ON CONFLICT (key, member) DO UPDATE SET score = EXCLUDED.score`,
					[zsetRows.map((row) => row[0]), zsetRows.map((row) => row[1]), zsetRows.map((row) => row[2])],
				);
			}
			if (listRows.length > 0) {
				await client.query(
					`INSERT INTO ${T.list} (key, seq, value) SELECT * FROM unnest($1::bytea[], $2::bigint[], $3::bytea[])
					ON CONFLICT (key, seq) DO UPDATE SET value = EXCLUDED.value`,
					[listRows.map((row) => row[0]), listRows.map((row) => row[1]), listRows.map((row) => row[2])],
				);
			}
			if (keyRows.length > 0) {
				await client.query(
					`INSERT INTO ${T.key} (key, kind, value, expires_at, list_head, list_tail)
					SELECT k, d::"char", v, CASE WHEN t IS NULL THEN NULL ELSE now() + t * interval '1 millisecond' END,
						CASE WHEN d = 'l' THEN 0 END, n
					FROM unnest($1::bytea[], $2::text[], $3::bytea[], $4::bigint[], $5::bigint[]) AS u(k, d, v, t, n)
					ON CONFLICT (key) DO UPDATE SET kind = EXCLUDED.kind, value = EXCLUDED.value, expires_at = EXCLUDED.expires_at,
						list_head = EXCLUDED.list_head, list_tail = EXCLUDED.list_tail`,
					[
						keyRows.map((row) => row.key),
						keyRows.map((row) => row.kind),
						keyRows.map((row) => row.value),
						keyRows.map((row) => row.ttlMs),
						keyRows.map((row) => row.tail),
					],
				);
			}
			await client.query('COMMIT');
		} catch (error) {
			await client.query('ROLLBACK').catch(() => undefined);
			throw error;
		} finally {
			client.release();
		}
	}
}
