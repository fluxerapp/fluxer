// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {KVClient} from '@pkgs/kv_client/src/KVClient';
import {KVClientError} from '@pkgs/kv_client/src/KVClientError';
import {KVTransferError, PostgresTransferSide, transferKV, ValkeyTransferSide} from '@pkgs/kv_client/src/KVTransfer';
import {PostgresKVProvider, postgresKVClientConfig} from '@pkgs/kv_client/src/PostgresKVProvider';
import {POSTGRES_KV_FUNCTIONS, POSTGRES_KV_TABLES} from '@pkgs/kv_client/src/PostgresKVSchema';
import Redis from 'ioredis';
import pg from 'pg';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const VALKEY_URL = process.env.FLUXER_TEST_KV_VALKEY_URL ?? '';
const POSTGRES_URL = process.env.FLUXER_TEST_KV_POSTGRES_URL ?? '';
const SEED = Number(process.env.FLUXER_TEST_KV_SEED ?? 20261108);
const STEPS = Number(process.env.FLUXER_TEST_KV_STEPS ?? 2500);

const KEYS = ['k:a', 'k:b', 'k:c', 'k:d', 'k:\u0000nul', 'k:ü日本', 'k:🎉', 'k:%_\\'];
const VALUE_PIECES = ['', 'x', '1', '42', '-7', 'ü', '\u0000', '日本', '{"a":1}', 'abc', '🎉'];
const SCORES = [-3, -1.5, 0, 0, 1, 2, 2, 2.5, 7, 100];
const BOUNDS: Array<string | number> = ['-inf', '+inf', -2, 0, 1, 2, 3, 50];

type Outcome = {ok: true; value: unknown} | {ok: false};
type ReadKind = 'string' | 'hash' | 'set' | 'zset' | 'list';

interface Operation {
	name: string;
	reads?: ReadKind;
	keys: Array<string>;
	run(provider: IKVProvider): Promise<unknown>;
	compare?: 'ttl' | 'sorted' | 'exact';
}

function mulberry32(seed: number): () => number {
	let state = seed >>> 0;
	return () => {
		state = (state + 0x6d2b79f5) >>> 0;
		let t = state;
		t = Math.imul(t ^ (t >>> 15), t | 1);
		t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
		return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
	};
}

async function outcome(fn: () => Promise<unknown>): Promise<Outcome> {
	try {
		return {ok: true, value: await fn()};
	} catch {
		return {ok: false};
	}
}

function sortedDeep(value: unknown): unknown {
	if (Array.isArray(value)) return [...value].map(sortedDeep).sort();
	return value;
}

function expectSameOutcome(step: string, valkey: Outcome, postgres: Outcome, mode: Operation['compare']): void {
	expect(postgres.ok, `${step}: success`).toBe(valkey.ok);
	if (!valkey.ok || !postgres.ok) return;
	if (mode === 'ttl' && typeof valkey.value === 'number' && typeof postgres.value === 'number') {
		if (valkey.value >= 0 && postgres.value >= 0) {
			expect(Math.abs(valkey.value - postgres.value), `${step}: ttl`).toBeLessThanOrEqual(1);
			return;
		}
	}
	if (mode === 'sorted') {
		expect(sortedDeep(postgres.value), step).toEqual(sortedDeep(valkey.value));
		return;
	}
	expect(postgres.value, step).toEqual(valkey.value);
}

function parseStored(value: string | null): unknown {
	if (value === null) return null;
	try {
		return JSON.parse(value);
	} catch {
		return value;
	}
}

function readEmailOwnerScripts(): Record<string, string> {
	const source = readFileSync(
		new URL('../../../../src/api/user/repositories/account/crud/UserEmailOwnershipRepository.ts', import.meta.url),
		'utf8',
	);
	const script = (name: string): string => {
		const match = source.match(new RegExp(`const ${name} = \`([\\s\\S]*?)\`;`));
		if (!match) throw new Error(`missing ${name}`);
		return match[1];
	};
	return {
		emailOwnerClaim: script('EMAIL_OWNER_BEGIN_CLAIM_SCRIPT'),
		emailOwnerFinalize: script('EMAIL_OWNER_FINALIZE_CLAIM_SCRIPT'),
		emailOwnerRelease: script('EMAIL_OWNER_RELEASE_SCRIPT'),
	};
}

function sleep(ms: number): Promise<void> {
	return new Promise((resolve) => setTimeout(resolve, ms));
}

async function waitForAsync(condition: () => Promise<boolean>, timeoutMs: number): Promise<void> {
	const deadline = Date.now() + timeoutMs;
	while (!(await condition())) {
		if (Date.now() > deadline) throw new Error('condition not met in time');
		await sleep(50);
	}
}

async function waitFor(condition: () => boolean, timeoutMs: number): Promise<void> {
	const deadline = Date.now() + timeoutMs;
	while (!condition()) {
		if (Date.now() > deadline) throw new Error('condition not met in time');
		await sleep(20);
	}
}

describe.skipIf(VALKEY_URL === '' || POSTGRES_URL === '')('PostgresKVProvider parity with Valkey', () => {
	let redis: Redis;
	let valkey: KVClient;
	let postgres: PostgresKVProvider;
	let admin: pg.Pool;
	const extraProviders: Array<PostgresKVProvider> = [];

	function provider(options: {maxBytes?: number; activityDebounceMs?: number} = {}): PostgresKVProvider {
		const created = new PostgresKVProvider({
			connection: {url: POSTGRES_URL},
			maxConnections: 8,
			activityDebounceMs: 0,
			...options,
		});
		extraProviders.push(created);
		return created;
	}

	async function listenerCount(): Promise<number> {
		const {rows} = await admin.query(
			`SELECT count(*)::int AS n FROM pg_stat_activity WHERE query = 'LISTEN fluxer_kvs' AND datname = current_database()`,
		);
		return rows[0].n;
	}

	async function typeOf(key: string): Promise<string> {
		return await redis.type(key);
	}

	beforeAll(async () => {
		redis = new Redis(VALKEY_URL);
		valkey = new KVClient(VALKEY_URL);
		admin = new pg.Pool(postgresKVClientConfig({url: POSTGRES_URL}, {maxConnections: 4}));
		await admin.query(`DROP TABLE IF EXISTS ${Object.values(POSTGRES_KV_TABLES).join(', ')}`);
		for (const name of Object.values(POSTGRES_KV_FUNCTIONS)) {
			await admin.query(`DROP FUNCTION IF EXISTS ${name} CASCADE`);
		}
		postgres = new PostgresKVProvider({connection: {url: POSTGRES_URL}, maxConnections: 8, activityDebounceMs: 0});
	});

	afterAll(async () => {
		vi.useRealTimers();
		for (const created of extraProviders) await created.close();
		await postgres?.close();
		valkey?.close();
		redis?.disconnect();
		await admin?.end();
	});

	beforeEach(async () => {
		vi.useRealTimers();
		await redis.flushdb();
		await postgres.exists('warm');
		await admin.query(
			`TRUNCATE ${Object.values(POSTGRES_KV_TABLES)
				.filter((table) => table !== POSTGRES_KV_TABLES.meta)
				.join(', ')}`,
		);
		await admin.query(`DELETE FROM ${POSTGRES_KV_TABLES.meta} WHERE name <> 'schema_version'`);
	});

	it('creates the seven tables, the bucket table unlogged, and runs the pool without synchronous commit', async () => {
		const {rows} = await admin.query(
			`SELECT relname, relpersistence FROM pg_class WHERE relname LIKE 'fluxer_kvs_%' AND relkind = 'r' ORDER BY relname`,
		);
		expect(rows.map((row) => row.relname)).toEqual(Object.values(POSTGRES_KV_TABLES).sort());
		expect(rows.find((row) => row.relname === POSTGRES_KV_TABLES.bucket)?.relpersistence).toBe('u');
		const pool = (postgres as unknown as {pool: pg.Pool}).pool;
		const setting = await pool.query('SHOW synchronous_commit');
		expect(setting.rows[0].synchronous_commit).toBe('off');
		const timeout = await pool.query('SHOW statement_timeout');
		expect(timeout.rows[0].statement_timeout).toBe('5s');
	});

	it('matches Valkey over a seeded random command sequence', async () => {
		const random = mulberry32(SEED);
		const pick = <T>(items: ReadonlyArray<T>): T => items[Math.floor(random() * items.length)];
		const int = (min: number, max: number): number => min + Math.floor(random() * (max - min + 1));
		const value = (): string => Array.from({length: int(0, 3)}, () => pick(VALUE_PIECES)).join('');
		const members = (): Array<string> =>
			Array.from({length: int(1, 4)}, () => pick(['m1', 'm2', 'm3', 'ü', '\u0000', 'z']));
		const generators: Array<() => Operation> = [
			() => {
				const key = pick(KEYS);
				return {name: `get ${key}`, reads: 'string', keys: [key], run: (p) => p.get(key)};
			},
			() => {
				const key = pick(KEYS);
				const v = value();
				const args = pick<Array<string | number>>([[], ['EX', int(60, 600)], ['NX'], ['EX', int(60, 600), 'NX']]);
				return {name: `set ${key} ${args.join(' ')}`, keys: [key], run: (p) => p.set(key, v, ...args)};
			},
			() => {
				const key = pick(KEYS);
				const v = value();
				const ttl = int(60, 600);
				return {name: `setex ${key}`, keys: [key], run: (p) => p.setex(key, ttl, v)};
			},
			() => {
				const key = pick(KEYS);
				const v = value();
				const ttl = random() < 0.5 ? undefined : int(60, 600);
				return {name: `setnx ${key}`, keys: [key], run: (p) => p.setnx(key, v, ttl)};
			},
			() => {
				const keys = [pick(KEYS), pick(KEYS), pick(KEYS)];
				return {name: `mget ${keys.join(',')}`, keys, run: (p) => p.mget(...keys)};
			},
			() => {
				const keys = Array.from({length: int(1, 3)}, () => pick(KEYS));
				return {name: `del ${keys.join(',')}`, keys, run: (p) => p.del(...keys)};
			},
			() => {
				const key = pick(KEYS);
				return {name: `exists ${key}`, keys: [key], run: (p) => p.exists(key)};
			},
			() => {
				const key = pick(KEYS);
				const ttl = pick([int(60, 600), int(60, 600), 0, -1]);
				return {name: `expire ${key} ${ttl}`, keys: [key], run: (p) => p.expire(key, ttl)};
			},
			() => {
				const key = pick(KEYS);
				return {name: `ttl ${key}`, keys: [key], run: (p) => p.ttl(key), compare: 'ttl'};
			},
			() => {
				const key = pick(KEYS);
				return {name: `incr ${key}`, keys: [key], run: (p) => p.incr(key)};
			},
			() => {
				const key = pick(KEYS);
				return {name: `getdel ${key}`, reads: 'string', keys: [key], run: (p) => p.getdel(key)};
			},
			() => {
				const key = pick(KEYS);
				const m = members();
				return {name: `sadd ${key}`, keys: [key], run: (p) => p.sadd(key, ...m)};
			},
			() => {
				const key = pick(KEYS);
				const m = members();
				return {name: `srem ${key}`, keys: [key], run: (p) => p.srem(key, ...m)};
			},
			() => {
				const key = pick(KEYS);
				return {name: `smembers ${key}`, reads: 'set', keys: [key], run: (p) => p.smembers(key), compare: 'sorted'};
			},
			() => {
				const key = pick(KEYS);
				const m = pick(['m1', 'm2', 'ü', '\u0000']);
				return {name: `sismember ${key}`, reads: 'set', keys: [key], run: (p) => p.sismember(key, m)};
			},
			() => {
				const key = pick(KEYS);
				return {name: `scard ${key}`, reads: 'set', keys: [key], run: (p) => p.scard(key)};
			},
			() => {
				const key = pick(KEYS);
				const pairs = members().flatMap((member) => [pick(SCORES), member]);
				return {name: `zadd ${key}`, keys: [key], run: (p) => p.zadd(key, ...pairs)};
			},
			() => {
				const key = pick(KEYS);
				const m = members();
				return {name: `zrem ${key}`, keys: [key], run: (p) => p.zrem(key, ...m)};
			},
			() => {
				const key = pick(KEYS);
				return {name: `zcard ${key}`, reads: 'zset', keys: [key], run: (p) => p.zcard(key)};
			},
			() => {
				const key = pick(KEYS);
				const min = pick(BOUNDS);
				const max = pick(BOUNDS);
				const limit = random() < 0.4 ? ['LIMIT', int(0, 3), int(0, 3)] : [];
				return {
					name: `zrangebyscore ${key} ${min} ${max} ${limit.join(' ')}`,
					reads: 'zset',
					keys: [key],
					run: (p) => p.zrangebyscore(key, min, max, ...limit),
				};
			},
			() => {
				const key = pick(KEYS);
				const values = Array.from({length: int(0, 4)}, value);
				return {
					name: `rpush ${key} ${values.length}`,
					reads: values.length === 0 ? 'list' : undefined,
					keys: [key],
					run: (p) => p.rpush(key, ...values),
				};
			},
			() => {
				const key = pick(KEYS);
				const count = random() < 0.3 ? undefined : int(0, 4);
				return {name: `lpop ${key} ${count}`, keys: [key], run: (p) => p.lpop(key, count)};
			},
			() => {
				const key = pick(KEYS);
				return {name: `llen ${key}`, reads: 'list', keys: [key], run: (p) => p.llen(key)};
			},
			() => {
				const key = pick(KEYS);
				const start = int(-5, 4);
				const stop = int(-5, 6);
				return {
					name: `lrange ${key} ${start} ${stop}`,
					reads: 'list',
					keys: [key],
					run: (p) => p.lrange(key, start, stop),
				};
			},
			() => {
				const key = pick(KEYS);
				const start = int(-4, 3);
				const stop = int(-4, 5);
				return {name: `ltrim ${key} ${start} ${stop}`, keys: [key], run: (p) => p.ltrim(key, start, stop)};
			},
			() => {
				const key = pick(KEYS);
				const field = pick(['f1', 'f2', 'ü', '\u0000', '']);
				const v = value();
				return {name: `hset ${key}`, keys: [key], run: (p) => p.hset(key, field, v)};
			},
			() => {
				const key = pick(KEYS);
				const fields = [pick(['f1', 'f2', 'ü', '\u0000', '']), pick(['f1', 'zz'])];
				return {name: `hdel ${key}`, keys: [key], run: (p) => p.hdel(key, ...fields)};
			},
			() => {
				const key = pick(KEYS);
				return {name: `hgetall ${key}`, reads: 'hash', keys: [key], run: (p) => p.hgetall(key)};
			},
			() => {
				const key = pick(KEYS);
				const token = pick(['t1', 't2']);
				const ttl = int(60, 600);
				return {name: `acquireLock ${key}`, keys: [key], run: (p) => p.acquireLock(key, token, ttl)};
			},
			() => {
				const key = pick(KEYS);
				const token = pick(['t1', 't2']);
				return {name: `releaseLock ${key}`, reads: 'string', keys: [key], run: (p) => p.releaseLock(key, token)};
			},
			() => {
				const key = pick(KEYS);
				const token = pick(['t1', 't2']);
				const ttl = int(60, 600);
				return {name: `extendLock ${key}`, reads: 'string', keys: [key], run: (p) => p.extendLock(key, token, ttl)};
			},
			() => ({name: 'scan k:*', keys: [], run: (p) => p.scan('k:*', 1000), compare: 'sorted'}),
			() => {
				const keys = [pick(KEYS), pick(KEYS), pick(KEYS), pick(KEYS)];
				const v = value();
				const m = members();
				const ttl = int(60, 600);
				return {
					name: `pipeline ${keys.join(',')}`,
					keys: keys.slice(0, 3),
					run: async (p) => {
						const results = await p
							.pipeline()
							.setex(keys[0], ttl, v)
							.setex(keys[1], ttl, v)
							.sadd(keys[2], ...m)
							.expire(keys[3], ttl)
							.del(keys[1])
							.exec();
						return results.map(([error, result]) => (error ? 'error' : result));
					},
				};
			},
			() => {
				const key = pick(KEYS);
				return {
					name: `multi hgetall+del ${key}`,
					reads: 'hash',
					keys: [key],
					run: async (p) => {
						const results = await p.multi().hgetall(key).del(key).exec();
						return results.map(([error, result]) => (error ? 'error' : result));
					},
				};
			},
		];
		let compared = 0;
		let skipped = 0;
		for (let step = 0; step < STEPS; step++) {
			const operation = pick(generators)();
			if (operation.reads) {
				const type = await typeOf(operation.keys[0]);
				if (type !== 'none' && type !== operation.reads) {
					skipped++;
					continue;
				}
			}
			const fromValkey = await outcome(() => operation.run(valkey));
			const fromPostgres = await outcome(() => operation.run(postgres));
			expectSameOutcome(`step ${step} ${operation.name}`, fromValkey, fromPostgres, operation.compare);
			compared++;
		}
		for (const key of KEYS) {
			const type = await typeOf(key);
			expectSameOutcome(
				`final exists ${key}`,
				await outcome(() => valkey.exists(key)),
				await outcome(() => postgres.exists(key)),
				'exact',
			);
			expectSameOutcome(
				`final ttl ${key}`,
				await outcome(() => valkey.ttl(key)),
				await outcome(() => postgres.ttl(key)),
				'ttl',
			);
			const read: Record<string, (p: IKVProvider) => Promise<unknown>> = {
				string: (p) => p.get(key),
				hash: (p) => p.hgetall(key),
				set: (p) => p.smembers(key),
				zset: (p) => p.zrangebyscore(key, '-inf', '+inf'),
				list: (p) => p.lrange(key, 0, -1),
			};
			if (read[type]) {
				expectSameOutcome(
					`final ${type} ${key}`,
					await outcome(() => read[type](valkey)),
					await outcome(() => read[type](postgres)),
					'sorted',
				);
			}
		}
		console.info(`parity sequence seed=${SEED}: ${compared} compared, ${skipped} skipped wrong-type reads`);
		expect(compared).toBeGreaterThan(STEPS * 0.7);
	}, 300_000);

	it('expires keys and revives expired collections like Valkey', async () => {
		for (const p of [valkey, postgres] as Array<IKVProvider>) {
			await p.set('exp:s', 'v', 'EX', 1);
			await p.hset('exp:h', 'old', '1');
			await p.expire('exp:h', 1);
			await p.rpush('exp:l', 'a', 'b');
			await p.expire('exp:l', 1);
			await p.sadd('exp:e', 'a');
			await p.expire('exp:e', 1);
		}
		await sleep(1300);
		for (const p of [valkey, postgres] as Array<IKVProvider>) {
			expect(await p.get('exp:s')).toBeNull();
			expect(await p.exists('exp:s')).toBe(0);
			expect(await p.ttl('exp:s')).toBe(-2);
			expect(await p.ttl('exp:h')).toBe(-2);
			expect(await p.hset('exp:h', 'new', '2')).toBe(1);
			expect(await p.hgetall('exp:h')).toEqual({new: '2'});
			expect(await p.ttl('exp:h')).toBe(-1);
			expect(await p.llen('exp:l')).toBe(0);
			expect(await p.rpush('exp:l', 'c')).toBe(1);
			expect(await p.lrange('exp:l', 0, -1)).toEqual(['c']);
			expect(await p.scard('exp:e')).toBe(0);
			expect(await p.setnx('exp:e', 'now a string')).toBe(true);
		}
		expect((await postgres.pruneExpired()).deletedKeys).toBe(1);
		await postgres.set('exp:gone', 'v', 'EX', 1);
		await sleep(1100);
		expect((await postgres.pruneExpired()).deletedKeys).toBe(1);
		const {rows} = await admin.query(`SELECT count(*)::int AS n FROM ${POSTGRES_KV_TABLES.key} WHERE key = $1`, [
			Buffer.from('exp:gone'),
		]);
		expect(rows[0].n).toBe(0);
	});

	it('matches the leaky bucket Lua field by field', async () => {
		const random = mulberry32(SEED + 1);
		vi.useFakeTimers({toFake: ['Date']});
		let now = 1_762_600_000_000;
		vi.setSystemTime(now);
		let checks = 0;
		for (let round = 0; round < 6; round++) {
			const key = `ratelimit:parity:${round}`;
			const limit = 1 + Math.floor(random() * 6);
			const windowMs = 500 + Math.floor(random() * 9500);
			for (let step = 0; step < 120; step++) {
				const cost = random() < 0.2 ? 0 : 1;
				const fromValkey = await valkey.checkLeakyBucketLimit(key, limit, windowMs, cost);
				const fromPostgres = await postgres.checkLeakyBucketLimit(key, limit, windowMs, cost);
				expect(fromPostgres, `round ${round} step ${step}`).toEqual(fromValkey);
				checks++;
				now += Math.floor(random() * random() * windowMs);
				vi.setSystemTime(now);
			}
		}
		vi.useRealTimers();
		for (const p of [valkey, postgres] as Array<IKVProvider>) {
			await expect(p.checkLeakyBucketLimit('ratelimit:bad', 0, 1000, 1)).rejects.toBeInstanceOf(KVClientError);
			await expect(p.checkLeakyBucketLimit('ratelimit:bad', 1, 1000, 2)).rejects.toBeInstanceOf(KVClientError);
		}
		expect(await postgres.exists('ratelimit:parity:0')).toBeGreaterThanOrEqual(0);
		expect(checks).toBe(720);
	});

	it('runs the three email-owner commands like the Lua scripts', async () => {
		const scripts = readEmailOwnerScripts();
		const random = mulberry32(SEED + 2);
		const key = 'email-owner:v1:parity';
		let now = 1_762_600_000_000;
		await redis.set(key, 'not json');
		await postgres.set(key, 'not json');
		for (let step = 0; step < 300; step++) {
			const command = ['emailOwnerClaim', 'emailOwnerFinalize', 'emailOwnerRelease'][Math.floor(random() * 3)];
			const user = random() < 0.5 ? '1001' : '1002';
			const token = random() < 0.5 ? 'tok-a' : 'tok-b';
			now += Math.floor(random() * 400_000);
			const argsByCommand: Record<string, Array<string | number>> = {
				emailOwnerClaim: [key, user, token, now, now + 600_000],
				emailOwnerFinalize: [key, user, token, now],
				emailOwnerRelease: [key, user, random() < 0.3 ? '' : token],
			};
			const args = argsByCommand[command];
			const fromValkey = await valkey.evalScript(command, scripts[command], 1, ...args);
			const fromPostgres = await postgres.evalScript(command, '', 1, ...args);
			expect(JSON.parse(String(fromPostgres)), `step ${step} ${command}`).toEqual(JSON.parse(String(fromValkey)));
			const storedValkey = await redis.get(key);
			const storedPostgres = await postgres.get(key);
			expect(parseStored(storedPostgres), `step ${step} state`).toEqual(parseStored(storedValkey));
		}
	});

	it('matches the bulk deletion and purge queue scripts', async () => {
		for (const p of [valkey, postgres] as Array<IKVProvider>) {
			await p.scheduleBulkDeletion('bulk:q', 'bulk:s:1', 100, 'member-1');
			await p.scheduleBulkDeletion('bulk:q', 'bulk:s:2', 200, 'member-2');
			expect(await p.claimBulkDeletion('bulk:q', 'member-1', 150, 999)).toBe(true);
			expect(await p.claimBulkDeletion('bulk:q', 'member-2', 150, 999)).toBe(false);
			expect(await p.claimBulkDeletion('bulk:q', 'missing', 150, 999)).toBe(false);
			expect(await p.zrangebyscore('bulk:q', '-inf', '+inf')).toEqual(['member-2', 'member-1']);
			expect(await p.removeBulkDeletion('bulk:q', 'bulk:s:1', 'member-1')).toBe(true);
			expect(await p.removeBulkDeletion('bulk:q', 'bulk:s:2')).toBe(true);
			expect(await p.removeBulkDeletion('bulk:q', 'bulk:s:2')).toBe(false);
			expect(await p.zcard('bulk:q')).toBe(0);
			expect(await p.get('bulk:s:1')).toBeNull();
		}
		vi.useFakeTimers({toFake: ['Date']});
		let now = 1_762_600_000_000;
		vi.setSystemTime(now);
		const members = Array.from({length: 40}, (_, index) => `purge-${index}`);
		await valkey.sadd('purge:q', ...members);
		await postgres.sadd('purge:q', ...members);
		const popped: Record<string, Array<string>> = {valkey: [], postgres: []};
		for (let round = 0; round < 12; round++) {
			const a = await valkey.dequeuePurgeBatch('purge:q', 'purge:b', 7, 10, 3, 1000);
			const b = await postgres.dequeuePurgeBatch('purge:q', 'purge:b', 7, 10, 3, 1000);
			expect(b.tokensConsumed, `round ${round}`).toBe(a.tokensConsumed);
			expect(b.entries.length).toBe(a.entries.length);
			popped.valkey.push(...a.entries);
			popped.postgres.push(...b.entries);
			const bucketValkey = await redis.get('purge:b');
			const bucketPostgres = await postgres.get('purge:b');
			expect(bucketPostgres === null ? null : JSON.parse(bucketPostgres)).toEqual(
				bucketValkey === null ? null : JSON.parse(bucketValkey),
			);
			now += 1500;
			vi.setSystemTime(now);
		}
		vi.useRealTimers();
		expect(new Set(popped.postgres).size).toBe(popped.postgres.length);
		expect(popped.postgres.length).toBe(popped.valkey.length);
		await redis.set('purge:bad', '{"tokens":-1,"lastRefill":1}');
		await postgres.set('purge:bad', '{"tokens":-1,"lastRefill":1}');
		await expect(valkey.dequeuePurgeBatch('purge:q2', 'purge:bad', 1, 1, 1, 1)).resolves.toEqual({
			entries: [],
			tokensConsumed: 0,
		});
		await expect(postgres.dequeuePurgeBatch('purge:q2', 'purge:bad', 1, 1, 1, 1)).resolves.toEqual({
			entries: [],
			tokensConsumed: 0,
		});
		await valkey.sadd('purge:q2', 'x');
		await postgres.sadd('purge:q2', 'x');
		await expect(valkey.dequeuePurgeBatch('purge:q2', 'purge:bad', 1, 1, 1, 1)).rejects.toBeInstanceOf(KVClientError);
		await expect(postgres.dequeuePurgeBatch('purge:q2', 'purge:bad', 1, 1, 1, 1)).rejects.toBeInstanceOf(KVClientError);
	});

	it('loses no field when the activity drain races 50 concurrent HSETs', async () => {
		const key = 'user_activity:pending';
		const writer = provider();
		const drained = new Map<string, number>();
		const writes = Array.from({length: 50}, (_, index) =>
			writer.hset(key, `user-${index}`, JSON.stringify({ts: index})),
		);
		const drains: Array<Promise<void>> = [];
		for (let round = 0; round < 10; round++) {
			drains.push(
				(async () => {
					await sleep(round * 3);
					const [[readError, hash], [deleteError, deleted]] = await postgres.multi().hgetall(key).del(key).exec();
					expect(readError).toBeNull();
					expect(deleteError).toBeNull();
					const fields = Object.keys(hash as Record<string, string>);
					expect(deleted).toBe(fields.length > 0 ? 1 : 0);
					for (const field of fields) drained.set(field, (drained.get(field) ?? 0) + 1);
				})(),
			);
		}
		await Promise.all([...writes, ...drains]);
		for (const field of Object.keys(await postgres.hgetall(key))) drained.set(field, (drained.get(field) ?? 0) + 1);
		expect(drained.size).toBe(50);
		expect([...drained.values()].every((count) => count === 1)).toBe(true);
	});

	it('returns full batches when two providers pop the same list concurrently', async () => {
		const values = Array.from({length: 1000}, (_, index) => `item-${index}`);
		await postgres.rpush('spool', ...values);
		const other = provider();
		const batches: Array<Array<string>> = [];
		await Promise.all(
			[postgres, other].map(async (p) => {
				while (true) {
					const batch = await p.lpop('spool', 100);
					if (batch.length === 0) return;
					batches.push(batch);
				}
			}),
		);
		expect(batches.every((batch) => batch.length === 100)).toBe(true);
		const all = batches.flat();
		expect(new Set(all).size).toBe(1000);
		expect([...all].sort()).toEqual([...values].sort());
		expect(await postgres.llen('spool')).toBe(0);
	});

	it('keeps zrem and claim consistent when they race', async () => {
		const other = provider();
		const members = Array.from({length: 200}, (_, index) => `job-${index}`);
		for (const member of members) await postgres.scheduleBulkDeletion('race:q', `race:s:${member}`, 10, member);
		const results = await Promise.all(
			members.flatMap((member) => [
				other.claimBulkDeletion('race:q', member, 100, 500),
				postgres.zrem('race:q', member),
			]),
		);
		expect(results.every((result) => typeof result === 'boolean' || result === 1)).toBe(true);
		expect(await postgres.zcard('race:q')).toBe(0);
	});

	it('refuses growing writes over the size cap and keeps reads, deletes and rate limits working', async () => {
		await postgres.set('cap:existing', 'v');
		const capped = provider({maxBytes: 1});
		const result = await capped.pruneExpired();
		expect(result.overCap).toBe(true);
		expect(result.sizeBytes).toBeGreaterThan(1);
		await expect(capped.setex('cap:new', 60, 'v')).rejects.toThrow(/OOM/);
		await expect(capped.rpush('cap:list', 'v')).rejects.toBeInstanceOf(KVClientError);
		await expect(capped.pipeline().setex('cap:p', 60, 'v').exec()).rejects.toBeInstanceOf(KVClientError);
		const fresh = provider({maxBytes: 1});
		await expect(fresh.hset('cap:h', 'f', 'v')).rejects.toThrow(/OOM/);
		expect(await capped.get('cap:existing')).toBe('v');
		expect(await capped.del('cap:existing')).toBe(1);
		expect((await capped.checkLeakyBucketLimit('ratelimit:cap', 5, 1000, 1)).allowed).toBe(true);
		const roomy = provider({maxBytes: 1_000_000_000});
		expect((await roomy.pruneExpired()).overCap).toBe(false);
		const reopened = provider({maxBytes: 1});
		await expect(reopened.setex('cap:after', 60, 'v')).resolves.toBeUndefined();
	});

	it('lifts the size cap once expired or deleted keys free their space', async () => {
		const keys = Array.from({length: 4000}, (_, i) => `cap:bulk:${i}`);
		for (let i = 0; i < keys.length; i += 500) {
			const pipeline = postgres.pipeline();
			for (const key of keys.slice(i, i + 500)) pipeline.setex(key, 3600, 'x'.repeat(1000));
			await pipeline.exec();
		}
		const capped = provider({maxBytes: 2_000_000});
		expect((await capped.pruneExpired()).overCap).toBe(true);
		for (let i = 0; i < keys.length; i += 500) await postgres.del(...keys.slice(i, i + 500));
		const result = await capped.pruneExpired();
		expect(result.sizeBytes).toBeGreaterThan(2_000_000);
		expect(result.overCap).toBe(false);
		await expect(capped.setex('cap:after-burst', 60, 'v')).resolves.toBeUndefined();
	});

	it('delivers pub/sub between two providers and refreshes subscribers after a reconnect', async () => {
		const subscriberProvider = provider();
		const subscription = subscriberProvider.duplicate();
		const received: Array<[string, string]> = [];
		subscription.on('message', (channel, message) => received.push([channel, message]));
		await subscription.connect();
		await subscription.subscribe('cache:refresh', 'instance:config');
		const second = subscriberProvider.duplicate();
		await second.connect();
		await second.subscribe('other');
		const started = Date.now();
		await postgres.publish('cache:refresh', 'hello ü');
		await postgres.publish('unrelated', 'ignored');
		await waitFor(() => received.length >= 1, 1000);
		expect(Date.now() - started).toBeLessThan(1000);
		expect(received).toEqual([['cache:refresh', 'hello ü']]);
		await expect(postgres.publish('cache:refresh', 'x'.repeat(8000))).rejects.toBeInstanceOf(KVClientError);
		expect(await listenerCount()).toBe(1);
		received.length = 0;
		await admin.query(
			`SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE query = 'LISTEN fluxer_kvs' AND datname = current_database()`,
		);
		await waitFor(() => received.length >= 2, 5000);
		expect([...received].sort()).toEqual([
			['cache:refresh', 'refresh'],
			['instance:config', 'refresh'],
		]);
		received.length = 0;
		await postgres.publish('instance:config', '{"source_id":"x"}');
		await waitFor(() => received.length >= 1, 1000);
		await subscription.quit();
		expect(await listenerCount()).toBe(1);
		await second.disconnect();
		await waitForAsync(async () => (await listenerCount()) === 0, 3000);
	});

	it('matches escaped scan patterns', async () => {
		const keys = ['ratelimit:a*b:1', 'ratelimit:a*b:2', 'ratelimit:aXb:3', 'p%q_1', 'pzq_1', 'p%qx1', 'k?[x]'];
		for (const p of [valkey, postgres] as Array<IKVProvider>) {
			for (const key of keys) await p.set(key, 'v');
			await p.checkLeakyBucketLimit('ratelimit:a*b:bucket', 3, 1000, 1);
		}
		for (const pattern of ['ratelimit:a\\*b*', 'p%q_*', 'p\\%q\\_*', 'k\\?\\[x\\]', 'ratelimit:a?b:*', '*']) {
			const fromValkey = (await valkey.scan(pattern, 1000)).sort();
			const fromPostgres = (await postgres.scan(pattern, 1000)).sort();
			expect(fromPostgres, pattern).toEqual(fromValkey);
		}
		await expect(postgres.scan('k[ab]', 10)).rejects.toBeInstanceOf(KVClientError);
	});

	it('debounces user activity writes and keeps the latest value', async () => {
		const debounced = provider({activityDebounceMs: 300});
		const observer = provider();
		await debounced.setex('user_activity:42', 3600, '1');
		await debounced.setex('user_activity:42', 3600, '2');
		await debounced.setex('user_activity:42', 3600, '3');
		expect(await debounced.get('user_activity:42')).toBe('3');
		expect(await observer.get('user_activity:42')).toBe('1');
		await sleep(450);
		expect(await observer.get('user_activity:42')).toBe('3');
		expect(await observer.ttl('user_activity:42')).toBeGreaterThan(3590);
	});

	it('copies Valkey into Postgres and back, verifies both, and drops only rate limits', async () => {
		const binary = Buffer.from([0x00, 0xff, 0x10, 0x80]);
		await redis.set('mig:string', 'plain ü');
		await redis.set('mig:ttl', 'short', 'EX', 600);
		await redis.set(binary, binary);
		await redis.hset('mig:hash', 'f1', 'v1', 'f\u0000', 'v\u0000');
		await redis.expire('mig:hash', 900);
		await redis.sadd('mig:set', 'a', 'b', 'ü');
		await redis.zadd('mig:zset', 1.5, 'one', -2, 'two', 0, 'zero');
		await redis.rpush('mig:list', ...Array.from({length: 2500}, (_, index) => `entry-${index}`));
		await redis.hset('mig:bighash', ...Array.from({length: 3000}, (_, index) => [`f${index}`, `v${index}`]).flat());
		await redis.set('ratelimit:someone', '{"level":1,"updatedAt":1}', 'PX', 60_000);
		const pool = new pg.Pool(postgresKVClientConfig({url: POSTGRES_URL}, {maxConnections: 4}));
		try {
			const valkeySide = new ValkeyTransferSide(redis, ['ratelimit:']);
			const postgresSide = new PostgresTransferSide(pool);
			await postgresSide.prepare();
			const options = {batchSize: 500, verifyOnly: false, replace: false, ttlToleranceMs: 5000};
			const imported = await transferKV(valkeySide, postgresSide, options);
			expect(imported.verified).toBe(true);
			expect(imported.skipped).toBe(1);
			expect(imported.copied).toBe(8);
			expect(imported.source.find((entry) => entry.kind === 'list')?.keys).toBe(1);
			expect(await postgres.get('mig:string')).toBe('plain ü');
			expect(await postgres.ttl('mig:ttl')).toBeGreaterThan(590);
			expect(await postgres.hgetall('mig:hash')).toEqual(await valkey.hgetall('mig:hash'));
			expect(await postgres.ttl('mig:hash')).toBeGreaterThan(890);
			expect((await postgres.smembers('mig:set')).sort()).toEqual((await valkey.smembers('mig:set')).sort());
			expect(await postgres.zrangebyscore('mig:zset', '-inf', '+inf')).toEqual(['two', 'zero', 'one']);
			expect(await postgres.lrange('mig:list', 0, -1)).toEqual(await valkey.lrange('mig:list', 0, -1));
			expect(await postgres.llen('mig:list')).toBe(2500);
			expect(await postgres.exists('ratelimit:someone')).toBe(0);
			const stored = await pool.query(`SELECT value FROM ${POSTGRES_KV_TABLES.key} WHERE key = $1`, [binary]);
			expect(Buffer.compare(stored.rows[0].value, binary)).toBe(0);
			await expect(transferKV(valkeySide, postgresSide, options)).rejects.toBeInstanceOf(KVTransferError);
			expect((await transferKV(valkeySide, postgresSide, {...options, verifyOnly: true})).verified).toBe(true);
			await postgres.scheduleBulkDeletion('asset_deletion:queue', 'asset_deletion:item:9', 42, 'after-switch');
			await postgres.lpop('mig:list', 10);
			const reverted = await transferKV(postgresSide, valkeySide, {...options, replace: true});
			expect(reverted.verified).toBe(true);
			expect(await valkey.zrangebyscore('asset_deletion:queue', '-inf', '+inf')).toEqual(['after-switch']);
			expect(await valkey.get('asset_deletion:item:9')).toBe('after-switch');
			expect(await valkey.llen('mig:list')).toBe(2490);
			expect(await valkey.ttl('mig:hash')).toBeGreaterThan(880);
			expect(Buffer.compare((await redis.getBuffer(binary)) ?? Buffer.alloc(0), binary)).toBe(0);
			expect(await redis.exists('ratelimit:someone')).toBe(0);
			await redis.xadd('mig:stream', '*', 'field', 'value');
			await expect(transferKV(valkeySide, postgresSide, {...options, replace: true})).rejects.toThrow(/mig:stream/);
		} finally {
			await pool.end();
		}
	}, 120_000);
});
