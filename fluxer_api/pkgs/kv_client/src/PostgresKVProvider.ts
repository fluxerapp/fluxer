// SPDX-License-Identifier: AGPL-3.0-or-later

import {DEFAULT_KV_TIMEOUT_MS} from '@fluxer/constants/src/Timeouts';
import type {
	IKVPipeline,
	IKVProvider,
	IKVSubscription,
	KVPurgeBatchResult,
	KVRateLimitResult,
} from '@pkgs/kv_client/src/IKVProvider';
import type {IKVLogger} from '@pkgs/kv_client/src/KVClientConfig';
import {KVClientError, KVClientErrorCode} from '@pkgs/kv_client/src/KVClientError';
import {
	createZSetMembersFromScorePairs,
	normalizeScoreBound,
	parseRangeByScoreArguments,
	parseSetArguments,
} from '@pkgs/kv_client/src/KVCommandArguments';
import {
	EMAIL_OWNER_COMMANDS,
	emailOwnerTransition,
	PURGE_BUCKET_TTL_SECONDS,
	planPurgeBatch,
	settlePurgeBatch,
} from '@pkgs/kv_client/src/KVScriptTransitions';
import {PostgresKVPipeline, type PostgresKVPipelineCommands} from '@pkgs/kv_client/src/PostgresKVPipeline';
import {
	ensurePostgresKVStoreSchema,
	POSTGRES_KV_ADVISORY_LOCK_NAMESPACE,
	POSTGRES_KV_FUNCTIONS,
	POSTGRES_KV_META_KEYS,
	POSTGRES_KV_NOTIFY_CHANNEL,
	POSTGRES_KV_NOTIFY_MAX_PAYLOAD_BYTES,
	POSTGRES_KV_SCHEMA_VERSION,
	POSTGRES_KV_TABLES,
	postgresKVLiveFilter,
} from '@pkgs/kv_client/src/PostgresKVSchema';
import {PostgresKVListener, PostgresKVSubscription} from '@pkgs/kv_client/src/PostgresKVSubscription';
import pg from 'pg';

export interface PostgresKVConnectionConfig {
	url?: string;
	host?: string;
	port?: number;
	database?: string;
	username?: string;
	password?: string;
	ssl?: boolean;
	sslCa?: string;
}

export interface PostgresKVProviderConfig {
	connection: PostgresKVConnectionConfig;
	maxConnections: number;
	maxBytes?: number | null;
	timeoutMs?: number;
	logger?: IKVLogger;
	activityDebounceMs?: number;
}

export interface PostgresKVPruneResult {
	deletedKeys: number;
	deletedBuckets: number;
	sizeBytes: number;
	overCap: boolean;
}

const POSTGRES_KV_ACTIVITY_KEY_PREFIX = 'user_activity:';
const POSTGRES_KV_DEFAULT_ACTIVITY_DEBOUNCE_MS = 15_000;
const POSTGRES_KV_PRUNE_BATCH_SIZE = 5000;
const POSTGRES_KV_PRUNE_MAX_BATCHES = 20;
const SIZE_GUARD_REFRESH_MS = 5000;
const REFRESH_MESSAGE = 'refresh';

type Queryable = Pick<pg.Pool, 'query'> | Pick<pg.PoolClient, 'query'>;
type Row = Record<string, unknown>;

const T = POSTGRES_KV_TABLES;
const F = POSTGRES_KV_FUNCTIONS;
const LIVE = postgresKVLiveFilter();
const LIVE_K = postgresKVLiveFilter('k');
const NOT_EXPIRED = (alias: string): string =>
	`NOT EXISTS (SELECT 1 FROM ${T.key} x WHERE x.key = ${alias}.key AND x.expires_at <= now())`;

const SQL = {
	get: `SELECT value FROM ${T.key} WHERE key = $1 AND kind = 's' AND ${LIVE}`,
	mget: `SELECT key, value FROM ${T.key} WHERE key = ANY($1::bytea[]) AND kind = 's' AND ${LIVE}`,
	set: `SELECT ${F.set}($1, $2, $3, $4) AS ok`,
	setBatch: `SELECT ${F.set}(k, v, t, false) FROM unnest($1::bytea[], $2::bytea[], $3::bigint[]) AS u(k, v, t)`,
	del: `WITH input AS (SELECT DISTINCT unnest($1::bytea[]) AS key),
k AS (DELETE FROM ${T.key} t USING input i WHERE t.key = i.key RETURNING t.key, t.kind, (t.expires_at IS NULL OR t.expires_at > now()) AS live),
h AS (DELETE FROM ${T.hash} t USING input i WHERE t.key = i.key RETURNING t.key),
s AS (DELETE FROM ${T.set} t USING input i WHERE t.key = i.key RETURNING t.key),
z AS (DELETE FROM ${T.zset} t USING input i WHERE t.key = i.key RETURNING t.key),
l AS (DELETE FROM ${T.list} t USING input i WHERE t.key = i.key RETURNING t.key),
b AS (DELETE FROM ${T.bucket} t USING input i WHERE t.key = i.key RETURNING t.key, t.expires_ms > $2 AS live),
members AS (SELECT key FROM h UNION SELECT key FROM s UNION SELECT key FROM z)
SELECT count(*)::int AS deleted FROM input i WHERE
	EXISTS (SELECT 1 FROM k WHERE k.key = i.key AND k.live AND (k.kind IN ('s', 'l') OR EXISTS (SELECT 1 FROM members m WHERE m.key = i.key)))
	OR (NOT EXISTS (SELECT 1 FROM k WHERE k.key = i.key) AND EXISTS (SELECT 1 FROM members m WHERE m.key = i.key))
	OR EXISTS (SELECT 1 FROM b WHERE b.key = i.key AND b.live)`,
	describe: `SELECT k.kind, k.expires_at IS NULL AS persistent, (k.expires_at IS NOT NULL AND k.expires_at <= now()) AS expired,
	floor(extract(epoch FROM (k.expires_at - now())) * 1000)::bigint AS remaining_ms,
	(EXISTS (SELECT 1 FROM ${T.hash} WHERE key = $1) OR EXISTS (SELECT 1 FROM ${T.set} WHERE key = $1) OR EXISTS (SELECT 1 FROM ${T.zset} WHERE key = $1)) AS has_members,
	(SELECT expires_ms - $2 FROM ${T.bucket} WHERE key = $1 AND expires_ms > $2) AS bucket_remaining_ms
FROM (SELECT 1) one LEFT JOIN ${T.key} k ON k.key = $1`,
	expire: `SELECT ${F.expire}($1, $2, $3) AS changed`,
	incr: `SELECT ${F.incr}($1) AS value`,
	getdel: `DELETE FROM ${T.key} WHERE key = $1 AND kind = 's' RETURNING value, ${LIVE} AS live`,
	sadd: `SELECT ${F.sadd}($1, $2) AS changed`,
	srem: `SELECT ${F.srem}($1, $2) AS changed`,
	smembers: `SELECT member FROM ${T.set} s WHERE s.key = $1 AND ${NOT_EXPIRED('s')} ORDER BY member`,
	sismember: `SELECT 1 FROM ${T.set} s WHERE s.key = $1 AND s.member = $2 AND ${NOT_EXPIRED('s')}`,
	scard: `SELECT count(*)::int AS n FROM ${T.set} s WHERE s.key = $1 AND ${NOT_EXPIRED('s')}`,
	zadd: `SELECT ${F.zadd}($1, $2, $3) AS changed`,
	zrem: `SELECT ${F.zrem}($1, $2) AS changed`,
	zcard: `SELECT count(*)::int AS n FROM ${T.zset} z WHERE z.key = $1 AND ${NOT_EXPIRED('z')}`,
	zrangebyscore: `SELECT member FROM ${T.zset} z WHERE z.key = $1 AND z.score >= $2 AND z.score <= $3 AND ${NOT_EXPIRED('z')}
ORDER BY z.score, z.member OFFSET $4 LIMIT $5`,
	rpush: `SELECT ${F.rpush}($1, $2) AS length`,
	lpop: `SELECT ${F.lpop}($1, $2) AS values`,
	llen: `SELECT list_tail - list_head AS n FROM ${T.key} WHERE key = $1 AND kind = 'l' AND ${LIVE}`,
	lrange: `WITH k AS (SELECT list_head AS h, list_tail - list_head AS n FROM ${T.key} WHERE key = $1 AND kind = 'l' AND ${LIVE}),
r AS (SELECT h, CASE WHEN $2::bigint < 0 THEN greatest(n + $2::bigint, 0) ELSE $2::bigint END AS lo,
	CASE WHEN $3::bigint < 0 THEN n + $3::bigint ELSE least($3::bigint, n - 1) END AS hi FROM k)
SELECT l.value FROM r, ${T.list} l WHERE l.key = $1 AND l.seq >= r.h + r.lo AND l.seq <= r.h + r.hi ORDER BY l.seq`,
	ltrim: `SELECT ${F.ltrim}($1, $2, $3)`,
	hset: `SELECT ${F.hset}($1, $2, $3) AS changed`,
	hdel: `SELECT ${F.hdel}($1, $2) AS changed`,
	hgetall: `SELECT field, value FROM ${T.hash} h WHERE h.key = $1 AND ${NOT_EXPIRED('h')} ORDER BY field`,
	drainHash: `WITH k AS (DELETE FROM ${T.key} WHERE key = $1 RETURNING kind, ${LIVE} AS live),
h AS (DELETE FROM ${T.hash} WHERE key = $1 RETURNING field, value),
s AS (DELETE FROM ${T.set} WHERE key = $1 RETURNING 1),
z AS (DELETE FROM ${T.zset} WHERE key = $1 RETURNING 1),
l AS (DELETE FROM ${T.list} WHERE key = $1 RETURNING 1),
agg AS (SELECT array_agg(field ORDER BY field) AS fields, array_agg(value ORDER BY field) AS vals FROM h)
SELECT (SELECT kind FROM k) AS kind, (SELECT live FROM k) AS live, agg.fields, agg.vals,
	EXISTS (SELECT 1 FROM s UNION ALL SELECT 1 FROM z UNION ALL SELECT 1 FROM l) AS other FROM agg`,
	publish: 'SELECT pg_notify($1, $2)',
	releaseLock: `DELETE FROM ${T.key} WHERE key = $1 AND kind = 's' AND value = $2 AND ${LIVE} RETURNING 1`,
	extendLock: `UPDATE ${T.key} SET expires_at = now() + $3::bigint * interval '1 millisecond'
WHERE key = $1 AND kind = 's' AND value = $2 AND ${LIVE} RETURNING 1`,
	leakyBucket: `SELECT * FROM ${F.leakyBucket}($1, $2, $3, $4, $5)`,
	scheduleBulkDeletion: `SELECT ${F.zadd}($1, ARRAY[$3::float8], ARRAY[$4::bytea]), ${F.set}($2, $4, NULL, false)`,
	claimBulkDeletion: `UPDATE ${T.zset} z SET score = $4 WHERE z.key = $1 AND z.member = $2 AND z.score <= $3 AND ${NOT_EXPIRED('z')} RETURNING 1`,
	removeBulkDeletion: `WITH zm AS (DELETE FROM ${T.zset} WHERE key = $1 AND member = $3 AND length($3) > 0 RETURNING 1),
sec AS (DELETE FROM ${T.key} WHERE key = $2 AND kind = 's' RETURNING value, ${LIVE} AS live),
zv AS (DELETE FROM ${T.zset} WHERE key = $1 AND NOT EXISTS (SELECT 1 FROM zm) AND member = (SELECT value FROM sec WHERE live) RETURNING 1)
SELECT (EXISTS (SELECT 1 FROM zm) OR EXISTS (SELECT 1 FROM sec WHERE live)) AS removed`,
	lockKey: 'SELECT pg_advisory_xact_lock($1, hashtext(encode($2::bytea, $3)))',
	lockedString: `SELECT value FROM ${T.key} k WHERE k.key = $1 AND k.kind = 's' AND ${LIVE_K} FOR UPDATE`,
	deleteKey: `SELECT ${F.purge}($1)`,
	purgeQueue: `DELETE FROM ${T.set} WHERE key = $1 AND member IN (
	SELECT member FROM ${T.set} WHERE key = $1 ORDER BY member LIMIT $2 FOR UPDATE SKIP LOCKED
) RETURNING member`,
	purgeQueueCleanup: `DELETE FROM ${T.key} WHERE key = $1 AND kind = 'e' AND NOT EXISTS (SELECT 1 FROM ${T.set} WHERE key = $1)`,
	scan: `SELECT key FROM (
	SELECT k.key FROM ${T.key} k WHERE k.key >= $1 AND ($2::bytea IS NULL OR k.key < $2) AND k.key LIKE $3 AND ${LIVE_K} AND k.kind IN ('s', 'l')
	UNION SELECT h.key FROM ${T.hash} h WHERE h.key >= $1 AND ($2::bytea IS NULL OR h.key < $2) AND h.key LIKE $3 AND ${NOT_EXPIRED('h')}
	UNION SELECT s.key FROM ${T.set} s WHERE s.key >= $1 AND ($2::bytea IS NULL OR s.key < $2) AND s.key LIKE $3 AND ${NOT_EXPIRED('s')}
	UNION SELECT z.key FROM ${T.zset} z WHERE z.key >= $1 AND ($2::bytea IS NULL OR z.key < $2) AND z.key LIKE $3 AND ${NOT_EXPIRED('z')}
	UNION SELECT b.key FROM ${T.bucket} b WHERE b.key >= $1 AND ($2::bytea IS NULL OR b.key < $2) AND b.key LIKE $3 AND b.expires_ms > $4
) keys ORDER BY key LIMIT $5`,
	sweepKeys: `WITH doomed AS (
	SELECT key FROM ${T.key} WHERE expires_at <= now() ORDER BY expires_at LIMIT $1 FOR UPDATE SKIP LOCKED
), k AS (DELETE FROM ${T.key} t USING doomed d WHERE t.key = d.key RETURNING t.key, t.kind),
h AS (DELETE FROM ${T.hash} t USING k WHERE t.key = k.key AND k.kind = 'h'),
s AS (DELETE FROM ${T.set} t USING k WHERE t.key = k.key AND k.kind = 'e'),
z AS (DELETE FROM ${T.zset} t USING k WHERE t.key = k.key AND k.kind = 'z'),
l AS (DELETE FROM ${T.list} t USING k WHERE t.key = k.key AND k.kind = 'l')
SELECT count(*)::int AS deleted FROM k`,
	sweepBuckets: `DELETE FROM ${T.bucket} WHERE expires_ms <= $1`,
	size: `SELECT coalesce(sum(pg_total_relation_size(c.oid)), 0)::bigint AS bytes FROM pg_class c
WHERE c.relname = ANY($1::text[]) AND c.relkind = 'r'`,
	liveSize: `SELECT coalesce(sum(est), 0)::bigint AS bytes FROM (${Object.values(T)
		.map(
			(table) =>
				`SELECT pg_total_relation_size('${table}'::regclass) * least(1, (SELECT coalesce(sum(pg_column_size(r.*) + 4), 0) FROM ${table} r)::float8 / greatest(pg_relation_size('${table}'::regclass), 1)) AS est`,
		)
		.join(' UNION ALL ')}) live`,
	writeMeta: `INSERT INTO ${T.meta} (name, value) VALUES ($1, $2), ($3, $4) ON CONFLICT (name) DO UPDATE SET value = EXCLUDED.value`,
	readOverCap: `SELECT value FROM ${T.meta} WHERE name = $1`,
} as const;

const SQL_NAMES = new Map<string, string>(Object.entries(SQL).map(([name, text]) => [text, `kvs_v1_${name}`]));

export function postgresKVClientConfig(
	connection: PostgresKVConnectionConfig,
	options: {timeoutMs?: number; maxConnections?: number; synchronousCommitOff?: boolean} = {},
): pg.PoolConfig {
	const {url} = connection;
	const timeoutMs = options.timeoutMs;
	return {
		connectionString: url || undefined,
		host: url ? undefined : (connection.host ?? '127.0.0.1'),
		port: url ? undefined : (connection.port ?? 5432),
		database: url ? undefined : (connection.database ?? 'fluxer'),
		user: url ? undefined : (connection.username ?? 'fluxer'),
		password: url ? undefined : (connection.password ?? 'fluxer'),
		ssl: connection.ssl
			? {rejectUnauthorized: true, ca: connection.sslCa?.replaceAll('\\n', '\n') || undefined}
			: undefined,
		max: options.maxConnections,
		options: options.synchronousCommitOff ? '-c synchronous_commit=off' : undefined,
		connectionTimeoutMillis: timeoutMs,
		statement_timeout: timeoutMs,
		lock_timeout: timeoutMs,
		idle_in_transaction_session_timeout: timeoutMs,
		query_timeout: timeoutMs === undefined ? undefined : timeoutMs + 1000,
		keepAlive: true,
	};
}

function encodeKV(value: string): Buffer {
	return Buffer.from(value, 'utf8');
}

function decode(value: unknown): string {
	if (Buffer.isBuffer(value)) return value.toString('utf8');
	throw new KVClientError({code: KVClientErrorCode.INVALID_RESPONSE, message: 'KV row held a non-binary value'});
}

function int(value: unknown): number {
	return typeof value === 'number' ? value : Number(value);
}

function assertTtlSeconds(command: string, ttlSeconds: number): number {
	if (!Number.isSafeInteger(ttlSeconds)) {
		throw new KVClientError({
			code: KVClientErrorCode.REQUEST_FAILED,
			message: `KV request failed (${command}): ERR value is not an integer or out of range`,
		});
	}
	return ttlSeconds;
}

function assertPositiveTtl(command: string, ttlSeconds: number): number {
	if (assertTtlSeconds(command, ttlSeconds) <= 0) {
		throw new KVClientError({
			code: KVClientErrorCode.REQUEST_FAILED,
			message: `KV request failed (${command}): ERR invalid expire time in '${command}' command`,
		});
	}
	return ttlSeconds;
}

interface PostgresKVGlob {
	lower: Buffer;
	upper: Buffer | null;
	like: Buffer;
}

function translatePostgresKVGlob(pattern: string): PostgresKVGlob {
	let like = '';
	let prefix = '';
	let literal = true;
	for (let index = 0; index < pattern.length; index++) {
		const char = pattern[index];
		if (char === '\\') {
			index++;
			if (index >= pattern.length) {
				throw new KVClientError({code: KVClientErrorCode.INVALID_ARGUMENT, message: 'scan pattern ends in an escape'});
			}
			const escaped = pattern[index];
			like += /[%_\\]/.test(escaped) ? `\\${escaped}` : escaped;
			if (literal) prefix += escaped;
			continue;
		}
		if (char === '*' || char === '?') {
			like += char === '*' ? '%' : '_';
			literal = false;
			continue;
		}
		if (char === '[' || char === ']') {
			throw new KVClientError({
				code: KVClientErrorCode.INVALID_ARGUMENT,
				message: 'scan patterns with character classes are not supported',
			});
		}
		like += /[%_\\]/.test(char) ? `\\${char}` : char;
		if (literal) prefix += char;
	}
	const lower = encodeKV(prefix);
	return {lower, upper: nextPrefix(lower), like: encodeKV(like)};
}

function nextPrefix(prefix: Buffer): Buffer | null {
	let end = prefix.length;
	while (end > 0 && prefix[end - 1] === 0xff) end--;
	if (end === 0) return null;
	const upper = Buffer.from(prefix.subarray(0, end));
	upper[end - 1]++;
	return upper;
}

interface PendingActivity {
	value: string;
	ttlSeconds: number;
}

export class PostgresKVProvider implements IKVProvider {
	private readonly pool: pg.Pool;
	private readonly listener: PostgresKVListener;
	private readonly logger: IKVLogger;
	private readonly maxBytes: number | null;
	private readonly activityDebounceMs: number;
	private activityRecent = new Set<string>();
	private activityPrevious = new Set<string>();
	private activityRotatedAt = 0;
	private readonly activityPending = new Map<string, PendingActivity>();
	private activityTimer: NodeJS.Timeout | null = null;
	private activityFlush: Promise<void> | null = null;
	private schema: Promise<void> | null = null;
	private overCap = false;
	private overCapCheckedAt = 0;
	private closing: Promise<void> | null = null;

	constructor(config: PostgresKVProviderConfig) {
		if (!Number.isSafeInteger(config.maxConnections) || config.maxConnections < 1) {
			throw new Error('Postgres KV pool needs at least one connection');
		}
		const timeoutMs = config.timeoutMs ?? DEFAULT_KV_TIMEOUT_MS;
		this.logger = config.logger ?? {debug() {}, error() {}};
		this.maxBytes = config.maxBytes ?? null;
		this.activityDebounceMs = config.activityDebounceMs ?? POSTGRES_KV_DEFAULT_ACTIVITY_DEBOUNCE_MS;
		this.pool = new pg.Pool(
			postgresKVClientConfig(config.connection, {
				timeoutMs,
				maxConnections: config.maxConnections,
				synchronousCommitOff: true,
			}),
		);
		this.pool.on('error', (error) => this.logger.error({error}, 'Postgres KV idle connection error'));
		this.listener = new PostgresKVListener({
			clientConfig: postgresKVClientConfig(config.connection, {timeoutMs}),
			channel: POSTGRES_KV_NOTIFY_CHANNEL,
			refreshMessage: REFRESH_MESSAGE,
			logger: this.logger,
		});
	}

	async close(): Promise<void> {
		this.closing ??= this.shutdown();
		await this.closing;
	}

	private async shutdown(): Promise<void> {
		if (this.activityTimer !== null) {
			clearTimeout(this.activityTimer);
			this.activityTimer = null;
		}
		try {
			await this.flushActivity();
		} catch (error) {
			this.logger.error({error}, 'Failed to flush debounced KV activity writes');
		}
		await this.listener.close();
		await this.pool.end();
	}

	async get(key: string): Promise<string | null> {
		const pending = this.activityPending.get(key);
		if (pending !== undefined) return pending.value;
		return await this.run('get', async () => {
			const {rows} = await this.query(this.pool, SQL.get, [encodeKV(key)]);
			return rows.length === 0 ? null : decode(rows[0].value);
		});
	}

	async set(key: string, value: string, ...args: Array<string | number>): Promise<string | null> {
		const options = parseSetArguments(args);
		if (options.ttlSeconds !== undefined) assertPositiveTtl('set', options.ttlSeconds);
		await this.flushActivityKey(key);
		return await this.run(
			'set',
			async () => {
				const ttlMs = options.ttlSeconds === undefined ? null : options.ttlSeconds * 1000;
				const {rows} = await this.query(this.pool, SQL.set, [encodeKV(key), encodeKV(value), ttlMs, options.useNx]);
				return rows[0].ok === true ? 'OK' : null;
			},
			true,
		);
	}

	async setex(key: string, ttlSeconds: number, value: string): Promise<void> {
		assertPositiveTtl('setex', ttlSeconds);
		if (this.debounceActivity(key, ttlSeconds, value)) return;
		await this.run(
			'setex',
			async () => {
				await this.query(this.pool, SQL.set, [encodeKV(key), encodeKV(value), ttlSeconds * 1000, false]);
			},
			true,
		);
	}

	async setnx(key: string, value: string, ttlSeconds?: number): Promise<boolean> {
		if (ttlSeconds !== undefined) assertPositiveTtl('setnx', ttlSeconds);
		await this.flushActivityKey(key);
		return await this.run(
			'setnx',
			async () => {
				const ttlMs = ttlSeconds === undefined ? null : ttlSeconds * 1000;
				const {rows} = await this.query(this.pool, SQL.set, [encodeKV(key), encodeKV(value), ttlMs, true]);
				return rows[0].ok === true;
			},
			true,
		);
	}

	async mget(...keys: Array<string>): Promise<Array<string | null>> {
		if (keys.length === 0) return [];
		return await this.run('mget', async () => {
			const {rows} = await this.query(this.pool, SQL.mget, [keys.map(encodeKV)]);
			const found = new Map<string, string>();
			for (const row of rows) found.set(decode(row.key), decode(row.value));
			return keys.map((key) => this.activityPending.get(key)?.value ?? found.get(key) ?? null);
		});
	}

	async del(...keys: Array<string>): Promise<number> {
		if (keys.length === 0) return 0;
		for (const key of keys) this.activityPending.delete(key);
		return await this.run('del', async () => await this.delWith(this.pool, keys));
	}

	async exists(key: string): Promise<number> {
		return await this.run('exists', async () => ((await this.describe(key)).exists ? 1 : 0));
	}

	async expire(key: string, ttlSeconds: number): Promise<number> {
		assertTtlSeconds('expire', ttlSeconds);
		await this.flushActivityKey(key);
		return await this.run('expire', async () => await this.expireWith(this.pool, key, ttlSeconds));
	}

	async ttl(key: string): Promise<number> {
		return await this.run('ttl', async () => (await this.describe(key)).ttl);
	}

	async incr(key: string): Promise<number> {
		await this.flushActivityKey(key);
		return await this.run(
			'incr',
			async () => {
				const {rows} = await this.query(this.pool, SQL.incr, [encodeKV(key)]);
				return int(rows[0].value);
			},
			true,
		);
	}

	async getdel(key: string): Promise<string | null> {
		const pending = this.activityPending.get(key);
		this.activityPending.delete(key);
		return await this.run('getdel', async () => {
			const {rows} = await this.query(this.pool, SQL.getdel, [encodeKV(key)]);
			if (rows.length === 0 || rows[0].live !== true) return pending?.value ?? null;
			return pending?.value ?? decode(rows[0].value);
		});
	}

	async sadd(key: string, ...members: Array<string>): Promise<number> {
		if (members.length === 0) return 0;
		return await this.run('sadd', async () => await this.saddWith(this.pool, key, members), true);
	}

	async srem(key: string, ...members: Array<string>): Promise<number> {
		if (members.length === 0) return 0;
		return await this.run('srem', async () => {
			const {rows} = await this.query(this.pool, SQL.srem, [encodeKV(key), members.map(encodeKV)]);
			return int(rows[0].changed);
		});
	}

	async smembers(key: string): Promise<Array<string>> {
		return await this.run('smembers', async () => {
			const {rows} = await this.query(this.pool, SQL.smembers, [encodeKV(key)]);
			return rows.map((row) => decode(row.member));
		});
	}

	async sismember(key: string, member: string): Promise<number> {
		return await this.run('sismember', async () => {
			const {rows} = await this.query(this.pool, SQL.sismember, [encodeKV(key), encodeKV(member)]);
			return rows.length;
		});
	}

	async scard(key: string): Promise<number> {
		return await this.run('scard', async () => {
			const {rows} = await this.query(this.pool, SQL.scard, [encodeKV(key)]);
			return int(rows[0].n);
		});
	}

	async zadd(key: string, ...scoreMembers: Array<number | string>): Promise<number> {
		if (scoreMembers.length === 0) return 0;
		const members = createZSetMembersFromScorePairs(scoreMembers);
		return await this.run(
			'zadd',
			async () => {
				const {rows} = await this.query(this.pool, SQL.zadd, [
					encodeKV(key),
					members.map((member) => member.score),
					members.map((member) => encodeKV(member.value)),
				]);
				return int(rows[0].changed);
			},
			true,
		);
	}

	async zrem(key: string, ...members: Array<string>): Promise<number> {
		if (members.length === 0) return 0;
		return await this.run('zrem', async () => {
			const {rows} = await this.query(this.pool, SQL.zrem, [encodeKV(key), members.map(encodeKV)]);
			return int(rows[0].changed);
		});
	}

	async zcard(key: string): Promise<number> {
		return await this.run('zcard', async () => {
			const {rows} = await this.query(this.pool, SQL.zcard, [encodeKV(key)]);
			return int(rows[0].n);
		});
	}

	async zrangebyscore(
		key: string,
		min: string | number,
		max: string | number,
		...args: Array<string | number>
	): Promise<Array<string>> {
		const options = parseRangeByScoreArguments(args);
		const minBound = scoreBound(normalizeScoreBound(min));
		const maxBound = scoreBound(normalizeScoreBound(max));
		return await this.run('zrangebyscore', async () => {
			const {rows} = await this.query(this.pool, SQL.zrangebyscore, [
				encodeKV(key),
				minBound,
				maxBound,
				options.limit?.offset ?? 0,
				options.limit?.count ?? null,
			]);
			return rows.map((row) => decode(row.member));
		});
	}

	async rpush(key: string, ...values: Array<string>): Promise<number> {
		if (values.length === 0) return await this.llen(key);
		return await this.run(
			'rpush',
			async () => {
				const {rows} = await this.query(this.pool, SQL.rpush, [encodeKV(key), values.map(encodeKV)]);
				return int(rows[0].length);
			},
			true,
		);
	}

	async lpop(key: string, count?: number): Promise<Array<string>> {
		if (count !== undefined && count <= 0) return [];
		return await this.run('lpop', async () => {
			const {rows} = await this.query(this.pool, SQL.lpop, [encodeKV(key), count ?? 1]);
			return (rows[0].values as Array<Buffer>).map(decode);
		});
	}

	async llen(key: string): Promise<number> {
		return await this.run('llen', async () => {
			const {rows} = await this.query(this.pool, SQL.llen, [encodeKV(key)]);
			return rows.length === 0 ? 0 : int(rows[0].n);
		});
	}

	async lrange(key: string, start: number, stop: number): Promise<Array<string>> {
		return await this.run('lrange', async () => {
			const {rows} = await this.query(this.pool, SQL.lrange, [encodeKV(key), start, stop]);
			return rows.map((row) => decode(row.value));
		});
	}

	async ltrim(key: string, start: number, stop: number): Promise<void> {
		await this.run('ltrim', async () => {
			await this.query(this.pool, SQL.ltrim, [encodeKV(key), start, stop]);
		});
	}

	async hset(key: string, field: string, value: string): Promise<number> {
		return await this.run(
			'hset',
			async () => {
				const {rows} = await this.query(this.pool, SQL.hset, [encodeKV(key), encodeKV(field), encodeKV(value)]);
				return int(rows[0].changed);
			},
			true,
		);
	}

	async hdel(key: string, ...fields: Array<string>): Promise<number> {
		if (fields.length === 0) return 0;
		return await this.run('hdel', async () => {
			const {rows} = await this.query(this.pool, SQL.hdel, [encodeKV(key), fields.map(encodeKV)]);
			return int(rows[0].changed);
		});
	}

	async hgetall(key: string): Promise<Record<string, string>> {
		return await this.run('hgetall', async () => await this.hgetallWith(this.pool, key));
	}

	async publish(channel: string, message: string): Promise<number> {
		const payload = JSON.stringify([channel, message]);
		if (Buffer.byteLength(payload, 'utf8') > POSTGRES_KV_NOTIFY_MAX_PAYLOAD_BYTES) {
			throw new KVClientError({
				code: KVClientErrorCode.INVALID_ARGUMENT,
				message: `KV publish payload is larger than ${POSTGRES_KV_NOTIFY_MAX_PAYLOAD_BYTES} bytes`,
			});
		}
		return await this.run('publish', async () => {
			await this.query(this.pool, SQL.publish, [POSTGRES_KV_NOTIFY_CHANNEL, payload]);
			return 0;
		});
	}

	duplicate(): IKVSubscription {
		this.assertOpen();
		return new PostgresKVSubscription(this.listener);
	}

	async acquireLock(key: string, token: string, ttlSeconds: number): Promise<boolean> {
		assertPositiveTtl('set', ttlSeconds);
		return await this.run(
			'acquireLock',
			async () => {
				const {rows} = await this.query(this.pool, SQL.set, [encodeKV(key), encodeKV(token), ttlSeconds * 1000, true]);
				return rows[0].ok === true;
			},
			true,
		);
	}

	async releaseLock(key: string, token: string): Promise<boolean> {
		return await this.run('releaseLock', async () => {
			const {rows} = await this.query(this.pool, SQL.releaseLock, [encodeKV(key), encodeKV(token)]);
			return rows.length === 1;
		});
	}

	async extendLock(key: string, token: string, ttlSeconds: number): Promise<boolean> {
		assertPositiveTtl('expire', ttlSeconds);
		return await this.run('extendLock', async () => {
			const {rows} = await this.query(this.pool, SQL.extendLock, [encodeKV(key), encodeKV(token), ttlSeconds * 1000]);
			return rows.length === 1;
		});
	}

	async checkLeakyBucketLimit(key: string, limit: number, windowMs: number, cost: number): Promise<KVRateLimitResult> {
		return await this.run('checkLeakyBucketLimit', async () => {
			const {rows} = await this.query(this.pool, SQL.leakyBucket, [encodeKV(key), Date.now(), limit, windowMs, cost]);
			const row = rows[0];
			return {
				allowed: row.allowed === true,
				limit: int(row.limit),
				remaining: int(row.remaining),
				resetAfterMs: int(row.reset_after_ms),
				resetAtMs: int(row.reset_at_ms),
				retryAfterMs: int(row.retry_after_ms),
			};
		});
	}

	async scheduleBulkDeletion(queueKey: string, secondaryKey: string, score: number, value: string): Promise<void> {
		await this.run(
			'scheduleBulkDeletion',
			async () => {
				await this.query(this.pool, SQL.scheduleBulkDeletion, [
					encodeKV(queueKey),
					encodeKV(secondaryKey),
					score,
					encodeKV(value),
				]);
			},
			true,
		);
	}

	async claimBulkDeletion(queueKey: string, member: string, maxScore: number, leaseScore: number): Promise<boolean> {
		return await this.run('claimBulkDeletion', async () => {
			const {rows} = await this.query(this.pool, SQL.claimBulkDeletion, [
				encodeKV(queueKey),
				encodeKV(member),
				maxScore,
				leaseScore,
			]);
			return rows.length === 1;
		});
	}

	async removeBulkDeletion(queueKey: string, secondaryKey: string, member = ''): Promise<boolean> {
		return await this.run('removeBulkDeletion', async () => {
			const {rows} = await this.query(this.pool, SQL.removeBulkDeletion, [
				encodeKV(queueKey),
				encodeKV(secondaryKey),
				encodeKV(member),
			]);
			return rows[0].removed === true;
		});
	}

	async dequeuePurgeBatch(
		queueKey: string,
		bucketKey: string,
		maxItems: number,
		maxTokens: number,
		refillRate: number,
		refillIntervalMs: number,
	): Promise<KVPurgeBatchResult> {
		return await this.run('dequeuePurgeBatch', async () =>
			this.transaction(async (client) => {
				const queue = encodeKV(queueKey);
				await this.query(client, SQL.lockKey, [POSTGRES_KV_ADVISORY_LOCK_NAMESPACE, queue, 'hex']);
				const size = int((await this.query(client, SQL.scard, [queue])).rows[0].n);
				const bucketRows = (await this.query(client, SQL.get, [encodeKV(bucketKey)])).rows;
				const plan = planPurgeBatch(
					bucketRows.length === 0 ? null : decode(bucketRows[0].value),
					size,
					maxItems,
					maxTokens,
					refillRate,
					refillIntervalMs,
					Date.now(),
				);
				let entries: Array<string> = [];
				if (plan.toPop > 0) {
					const {rows} = await this.query(client, SQL.purgeQueue, [queue, plan.toPop]);
					entries = rows.map((row) => decode(row.member));
					await this.query(client, SQL.purgeQueueCleanup, [queue]);
				}
				const settled = settlePurgeBatch(plan, entries.length);
				if (settled.bucketValue !== null) {
					await this.query(client, SQL.set, [
						encodeKV(bucketKey),
						encodeKV(settled.bucketValue),
						PURGE_BUCKET_TTL_SECONDS * 1000,
						false,
					]);
				}
				return {entries, tokensConsumed: settled.tokensConsumed};
			}),
		);
	}

	async evalScript(
		command: string,
		_script: string,
		keyCount: number,
		...args: Array<string | number>
	): Promise<unknown> {
		if (keyCount !== 1) {
			throw new KVClientError({
				code: KVClientErrorCode.INVALID_ARGUMENT,
				message: `KV script ${command} must use exactly one key`,
			});
		}
		const key = encodeKV(String(args[0]));
		return await this.run(
			command,
			async () =>
				this.transaction(async (client) => {
					await this.query(client, SQL.lockKey, [POSTGRES_KV_ADVISORY_LOCK_NAMESPACE, key, 'hex']);
					const {rows} = await this.query(client, SQL.lockedString, [key]);
					const current = rows.length === 0 ? null : decode(rows[0].value);
					const transition = emailOwnerTransition(command, current, args.slice(1));
					if (transition.write.kind === 'set') {
						await this.query(client, SQL.set, [key, encodeKV(transition.write.value), transition.write.ttlMs, false]);
					} else if (transition.write.kind === 'delete') {
						await this.query(client, SQL.deleteKey, [key]);
					}
					return transition.reply;
				}),
			command !== EMAIL_OWNER_COMMANDS.release,
		);
	}

	async scan(pattern: string, count: number): Promise<Array<string>> {
		const glob = translatePostgresKVGlob(pattern);
		const limit = Math.max(1, Math.floor(count));
		return await this.run('scan', async () => {
			const {rows} = await this.query(this.pool, SQL.scan, [glob.lower, glob.upper, glob.like, Date.now(), limit]);
			return rows.map((row) => decode(row.key));
		});
	}

	pipeline(): IKVPipeline {
		this.assertOpen();
		return new PostgresKVPipeline(this.pipelineCommands(), 'pipeline');
	}

	multi(): IKVPipeline {
		this.assertOpen();
		return new PostgresKVPipeline(this.pipelineCommands(), 'multi');
	}

	isClustered(): boolean {
		return false;
	}

	async pruneExpired(
		batchSize = POSTGRES_KV_PRUNE_BATCH_SIZE,
		maxBatches = POSTGRES_KV_PRUNE_MAX_BATCHES,
	): Promise<PostgresKVPruneResult> {
		return await this.run('pruneExpired', async () => {
			let deletedKeys = 0;
			for (let batch = 0; batch < maxBatches; batch++) {
				const deleted = int((await this.query(this.pool, SQL.sweepKeys, [batchSize])).rows[0].deleted);
				deletedKeys += deleted;
				if (deleted < batchSize) break;
			}
			const deletedBuckets = (await this.query(this.pool, SQL.sweepBuckets, [Date.now()])).rowCount ?? 0;
			const sizeBytes = int((await this.query(this.pool, SQL.size, [Object.values(T)])).rows[0].bytes);
			const overCap =
				this.maxBytes !== null &&
				sizeBytes > this.maxBytes &&
				int((await this.query(this.pool, SQL.liveSize, [])).rows[0].bytes) > this.maxBytes;
			await this.query(this.pool, SQL.writeMeta, [
				POSTGRES_KV_META_KEYS.overCap,
				overCap ? '1' : '0',
				POSTGRES_KV_META_KEYS.sizeBytes,
				String(sizeBytes),
			]);
			this.overCap = overCap;
			this.overCapCheckedAt = Date.now();
			return {deletedKeys, deletedBuckets, sizeBytes, overCap};
		});
	}

	private pipelineCommands(): PostgresKVPipelineCommands {
		return {
			connect: async () => {
				this.assertOpen();
				await this.ensureSchema();
				return await this.pool.connect();
			},
			assertUnderCap: async () => await this.assertUnderCap(),
			setexBatch: async (db, entries) => {
				for (const entry of entries) assertPositiveTtl('setex', entry.ttlSeconds);
				const latest = new Map(entries.map((entry) => [entry.key, entry]));
				for (const key of latest.keys()) this.activityPending.delete(key);
				const batch = [...latest.values()];
				await this.query(db, SQL.setBatch, [
					batch.map((entry) => encodeKV(entry.key)),
					batch.map((entry) => encodeKV(entry.value)),
					batch.map((entry) => entry.ttlSeconds * 1000),
				]);
			},
			del: async (db, key) => {
				this.activityPending.delete(key);
				return await this.delWith(db, [key]);
			},
			expire: async (db, key, ttlSeconds) => await this.expireWith(db, key, assertTtlSeconds('expire', ttlSeconds)),
			sadd: async (db, key, members) => await this.saddWith(db, key, members),
			hgetall: async (db, key) => await this.hgetallWith(db, key),
			drainHash: async (db, key) => {
				const {rows} = await this.query(db, SQL.drainHash, [encodeKV(key)]);
				const row = rows[0];
				const fields = (row.fields as Array<Buffer> | null) ?? [];
				const values = (row.vals as Array<Buffer> | null) ?? [];
				const keyLive = row.live as boolean | null;
				const kind = row.kind as string | null;
				const expired = keyLive === false;
				const hash: Record<string, string> = {};
				if (!expired) {
					fields.forEach((field, index) => {
						hash[decode(field)] = decode(values[index]);
					});
				}
				const present = fields.length > 0 || row.other === true || (keyLive === true && (kind === 's' || kind === 'l'));
				const deleted = !expired && present ? 1 : 0;
				return {hash, deleted};
			},
			normalizeError: (command, error) => this.normalizeError(command, error),
		};
	}

	private async delWith(db: Queryable, keys: Array<string>): Promise<number> {
		const {rows} = await this.query(db, SQL.del, [keys.map(encodeKV), Date.now()]);
		return int(rows[0].deleted);
	}

	private async expireWith(db: Queryable, key: string, ttlSeconds: number): Promise<number> {
		const {rows} = await this.query(db, SQL.expire, [encodeKV(key), ttlSeconds * 1000, Date.now()]);
		return int(rows[0].changed);
	}

	private async saddWith(db: Queryable, key: string, members: Array<string>): Promise<number> {
		const {rows} = await this.query(db, SQL.sadd, [encodeKV(key), members.map(encodeKV)]);
		return int(rows[0].changed);
	}

	private async hgetallWith(db: Queryable, key: string): Promise<Record<string, string>> {
		const {rows} = await this.query(db, SQL.hgetall, [encodeKV(key)]);
		const hash: Record<string, string> = {};
		for (const row of rows) hash[decode(row.field)] = decode(row.value);
		return hash;
	}

	private async describe(key: string): Promise<{exists: boolean; ttl: number}> {
		await this.flushActivityKey(key);
		const {rows} = await this.query(this.pool, SQL.describe, [encodeKV(key), Date.now()]);
		const row = rows[0];
		const kind = row.kind as string | null;
		const hasMembers = row.has_members === true;
		if (kind !== null) {
			const collection = kind === 'h' || kind === 'e' || kind === 'z';
			if (row.expired === true || (collection && !hasMembers)) return {exists: false, ttl: -2};
			if (row.persistent === true) return {exists: true, ttl: -1};
			return {exists: true, ttl: Math.floor((int(row.remaining_ms) + 500) / 1000)};
		}
		if (hasMembers) return {exists: true, ttl: -1};
		if (row.bucket_remaining_ms !== null) {
			return {exists: true, ttl: Math.floor((int(row.bucket_remaining_ms) + 500) / 1000)};
		}
		return {exists: false, ttl: -2};
	}

	private debounceActivity(key: string, ttlSeconds: number, value: string): boolean {
		if (this.activityDebounceMs <= 0 || !key.startsWith(POSTGRES_KV_ACTIVITY_KEY_PREFIX) || this.closing !== null) {
			return false;
		}
		const now = Date.now();
		if (now - this.activityRotatedAt >= this.activityDebounceMs) {
			this.activityPrevious =
				now - this.activityRotatedAt >= 2 * this.activityDebounceMs ? new Set() : this.activityRecent;
			this.activityRecent = new Set();
			this.activityRotatedAt = now;
		}
		if (!this.activityRecent.has(key) && !this.activityPrevious.has(key)) {
			this.activityRecent.add(key);
			this.activityPending.delete(key);
			return false;
		}
		this.activityPending.set(key, {value, ttlSeconds});
		if (this.activityTimer === null) {
			this.activityTimer = setTimeout(() => {
				this.activityTimer = null;
				this.flushActivity().catch((error) =>
					this.logger.error({error}, 'Failed to flush debounced KV activity writes'),
				);
			}, this.activityDebounceMs);
			this.activityTimer.unref();
		}
		return true;
	}

	private async flushActivityKey(key: string): Promise<void> {
		if (!this.activityPending.has(key)) return;
		try {
			await this.flushActivity();
		} catch (error) {
			throw this.normalizeError('setex', error);
		}
	}

	private async flushActivity(): Promise<void> {
		while (this.activityFlush !== null) await this.activityFlush;
		if (this.activityPending.size === 0) return;
		const entries = [...this.activityPending.entries()];
		this.activityPending.clear();
		this.activityFlush = (async () => {
			try {
				await this.ensureSchema();
				await this.assertUnderCap();
				await this.query(this.pool, SQL.setBatch, [
					entries.map(([key]) => encodeKV(key)),
					entries.map(([, entry]) => encodeKV(entry.value)),
					entries.map(([, entry]) => entry.ttlSeconds * 1000),
				]);
			} finally {
				this.activityFlush = null;
			}
		})();
		await this.activityFlush;
	}

	private async run<T>(command: string, fn: () => Promise<T>, grows = false): Promise<T> {
		try {
			this.assertOpen();
			await this.ensureSchema();
			if (grows) await this.assertUnderCap();
			return await fn();
		} catch (error) {
			throw this.normalizeError(command, error);
		}
	}

	private async transaction<T>(fn: (client: pg.PoolClient) => Promise<T>): Promise<T> {
		const client = await this.pool.connect();
		let broken = false;
		try {
			await client.query('BEGIN');
			const result = await fn(client);
			await client.query('COMMIT');
			return result;
		} catch (error) {
			try {
				await client.query('ROLLBACK');
			} catch {
				broken = true;
			}
			throw error;
		} finally {
			client.release(broken);
		}
	}

	private async query(db: Queryable, text: string, values: Array<unknown>): Promise<pg.QueryResult<Row>> {
		return await db.query<Row>({name: SQL_NAMES.get(text), text, values});
	}

	private async ensureSchema(): Promise<void> {
		this.schema ??= (async () => {
			const client = await this.pool.connect();
			let version: number;
			try {
				version = await ensurePostgresKVStoreSchema(client);
			} finally {
				client.release();
			}
			if (version < POSTGRES_KV_SCHEMA_VERSION) {
				throw new Error(`Postgres KV schema ${version} is older than ${POSTGRES_KV_SCHEMA_VERSION}`);
			}
		})().catch((error: unknown) => {
			this.schema = null;
			throw error;
		});
		await this.schema;
	}

	private async assertUnderCap(): Promise<void> {
		if (this.maxBytes === null) return;
		const now = Date.now();
		if (now - this.overCapCheckedAt >= SIZE_GUARD_REFRESH_MS) {
			const {rows} = await this.query(this.pool, SQL.readOverCap, [POSTGRES_KV_META_KEYS.overCap]);
			this.overCap = rows[0]?.value === '1';
			this.overCapCheckedAt = now;
		}
		if (this.overCap) {
			throw new KVClientError({
				code: KVClientErrorCode.REQUEST_FAILED,
				message: `OOM the key-value tables are larger than FLUXER_KV_POSTGRES_MAX_BYTES (${this.maxBytes})`,
			});
		}
	}

	private assertOpen(): void {
		if (this.closing !== null) {
			throw new Error('KV client is closed');
		}
	}

	private normalizeError(command: string, error: unknown): KVClientError {
		if (error instanceof KVClientError) return error;
		const code = typeof error === 'object' && error !== null && 'code' in error ? String(error.code) : '';
		const message = error instanceof Error ? error.message : String(error);
		if (code === '57014' || code === '55P03' || /timeout|timed out/i.test(message)) {
			return new KVClientError({code: KVClientErrorCode.TIMEOUT, message: `KV request timed out: ${command}`});
		}
		if (code === '22P02' || code === '22003') {
			return new KVClientError({
				code: KVClientErrorCode.REQUEST_FAILED,
				message: `KV request failed (${command}): ERR value is not an integer or out of range`,
			});
		}
		return new KVClientError({
			code: KVClientErrorCode.REQUEST_FAILED,
			message: `KV request failed (${command}): ${message}`,
		});
	}
}

function scoreBound(bound: string | number): number {
	if (bound === '-inf') return Number.NEGATIVE_INFINITY;
	if (bound === '+inf') return Number.POSITIVE_INFINITY;
	return Number(bound);
}
