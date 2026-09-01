// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {IKVPipeline, IKVProvider, IKVSubscription, KVRateLimitResult} from '@pkgs/kv_client/src/IKVProvider';
import {
	type IKVLogger,
	type KVClientConfig,
	type ResolvedKVClientConfig,
	resolveKVClientConfig,
} from '@pkgs/kv_client/src/KVClientConfig';
import {KVClientError, KVClientErrorCode} from '@pkgs/kv_client/src/KVClientError';
import {
	createStringEntriesFromPairs,
	createZSetMembersFromScorePairs,
	normalizeScoreBound,
	parseRangeByScoreArguments,
	parseSetArguments,
} from '@pkgs/kv_client/src/KVCommandArguments';
import {runSlotBatches, splitIntoSlotBatches} from '@pkgs/kv_client/src/KVHashSlots';
import {KVPipeline} from '@pkgs/kv_client/src/KVPipeline';
import {KVSubscription} from '@pkgs/kv_client/src/KVSubscription';
import Redis, {Cluster} from 'ioredis';

const RELEASE_LOCK_SCRIPT = `
if redis.call('GET', KEYS[1]) == ARGV[1] then
	return redis.call('DEL', KEYS[1])
end
return 0
`;
const EXTEND_LOCK_SCRIPT = `
if redis.call('GET', KEYS[1]) == ARGV[1] then
	redis.call('EXPIRE', KEYS[1], ARGV[2])
	return 1
end
return 0
`;
const RENEW_SNOWFLAKE_SCRIPT = `
if redis.call('GET', KEYS[1]) == ARGV[1] then
	redis.call('SET', KEYS[1], ARGV[1], 'EX', ARGV[2])
	return 1
end
return 0
`;
const TRY_CONSUME_TOKENS_SCRIPT = `
local key = KEYS[1]
local now = tonumber(ARGV[1])
local requested = tonumber(ARGV[2])
local maxTokens = tonumber(ARGV[3])
local refillRate = tonumber(ARGV[4])
local refillIntervalMs = tonumber(ARGV[5])

local data = redis.call('GET', key)
local tokens = maxTokens
local lastRefill = now

if data then
	local ok, bucket = pcall(cjson.decode, data)
	if ok and bucket then
		tokens = tonumber(bucket.tokens) or maxTokens
		lastRefill = tonumber(bucket.lastRefill) or now
	end
end

local elapsed = now - lastRefill
if elapsed >= refillIntervalMs then
	local intervals = math.floor(elapsed / refillIntervalMs)
	local tokensToAdd = intervals * refillRate
	if tokensToAdd > 0 then
		tokens = math.min(maxTokens, tokens + tokensToAdd)
		lastRefill = now
	end
end

local consumed = 0
if tokens >= requested then
	consumed = requested
	tokens = tokens - requested
elseif tokens > 0 then
	consumed = tokens
	tokens = 0
end

redis.call('SET', key, cjson.encode({tokens = tokens, lastRefill = lastRefill}), 'EX', 3600)
return consumed
`;
const CHECK_LEAKY_BUCKET_LIMIT_SCRIPT = `
local key = KEYS[1]
local nowMs = tonumber(ARGV[1])
local limit = tonumber(ARGV[2])
local windowMs = tonumber(ARGV[3])
local cost = tonumber(ARGV[4])

if not limit or limit <= 0 then
	error('limit must be positive')
end
if not windowMs or windowMs <= 0 then
	error('windowMs must be positive')
end
if cost ~= 0 and cost ~= 1 then
	error('cost must be 0 or 1')
end

local capacity = math.max(1, math.floor(limit))
local leakWindowMs = math.max(1, math.floor(windowMs))
local level = 0
local updatedAt = nowMs

local rawState = redis.call('GET', key)
if rawState then
	local ok, state = pcall(cjson.decode, rawState)
	if ok and state then
		level = tonumber(state.level) or 0
		updatedAt = tonumber(state.updatedAt) or nowMs
	end
end

local elapsed = nowMs - updatedAt
if elapsed > 0 then
	local leaked = (elapsed / leakWindowMs) * capacity
	level = math.max(0, level - leaked)
	updatedAt = nowMs
end

local leakPerMs = capacity / leakWindowMs

local function leakyBucketResetAfterMs(currentLevel)
	if currentLevel <= 0 then
		return 0
	end
	return math.max(0, math.ceil(currentLevel / leakPerMs))
end

local function leakyBucketRetryAfterMs(currentLevel)
	local overflow = currentLevel + cost - capacity
	if overflow <= 0 then
		return 0
	end
	return math.max(1, math.ceil(overflow / leakPerMs))
end

local function createResult(allowed, remaining, resetAfterMs, retryAfterMs)
	return cjson.encode({
		allowed = allowed,
		limit = capacity,
		remaining = remaining,
		resetAfterMs = resetAfterMs,
		resetAtMs = nowMs + resetAfterMs,
		retryAfterMs = retryAfterMs,
	})
end

if cost == 0 then
	local remaining = math.max(0, math.floor(capacity - level))
	local resetAfterMs = leakyBucketResetAfterMs(level)
	if level > 0 then
		redis.call('SET', key, cjson.encode({level = level, updatedAt = updatedAt}), 'PX', math.max(1, resetAfterMs))
	else
		redis.call('DEL', key)
	end
	return createResult(true, remaining, resetAfterMs, 0)
end

if level + cost > capacity then
	local retryAfterMs = leakyBucketRetryAfterMs(level)
	local resetAfterMs = leakyBucketResetAfterMs(level)
	local ttlMs = math.max(1, resetAfterMs, retryAfterMs)
	redis.call('SET', key, cjson.encode({level = level, updatedAt = updatedAt}), 'PX', ttlMs)
	return createResult(false, 0, ttlMs, retryAfterMs)
end

level = level + cost
local remaining = math.max(0, math.floor(capacity - level))
local resetAfterMs = leakyBucketResetAfterMs(level)
local ttlMs = math.max(1, resetAfterMs)
redis.call('SET', key, cjson.encode({level = level, updatedAt = updatedAt}), 'PX', ttlMs)
return createResult(true, remaining, resetAfterMs, 0)
`;
const SCHEDULE_BULK_DELETION_SCRIPT = `
redis.call('ZADD', KEYS[1], ARGV[1], ARGV[2])
redis.call('SET', KEYS[2], ARGV[2])
return 1
`;
const DEQUEUE_PURGE_BATCH_SCRIPT = `
local queueKey = KEYS[1]
local bucketKey = KEYS[2]
local maxItems = tonumber(ARGV[1])
local now = tonumber(ARGV[2])
local maxTokens = tonumber(ARGV[3])
local refillRate = tonumber(ARGV[4])
local refillIntervalMs = tonumber(ARGV[5])

local queueSize = redis.call('SCARD', queueKey)
if queueSize == 0 then
	return cjson.encode({urls = {}, tokens = 0})
end

local tokens = maxTokens
local lastRefill = now
local data = redis.call('GET', bucketKey)
if data then
	local ok, bucket = pcall(cjson.decode, data)
	if ok and bucket then
		tokens = tonumber(bucket.tokens) or maxTokens
		lastRefill = tonumber(bucket.lastRefill) or now
	end
end

local elapsed = now - lastRefill
if elapsed >= refillIntervalMs then
	local intervals = math.floor(elapsed / refillIntervalMs)
	tokens = math.min(maxTokens, tokens + intervals * refillRate)
	lastRefill = now
end

local toPop = math.min(maxItems, math.floor(tokens), queueSize)
if toPop <= 0 then
	redis.call('SET', bucketKey, cjson.encode({tokens = tokens, lastRefill = lastRefill}), 'EX', 3600)
	return cjson.encode({urls = {}, tokens = 0})
end

local urls = redis.call('SPOP', queueKey, toPop)
tokens = tokens - #urls

redis.call('SET', bucketKey, cjson.encode({tokens = tokens, lastRefill = lastRefill}), 'EX', 3600)
return cjson.encode({urls = urls, tokens = #urls})
`;
const CLAIM_BULK_DELETION_SCRIPT = `
local score = redis.call('ZSCORE', KEYS[1], ARGV[1])
if not score then
	return 0
end
if tonumber(score) > tonumber(ARGV[2]) then
	return 0
end
redis.call('ZADD', KEYS[1], ARGV[3], ARGV[1])
return 1
`;
const REMOVE_BULK_DELETION_SCRIPT = `
local member = ARGV[1]
if member ~= '' and redis.call('ZREM', KEYS[1], member) == 1 then
	redis.call('DEL', KEYS[2])
	return 1
end
local value = redis.call('GET', KEYS[2])
if not value then
	return 0
end
redis.call('ZREM', KEYS[1], value)
redis.call('DEL', KEYS[2])
return 1
`;

const SCRIPT_SHA_CACHE = new Map<string, string>();

interface ScriptPurgeBatchResult {
	urls: Array<string>;
	tokens: number;
}

export class KVClient implements IKVProvider {
	private readonly client: Redis | Cluster;
	private readonly config: ResolvedKVClientConfig;
	private readonly logger: IKVLogger;
	private readonly url: string;
	private readonly timeoutMs: number;

	constructor(config: KVClientConfig | string) {
		const resolvedConfig = resolveKVClientConfig(config);
		this.config = resolvedConfig;
		this.url = resolvedConfig.url;
		this.timeoutMs = resolvedConfig.timeoutMs;
		this.logger = resolvedConfig.logger;
		if (resolvedConfig.mode === 'cluster') {
			this.client = this.createClusterClient(resolvedConfig);
		} else {
			this.client = new Redis(this.url, {
				connectTimeout: this.timeoutMs,
				commandTimeout: this.timeoutMs,
				maxRetriesPerRequest: 1,
				retryStrategy: createRetryStrategy(),
			});
		}
	}

	private createClusterClient(clusterConfig: ResolvedKVClientConfig): Cluster {
		const nodes =
			clusterConfig.clusterNodes.length > 0 ? clusterConfig.clusterNodes : parseClusterNodesFromUrl(clusterConfig.url);
		const natMap = clusterConfig.clusterNatMap;
		const hasNatMap = Object.keys(natMap).length > 0;
		return new Cluster(nodes, {
			clusterRetryStrategy: createRetryStrategy(),
			redisOptions: {
				connectTimeout: clusterConfig.timeoutMs,
				commandTimeout: clusterConfig.timeoutMs,
				maxRetriesPerRequest: 1,
			},
			scaleReads: 'master',
			...(hasNatMap ? {natMap} : {}),
		});
	}

	async health(): Promise<boolean> {
		try {
			return (await this.execute('health', async () => this.client.ping())) === 'PONG';
		} catch (error) {
			this.logger.debug({url: this.url, error}, 'KV health check failed');
			return false;
		}
	}

	async get(key: string): Promise<string | null> {
		return await this.execute('get', async () => this.client.get(key));
	}

	async set(key: string, value: string, ...args: Array<string | number>): Promise<string | null> {
		const options = parseSetArguments(args);
		if (options.useNx) {
			if (options.ttlSeconds !== undefined) {
				const ttlSeconds = options.ttlSeconds;
				return await this.execute('set', async () => {
					const result = await this.client.call('SET', key, value, 'EX', ttlSeconds, 'NX');
					return normalizeStringOrNull(result);
				});
			}
			return await this.execute('set', async () => {
				const result = await this.client.call('SET', key, value, 'NX');
				return normalizeStringOrNull(result);
			});
		}
		if (options.ttlSeconds !== undefined) {
			const ttlSeconds = options.ttlSeconds;
			return await this.execute('set', async () => {
				const result = await this.client.call('SET', key, value, 'EX', ttlSeconds);
				return normalizeStringOrNull(result);
			});
		}
		return await this.execute('set', async () => this.client.set(key, value));
	}

	async setex(key: string, ttlSeconds: number, value: string): Promise<void> {
		await this.execute('setex', async () => {
			await this.client.setex(key, ttlSeconds, value);
		});
	}

	async setnx(key: string, value: string, ttlSeconds?: number): Promise<boolean> {
		if (ttlSeconds !== undefined) {
			const ttlSecondsValue = ttlSeconds;
			const result = await this.execute('setnx', async () => {
				const commandResult = await this.client.call('SET', key, value, 'EX', ttlSecondsValue, 'NX');
				return normalizeStringOrNull(commandResult);
			});
			return result === 'OK';
		}
		return (await this.execute('setnx', async () => this.client.setnx(key, value))) === 1;
	}

	async mget(...keys: Array<string>): Promise<Array<string | null>> {
		if (keys.length === 0) {
			return [];
		}
		return await this.execute('mget', async () => {
			const values = new Array<string | null>(keys.length).fill(null);
			const batches = this.splitBySlot(
				keys.map((key, index) => ({key, index})),
				(entry) => entry.key,
			);
			await runSlotBatches(batches, async (batch) => {
				const batchValues = await this.client.mget(...batch.map((entry) => entry.key));
				for (const [position, entry] of batch.entries()) {
					values[entry.index] = batchValues[position] ?? null;
				}
			});
			return values;
		});
	}

	async mset(...args: Array<string>): Promise<void> {
		const entries = createStringEntriesFromPairs(args);
		if (entries.length === 0) {
			return;
		}
		await this.execute('mset', async () => {
			const batches = this.splitBySlot(entries, (entry) => entry.key);
			await runSlotBatches(batches, async (batch) => {
				await this.client.mset(...batch.flatMap((entry) => [entry.key, entry.value]));
			});
		});
	}

	async del(...keys: Array<string>): Promise<number> {
		if (keys.length === 0) {
			return 0;
		}
		return await this.execute('del', async () => {
			const deleted: Array<number> = [];
			const batches = this.splitBySlot(keys, (key) => key);
			await runSlotBatches(batches, async (batch) => {
				deleted.push(await this.client.del(...batch));
			});
			return deleted.reduce((total, count) => total + count, 0);
		});
	}

	async exists(key: string): Promise<number> {
		return await this.execute('exists', async () => this.client.exists(key));
	}

	async expire(key: string, ttlSeconds: number): Promise<number> {
		return await this.execute('expire', async () => this.client.expire(key, ttlSeconds));
	}

	async ttl(key: string): Promise<number> {
		return await this.execute('ttl', async () => this.client.ttl(key));
	}

	async incr(key: string): Promise<number> {
		return await this.execute('incr', async () => this.client.incr(key));
	}

	async getex(key: string, ttlSeconds: number): Promise<string | null> {
		return await this.execute('getex', async () => {
			const result = await this.client.call('GETEX', key, 'EX', ttlSeconds);
			return normalizeStringOrNull(result);
		});
	}

	async getdel(key: string): Promise<string | null> {
		return await this.execute('getdel', async () => {
			const result = await this.client.call('GETDEL', key);
			return normalizeStringOrNull(result);
		});
	}

	async sadd(key: string, ...members: Array<string>): Promise<number> {
		if (members.length === 0) {
			return 0;
		}
		return await this.execute('sadd', async () => this.client.sadd(key, ...members));
	}

	async srem(key: string, ...members: Array<string>): Promise<number> {
		if (members.length === 0) {
			return 0;
		}
		return await this.execute('srem', async () => this.client.srem(key, ...members));
	}

	async smembers(key: string): Promise<Array<string>> {
		return await this.execute('smembers', async () => this.client.smembers(key));
	}

	async sismember(key: string, member: string): Promise<number> {
		return await this.execute('sismember', async () => this.client.sismember(key, member));
	}

	async scard(key: string): Promise<number> {
		return await this.execute('scard', async () => this.client.scard(key));
	}

	async spop(key: string, count: number = 1): Promise<Array<string>> {
		if (count <= 0) {
			return [];
		}
		return await this.execute('spop', async () => {
			const result = await this.client.spop(key, count);
			if (result === null) {
				return [];
			}
			return Array.isArray(result) ? result : [result];
		});
	}

	async zadd(key: string, ...scoreMembers: Array<number | string>): Promise<number> {
		if (scoreMembers.length === 0) {
			return 0;
		}
		const members = createZSetMembersFromScorePairs(scoreMembers);
		const args = members.flatMap((member) => [member.score, member.value]);
		return await this.execute('zadd', async () => this.client.zadd(key, ...args));
	}

	async zrem(key: string, ...members: Array<string>): Promise<number> {
		if (members.length === 0) {
			return 0;
		}
		return await this.execute('zrem', async () => this.client.zrem(key, ...members));
	}

	async zcard(key: string): Promise<number> {
		return await this.execute('zcard', async () => this.client.zcard(key));
	}

	async zrangebyscore(
		key: string,
		min: string | number,
		max: string | number,
		...args: Array<string | number>
	): Promise<Array<string>> {
		const options = parseRangeByScoreArguments(args);
		const minBound = normalizeScoreBound(min);
		const maxBound = normalizeScoreBound(max);
		if (options.limit === undefined) {
			return await this.execute('zrangebyscore', async () => this.client.zrangebyscore(key, minBound, maxBound));
		}
		const {offset, count} = options.limit;
		return await this.execute('zrangebyscore', async () =>
			this.client.zrangebyscore(key, minBound, maxBound, 'LIMIT', offset, count),
		);
	}

	async rpush(key: string, ...values: Array<string>): Promise<number> {
		if (values.length === 0) {
			return await this.llen(key);
		}
		return await this.execute('rpush', async () => this.client.rpush(key, ...values));
	}

	async lpop(key: string, count?: number): Promise<Array<string>> {
		if (count !== undefined && count <= 0) {
			return [];
		}
		return await this.execute('lpop', async () => {
			if (count !== undefined) {
				const result = await this.client.call('LPOP', key, count);
				if (result === null) {
					return [];
				}
				if (Array.isArray(result)) {
					return result.map((entry) => String(entry));
				}
				return [String(result)];
			}
			const single = await this.client.lpop(key);
			return single === null ? [] : [single];
		});
	}

	async llen(key: string): Promise<number> {
		return await this.execute('llen', async () => this.client.llen(key));
	}

	async hset(key: string, field: string, value: string): Promise<number> {
		return await this.execute('hset', async () => this.client.hset(key, field, value));
	}

	async hdel(key: string, ...fields: Array<string>): Promise<number> {
		if (fields.length === 0) {
			return 0;
		}
		return await this.execute('hdel', async () => this.client.hdel(key, ...fields));
	}

	async hget(key: string, field: string): Promise<string | null> {
		return await this.execute('hget', async () => this.client.hget(key, field));
	}

	async hgetall(key: string): Promise<Record<string, string>> {
		return await this.execute('hgetall', async () => this.client.hgetall(key));
	}

	async publish(channel: string, message: string): Promise<number> {
		return await this.execute('publish', async () => this.client.publish(channel, message));
	}

	duplicate(): IKVSubscription {
		return new KVSubscription({
			url: this.url,
			mode: this.config.mode,
			clusterNodes: this.config.clusterNodes,
			timeoutMs: this.timeoutMs,
			logger: this.logger,
		});
	}

	async acquireLock(key: string, token: string, ttlSeconds: number): Promise<boolean> {
		const result = await this.execute('acquireLock', async () => {
			const commandResult = await this.client.call('SET', key, token, 'EX', ttlSeconds, 'NX');
			return normalizeStringOrNull(commandResult);
		});
		return result === 'OK';
	}

	async releaseLock(key: string, token: string): Promise<boolean> {
		const result = await this.executeScript('releaseLock', RELEASE_LOCK_SCRIPT, 1, key, token);
		return Number(result) === 1;
	}

	async extendLock(key: string, token: string, ttlSeconds: number): Promise<boolean> {
		const result = await this.executeScript('extendLock', EXTEND_LOCK_SCRIPT, 1, key, token, ttlSeconds);
		return Number(result) === 1;
	}

	async renewSnowflakeNode(key: string, instanceId: string, ttlSeconds: number): Promise<boolean> {
		const result = await this.executeScript(
			'renewSnowflakeNode',
			RENEW_SNOWFLAKE_SCRIPT,
			1,
			key,
			instanceId,
			ttlSeconds,
		);
		return Number(result) === 1;
	}

	async checkLeakyBucketLimit(key: string, limit: number, windowMs: number, cost: number): Promise<KVRateLimitResult> {
		const now = Date.now();
		return await this.executeRateLimitScript(
			'checkLeakyBucketLimit',
			CHECK_LEAKY_BUCKET_LIMIT_SCRIPT,
			key,
			now,
			limit,
			windowMs,
			cost,
		);
	}

	async tryConsumeTokens(
		key: string,
		requested: number,
		maxTokens: number,
		refillRate: number,
		refillIntervalMs: number,
	): Promise<number> {
		const now = Date.now();
		const result = await this.executeScript(
			'tryConsumeTokens',
			TRY_CONSUME_TOKENS_SCRIPT,
			1,
			key,
			now,
			requested,
			maxTokens,
			refillRate,
			refillIntervalMs,
		);
		return Number(result);
	}

	async scheduleBulkDeletion(queueKey: string, secondaryKey: string, score: number, value: string): Promise<void> {
		await this.executeScript(
			'scheduleBulkDeletion',
			SCHEDULE_BULK_DELETION_SCRIPT,
			2,
			queueKey,
			secondaryKey,
			score,
			value,
		);
	}

	async claimBulkDeletion(queueKey: string, member: string, maxScore: number, leaseScore: number): Promise<boolean> {
		const result = await this.executeScript(
			'claimBulkDeletion',
			CLAIM_BULK_DELETION_SCRIPT,
			1,
			queueKey,
			member,
			maxScore,
			leaseScore,
		);
		return Number(result) === 1;
	}

	async removeBulkDeletion(queueKey: string, secondaryKey: string, member = ''): Promise<boolean> {
		const result = await this.executeScript(
			'removeBulkDeletion',
			REMOVE_BULK_DELETION_SCRIPT,
			2,
			queueKey,
			secondaryKey,
			member,
		);
		return Number(result) === 1;
	}

	async dequeuePurgeBatch(
		queueKey: string,
		bucketKey: string,
		maxItems: number,
		maxTokens: number,
		refillRate: number,
		refillIntervalMs: number,
	): Promise<{
		urls: Array<string>;
		tokensConsumed: number;
	}> {
		const now = Date.now();
		const parsed = await this.executeJsonScript<ScriptPurgeBatchResult>(
			'dequeuePurgeBatch',
			DEQUEUE_PURGE_BATCH_SCRIPT,
			2,
			queueKey,
			bucketKey,
			maxItems,
			now,
			maxTokens,
			refillRate,
			refillIntervalMs,
		);
		return {urls: parsed.urls, tokensConsumed: parsed.tokens};
	}

	async evalScript(
		command: string,
		script: string,
		keyCount: number,
		...args: Array<string | number>
	): Promise<unknown> {
		return await this.executeScript(command, script, keyCount, ...args);
	}

	async scan(pattern: string, count: number): Promise<Array<string>> {
		return await this.execute('scan', async () => {
			const limit = Math.max(1, Math.floor(count));
			let cursor = '0';
			const keys: Array<string> = [];
			do {
				const [nextCursor, batch] = await this.client.scan(cursor, 'MATCH', pattern, 'COUNT', limit);
				cursor = nextCursor;
				keys.push(...batch);
				if (keys.length >= limit) {
					break;
				}
			} while (cursor !== '0');
			return keys.slice(0, limit);
		});
	}

	isClustered(): boolean {
		return this.config.mode === 'cluster';
	}

	private splitBySlot<T>(items: ReadonlyArray<T>, keyOf: (item: T) => string): Array<Array<T>> {
		return splitIntoSlotBatches(items, keyOf, this.isClustered());
	}

	pipeline(): IKVPipeline {
		return new KVPipeline({
			createCommander: () => this.client.pipeline(),
			normalizeError: (command, error) => this.normalizeError(command, error),
			mode: 'pipeline',
		});
	}

	multi(): IKVPipeline {
		return new KVPipeline({
			createCommander: () => this.client.multi(),
			normalizeError: (command, error) => this.normalizeError(command, error),
			mode: 'multi',
		});
	}

	private async execute<T>(command: string, fn: () => Promise<T>): Promise<T> {
		try {
			return await fn();
		} catch (error) {
			throw this.normalizeError(command, error);
		}
	}

	private async executeScript(
		command: string,
		script: string,
		keyCount: number,
		...args: Array<string | number>
	): Promise<unknown> {
		return await this.execute(command, async () => this.evalCachedScript(script, keyCount, args));
	}

	private async evalCachedScript(script: string, keyCount: number, args: Array<string | number>): Promise<unknown> {
		try {
			return await this.client.evalsha(getScriptSha(script), keyCount, ...args);
		} catch (error) {
			if (!isNoScriptError(error)) {
				throw error;
			}
			return await this.client.eval(script, keyCount, ...args);
		}
	}

	private async executeJsonScript<T>(
		command: string,
		script: string,
		keyCount: number,
		...args: Array<string | number>
	): Promise<T> {
		const result = await this.executeScript(command, script, keyCount, ...args);
		try {
			return JSON.parse(String(result)) as T;
		} catch (error) {
			throw new KVClientError({
				code: KVClientErrorCode.INVALID_RESPONSE,
				message: `KV request returned invalid JSON (${command}): ${getErrorMessage(error)}`,
			});
		}
	}

	private async executeRateLimitScript(
		command: string,
		script: string,
		key: string,
		nowMs: number,
		limit: number,
		windowMs: number,
		cost: number,
	): Promise<KVRateLimitResult> {
		const parsed = await this.executeJsonScript<KVRateLimitResult>(
			command,
			script,
			1,
			key,
			nowMs,
			limit,
			windowMs,
			cost,
		);
		return normalizeRateLimitResult(parsed);
	}

	private normalizeError(command: string, error: unknown): KVClientError {
		if (error instanceof KVClientError) {
			return error;
		}
		if (isTimeoutError(error)) {
			return new KVClientError({
				code: KVClientErrorCode.TIMEOUT,
				message: `KV request timed out: ${command}`,
			});
		}
		return new KVClientError({
			code: KVClientErrorCode.REQUEST_FAILED,
			message: `KV request failed (${command}): ${getErrorMessage(error)}`,
		});
	}
}

function parseClusterNodesFromUrl(url: string): Array<{
	host: string;
	port: number;
}> {
	try {
		const parsed = new URL(url);
		return [{host: parsed.hostname, port: Number.parseInt(parsed.port || '6379', 10)}];
	} catch {
		return [{host: '127.0.0.1', port: 6379}];
	}
}

function createRetryStrategy(): (times: number) => number {
	return (times: number) => {
		const backoffMs = Math.min(times * 100, 2000);
		return backoffMs;
	};
}

function normalizeStringOrNull(value: unknown): string | null {
	if (value === null || value === undefined) {
		return null;
	}
	return String(value);
}

function normalizeRateLimitResult(result: KVRateLimitResult): KVRateLimitResult {
	return {
		allowed: Boolean(result.allowed),
		limit: Number(result.limit),
		remaining: Number(result.remaining),
		resetAfterMs: Number(result.resetAfterMs),
		resetAtMs: Number(result.resetAtMs),
		retryAfterMs: Number(result.retryAfterMs),
	};
}

function getScriptSha(script: string): string {
	const cached = SCRIPT_SHA_CACHE.get(script);
	if (cached !== undefined) {
		return cached;
	}
	const sha = createHash('sha1').update(script).digest('hex');
	SCRIPT_SHA_CACHE.set(script, sha);
	return sha;
}

function isNoScriptError(error: unknown): boolean {
	if (!(error instanceof Error)) {
		return false;
	}
	return error.message.includes('NOSCRIPT');
}

function isTimeoutError(error: unknown): boolean {
	if (!(error instanceof Error)) {
		return false;
	}
	const errorWithCode = error as Error & {
		code?: string;
	};
	if (errorWithCode.code === 'ETIMEDOUT' || errorWithCode.code === 'ESOCKETTIMEDOUT') {
		return true;
	}
	const message = error.message.toLowerCase();
	return message.includes('timed out') || message.includes('timeout');
}

function getErrorMessage(error: unknown): string {
	if (error instanceof Error) {
		return error.message;
	}
	return String(error);
}
