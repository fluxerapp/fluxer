/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {IKVPipeline, IKVProvider, IKVSubscription} from '@fluxer/kv_client/src/IKVProvider';
import {
	type IKVLogger,
	type KVClientConfig,
	type ResolvedKVClientConfig,
	resolveKVClientConfig,
} from '@fluxer/kv_client/src/KVClientConfig';
import {KVClientError, KVClientErrorCode} from '@fluxer/kv_client/src/KVClientError';
import {
	createStringEntriesFromPairs,
	createZSetMembersFromScorePairs,
	normalizeScoreBound,
	parseRangeByScoreArguments,
	parseSetArguments,
} from '@fluxer/kv_client/src/KVCommandArguments';
import {KVPipeline} from '@fluxer/kv_client/src/KVPipeline';
import {KVSubscription} from '@fluxer/kv_client/src/KVSubscription';
import Redis, {Cluster} from 'ioredis';

const RELEASE_LOCK_SCRIPT = `
if redis.call('GET', KEYS[1]) == ARGV[1] then
	return redis.call('DEL', KEYS[1])
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

const SCHEDULE_BULK_DELETION_SCRIPT = `
redis.call('ZADD', KEYS[1], ARGV[1], ARGV[2])
redis.call('SET', KEYS[2], ARGV[2])
return 1
`;

const REMOVE_BULK_DELETION_SCRIPT = `
local value = redis.call('GET', KEYS[2])
if not value then
	return 0
end
redis.call('ZREM', KEYS[1], value)
redis.call('DEL', KEYS[2])
return 1
`;

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
		return await this.execute('mget', async () => this.client.mget(...keys));
	}

	async mset(...args: Array<string>): Promise<void> {
		const entries = createStringEntriesFromPairs(args);
		if (entries.length === 0) {
			return;
		}

		const pairs = entries.flatMap((entry) => [entry.key, entry.value]);
		await this.execute('mset', async () => {
			await this.client.mset(...pairs);
		});
	}

	async del(...keys: Array<string>): Promise<number> {
		if (keys.length === 0) {
			return 0;
		}
		return await this.execute('del', async () => this.client.del(...keys));
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

	async releaseLock(key: string, token: string): Promise<boolean> {
		const result = await this.execute('releaseLock', async () => this.client.eval(RELEASE_LOCK_SCRIPT, 1, key, token));
		return Number(result) === 1;
	}

	async renewSnowflakeNode(key: string, instanceId: string, ttlSeconds: number): Promise<boolean> {
		const result = await this.execute('renewSnowflakeNode', async () =>
			this.client.eval(RENEW_SNOWFLAKE_SCRIPT, 1, key, instanceId, ttlSeconds),
		);
		return Number(result) === 1;
	}

	async tryConsumeTokens(
		key: string,
		requested: number,
		maxTokens: number,
		refillRate: number,
		refillIntervalMs: number,
	): Promise<number> {
		const now = Date.now();
		const result = await this.execute('tryConsumeTokens', async () =>
			this.client.eval(TRY_CONSUME_TOKENS_SCRIPT, 1, key, now, requested, maxTokens, refillRate, refillIntervalMs),
		);
		return Number(result);
	}

	async scheduleBulkDeletion(queueKey: string, secondaryKey: string, score: number, value: string): Promise<void> {
		await this.execute('scheduleBulkDeletion', async () => {
			await this.client.eval(SCHEDULE_BULK_DELETION_SCRIPT, 2, queueKey, secondaryKey, score, value);
		});
	}

	async removeBulkDeletion(queueKey: string, secondaryKey: string): Promise<boolean> {
		const result = await this.execute('removeBulkDeletion', async () =>
			this.client.eval(REMOVE_BULK_DELETION_SCRIPT, 2, queueKey, secondaryKey),
		);
		return Number(result) === 1;
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

function parseClusterNodesFromUrl(url: string): Array<{host: string; port: number}> {
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

function isTimeoutError(error: unknown): boolean {
	if (!(error instanceof Error)) {
		return false;
	}

	const errorWithCode = error as Error & {code?: string};
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
