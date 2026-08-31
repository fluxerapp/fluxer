// SPDX-License-Identifier: AGPL-3.0-or-later

import {classifyKeyType} from '@pkgs/cache/src/CacheKeyClassification';
import {
	formatLockKey,
	generateLockToken,
	validateLockKey,
	validateLockToken,
} from '@pkgs/cache/src/CacheLockValidation';
import type {CacheLogger, CacheTelemetry} from '@pkgs/cache/src/CacheProviderTypes';
import {parseCachedValue, safeJsonParse, serializeValue} from '@pkgs/cache/src/CacheSerialization';
import {type CacheLookupResult, ICacheService} from '@pkgs/cache/src/ICacheService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

interface KVCacheProviderConfig {
	client: IKVProvider;
	cacheName?: string;
	logger?: CacheLogger;
	telemetry?: CacheTelemetry;
}

export class KVCacheProvider extends ICacheService {
	private client: IKVProvider;
	private cacheName: string;
	private logger?: CacheLogger;
	private telemetry?: CacheTelemetry;

	constructor(config: KVCacheProviderConfig) {
		super();
		this.client = config.client;
		this.cacheName = config.cacheName ?? 'kv';
		this.logger = config.logger;
		this.telemetry = config.telemetry;
	}

	private async instrumented<T>(
		operation: string,
		key: string,
		fn: () => Promise<T>,
		statusResolver?: (result: T) => string,
	): Promise<T> {
		const start = Date.now();
		try {
			const result = await fn();
			const duration = Date.now() - start;
			const status = statusResolver ? statusResolver(result) : 'success';
			this.telemetry?.recordCounter({
				name: 'cache.operation',
				dimensions: {operation, cache_name: this.cacheName, status, key_type: classifyKeyType(key)},
			});
			this.telemetry?.recordHistogram({
				name: 'cache.operation_latency',
				valueMs: duration,
				dimensions: {operation, cache_name: this.cacheName},
			});
			return result;
		} catch (error) {
			const duration = Date.now() - start;
			this.telemetry?.recordCounter({
				name: 'cache.operation',
				dimensions: {operation, cache_name: this.cacheName, status: 'error', key_type: classifyKeyType(key)},
			});
			this.telemetry?.recordHistogram({
				name: 'cache.operation_latency',
				valueMs: duration,
				dimensions: {operation, cache_name: this.cacheName},
			});
			throw error;
		}
	}

	async getEntry<T>(key: string): Promise<CacheLookupResult<T>> {
		return this.instrumented(
			'get',
			key,
			async (): Promise<CacheLookupResult<T>> => {
				const value = await this.client.get(key);
				if (value == null) return {hit: false};
				return parseCachedValue<T>(value, this.logger);
			},
			(result) => (result.hit ? 'hit' : 'miss'),
		);
	}

	async set<T>(key: string, value: T, ttlSeconds?: number): Promise<void> {
		return this.instrumented('set', key, async () => {
			const serialized = serializeValue(value);
			if (ttlSeconds) {
				await this.client.setex(key, ttlSeconds, serialized);
			} else {
				await this.client.set(key, serialized);
			}
		});
	}

	protected async deleteEntry(key: string): Promise<void> {
		return this.instrumented('delete', key, async () => {
			await this.client.del(key);
		});
	}

	async getAndDelete<T>(key: string): Promise<T | null> {
		const value = await this.client.getdel(key);
		if (value == null) {
			return null;
		}
		return safeJsonParse<T>(value, this.logger);
	}

	async exists(key: string): Promise<boolean> {
		const result = await this.client.exists(key);
		return result === 1;
	}

	async expire(key: string, ttlSeconds: number): Promise<void> {
		await this.client.expire(key, ttlSeconds);
	}

	async ttl(key: string): Promise<number> {
		return await this.client.ttl(key);
	}

	async mget<T>(keys: Array<string>): Promise<Array<T | null>> {
		if (keys.length === 0) return [];
		const values = await this.client.mget(...keys);
		return values.map((value: string | null) => {
			if (value == null) return null;
			return safeJsonParse<T>(value, this.logger);
		});
	}

	async mset<T>(
		entries: Array<{
			key: string;
			value: T;
			ttlSeconds?: number;
		}>,
	): Promise<void> {
		if (entries.length === 0) return;
		const withoutTtl: Array<{
			key: string;
			value: T;
		}> = [];
		const withTtl: Array<{
			key: string;
			value: T;
			ttlSeconds: number;
		}> = [];
		for (const entry of entries) {
			if (entry.ttlSeconds) {
				withTtl.push({
					key: entry.key,
					value: entry.value,
					ttlSeconds: entry.ttlSeconds,
				});
			} else {
				withoutTtl.push({
					key: entry.key,
					value: entry.value,
				});
			}
		}
		const pipeline = this.client.pipeline();
		for (const entry of withoutTtl) {
			pipeline.set(entry.key, serializeValue(entry.value));
		}
		for (const entry of withTtl) {
			pipeline.setex(entry.key, entry.ttlSeconds, serializeValue(entry.value));
		}
		await pipeline.exec();
	}

	async deletePattern(pattern: string): Promise<number> {
		const keys = await this.client.scan(pattern, 1000);
		if (keys.length === 0) return 0;
		return await this.client.del(...keys);
	}

	async acquireLock(key: string, ttlSeconds: number): Promise<string | null> {
		validateLockKey(key);
		const token = generateLockToken();
		const lockKey = formatLockKey(key);
		return (await this.client.acquireLock(lockKey, token, ttlSeconds)) ? token : null;
	}

	async releaseLock(key: string, token: string): Promise<boolean> {
		validateLockKey(key);
		validateLockToken(token);
		const lockKey = formatLockKey(key);
		return await this.client.releaseLock(lockKey, token);
	}

	async extendLock(key: string, token: string, ttlSeconds: number): Promise<boolean> {
		validateLockKey(key);
		validateLockToken(token);
		const lockKey = formatLockKey(key);
		return await this.client.extendLock(lockKey, token, ttlSeconds);
	}

	async getAndRenewTtl<T>(key: string, newTtlSeconds: number): Promise<T | null> {
		const value = await this.client.getex(key, newTtlSeconds);
		if (value == null) return null;
		return safeJsonParse<T>(value, this.logger);
	}

	async publish(channel: string, message: string): Promise<void> {
		await this.client.publish(channel, message);
	}

	async sadd(key: string, member: string, ttlSeconds?: number): Promise<void> {
		const pipeline = this.client.pipeline();
		pipeline.sadd(key, member);
		if (ttlSeconds) {
			pipeline.expire(key, ttlSeconds);
		}
		await pipeline.exec();
	}

	async srem(key: string, member: string): Promise<void> {
		await this.client.srem(key, member);
	}

	async smembers(key: string): Promise<Set<string>> {
		const members = await this.client.smembers(key);
		return new Set(members);
	}

	async sismember(key: string, member: string): Promise<boolean> {
		const result = await this.client.sismember(key, member);
		return result === 1;
	}
}
