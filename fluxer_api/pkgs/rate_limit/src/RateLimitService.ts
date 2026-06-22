// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KVRateLimitResult} from '@pkgs/kv_client/src/IKVProvider';
import type {
	BucketConfig,
	IRateLimitService,
	RateLimitConfig,
	RateLimitResult,
} from '@pkgs/rate_limit/src/IRateLimitService';
import {RateLimitKeyFactory} from '@pkgs/rate_limit/src/internal/RateLimitKeyFactory';
import {assertPositiveFiniteNumber} from '@pkgs/rate_limit/src/internal/RateLimitValidation';

interface IRateLimitStore {
	checkLeakyBucketLimit(key: string, limit: number, windowMs: number, cost: number): Promise<KVRateLimitResult>;
	del(...keys: Array<string>): Promise<number>;
	scan(pattern: string, count: number): Promise<Array<string>>;
}

interface InMemoryLeakyBucketState {
	level: number;
	updatedAtMs: number;
}

interface RateLimitServiceOptions {
	globalWindowMs?: number;
	getCurrentTimeMs?: () => number;
}

class InMemoryRateLimitStore implements IRateLimitStore {
	private readonly leakyBucketState = new Map<string, InMemoryLeakyBucketState>();

	constructor(private readonly getCurrentTimeMs: () => number = () => Date.now()) {}

	async checkLeakyBucketLimit(key: string, limit: number, windowMs: number, cost: number): Promise<KVRateLimitResult> {
		const nowMs = this.getCurrentTimeMs();
		const capacity = Math.max(1, Math.floor(limit));
		const leakWindowMs = Math.max(1, Math.floor(windowMs));
		let bucket = this.leakyBucketState.get(key);
		if (!bucket) {
			bucket = {level: 0, updatedAtMs: nowMs};
			this.leakyBucketState.set(key, bucket);
		}
		drainLeakyBucket(bucket, capacity, leakWindowMs, nowMs);
		if (cost <= 0) {
			const remaining = Math.max(0, Math.floor(capacity - bucket.level));
			const resetAfterMs = leakyBucketResetAfterMs(bucket, capacity, leakWindowMs);
			return createStoreResult(true, capacity, remaining, nowMs, resetAfterMs, 0);
		}
		if (cost !== 1) {
			throw new Error(`Unsupported leaky bucket cost: ${cost}`);
		}
		if (bucket.level + cost > capacity) {
			const retryAfterMs = leakyBucketRetryAfterMs(bucket, capacity, leakWindowMs, cost);
			const resetAfterMs = leakyBucketResetAfterMs(bucket, capacity, leakWindowMs);
			return createStoreResult(false, capacity, 0, nowMs, Math.max(resetAfterMs, retryAfterMs), retryAfterMs);
		}
		bucket.level += cost;
		const remaining = Math.max(0, Math.floor(capacity - bucket.level));
		const resetAfterMs = leakyBucketResetAfterMs(bucket, capacity, leakWindowMs);
		return createStoreResult(true, capacity, remaining, nowMs, resetAfterMs, 0);
	}

	async del(...keys: Array<string>): Promise<number> {
		let deleted = 0;
		for (const key of keys) {
			const deletedBucket = this.leakyBucketState.delete(key);
			if (deletedBucket) {
				deleted++;
			}
		}
		return deleted;
	}

	async scan(pattern: string, count: number): Promise<Array<string>> {
		const limit = Math.max(1, Math.floor(count));
		const matcher = compileGlobMatcher(pattern);
		const matches = new Set<string>();
		for (const key of this.leakyBucketState.keys()) {
			if (matcher(key)) {
				matches.add(key);
				if (matches.size >= limit) break;
			}
		}
		return Array.from(matches);
	}
}

function compileGlobMatcher(pattern: string): (key: string) => boolean {
	const escaped = pattern
		.replace(/[.+^${}()|[\]\\]/g, '\\$&')
		.replace(/\*/g, '.*')
		.replace(/\?/g, '.');
	const regex = new RegExp(`^${escaped}$`);
	return (key: string) => regex.test(key);
}

function drainLeakyBucket(bucket: InMemoryLeakyBucketState, capacity: number, windowMs: number, nowMs: number): void {
	const elapsed = nowMs - bucket.updatedAtMs;
	if (elapsed <= 0) {
		return;
	}
	const leaked = (elapsed / windowMs) * capacity;
	bucket.level = Math.max(0, bucket.level - leaked);
	bucket.updatedAtMs = nowMs;
}

function leakyBucketRetryAfterMs(
	bucket: InMemoryLeakyBucketState,
	capacity: number,
	windowMs: number,
	cost: number,
): number {
	const overflow = bucket.level + cost - capacity;
	if (overflow <= 0) {
		return 0;
	}
	return Math.max(1, Math.ceil(overflow / (capacity / windowMs)));
}

function leakyBucketResetAfterMs(bucket: InMemoryLeakyBucketState, capacity: number, windowMs: number): number {
	if (bucket.level <= 0) {
		return 0;
	}
	return Math.max(0, Math.ceil(bucket.level / (capacity / windowMs)));
}

function millisecondsToDecimalSeconds(milliseconds: number): number {
	if (milliseconds <= 0) {
		return 0;
	}
	return milliseconds / 1000;
}

function createStoreResult(
	allowed: boolean,
	limit: number,
	remaining: number,
	nowMs: number,
	resetAfterMs: number,
	retryAfterMs: number,
): KVRateLimitResult {
	return {
		allowed,
		limit,
		remaining,
		resetAfterMs,
		resetAtMs: nowMs + resetAfterMs,
		retryAfterMs,
	};
}

function createRateLimitResult(result: KVRateLimitResult, global?: boolean): RateLimitResult {
	const resetAfterDecimal = millisecondsToDecimalSeconds(result.resetAfterMs);
	const retryAfterDecimal = millisecondsToDecimalSeconds(result.retryAfterMs);
	return {
		allowed: result.allowed,
		limit: result.limit,
		remaining: result.remaining,
		resetTime: new Date(result.resetAtMs),
		resetAfterDecimal,
		retryAfter: result.retryAfterMs > 0 ? Math.max(1, Math.ceil(result.retryAfterMs / 1000)) : undefined,
		retryAfterDecimal: result.retryAfterMs > 0 ? Math.max(0.001, retryAfterDecimal) : undefined,
		...(global !== undefined && {global}),
	};
}

export class RateLimitService implements IRateLimitService {
	private static readonly DEFAULT_GLOBAL_WINDOW_MS = 1000;
	private readonly keyFactory = new RateLimitKeyFactory();
	private readonly globalWindowMs: number;

	constructor(
		private readonly store: IRateLimitStore,
		options: RateLimitServiceOptions = {},
	) {
		this.globalWindowMs = options.globalWindowMs ?? RateLimitService.DEFAULT_GLOBAL_WINDOW_MS;
		assertPositiveFiniteNumber(this.globalWindowMs, 'globalWindowMs');
	}

	async checkLimit(config: RateLimitConfig): Promise<RateLimitResult> {
		const key = this.keyFactory.getIdentifierKey(config.identifier);
		const result = await this.store.checkLeakyBucketLimit(key, config.maxAttempts, config.windowMs, 1);
		return createRateLimitResult(result);
	}

	async peekLimit(config: RateLimitConfig): Promise<RateLimitResult> {
		const key = this.keyFactory.getIdentifierKey(config.identifier);
		const result = await this.store.checkLeakyBucketLimit(key, config.maxAttempts, config.windowMs, 0);
		return createRateLimitResult(result);
	}

	async checkBucketLimit(bucket: string, config: BucketConfig): Promise<RateLimitResult> {
		const key = this.keyFactory.getBucketKey(bucket);
		const result = await this.store.checkLeakyBucketLimit(key, config.limit, config.windowMs, 1);
		return createRateLimitResult(result);
	}

	async checkGlobalLimit(identifier: string, limit: number): Promise<RateLimitResult> {
		const key = this.keyFactory.getGlobalKey(identifier);
		const result = await this.store.checkLeakyBucketLimit(key, limit, this.globalWindowMs, 1);
		return createRateLimitResult(result, true);
	}

	async resetLimit(identifier: string): Promise<void> {
		const key = this.keyFactory.getIdentifierKey(identifier);
		await this.store.del(key);
	}

	async clearLimitsByIdentifierPrefix(identifierPrefix: string): Promise<number> {
		if (identifierPrefix.length === 0) {
			throw new Error('identifierPrefix must be non-empty');
		}
		const sentinelKey = this.keyFactory.getIdentifierKey(`${identifierPrefix}\x00`);
		const keyPrefix = sentinelKey.slice(0, -1);
		const pattern = `${keyPrefix}*`;
		let totalDeleted = 0;
		const batchSize = 256;
		while (true) {
			const keys = await this.store.scan(pattern, batchSize);
			if (keys.length === 0) break;
			totalDeleted += await this.store.del(...keys);
			if (keys.length < batchSize) break;
		}
		return totalDeleted;
	}
}

function createRateLimitService(
	store: IRateLimitStore | null,
	options: RateLimitServiceOptions = {},
): RateLimitService | null {
	if (!store) {
		return null;
	}
	return new RateLimitService(store, options);
}
