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

interface RateLimitServiceOptions {
	globalWindowMs?: number;
}

function millisecondsToDecimalSeconds(milliseconds: number): number {
	if (milliseconds <= 0) {
		return 0;
	}
	return milliseconds / 1000;
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
