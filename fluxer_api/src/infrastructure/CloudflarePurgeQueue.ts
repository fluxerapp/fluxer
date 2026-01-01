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

import type {Redis} from 'ioredis';
import {Logger} from '~/Logger';

export interface ICloudflarePurgeQueue {
	addUrls(urls: Array<string>): Promise<void>;
	getBatch(count: number): Promise<Array<string>>;
	getQueueSize(): Promise<number>;
	clear(): Promise<void>;
	tryConsumeTokens(
		requestedTokens: number,
		maxTokens: number,
		refillRate: number,
		refillIntervalMs: number,
	): Promise<number>;
	getAvailableTokens(maxTokens: number, refillRate: number, refillIntervalMs: number): Promise<number>;
}

export class CloudflarePurgeQueue implements ICloudflarePurgeQueue {
	private redis: Redis;
	private readonly queueKey = 'cloudflare:purge:urls';
	private readonly tokenBucketKey = 'cloudflare:purge:token_bucket';

	constructor(redis: Redis) {
		this.redis = redis;
	}

	async addUrls(urls: Array<string>): Promise<void> {
		if (urls.length === 0) {
			return;
		}

		const normalized = Array.from(
			new Set(urls.map((url) => this.normalizePrefix(url)).filter((prefix) => prefix !== '')),
		);

		if (normalized.length === 0) {
			return;
		}

		try {
			await this.redis.sadd(this.queueKey, ...normalized);
			Logger.debug({count: normalized.length}, 'Added prefixes to Cloudflare purge queue');
		} catch (error) {
			Logger.error({error, urls}, 'Failed to add prefixes to Cloudflare purge queue');
			throw error;
		}
	}

	private normalizePrefix(rawUrl: string): string {
		const trimmed = rawUrl.trim();
		if (trimmed === '') {
			return '';
		}

		try {
			const parsed = new URL(trimmed);
			return `${parsed.host}${parsed.pathname}`;
		} catch {
			const [withoutQuery] = trimmed.split(/[?#]/);
			return withoutQuery.replace(/^https?:\/\//, '');
		}
	}

	async getBatch(count: number): Promise<Array<string>> {
		if (count <= 0) {
			return [];
		}

		try {
			const urls = await this.redis.spop(this.queueKey, count);
			return Array.isArray(urls) ? urls : urls ? [urls] : [];
		} catch (error) {
			Logger.error({error, count}, 'Failed to get batch from Cloudflare purge queue');
			throw error;
		}
	}

	async getQueueSize(): Promise<number> {
		try {
			return await this.redis.scard(this.queueKey);
		} catch (error) {
			Logger.error({error}, 'Failed to get Cloudflare purge queue size');
			throw error;
		}
	}

	async clear(): Promise<void> {
		try {
			await this.redis.del(this.queueKey);
			Logger.debug('Cleared Cloudflare purge queue');
		} catch (error) {
			Logger.error({error}, 'Failed to clear Cloudflare purge queue');
			throw error;
		}
	}

	async tryConsumeTokens(
		requestedTokens: number,
		maxTokens: number,
		refillRate: number,
		refillIntervalMs: number,
	): Promise<number> {
		try {
			const luaScript = `
				local key = KEYS[1]
				local now = tonumber(ARGV[1])
				local requested = tonumber(ARGV[2])
				local maxTokens = tonumber(ARGV[3])
				local refillRate = tonumber(ARGV[4])
				local refillInterval = tonumber(ARGV[5])
				local ttl = tonumber(ARGV[6])

				-- Get current bucket state
				local bucketData = redis.call("GET", key)
				local tokens
				local lastRefill

				if bucketData then
					local parsed = cjson.decode(bucketData)
					tokens = parsed.tokens
					lastRefill = parsed.lastRefill

					-- Calculate tokens to add based on time elapsed
					local elapsed = now - lastRefill
					local tokensToAdd = math.floor(elapsed / refillInterval) * refillRate

					if tokensToAdd > 0 then
						tokens = math.min(maxTokens, tokens + tokensToAdd)
						lastRefill = now
					end
				else
					-- Initialize bucket with full tokens
					tokens = maxTokens
					lastRefill = now
				end

				-- Try to consume tokens
				local consumed = 0
				if tokens >= requested then
					consumed = requested
					tokens = tokens - requested
				elseif tokens > 0 then
					consumed = tokens
					tokens = 0
				end

				-- Save updated state
				local newData = cjson.encode({tokens = tokens, lastRefill = lastRefill})
				redis.call("SET", key, newData, "EX", ttl)

				return consumed
			`;

			const consumed = (await this.redis.eval(
				luaScript,
				1,
				this.tokenBucketKey,
				Date.now().toString(),
				requestedTokens.toString(),
				maxTokens.toString(),
				refillRate.toString(),
				refillIntervalMs.toString(),
				'3600',
			)) as number;

			Logger.debug({requested: requestedTokens, consumed}, 'Tried to consume tokens from bucket');
			return consumed;
		} catch (error) {
			Logger.error({error, requestedTokens}, 'Failed to consume tokens');
			throw error;
		}
	}

	async getAvailableTokens(maxTokens: number, refillRate: number, refillIntervalMs: number): Promise<number> {
		try {
			const now = Date.now();
			const bucketData = await this.redis.get(this.tokenBucketKey);

			if (!bucketData) {
				return maxTokens;
			}

			const parsed = JSON.parse(bucketData);
			let tokens = parsed.tokens;
			const lastRefill = parsed.lastRefill;

			const elapsed = now - lastRefill;
			const tokensToAdd = Math.floor(elapsed / refillIntervalMs) * refillRate;

			if (tokensToAdd > 0) {
				tokens = Math.min(maxTokens, tokens + tokensToAdd);
			}

			return tokens;
		} catch (error) {
			Logger.error({error}, 'Failed to get available tokens');
			throw error;
		}
	}

	async resetTokenBucket(): Promise<void> {
		try {
			await this.redis.del(this.tokenBucketKey);
			Logger.debug('Reset token bucket');
		} catch (error) {
			Logger.error({error}, 'Failed to reset token bucket');
			throw error;
		}
	}
}

export class NoopCloudflarePurgeQueue implements ICloudflarePurgeQueue {
	async addUrls(_urls: Array<string>): Promise<void> {}

	async getBatch(_count: number): Promise<Array<string>> {
		return [];
	}

	async getQueueSize(): Promise<number> {
		return 0;
	}

	async clear(): Promise<void> {}

	async tryConsumeTokens(
		_requestedTokens: number,
		_maxTokens: number,
		_refillRate: number,
		_refillIntervalMs: number,
	): Promise<number> {
		return 0;
	}

	async getAvailableTokens(maxTokens: number, _refillRate: number, _refillIntervalMs: number): Promise<number> {
		return maxTokens;
	}
}
