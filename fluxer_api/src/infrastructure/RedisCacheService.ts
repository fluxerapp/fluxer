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
import {ICacheService} from './ICacheService';

export class RedisCacheService extends ICacheService {
	private redis: Redis;

	constructor(redis: Redis) {
		super();
		this.redis = redis;
	}

	async get<T>(key: string): Promise<T | null> {
		const value = await this.redis.get(key);
		if (value == null) return null;

		try {
			return JSON.parse(value);
		} catch {
			return null;
		}
	}

	async set<T>(key: string, value: T, ttlSeconds?: number): Promise<void> {
		const serializedValue = JSON.stringify(value);

		if (ttlSeconds) {
			await this.redis.setex(key, ttlSeconds, serializedValue);
		} else {
			await this.redis.set(key, serializedValue);
		}
	}

	async delete(key: string): Promise<void> {
		await this.redis.del(key);
	}

	async getAndDelete<T>(key: string): Promise<T | null> {
		const pipeline = this.redis.multi();
		pipeline.get(key);
		pipeline.del(key);

		const results = await pipeline.exec();
		if (!results || results.length === 0) {
			return null;
		}

		const [getResult] = results;
		if (!getResult || getResult[1] == null) {
			return null;
		}

		try {
			return JSON.parse(getResult[1] as string);
		} catch {
			return null;
		}
	}

	async exists(key: string): Promise<boolean> {
		const result = await this.redis.exists(key);
		return result === 1;
	}

	async expire(key: string, ttlSeconds: number): Promise<void> {
		await this.redis.expire(key, ttlSeconds);
	}

	async ttl(key: string): Promise<number> {
		return await this.redis.ttl(key);
	}

	async mget<T>(keys: Array<string>): Promise<Array<T | null>> {
		if (keys.length === 0) return [];

		const values = await this.redis.mget(...keys);
		return values.map((value) => {
			if (value == null) return null;
			try {
				return JSON.parse(value);
			} catch {
				return null;
			}
		});
	}

	async mset<T>(entries: Array<{key: string; value: T; ttlSeconds?: number}>): Promise<void> {
		if (entries.length === 0) return;

		const withoutTtl: Array<{key: string; value: T}> = [];
		const withTtl: Array<{key: string; value: T; ttlSeconds: number}> = [];

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

		const pipeline = this.redis.pipeline();

		if (withoutTtl.length > 0) {
			const flatArgs: Array<string> = [];
			for (const entry of withoutTtl) {
				flatArgs.push(entry.key, JSON.stringify(entry.value));
			}
			pipeline.mset(...flatArgs);
		}

		for (const entry of withTtl) {
			pipeline.setex(entry.key, entry.ttlSeconds, JSON.stringify(entry.value));
		}

		await pipeline.exec();
	}

	async deletePattern(pattern: string): Promise<number> {
		const redisPattern = pattern.replace(/\*/g, '*');
		const keys = await this.redis.keys(redisPattern);
		if (keys.length === 0) return 0;

		await this.redis.del(...keys);
		return keys.length;
	}

	async acquireLock(key: string, ttlSeconds: number): Promise<string | null> {
		const token = Math.random().toString(36).substring(2, 15);
		const lockKey = `lock:${key}`;

		const result = await this.redis.set(lockKey, token, 'EX', ttlSeconds, 'NX');
		return result === 'OK' ? token : null;
	}

	async releaseLock(key: string, token: string): Promise<boolean> {
		const lockKey = `lock:${key}`;

		const luaScript = `
			if redis.call("GET", KEYS[1]) == ARGV[1] then
				return redis.call("DEL", KEYS[1])
			else
				return 0
			end
		`;

		const result = (await this.redis.eval(luaScript, 1, lockKey, token)) as number;
		return result === 1;
	}

	async getAndRenewTtl<T>(key: string, newTtlSeconds: number): Promise<T | null> {
		const pipeline = this.redis.pipeline();
		pipeline.get(key);
		pipeline.expire(key, newTtlSeconds);

		const results = await pipeline.exec();
		if (!results) return null;

		const [getResult] = results;
		if (!getResult || getResult[1] == null) return null;

		try {
			return JSON.parse(getResult[1] as string);
		} catch {
			return null;
		}
	}

	async publish(channel: string, message: string): Promise<void> {
		await this.redis.publish(channel, message);
	}
}
