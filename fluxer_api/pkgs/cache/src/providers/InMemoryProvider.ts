// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	formatLockKey,
	generateLockToken,
	validateLockKey,
	validateLockToken,
} from '@pkgs/cache/src/CacheLockValidation';
import {type CacheLookupResult, ICacheService} from '@pkgs/cache/src/ICacheService';

interface CacheEntry<T> {
	value: T;
	expiresAt?: number;
}

interface InMemoryProviderConfig {
	maxSize?: number;
	cleanupIntervalMs?: number;
}

export class InMemoryProvider extends ICacheService {
	private cache = new Map<string, CacheEntry<unknown>>();
	private sets = new Map<string, Set<string>>();
	private locks = new Map<
		string,
		{
			token: string;
			expiresAt: number;
		}
	>();
	private maxSize: number;
	private cleanupInterval?: NodeJS.Timeout;

	constructor(config: InMemoryProviderConfig = {}) {
		super();
		this.maxSize = config.maxSize ?? 10000;
		if (config.cleanupIntervalMs) {
			this.cleanupInterval = setInterval(() => this.cleanup(), config.cleanupIntervalMs);
		}
	}

	private cleanup(): void {
		const now = Date.now();
		for (const [key, entry] of this.cache.entries()) {
			if (entry.expiresAt && entry.expiresAt <= now) {
				this.cache.delete(key);
			}
		}
		for (const [key, lock] of this.locks.entries()) {
			if (lock.expiresAt <= now) {
				this.locks.delete(key);
			}
		}
	}

	private isExpired(entry: CacheEntry<unknown>): boolean {
		if (!entry.expiresAt) return false;
		return Date.now() >= entry.expiresAt;
	}

	private evictIfNeeded(): void {
		if (this.cache.size >= this.maxSize) {
			const firstKey = this.cache.keys().next().value;
			if (firstKey !== undefined) {
				this.cache.delete(firstKey);
			}
		}
	}

	async getEntry<T>(key: string): Promise<CacheLookupResult<T>> {
		const entry = this.cache.get(key) as CacheEntry<T> | undefined;
		if (!entry) return {hit: false};
		if (this.isExpired(entry)) {
			this.cache.delete(key);
			return {hit: false};
		}
		return {hit: true, value: entry.value};
	}

	async set<T>(key: string, value: T, ttlSeconds?: number): Promise<void> {
		this.evictIfNeeded();
		const entry: CacheEntry<T> = {
			value,
			expiresAt: ttlSeconds ? Date.now() + ttlSeconds * 1000 : undefined,
		};
		this.cache.set(key, entry);
	}

	protected async deleteEntry(key: string): Promise<void> {
		this.cache.delete(key);
	}

	async getAndDelete<T>(key: string): Promise<T | null> {
		const value = await this.get<T>(key);
		if (value !== null) {
			this.cache.delete(key);
		}
		return value;
	}

	async exists(key: string): Promise<boolean> {
		const entry = this.cache.get(key);
		if (!entry) return false;
		if (this.isExpired(entry)) {
			this.cache.delete(key);
			return false;
		}
		return true;
	}

	async expire(key: string, ttlSeconds: number): Promise<void> {
		const entry = this.cache.get(key);
		if (entry && !this.isExpired(entry)) {
			entry.expiresAt = Date.now() + ttlSeconds * 1000;
		}
	}

	async ttl(key: string): Promise<number> {
		const entry = this.cache.get(key);
		if (!entry || this.isExpired(entry)) {
			return -2;
		}
		if (!entry.expiresAt) {
			return -1;
		}
		const ttlMs = entry.expiresAt - Date.now();
		return Math.max(0, Math.floor(ttlMs / 1000));
	}

	async mget<T>(keys: Array<string>): Promise<Array<T | null>> {
		const results: Array<T | null> = [];
		for (const key of keys) {
			results.push(await this.get<T>(key));
		}
		return results;
	}

	async mset<T>(
		entries: Array<{
			key: string;
			value: T;
			ttlSeconds?: number;
		}>,
	): Promise<void> {
		for (const entry of entries) {
			await this.set(entry.key, entry.value, entry.ttlSeconds);
		}
	}

	async deletePattern(pattern: string): Promise<number> {
		const regex = new RegExp(pattern.replace(/\*/g, '.*'));
		let deletedCount = 0;
		for (const key of this.cache.keys()) {
			if (regex.test(key)) {
				this.cache.delete(key);
				deletedCount++;
			}
		}
		return deletedCount;
	}

	async acquireLock(key: string, ttlSeconds: number): Promise<string | null> {
		validateLockKey(key);
		const lockKey = formatLockKey(key);
		const existingLock = this.locks.get(lockKey);
		if (existingLock && existingLock.expiresAt > Date.now()) {
			return null;
		}
		const token = generateLockToken();
		this.locks.set(lockKey, {
			token,
			expiresAt: Date.now() + ttlSeconds * 1000,
		});
		return token;
	}

	async releaseLock(key: string, token: string): Promise<boolean> {
		validateLockKey(key);
		validateLockToken(token);
		const lockKey = formatLockKey(key);
		const lock = this.locks.get(lockKey);
		if (!lock || lock.token !== token) {
			return false;
		}
		this.locks.delete(lockKey);
		return true;
	}

	async extendLock(key: string, token: string, ttlSeconds: number): Promise<boolean> {
		validateLockKey(key);
		validateLockToken(token);
		const lockKey = formatLockKey(key);
		const lock = this.locks.get(lockKey);
		if (!lock || lock.token !== token || lock.expiresAt <= Date.now()) {
			return false;
		}
		this.locks.set(lockKey, {
			token,
			expiresAt: Date.now() + ttlSeconds * 1000,
		});
		return true;
	}

	async getAndRenewTtl<T>(key: string, newTtlSeconds: number): Promise<T | null> {
		const value = await this.get<T>(key);
		if (value !== null) {
			await this.expire(key, newTtlSeconds);
		}
		return value;
	}

	async publish(_channel: string, _message: string): Promise<void> {
		return;
	}

	async sadd(key: string, member: string, ttlSeconds?: number): Promise<void> {
		let set = this.sets.get(key);
		if (!set) {
			set = new Set<string>();
			this.sets.set(key, set);
		}
		set.add(member);
		if (ttlSeconds) {
			await this.set(`${key}:expiry`, {}, ttlSeconds);
		}
	}

	async srem(key: string, member: string): Promise<void> {
		const set = this.sets.get(key);
		if (set) {
			set.delete(member);
			if (set.size === 0) {
				this.sets.delete(key);
			}
		}
	}

	async smembers(key: string): Promise<Set<string>> {
		const expiryExists = await this.exists(`${key}:expiry`);
		if (!expiryExists && this.sets.has(key)) {
			return new Set();
		}
		return this.sets.get(key) ?? new Set<string>();
	}

	async sismember(key: string, member: string): Promise<boolean> {
		const set = this.sets.get(key);
		if (!set) return false;
		const expiryExists = await this.exists(`${key}:expiry`);
		if (!expiryExists) {
			return false;
		}
		return set.has(member);
	}

	destroy(): void {
		if (this.cleanupInterval) {
			clearInterval(this.cleanupInterval);
			this.cleanupInterval = undefined;
		}
		this.cache.clear();
		this.sets.clear();
		this.locks.clear();
	}
}
