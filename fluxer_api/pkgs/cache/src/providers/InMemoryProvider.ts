// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	formatLockKey,
	generateLockToken,
	validateLockKey,
	validateLockToken,
} from '@pkgs/cache/src/CacheLockValidation';
import {type CacheLookupResult, ICacheService} from '@pkgs/cache/src/ICacheService';

interface CacheEntry {
	expiresAt?: number;
}

interface CacheValueEntry extends CacheEntry {
	kind: 'value';
	value: unknown;
}

interface CacheSetEntry extends CacheEntry {
	kind: 'set';
	value: Set<string>;
}

interface CacheLockEntry extends CacheEntry {
	token: string;
	expiresAt: number;
}

interface InMemoryProviderConfig {
	maxSize?: number;
	cleanupIntervalMs?: number;
}

export class InMemoryProvider extends ICacheService {
	private readonly cache = new Map<string, CacheValueEntry | CacheSetEntry>();
	private readonly locks = new Map<string, CacheLockEntry>();
	private readonly maxSize: number;
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
			if (this.isExpired(entry, now)) {
				this.cache.delete(key);
			}
		}
		for (const [key, lock] of this.locks.entries()) {
			if (this.isExpired(lock, now)) {
				this.locks.delete(key);
			}
		}
	}

	private isExpired(entry: CacheEntry, now = Date.now()): boolean {
		return entry.expiresAt !== undefined && now >= entry.expiresAt;
	}

	private getLiveEntry(key: string): CacheValueEntry | CacheSetEntry | undefined {
		const entry = this.cache.get(key);
		if (entry && this.isExpired(entry)) {
			this.cache.delete(key);
			return undefined;
		}
		return entry;
	}

	private getValueEntry(key: string): CacheValueEntry | undefined {
		const entry = this.getLiveEntry(key);
		if (entry && entry.kind !== 'value') {
			throw new Error('Cache key contains a set, not a value.');
		}
		return entry;
	}

	private getSetEntry(key: string): CacheSetEntry | undefined {
		const entry = this.getLiveEntry(key);
		if (entry && entry.kind !== 'set') {
			throw new Error('Cache key contains a value, not a set.');
		}
		return entry;
	}

	private getLiveLock(key: string): CacheLockEntry | undefined {
		const lock = this.locks.get(key);
		if (lock && this.isExpired(lock)) {
			this.locks.delete(key);
			return undefined;
		}
		return lock;
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
		const entry = this.getValueEntry(key);
		return entry ? {hit: true, value: entry.value as T} : {hit: false};
	}

	async set<T>(key: string, value: T, ttlSeconds?: number): Promise<void> {
		if (!this.cache.has(key)) {
			this.evictIfNeeded();
		}
		const entry: CacheValueEntry = {
			kind: 'value',
			value,
			expiresAt: ttlSeconds ? Date.now() + ttlSeconds * 1000 : undefined,
		};
		this.cache.set(key, entry);
	}

	protected async deleteEntry(key: string): Promise<void> {
		this.cache.delete(key);
	}

	async getAndDelete<T>(key: string): Promise<T | null> {
		const entry = this.getValueEntry(key);
		this.cache.delete(key);
		return entry ? (entry.value as T) : null;
	}

	async exists(key: string): Promise<boolean> {
		return this.getLiveEntry(key) !== undefined;
	}

	async expire(key: string, ttlSeconds: number): Promise<void> {
		const entry = this.getLiveEntry(key);
		if (entry) {
			entry.expiresAt = Date.now() + ttlSeconds * 1000;
		}
	}

	async ttl(key: string): Promise<number> {
		const entry = this.getLiveEntry(key);
		if (!entry) {
			return -2;
		}
		if (entry.expiresAt === undefined) {
			return -1;
		}
		const ttlMs = entry.expiresAt - Date.now();
		return Math.max(0, Math.floor(ttlMs / 1000));
	}

	async mget<T>(keys: Array<string>): Promise<Array<T | null>> {
		return keys.map((key) => {
			const entry = this.getLiveEntry(key);
			return entry?.kind === 'value' ? (entry.value as T) : null;
		});
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
		if (this.getLiveLock(lockKey)) {
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
		const lock = this.getLiveLock(lockKey);
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
		const lock = this.getLiveLock(lockKey);
		if (!lock || lock.token !== token) {
			return false;
		}
		lock.expiresAt = Date.now() + ttlSeconds * 1000;
		return true;
	}

	async getAndRenewTtl<T>(key: string, newTtlSeconds: number): Promise<T | null> {
		const entry = this.getValueEntry(key);
		if (!entry) {
			return null;
		}
		entry.expiresAt = Date.now() + newTtlSeconds * 1000;
		return entry.value as T;
	}

	async publish(_channel: string, _message: string): Promise<void> {
		return;
	}

	async sadd(key: string, member: string, ttlSeconds?: number): Promise<void> {
		let entry = this.getSetEntry(key);
		if (!entry) {
			this.evictIfNeeded();
			entry = {kind: 'set', value: new Set<string>()};
			this.cache.set(key, entry);
		}
		entry.value.add(member);
		if (ttlSeconds) {
			entry.expiresAt = Date.now() + ttlSeconds * 1000;
		}
	}

	async srem(key: string, member: string): Promise<void> {
		const entry = this.getSetEntry(key);
		if (!entry) {
			return;
		}
		entry.value.delete(member);
		if (entry.value.size === 0) {
			this.cache.delete(key);
		}
	}

	async smembers(key: string): Promise<Set<string>> {
		return new Set(this.getSetEntry(key)?.value);
	}

	async sismember(key: string, member: string): Promise<boolean> {
		return this.getSetEntry(key)?.value.has(member) ?? false;
	}

	destroy(): void {
		if (this.cleanupInterval) {
			clearInterval(this.cleanupInterval);
			this.cleanupInterval = undefined;
		}
		this.cache.clear();
		this.locks.clear();
	}
}
