// SPDX-License-Identifier: AGPL-3.0-or-later

const CACHE_INFLIGHT_MAX_ENTRIES = 10000;

interface CacheMSetEntry<T> {
	key: string;
	value: T;
	ttlSeconds?: number;
}

export type CacheLookupResult<T> = {hit: true; value: T} | {hit: false};

type CacheTtlSeconds<T> = number | ((value: T) => number);

export abstract class ICacheService {
	private readonly inflightValues = new Map<string, Promise<unknown>>();

	abstract getEntry<T>(key: string): Promise<CacheLookupResult<T>>;

	abstract set<T>(key: string, value: T, ttlSeconds?: number): Promise<void>;

	abstract delete(key: string): Promise<void>;

	abstract getAndDelete<T>(key: string): Promise<T | null>;

	abstract exists(key: string): Promise<boolean>;

	abstract expire(key: string, ttlSeconds: number): Promise<void>;

	abstract ttl(key: string): Promise<number>;

	abstract mget<T>(keys: Array<string>): Promise<Array<T | null>>;

	abstract mset<T>(entries: Array<CacheMSetEntry<T>>): Promise<void>;

	abstract deletePattern(pattern: string): Promise<number>;

	abstract acquireLock(key: string, ttlSeconds: number): Promise<string | null>;

	abstract releaseLock(key: string, token: string): Promise<boolean>;

	abstract extendLock(key: string, token: string, ttlSeconds: number): Promise<boolean>;

	abstract getAndRenewTtl<T>(key: string, newTtlSeconds: number): Promise<T | null>;

	abstract publish(channel: string, message: string): Promise<void>;

	abstract sadd(key: string, member: string, ttlSeconds?: number): Promise<void>;

	abstract srem(key: string, member: string): Promise<void>;

	abstract smembers(key: string): Promise<Set<string>>;

	abstract sismember(key: string, member: string): Promise<boolean>;

	async get<T>(key: string): Promise<T | null> {
		const entry = await this.getEntry<T>(key);
		return entry.hit ? entry.value : null;
	}

	async getOrSet<T>(key: string, valueFactory: () => Promise<T>, ttlSeconds?: CacheTtlSeconds<T>): Promise<T> {
		const existing = await this.getEntry<T>(key);
		if (existing.hit) {
			return existing.value;
		}
		const inflight = this.inflightValues.get(key);
		if (inflight) {
			return (await inflight) as T;
		}
		if (this.inflightValues.size >= CACHE_INFLIGHT_MAX_ENTRIES) {
			return await this.produceAndStore(key, valueFactory, ttlSeconds);
		}
		const pending = this.produceAndStore(key, valueFactory, ttlSeconds).finally(() => {
			this.inflightValues.delete(key);
		});
		this.inflightValues.set(key, pending);
		return await pending;
	}

	private async produceAndStore<T>(
		key: string,
		valueFactory: () => Promise<T>,
		ttlSeconds?: CacheTtlSeconds<T>,
	): Promise<T> {
		const value = await valueFactory();
		await this.set(key, value, typeof ttlSeconds === 'function' ? ttlSeconds(value) : ttlSeconds);
		return value;
	}
}
