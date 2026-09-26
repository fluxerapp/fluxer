// SPDX-License-Identifier: AGPL-3.0-or-later

const CACHE_INFLIGHT_MAX_ENTRIES = 10000;
const CACHE_INFLIGHT_JOIN_RETRIES = 1;
const CACHE_PRODUCE_TIMEOUT_MS = 15000;
const CACHE_PRODUCE_TIMEOUT_MESSAGE = 'Cache produce timed out';

interface CacheMSetEntry<T> {
	key: string;
	value: T;
	ttlSeconds?: number;
}

interface CacheProduceTracking {
	generation: number;
	produces: number;
}

interface CacheProduceAbandonment {
	abandoned: boolean;
}

export type CacheLookupResult<T> = {hit: true; value: T} | {hit: false};

type CacheTtlSeconds<T> = number | ((value: T) => number);

type CacheJoinResult<T> = {joined: true; value: T} | {joined: false; error: unknown};

export abstract class ICacheService {
	private readonly inflightValues = new Map<string, Promise<unknown>>();
	private readonly produceInvalidations = new Map<string, CacheProduceTracking>();

	abstract getEntry<T>(key: string): Promise<CacheLookupResult<T>>;

	abstract set<T>(key: string, value: T, ttlSeconds?: number): Promise<void>;

	protected abstract deleteEntry(key: string): Promise<void>;

	async delete(key: string): Promise<void> {
		const tracked = this.produceInvalidations.get(key);
		if (tracked) {
			tracked.generation += 1;
		}
		await this.deleteEntry(key);
	}

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

	async getOrSet<T>(
		key: string,
		valueFactory: () => Promise<T>,
		ttlSeconds?: CacheTtlSeconds<T>,
		produceTimeoutMs: number = CACHE_PRODUCE_TIMEOUT_MS,
	): Promise<T> {
		let generation = this.trackProduce(key);
		try {
			for (let attempt = 0; ; attempt++) {
				const existing = await this.getEntry<T>(key);
				if (existing.hit) {
					return existing.value;
				}
				const inflight = this.inflightValues.get(key);
				if (!inflight) {
					return await this.produceSingleFlight(key, valueFactory, ttlSeconds, generation, produceTimeoutMs);
				}
				const joined = await this.joinInflight<T>(inflight);
				if (joined.joined) {
					return joined.value;
				}
				if (attempt >= CACHE_INFLIGHT_JOIN_RETRIES) {
					throw joined.error;
				}
				generation = this.currentGeneration(key);
			}
		} finally {
			this.releaseProduce(key);
		}
	}

	private trackProduce(key: string): number {
		const tracked = this.produceInvalidations.get(key);
		if (tracked) {
			tracked.produces += 1;
			return tracked.generation;
		}
		this.produceInvalidations.set(key, {generation: 0, produces: 1});
		return 0;
	}

	private currentGeneration(key: string): number {
		return this.produceInvalidations.get(key)?.generation ?? 0;
	}

	private releaseProduce(key: string): void {
		const tracked = this.produceInvalidations.get(key);
		if (!tracked) {
			return;
		}
		tracked.produces -= 1;
		if (tracked.produces <= 0) {
			this.produceInvalidations.delete(key);
		}
	}

	private async joinInflight<T>(inflight: Promise<unknown>): Promise<CacheJoinResult<T>> {
		try {
			return {joined: true, value: (await inflight) as T};
		} catch (error) {
			return {joined: false, error};
		}
	}

	private async produceSingleFlight<T>(
		key: string,
		valueFactory: () => Promise<T>,
		ttlSeconds: CacheTtlSeconds<T> | undefined,
		generation: number,
		produceTimeoutMs: number,
	): Promise<T> {
		const abandonment: CacheProduceAbandonment = {abandoned: false};
		const produced = this.boundProduce(
			this.produceAndStore(key, valueFactory, ttlSeconds, generation, abandonment),
			abandonment,
			produceTimeoutMs,
		);
		if (this.inflightValues.size >= CACHE_INFLIGHT_MAX_ENTRIES) {
			return await produced;
		}
		const pending = produced.finally(() => {
			this.inflightValues.delete(key);
		});
		this.inflightValues.set(key, pending);
		return await pending;
	}

	private boundProduce<T>(
		produced: Promise<T>,
		abandonment: CacheProduceAbandonment,
		produceTimeoutMs: number,
	): Promise<T> {
		return new Promise<T>((resolve, reject) => {
			const timer = setTimeout(() => {
				abandonment.abandoned = true;
				reject(new Error(CACHE_PRODUCE_TIMEOUT_MESSAGE));
			}, produceTimeoutMs);
			timer.unref?.();
			produced.then(
				(value) => {
					clearTimeout(timer);
					resolve(value);
				},
				(error: unknown) => {
					clearTimeout(timer);
					reject(error);
				},
			);
		});
	}

	private async produceAndStore<T>(
		key: string,
		valueFactory: () => Promise<T>,
		ttlSeconds: CacheTtlSeconds<T> | undefined,
		generation: number,
		abandonment: CacheProduceAbandonment,
	): Promise<T> {
		const value = await valueFactory();
		if (!abandonment.abandoned && this.currentGeneration(key) === generation) {
			await this.set(key, value, typeof ttlSeconds === 'function' ? ttlSeconds(value) : ttlSeconds);
		}
		return value;
	}
}
