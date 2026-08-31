// SPDX-License-Identifier: AGPL-3.0-or-later

export interface IKVPipeline {
	get(key: string): this;
	set(key: string, value: string): this;
	setex(key: string, ttlSeconds: number, value: string): this;
	del(key: string): this;
	expire(key: string, ttlSeconds: number): this;
	sadd(key: string, ...members: Array<string>): this;
	srem(key: string, ...members: Array<string>): this;
	zadd(key: string, score: number, value: string): this;
	zrem(key: string, ...members: Array<string>): this;
	hgetall(key: string): this;
	mset(...args: Array<string>): this;
	exec(): Promise<Array<[Error | null, unknown]>>;
}

export interface IKVSubscription {
	connect(): Promise<void>;
	on(event: 'message', callback: (channel: string, message: string) => void): void;
	on(event: 'error', callback: (error: Error) => void): void;
	off(event: 'message', callback: (channel: string, message: string) => void): void;
	off(event: 'error', callback: (error: Error) => void): void;
	subscribe(...channels: Array<string>): Promise<void>;
	unsubscribe(...channels: Array<string>): Promise<void>;
	quit(): Promise<void>;
	disconnect(): Promise<void>;
	removeAllListeners(event?: 'message' | 'error'): void;
}

export interface KVRateLimitResult {
	allowed: boolean;
	limit: number;
	remaining: number;
	resetAfterMs: number;
	resetAtMs: number;
	retryAfterMs: number;
}

export interface IKVProvider {
	get(key: string): Promise<string | null>;
	set(key: string, value: string, ...args: Array<string | number>): Promise<string | null>;
	setex(key: string, ttlSeconds: number, value: string): Promise<void>;
	setnx(key: string, value: string, ttlSeconds?: number): Promise<boolean>;
	mget(...keys: Array<string>): Promise<Array<string | null>>;
	mset(...args: Array<string>): Promise<void>;
	del(...keys: Array<string>): Promise<number>;
	exists(key: string): Promise<number>;
	expire(key: string, ttlSeconds: number): Promise<number>;
	ttl(key: string): Promise<number>;
	incr(key: string): Promise<number>;
	getex(key: string, ttlSeconds: number): Promise<string | null>;
	getdel(key: string): Promise<string | null>;
	sadd(key: string, ...members: Array<string>): Promise<number>;
	srem(key: string, ...members: Array<string>): Promise<number>;
	smembers(key: string): Promise<Array<string>>;
	sismember(key: string, member: string): Promise<number>;
	scard(key: string): Promise<number>;
	spop(key: string, count?: number): Promise<Array<string>>;
	zadd(key: string, ...scoreMembers: Array<number | string>): Promise<number>;
	zrem(key: string, ...members: Array<string>): Promise<number>;
	zcard(key: string): Promise<number>;
	zrangebyscore(
		key: string,
		min: string | number,
		max: string | number,
		...args: Array<string | number>
	): Promise<Array<string>>;
	rpush(key: string, ...values: Array<string>): Promise<number>;
	lpop(key: string, count?: number): Promise<Array<string>>;
	llen(key: string): Promise<number>;
	hset(key: string, field: string, value: string): Promise<number>;
	hdel(key: string, ...fields: Array<string>): Promise<number>;
	hget(key: string, field: string): Promise<string | null>;
	hgetall(key: string): Promise<Record<string, string>>;
	publish(channel: string, message: string): Promise<number>;
	duplicate(): IKVSubscription;
	acquireLock(key: string, token: string, ttlSeconds: number): Promise<boolean>;
	releaseLock(key: string, token: string): Promise<boolean>;
	extendLock(key: string, token: string, ttlSeconds: number): Promise<boolean>;
	renewSnowflakeNode(key: string, instanceId: string, ttlSeconds: number): Promise<boolean>;
	checkLeakyBucketLimit(key: string, limit: number, windowMs: number, cost: number): Promise<KVRateLimitResult>;
	tryConsumeTokens(
		key: string,
		requested: number,
		maxTokens: number,
		refillRate: number,
		refillIntervalMs: number,
	): Promise<number>;
	scheduleBulkDeletion(queueKey: string, secondaryKey: string, score: number, value: string): Promise<void>;
	removeBulkDeletion(queueKey: string, secondaryKey: string, member?: string): Promise<boolean>;
	scan(pattern: string, count: number): Promise<Array<string>>;
	dequeuePurgeBatch(
		queueKey: string,
		bucketKey: string,
		maxItems: number,
		maxTokens: number,
		refillRate: number,
		refillIntervalMs: number,
	): Promise<{
		urls: Array<string>;
		tokensConsumed: number;
	}>;
	pipeline(): IKVPipeline;
	multi(): IKVPipeline;
	health(): Promise<boolean>;
}
