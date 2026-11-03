// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVPipeline, IKVProvider, IKVSubscription, KVRateLimitResult} from '@pkgs/kv_client/src/IKVProvider';
import {parseRangeByScoreArguments, parseSetArguments} from '@pkgs/kv_client/src/KVCommandArguments';
import {vi} from 'vitest';

type MessageCallback = (channel: string, message: string) => void;
type ErrorCallback = (error: Error) => void;

class MockKVSubscription implements IKVSubscription {
	private readonly messageCallbacks = new Set<MessageCallback>();
	private readonly errorCallbacks = new Set<ErrorCallback>();
	subscribedChannels: Array<string> = [];
	async connect(): Promise<void> {}
	on(event: 'message' | 'error', callback: MessageCallback | ErrorCallback): void {
		if (event === 'message') this.messageCallbacks.add(callback as MessageCallback);
		else this.errorCallbacks.add(callback as ErrorCallback);
	}
	off(event: 'message' | 'error', callback: MessageCallback | ErrorCallback): void {
		if (event === 'message') this.messageCallbacks.delete(callback as MessageCallback);
		else this.errorCallbacks.delete(callback as ErrorCallback);
	}
	deliver(channel: string, message: string): void {
		if (this.subscribedChannels.includes(channel))
			for (const callback of this.messageCallbacks) callback(channel, message);
	}
	async subscribe(...channels: Array<string>): Promise<void> {
		this.subscribedChannels.push(...channels);
	}
	async quit(): Promise<void> {}
	async disconnect(): Promise<void> {}
	reset(): void {
		this.subscribedChannels = [];
		this.messageCallbacks.clear();
		this.errorCallbacks.clear();
	}
}

export class MockKVProvider implements IKVProvider {
	clustered = true;
	private subscription = new MockKVSubscription();
	private readonly stringStore = new Map<string, string>();
	private readonly setStore = new Map<string, Set<string>>();
	private readonly zsetStore = new Map<string, Map<string, number>>();
	private readonly listStore = new Map<string, Array<string>>();
	private readonly hashStore = new Map<string, Map<string, string>>();
	private readonly expiries = new Map<string, number>();
	readonly getSpy = vi.fn();
	readonly setnxSpy = vi.fn();
	readonly mgetSpy = vi.fn();
	readonly delSpy = vi.fn();
	readonly publishSpy = vi.fn();

	async get(key: string): Promise<string | null> {
		this.getSpy(key);
		this.evictIfExpired(key);
		return this.stringStore.get(key) ?? null;
	}

	async set(key: string, value: string, ...args: Array<string | number>): Promise<string | null> {
		const {ttlSeconds, useNx} = parseSetArguments(args);
		if (useNx && this.keyExists(key)) {
			return null;
		}
		this.ensureType(key, 'string');
		this.stringStore.set(key, value);
		if (ttlSeconds !== undefined) this.expiries.set(key, Date.now() + ttlSeconds * 1000);
		else this.expiries.delete(key);
		return 'OK';
	}

	async setex(key: string, ttlSeconds: number, value: string): Promise<void> {
		this.ensureType(key, 'string');
		this.stringStore.set(key, value);
		this.expiries.set(key, Date.now() + ttlSeconds * 1000);
	}

	async setnx(key: string, value: string, ttlSeconds?: number): Promise<boolean> {
		this.setnxSpy(key, value, ttlSeconds);
		if (this.keyExists(key)) {
			return false;
		}
		this.ensureType(key, 'string');
		this.stringStore.set(key, value);
		if (ttlSeconds !== undefined) {
			this.expiries.set(key, Date.now() + ttlSeconds * 1000);
		} else {
			this.expiries.delete(key);
		}
		return true;
	}

	async mget(...keys: Array<string>): Promise<Array<string | null>> {
		this.mgetSpy(...keys);
		return keys.map((key) => {
			this.evictIfExpired(key);
			return this.stringStore.get(key) ?? null;
		});
	}

	async del(...keys: Array<string>): Promise<number> {
		this.delSpy(...keys);
		let deleted = 0;
		for (const key of keys) {
			if (this.deleteKey(key)) {
				deleted++;
			}
		}
		return deleted;
	}

	async exists(key: string): Promise<number> {
		return this.keyExists(key) ? 1 : 0;
	}

	async expire(key: string, ttlSeconds: number): Promise<number> {
		if (!this.keyExists(key)) {
			return 0;
		}
		this.expiries.set(key, Date.now() + ttlSeconds * 1000);
		return 1;
	}

	async ttl(key: string): Promise<number> {
		this.evictIfExpired(key);
		if (!this.keyExists(key)) {
			return -2;
		}
		const expiry = this.expiries.get(key);
		if (expiry === undefined) {
			return -1;
		}
		const remainingSeconds = Math.floor((expiry - Date.now()) / 1000);
		if (remainingSeconds < 0) {
			this.deleteKey(key);
			return -2;
		}
		return remainingSeconds;
	}

	async incr(key: string): Promise<number> {
		this.evictIfExpired(key);
		const value = Number.parseInt(this.stringStore.get(key) ?? '0', 10) + 1;
		this.ensureType(key, 'string');
		this.stringStore.set(key, String(value));
		return value;
	}

	async getdel(key: string): Promise<string | null> {
		this.evictIfExpired(key);
		const value = this.stringStore.get(key) ?? null;
		this.deleteKey(key);
		return value;
	}

	async sadd(key: string, ...members: Array<string>): Promise<number> {
		this.evictIfExpired(key);
		this.ensureType(key, 'set');
		const membersSet = this.setStore.get(key)!;
		let added = 0;
		for (const member of members) {
			if (!membersSet.has(member)) {
				membersSet.add(member);
				added++;
			}
		}
		return added;
	}

	async srem(key: string, ...members: Array<string>): Promise<number> {
		this.evictIfExpired(key);
		const membersSet = this.setStore.get(key);
		if (!membersSet) {
			return 0;
		}
		let removed = 0;
		for (const member of members) {
			if (membersSet.delete(member)) {
				removed++;
			}
		}
		return removed;
	}

	async smembers(key: string): Promise<Array<string>> {
		this.evictIfExpired(key);
		const membersSet = this.setStore.get(key);
		return membersSet ? Array.from(membersSet) : [];
	}

	async sismember(key: string, member: string): Promise<number> {
		this.evictIfExpired(key);
		const membersSet = this.setStore.get(key);
		return membersSet?.has(member) ? 1 : 0;
	}

	async scard(key: string): Promise<number> {
		this.evictIfExpired(key);
		return this.setStore.get(key)?.size ?? 0;
	}

	private async spop(key: string, count: number = 1): Promise<Array<string>> {
		this.evictIfExpired(key);
		const membersSet = this.setStore.get(key);
		if (!membersSet || count <= 0) {
			return [];
		}
		const popped: Array<string> = [];
		const iterator = membersSet.values();
		for (let i = 0; i < count; i++) {
			const next = iterator.next();
			if (next.done) {
				break;
			}
			membersSet.delete(next.value);
			popped.push(next.value);
		}
		return popped;
	}

	async zadd(key: string, ...scoreMembers: Array<number | string>): Promise<number> {
		this.evictIfExpired(key);
		this.ensureType(key, 'zset');
		const members = this.zsetStore.get(key)!;
		let added = 0;
		for (let i = 0; i < scoreMembers.length; i += 2) {
			const scoreInput = scoreMembers[i];
			const memberInput = scoreMembers[i + 1];
			if (memberInput === undefined) {
				continue;
			}
			const score = Number(scoreInput);
			const member = String(memberInput);
			if (!Number.isFinite(score)) {
				continue;
			}
			if (!members.has(member)) {
				added++;
			}
			members.set(member, score);
		}
		return added;
	}

	async zrem(key: string, ...members: Array<string>): Promise<number> {
		this.evictIfExpired(key);
		const zset = this.zsetStore.get(key);
		if (!zset) {
			return 0;
		}
		let removed = 0;
		for (const member of members) {
			if (zset.delete(member)) {
				removed++;
			}
		}
		return removed;
	}

	async zcard(key: string): Promise<number> {
		this.evictIfExpired(key);
		return this.zsetStore.get(key)?.size ?? 0;
	}

	async zrangebyscore(
		key: string,
		min: string | number,
		max: string | number,
		...args: Array<string | number>
	): Promise<Array<string>> {
		this.evictIfExpired(key);
		const {limit: options} = parseRangeByScoreArguments(args);
		const zset = this.zsetStore.get(key);
		if (!zset) {
			return [];
		}
		const offset = options?.offset ?? 0;
		const limit = options?.count ?? Number.POSITIVE_INFINITY;
		const minBound = parseScoreBound(min, 'minimum');
		const maxBound = parseScoreBound(max, 'maximum');
		const sortedMembers = Array.from(zset.entries()).sort((a, b) => {
			if (a[1] === b[1]) {
				return a[0].localeCompare(b[0]);
			}
			return a[1] - b[1];
		});
		const filtered = sortedMembers
			.filter((entry) => isScoreInRange(entry[1], minBound, maxBound))
			.map((entry) => entry[0]);
		return filtered.slice(offset, offset + limit);
	}

	async rpush(key: string, ...values: Array<string>): Promise<number> {
		this.evictIfExpired(key);
		this.ensureType(key, 'list');
		const list = this.listStore.get(key)!;
		list.push(...values);
		return list.length;
	}

	async lpop(key: string, count: number = 1): Promise<Array<string>> {
		this.evictIfExpired(key);
		const list = this.listStore.get(key);
		if (!list || count <= 0) {
			return [];
		}
		const popped = list.splice(0, count);
		if (list.length === 0) {
			this.listStore.delete(key);
		}
		return popped;
	}

	async llen(key: string): Promise<number> {
		this.evictIfExpired(key);
		return this.listStore.get(key)?.length ?? 0;
	}

	async lrange(key: string, start: number, stop: number): Promise<Array<string>> {
		this.evictIfExpired(key);
		const list = this.listStore.get(key) ?? [];
		const from = start < 0 ? Math.max(list.length + start, 0) : start;
		const to = stop < 0 ? list.length + stop : Math.min(stop, list.length - 1);
		return from > to ? [] : list.slice(from, to + 1);
	}

	async ltrim(key: string, start: number, stop: number): Promise<void> {
		this.evictIfExpired(key);
		const list = this.listStore.get(key);
		if (!list) return;
		const kept = await this.lrange(key, start, stop);
		if (kept.length === 0) {
			this.listStore.delete(key);
			return;
		}
		list.splice(0, list.length, ...kept);
	}

	async hset(key: string, field: string, value: string): Promise<number> {
		this.evictIfExpired(key);
		this.ensureType(key, 'hash');
		const hash = this.hashStore.get(key)!;
		const isNew = !hash.has(field);
		hash.set(field, value);
		return isNew ? 1 : 0;
	}

	async hdel(key: string, ...fields: Array<string>): Promise<number> {
		this.evictIfExpired(key);
		const hash = this.hashStore.get(key);
		if (!hash) {
			return 0;
		}
		let removed = 0;
		for (const field of fields) {
			if (hash.delete(field)) {
				removed++;
			}
		}
		return removed;
	}

	async hgetall(key: string): Promise<Record<string, string>> {
		this.evictIfExpired(key);
		const hash = this.hashStore.get(key);
		if (!hash) {
			return {};
		}
		return Object.fromEntries(hash.entries());
	}

	async publish(channel: string, message: string): Promise<number> {
		this.publishSpy(channel, message);
		this.subscription.deliver(channel, message);
		return 1;
	}

	duplicate(): IKVSubscription {
		return this.subscription;
	}

	async acquireLock(key: string, token: string, ttlSeconds: number): Promise<boolean> {
		this.evictIfExpired(key);
		if (this.keyExists(key)) {
			return false;
		}
		this.ensureType(key, 'string');
		this.stringStore.set(key, token);
		this.expiries.set(key, Date.now() + ttlSeconds * 1000);
		return true;
	}

	async releaseLock(key: string, token: string): Promise<boolean> {
		this.evictIfExpired(key);
		const currentToken = this.stringStore.get(key);
		if (currentToken !== token) {
			return false;
		}
		this.deleteKey(key);
		return true;
	}

	async extendLock(key: string, token: string, ttlSeconds: number): Promise<boolean> {
		this.evictIfExpired(key);
		const currentToken = this.stringStore.get(key);
		if (currentToken !== token) {
			return false;
		}
		this.expiries.set(key, Date.now() + ttlSeconds * 1000);
		return true;
	}

	async checkLeakyBucketLimit(key: string, limit: number, windowMs: number, cost: number): Promise<KVRateLimitResult> {
		this.evictIfExpired(key);
		const nowMs = Date.now();
		const capacity = Math.max(1, Math.floor(limit));
		const leakWindowMs = Math.max(1, Math.floor(windowMs));
		const currentState = parseLeakyBucketState(this.stringStore.get(key), nowMs);
		drainLeakyBucket(currentState, capacity, leakWindowMs, nowMs);
		if (cost <= 0) {
			const remaining = Math.max(0, Math.floor(capacity - currentState.level));
			const resetAfterMs = calculateLeakyBucketResetAfterMs(currentState, capacity, leakWindowMs);
			return createRateLimitResult(true, capacity, remaining, nowMs, resetAfterMs, 0);
		}
		if (cost !== 1) {
			throw new Error(`Unsupported test leaky bucket cost: ${cost}`);
		}
		if (currentState.level + cost > capacity) {
			const retryAfterMs = calculateLeakyBucketRetryAfterMs(currentState, capacity, leakWindowMs, cost);
			const resetAfterMs = calculateLeakyBucketResetAfterMs(currentState, capacity, leakWindowMs);
			const ttlMs = Math.max(1, resetAfterMs, retryAfterMs);
			this.ensureType(key, 'string');
			this.stringStore.set(key, JSON.stringify({level: currentState.level, updatedAt: currentState.updatedAtMs}));
			this.expiries.set(key, nowMs + ttlMs);
			return createRateLimitResult(false, capacity, 0, nowMs, ttlMs, retryAfterMs);
		}
		currentState.level += cost;
		const remaining = Math.max(0, Math.floor(capacity - currentState.level));
		const resetAfterMs = calculateLeakyBucketResetAfterMs(currentState, capacity, leakWindowMs);
		const ttlMs = Math.max(1, resetAfterMs);
		this.ensureType(key, 'string');
		this.stringStore.set(key, JSON.stringify({level: currentState.level, updatedAt: currentState.updatedAtMs}));
		this.expiries.set(key, nowMs + ttlMs);
		return createRateLimitResult(true, capacity, remaining, nowMs, resetAfterMs, 0);
	}

	async scheduleBulkDeletion(queueKey: string, secondaryKey: string, score: number, value: string): Promise<void> {
		await this.zadd(queueKey, score, value);
		this.ensureType(secondaryKey, 'string');
		this.stringStore.set(secondaryKey, value);
		this.expiries.delete(secondaryKey);
	}

	async claimBulkDeletion(queueKey: string, member: string, maxScore: number, leaseScore: number): Promise<boolean> {
		this.evictIfExpired(queueKey);
		const score = this.zsetStore.get(queueKey)?.get(member);
		if (score === undefined || score > maxScore) {
			return false;
		}
		await this.zadd(queueKey, leaseScore, member);
		return true;
	}

	async removeBulkDeletion(queueKey: string, secondaryKey: string, member = ''): Promise<boolean> {
		this.evictIfExpired(secondaryKey);
		if (member !== '' && (await this.zrem(queueKey, member)) === 1) {
			this.deleteKey(secondaryKey);
			return true;
		}
		const value = this.stringStore.get(secondaryKey);
		if (value === undefined) {
			return false;
		}
		await this.zrem(queueKey, value);
		this.deleteKey(secondaryKey);
		return true;
	}

	async dequeuePurgeBatch(
		queueKey: string,
		bucketKey: string,
		maxItems: number,
		maxTokens: number,
		refillRate: number,
		refillIntervalMs: number,
	): Promise<{
		entries: Array<string>;
		tokensConsumed: number;
	}> {
		const now = Date.now();
		let tokens = maxTokens;
		let lastRefill = now;
		const rawState = this.stringStore.get(bucketKey);
		if (rawState !== undefined) {
			try {
				const parsed = JSON.parse(rawState) as {
					tokens?: number;
					lastRefill?: number;
				};
				tokens = parsed.tokens ?? maxTokens;
				lastRefill = parsed.lastRefill ?? now;
			} catch {}
		}
		const elapsed = now - lastRefill;
		if (elapsed >= refillIntervalMs) {
			const intervals = Math.floor(elapsed / refillIntervalMs);
			tokens = Math.min(maxTokens, tokens + intervals * refillRate);
			lastRefill = now;
		}
		const membersSet = this.setStore.get(queueKey);
		const queueSize = membersSet?.size ?? 0;
		const toPop = Math.min(maxItems, Math.floor(tokens), queueSize);
		if (toPop <= 0) {
			this.stringStore.set(bucketKey, JSON.stringify({tokens, lastRefill}));
			this.expiries.set(bucketKey, now + 3600 * 1000);
			return {entries: [], tokensConsumed: 0};
		}
		const entries = await this.spop(queueKey, toPop);
		tokens -= entries.length;
		this.stringStore.set(bucketKey, JSON.stringify({tokens, lastRefill}));
		this.expiries.set(bucketKey, now + 3600 * 1000);
		return {entries, tokensConsumed: entries.length};
	}

	async evalScript(
		command: string,
		_script: string,
		keyCount: number,
		...args: Array<string | number>
	): Promise<unknown> {
		if (keyCount !== 1) {
			throw new Error(`Unsupported mock script key count: ${keyCount}`);
		}
		const key = String(args[0]);
		if (command === 'emailOwnerClaim') {
			return this.evalEmailOwnerClaim(key, String(args[1]), String(args[2]), Number(args[3]), Number(args[4]));
		}
		if (command === 'emailOwnerFinalize') {
			return this.evalEmailOwnerFinalize(key, String(args[1]), String(args[2]), Number(args[3]));
		}
		if (command === 'emailOwnerRelease') {
			return this.evalEmailOwnerRelease(key, String(args[1]), String(args[2] ?? ''));
		}
		throw new Error(`Unsupported mock script command: ${command}`);
	}

	async scan(pattern: string, count: number): Promise<Array<string>> {
		const keys = this.getAllKeys().filter((key) => this.matchesPattern(key, pattern));
		return keys.slice(0, count);
	}

	pipeline(): IKVPipeline {
		return this.createPipeline();
	}

	multi(): IKVPipeline {
		return this.createPipeline();
	}

	isClustered(): boolean {
		return this.clustered;
	}

	getSubscription(): MockKVSubscription {
		return this.subscription;
	}

	private createPipeline(): IKVPipeline {
		const operations: Array<() => Promise<unknown>> = [];
		const pipeline: IKVPipeline = {
			setex: (key: string, ttlSeconds: number, value: string) => {
				operations.push(async () => await this.setex(key, ttlSeconds, value));
				return pipeline;
			},
			del: (key: string) => {
				operations.push(async () => await this.del(key));
				return pipeline;
			},
			expire: (key: string, ttlSeconds: number) => {
				operations.push(async () => await this.expire(key, ttlSeconds));
				return pipeline;
			},
			sadd: (key: string, ...members: Array<string>) => {
				operations.push(async () => await this.sadd(key, ...members));
				return pipeline;
			},
			hgetall: (key: string) => {
				operations.push(async () => await this.hgetall(key));
				return pipeline;
			},
			exec: async () => {
				const results: Array<[Error | null, unknown]> = [];
				for (const operation of operations) {
					try {
						const value = await operation();
						results.push([null, value]);
					} catch (error) {
						results.push([error as Error, null]);
					}
				}
				return results;
			},
		};
		return pipeline;
	}

	private evalEmailOwnerClaim(key: string, userId: string, token: string, nowMs: number, pendingUntil: number): string {
		this.evictIfExpired(key);
		const raw = this.stringStore.get(key);
		const pendingState = (epoch: number) => ({
			version: 1,
			status: 'pending',
			userId,
			token,
			epoch,
			pendingUntil,
			updatedAt: nowMs,
		});
		if (raw !== undefined) {
			const state = parseJsonObject(raw);
			if (state) {
				const ownerUserId = String(state.userId ?? '');
				const status = String(state.status ?? '');
				if (status === 'claimed') {
					if (ownerUserId === userId) {
						return JSON.stringify({
							status: 'already_owner',
							token: String(state.token ?? ''),
							epoch: Number(state.epoch ?? 0),
						});
					}
					return JSON.stringify({status: 'conflict', ownerUserId});
				}
				if (status === 'pending') {
					const currentPendingUntil = Number(state.pendingUntil ?? 0);
					if (currentPendingUntil > nowMs && ownerUserId !== userId) {
						return JSON.stringify({status: 'conflict', ownerUserId});
					}
					if (currentPendingUntil > nowMs && ownerUserId === userId) {
						state.pendingUntil = pendingUntil;
						state.updatedAt = nowMs;
						this.ensureType(key, 'string');
						this.stringStore.set(key, JSON.stringify(state));
						this.expiries.delete(key);
						return JSON.stringify({
							status: 'pending',
							token: String(state.token ?? ''),
							epoch: Number(state.epoch ?? 0),
						});
					}
					const epoch = Number(state.epoch ?? 0) + 1;
					this.ensureType(key, 'string');
					this.stringStore.set(key, JSON.stringify(pendingState(epoch)));
					this.expiries.delete(key);
					return JSON.stringify({status: 'pending', token, epoch});
				}
			}
		}
		this.ensureType(key, 'string');
		this.stringStore.set(key, JSON.stringify(pendingState(1)));
		this.expiries.delete(key);
		return JSON.stringify({status: 'pending', token, epoch: 1});
	}

	private evalEmailOwnerFinalize(key: string, userId: string, expectedToken: string, nowMs: number): string {
		this.evictIfExpired(key);
		const raw = this.stringStore.get(key);
		if (raw === undefined) {
			return JSON.stringify({status: 'missing'});
		}
		const state = parseJsonObject(raw);
		if (!state) {
			return JSON.stringify({status: 'mismatch'});
		}
		const ownerUserId = String(state.userId ?? '');
		if (ownerUserId !== userId) {
			return JSON.stringify({status: 'mismatch', ownerUserId});
		}
		if (String(state.token ?? '') !== expectedToken) {
			return JSON.stringify({status: 'mismatch', ownerUserId});
		}
		this.ensureType(key, 'string');
		this.stringStore.set(
			key,
			JSON.stringify({
				version: 1,
				status: 'claimed',
				userId,
				token: expectedToken,
				epoch: Number(state.epoch ?? 1),
				claimedAt: nowMs,
				updatedAt: nowMs,
			}),
		);
		this.expiries.delete(key);
		return JSON.stringify({status: 'finalized', ownerUserId});
	}

	private evalEmailOwnerRelease(key: string, userId: string, expectedToken: string): string {
		this.evictIfExpired(key);
		const raw = this.stringStore.get(key);
		if (raw === undefined) {
			return JSON.stringify({status: 'missing'});
		}
		const state = parseJsonObject(raw);
		if (!state) {
			this.deleteKey(key);
			return JSON.stringify({status: 'released'});
		}
		const ownerUserId = String(state.userId ?? '');
		if (ownerUserId !== userId) {
			return JSON.stringify({status: 'not_owner', ownerUserId});
		}
		if (expectedToken !== '' && String(state.token ?? '') !== expectedToken) {
			return JSON.stringify({status: 'token_mismatch', ownerUserId});
		}
		this.deleteKey(key);
		return JSON.stringify({status: 'released'});
	}

	private keyExists(key: string): boolean {
		this.evictIfExpired(key);
		return (
			this.stringStore.has(key) ||
			this.setStore.has(key) ||
			this.zsetStore.has(key) ||
			this.listStore.has(key) ||
			this.hashStore.has(key)
		);
	}

	private evictIfExpired(key: string): void {
		const expiry = this.expiries.get(key);
		if (expiry === undefined) {
			return;
		}
		if (expiry <= Date.now()) {
			this.deleteKey(key);
		}
	}

	private ensureType(key: string, type: 'string' | 'set' | 'zset' | 'list' | 'hash'): void {
		this.evictIfExpired(key);
		if (type !== 'string') {
			this.stringStore.delete(key);
		}
		if (type !== 'set') {
			this.setStore.delete(key);
		}
		if (type !== 'zset') {
			this.zsetStore.delete(key);
		}
		if (type !== 'list') {
			this.listStore.delete(key);
		}
		if (type !== 'hash') {
			this.hashStore.delete(key);
		}
		if (type === 'set' && !this.setStore.has(key)) {
			this.setStore.set(key, new Set());
		}
		if (type === 'zset' && !this.zsetStore.has(key)) {
			this.zsetStore.set(key, new Map());
		}
		if (type === 'list' && !this.listStore.has(key)) {
			this.listStore.set(key, []);
		}
		if (type === 'hash' && !this.hashStore.has(key)) {
			this.hashStore.set(key, new Map());
		}
	}

	private deleteKey(key: string): boolean {
		let deleted = false;
		if (this.stringStore.delete(key)) {
			deleted = true;
		}
		if (this.setStore.delete(key)) {
			deleted = true;
		}
		if (this.zsetStore.delete(key)) {
			deleted = true;
		}
		if (this.listStore.delete(key)) {
			deleted = true;
		}
		if (this.hashStore.delete(key)) {
			deleted = true;
		}
		if (this.expiries.delete(key)) {
			deleted = true;
		}
		return deleted;
	}

	private getAllKeys(): Array<string> {
		for (const key of this.expiries.keys()) {
			this.evictIfExpired(key);
		}
		const keys = new Set<string>();
		for (const key of this.stringStore.keys()) keys.add(key);
		for (const key of this.setStore.keys()) keys.add(key);
		for (const key of this.zsetStore.keys()) keys.add(key);
		for (const key of this.listStore.keys()) keys.add(key);
		for (const key of this.hashStore.keys()) keys.add(key);
		return Array.from(keys);
	}

	private matchesPattern(value: string, pattern: string): boolean {
		if (pattern === '*') {
			return true;
		}
		const escaped = pattern.replace(/[.+^${}()|[\]\\]/g, '\\$&');
		const regex = new RegExp(`^${escaped.replace(/\*/g, '.*').replace(/\?/g, '.')}$`);
		return regex.test(value);
	}

	reset(): void {
		this.stringStore.clear();
		this.setStore.clear();
		this.zsetStore.clear();
		this.listStore.clear();
		this.hashStore.clear();
		this.expiries.clear();
		this.subscription.reset();
		this.getSpy.mockClear();
		this.setnxSpy.mockClear();
		this.mgetSpy.mockClear();
		this.delSpy.mockClear();
		this.publishSpy.mockClear();
	}
}

interface ScoreBound {
	value: number;
	exclusive: boolean;
}

interface StoredLeakyBucketState {
	level: number;
	updatedAtMs: number;
}

function parseScoreBound(bound: string | number, side: 'minimum' | 'maximum'): ScoreBound {
	if (typeof bound === 'number') {
		return {value: bound, exclusive: false};
	}
	if (bound === '-inf') {
		return {value: Number.NEGATIVE_INFINITY, exclusive: false};
	}
	if (bound === '+inf') {
		return {value: Number.POSITIVE_INFINITY, exclusive: false};
	}
	const exclusive = bound.startsWith('(');
	const value = Number(exclusive ? bound.slice(1) : bound);
	if (Number.isFinite(value)) {
		return {value, exclusive};
	}
	return {
		value: side === 'minimum' ? Number.NEGATIVE_INFINITY : Number.POSITIVE_INFINITY,
		exclusive,
	};
}

function isScoreInRange(score: number, min: ScoreBound, max: ScoreBound): boolean {
	const minOk = min.exclusive ? score > min.value : score >= min.value;
	const maxOk = max.exclusive ? score < max.value : score <= max.value;
	return minOk && maxOk;
}

function parseLeakyBucketState(rawState: string | undefined, nowMs: number): StoredLeakyBucketState {
	if (rawState === undefined) {
		return {level: 0, updatedAtMs: nowMs};
	}
	try {
		const parsed = JSON.parse(rawState) as Partial<{
			level: number;
			updatedAt: number;
			updatedAtMs: number;
		}>;
		const level = Number.isFinite(parsed.level) ? parsed.level! : 0;
		const parsedUpdatedAt = parsed.updatedAtMs ?? parsed.updatedAt;
		const updatedAtMs = Number.isFinite(parsedUpdatedAt) ? parsedUpdatedAt! : nowMs;
		return {level, updatedAtMs};
	} catch {
		return {level: 0, updatedAtMs: nowMs};
	}
}

function parseJsonObject(raw: string): Record<string, unknown> | null {
	try {
		const parsed = JSON.parse(raw) as unknown;
		return parsed && typeof parsed === 'object' && !Array.isArray(parsed) ? (parsed as Record<string, unknown>) : null;
	} catch {
		return null;
	}
}

function drainLeakyBucket(state: StoredLeakyBucketState, capacity: number, windowMs: number, nowMs: number): void {
	const elapsed = nowMs - state.updatedAtMs;
	if (elapsed <= 0) {
		return;
	}
	state.level = Math.max(0, state.level - (elapsed / windowMs) * capacity);
	state.updatedAtMs = nowMs;
}

function calculateLeakyBucketRetryAfterMs(
	state: StoredLeakyBucketState,
	capacity: number,
	windowMs: number,
	cost: number,
): number {
	const overflow = state.level + cost - capacity;
	if (overflow <= 0) {
		return 0;
	}
	return Math.max(1, Math.ceil(overflow / (capacity / windowMs)));
}

function calculateLeakyBucketResetAfterMs(state: StoredLeakyBucketState, capacity: number, windowMs: number): number {
	if (state.level <= 0) {
		return 0;
	}
	return Math.max(0, Math.ceil(state.level / (capacity / windowMs)));
}

function createRateLimitResult(
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
