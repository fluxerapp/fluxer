// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KVRateLimitResult} from '@pkgs/kv_client/src/IKVProvider';
import type {RateLimitConfig, RateLimitResult} from '@pkgs/rate_limit/src/IRateLimitService';
import {RateLimitService} from '@pkgs/rate_limit/src/RateLimitService';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

interface BucketState {
	level: number;
	updatedAt: number;
}

class LeakyBucketStore {
	readonly buckets = new Map<string, BucketState>();

	async checkLeakyBucketLimit(key: string, limit: number, windowMs: number, cost: number): Promise<KVRateLimitResult> {
		const nowMs = Date.now();
		const capacity = Math.max(1, Math.floor(limit));
		const leakWindowMs = Math.max(1, Math.floor(windowMs));
		const leakPerMs = capacity / leakWindowMs;
		const state = this.buckets.get(key) ?? {level: 0, updatedAt: nowMs};
		const elapsed = nowMs - state.updatedAt;
		let level = state.level;
		if (elapsed > 0) {
			level = Math.max(0, level - elapsed * leakPerMs);
		}
		const resetAfter = (current: number) => (current <= 0 ? 0 : Math.max(0, Math.ceil(current / leakPerMs)));
		const result = (allowed: boolean, remaining: number, resetAfterMs: number, retryAfterMs: number) => ({
			allowed,
			limit: capacity,
			remaining,
			resetAfterMs,
			resetAtMs: nowMs + resetAfterMs,
			retryAfterMs,
		});
		if (cost === 0) {
			this.save(key, level, nowMs);
			return result(true, Math.max(0, Math.floor(capacity - level)), resetAfter(level), 0);
		}
		if (level + cost > capacity) {
			const retryAfterMs = Math.max(1, Math.ceil((level + cost - capacity) / leakPerMs));
			const ttlMs = Math.max(1, resetAfter(level), retryAfterMs);
			this.save(key, level, nowMs);
			return result(false, 0, ttlMs, retryAfterMs);
		}
		level += cost;
		this.save(key, level, nowMs);
		return result(true, Math.max(0, Math.floor(capacity - level)), resetAfter(level), 0);
	}

	async del(...keys: Array<string>): Promise<number> {
		let deleted = 0;
		for (const key of keys) {
			if (this.buckets.delete(key)) deleted += 1;
		}
		return deleted;
	}

	async scan(pattern: string, _count: number): Promise<Array<string>> {
		const prefix = pattern.replace(/\*$/, '').replace(/\\(.)/g, '$1');
		return [...this.buckets.keys()].filter((key) => key.startsWith(prefix));
	}

	private save(key: string, level: number, nowMs: number): void {
		if (level > 0) {
			this.buckets.set(key, {level, updatedAt: nowMs});
		} else {
			this.buckets.delete(key);
		}
	}
}

const CONFIG: RateLimitConfig = {identifier: 'report:target:1', maxAttempts: 5, windowMs: 60_000};

function retryAfterMs(result: RateLimitResult): number {
	return Math.round((result.retryAfterDecimal ?? 0) * 1000);
}

function expectPeekMatchesCheck(peek: RateLimitResult, check: RateLimitResult): void {
	expect(check.allowed).toBe(false);
	expect(peek).toMatchObject({allowed: false, remaining: 0, limit: check.limit});
	expect(peek.retryAfter).toBe(check.retryAfter);
	expect(retryAfterMs(peek) - retryAfterMs(check)).toBeGreaterThanOrEqual(0);
	expect(retryAfterMs(peek) - retryAfterMs(check)).toBeLessThanOrEqual(1);
}

async function fill(service: RateLimitService, config: RateLimitConfig): Promise<void> {
	for (let attempt = 0; attempt < config.maxAttempts; attempt += 1) {
		expect((await service.checkLimit(config)).allowed).toBe(true);
	}
}

describe('RateLimitService.peekLimit', () => {
	let store: LeakyBucketStore;
	let service: RateLimitService;

	beforeEach(() => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-10-04T12:00:00.000Z'));
		store = new LeakyBucketStore();
		service = new RateLimitService(store);
	});

	afterEach(() => {
		vi.useRealTimers();
	});

	it('allows an empty bucket without spending it', async () => {
		const peek = await service.peekLimit(CONFIG);
		expect(peek).toMatchObject({allowed: true, limit: 5, remaining: 5});
		expect(peek.retryAfter).toBeUndefined();
		expect(peek.retryAfterDecimal).toBeUndefined();
		expect(store.buckets.size).toBe(0);
	});

	it('allows while capacity remains and reports the remaining count', async () => {
		await service.checkLimit(CONFIG);
		await service.checkLimit(CONFIG);
		const peek = await service.peekLimit(CONFIG);
		expect(peek).toMatchObject({allowed: true, remaining: 3});
		expect(peek.retryAfter).toBeUndefined();
		expect((await service.peekLimit(CONFIG)).remaining).toBe(3);
	});

	it('rejects a full bucket with the retry-after a consuming check would report', async () => {
		await fill(service, CONFIG);
		const peek = await service.peekLimit(CONFIG);
		const check = await service.checkLimit(CONFIG);
		expectPeekMatchesCheck(peek, check);
		expect(retryAfterMs(peek)).toBe(12_000);
		expect(peek.retryAfter).toBe(12);
	});

	it('keeps rejecting while less than one slot has leaked', async () => {
		await fill(service, CONFIG);
		vi.advanceTimersByTime(6_000);
		const peek = await service.peekLimit(CONFIG);
		const check = await service.checkLimit(CONFIG);
		expectPeekMatchesCheck(peek, check);
		expect(retryAfterMs(peek)).toBe(6_000);
		expect(peek.retryAfter).toBe(6);
	});

	it('does not spend budget when it rejects', async () => {
		await fill(service, CONFIG);
		for (let attempt = 0; attempt < 3; attempt += 1) {
			expect((await service.peekLimit(CONFIG)).allowed).toBe(false);
		}
		vi.advanceTimersByTime(12_000);
		expect(await service.peekLimit(CONFIG)).toMatchObject({allowed: true, remaining: 1});
		expect((await service.checkLimit(CONFIG)).allowed).toBe(true);
		expect((await service.peekLimit(CONFIG)).allowed).toBe(false);
	});

	it('matches the consuming check on a single-slot bucket', async () => {
		const config: RateLimitConfig = {identifier: 'slowmode:1:2', maxAttempts: 1, windowMs: 10_000};
		await service.checkLimit(config);
		vi.advanceTimersByTime(2_500);
		const peek = await service.peekLimit(config);
		const check = await service.checkLimit(config);
		expectPeekMatchesCheck(peek, check);
		expect(peek.retryAfterDecimal).toBe(7.5);
		expect(peek.retryAfter).toBe(8);
		expect(peek.resetTime.getTime()).toBe(Date.now() + 7_500);
	});

	it('agrees with the consuming check at every point of a draining bucket', async () => {
		const config: RateLimitConfig = {identifier: 'crosspost:edit:1', maxAttempts: 3, windowMs: 1_000};
		await fill(service, config);
		for (let step = 0; step < 12; step += 1) {
			vi.advanceTimersByTime(37);
			const peek = await service.peekLimit(config);
			const check = await service.checkLimit(config);
			expect(peek.allowed).toBe(check.allowed);
			if (!check.allowed) {
				expectPeekMatchesCheck(peek, check);
			}
		}
	});
});
