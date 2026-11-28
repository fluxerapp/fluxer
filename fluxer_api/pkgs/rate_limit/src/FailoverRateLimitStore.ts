// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KVRateLimitResult} from '@pkgs/kv_client/src/IKVProvider';
import {KVClientError, KVClientErrorCode} from '@pkgs/kv_client/src/KVClientError';
import type {IRateLimitStore} from '@pkgs/rate_limit/src/RateLimitService';

export type RateLimitFallbackReason = 'error' | 'timeout' | 'open';

export interface FailoverRateLimitStoreOptions {
	deadlineMs?: number;
	openAfterFailures?: number;
	cooldownMs?: number;
	maxLocalKeys?: number;
	now?: () => number;
	elapsed?: () => number;
	onFallback?: (reason: RateLimitFallbackReason) => void;
	failClosed?: (key: string) => boolean;
	onFailClosed?: () => void;
}

interface LocalBucket {
	level: number;
	updatedAt: number;
}

const LOOP_LAG_TOLERANCE_MS = 50;

class DeadlineExceeded extends Error {}

class Breaker {
	private failures = 0;
	private openUntil = 0;

	constructor(
		private readonly openAfterFailures: number,
		private readonly cooldownMs: number,
		private readonly now: () => number,
	) {}

	isOpen(): boolean {
		return this.now() < this.openUntil;
	}

	succeeded(): void {
		this.failures = 0;
	}

	failed(): void {
		this.failures += 1;
		if (this.failures >= this.openAfterFailures) {
			this.openUntil = this.now() + this.cooldownMs;
			this.failures = 0;
		}
	}
}

function isUnavailable(error: unknown): boolean {
	return (
		error instanceof KVClientError &&
		(error.code === KVClientErrorCode.TIMEOUT || error.code === KVClientErrorCode.REQUEST_FAILED)
	);
}

export class FailoverRateLimitStore implements IRateLimitStore {
	private readonly deadlineMs: number;
	private readonly maxLocalKeys: number;
	private readonly now: () => number;
	private readonly elapsed: () => number;
	private readonly onFallback: (reason: RateLimitFallbackReason) => void;
	private readonly failClosed: (key: string) => boolean;
	private readonly onFailClosed: () => void;
	private readonly local = new Map<string, LocalBucket>();
	private readonly openBreaker: Breaker;
	private readonly closedBreaker: Breaker;

	constructor(
		private readonly primary: IRateLimitStore,
		options: FailoverRateLimitStoreOptions = {},
	) {
		this.deadlineMs = options.deadlineMs ?? 250;
		this.maxLocalKeys = options.maxLocalKeys ?? 100_000;
		this.now = options.now ?? Date.now;
		const openAfterFailures = options.openAfterFailures ?? 5;
		const cooldownMs = options.cooldownMs ?? 5000;
		this.openBreaker = new Breaker(openAfterFailures, cooldownMs, this.now);
		this.closedBreaker = new Breaker(openAfterFailures, cooldownMs, this.now);
		this.elapsed = options.elapsed ?? (() => performance.now());
		this.onFallback = options.onFallback ?? (() => {});
		this.failClosed = options.failClosed ?? (() => false);
		this.onFailClosed = options.onFailClosed ?? (() => {});
	}

	async checkLeakyBucketLimit(key: string, limit: number, windowMs: number, cost: number): Promise<KVRateLimitResult> {
		if (this.failClosed(key)) return this.checkClosed(key, limit, windowMs, cost);
		if (this.openBreaker.isOpen()) {
			this.onFallback('open');
			return this.checkLocal(key, limit, windowMs, cost);
		}
		try {
			const result = await this.withDeadline(this.primary.checkLeakyBucketLimit(key, limit, windowMs, cost));
			this.openBreaker.succeeded();
			return result;
		} catch (error) {
			if (!(error instanceof DeadlineExceeded) && !isUnavailable(error)) throw error;
			this.openBreaker.failed();
			this.onFallback(error instanceof DeadlineExceeded ? 'timeout' : 'error');
			return this.checkLocal(key, limit, windowMs, cost);
		}
	}

	private async checkClosed(key: string, limit: number, windowMs: number, cost: number): Promise<KVRateLimitResult> {
		if (this.closedBreaker.isOpen()) {
			this.onFailClosed();
			throw new KVClientError({
				code: KVClientErrorCode.REQUEST_FAILED,
				message: 'Rate limit store unavailable for a fail-closed key',
			});
		}
		try {
			const result = await this.primary.checkLeakyBucketLimit(key, limit, windowMs, cost);
			this.closedBreaker.succeeded();
			return result;
		} catch (error) {
			if (isUnavailable(error)) {
				this.closedBreaker.failed();
				this.onFailClosed();
			}
			throw error;
		}
	}

	del(...keys: Array<string>): Promise<number> {
		for (const key of keys) this.local.delete(key);
		return this.primary.del(...keys);
	}

	scan(pattern: string, count: number): Promise<Array<string>> {
		return this.primary.scan(pattern, count);
	}

	private withDeadline<T>(work: Promise<T>): Promise<T> {
		return new Promise<T>((resolve, reject) => {
			let settled = false;
			let timer: ReturnType<typeof setTimeout> | undefined;
			const started = this.elapsed();
			let extended = false;
			const expire = () => {
				const late = this.elapsed() - started - this.deadlineMs;
				setImmediate(() => {
					if (settled) return;
					if (!extended && late > LOOP_LAG_TOLERANCE_MS) {
						extended = true;
						timer = setTimeout(expire, this.deadlineMs);
						return;
					}
					settled = true;
					reject(new DeadlineExceeded());
				});
			};
			timer = setTimeout(expire, this.deadlineMs);
			work.then(
				(value) => {
					if (settled) return;
					settled = true;
					clearTimeout(timer);
					resolve(value);
				},
				(error: unknown) => {
					if (settled) return;
					settled = true;
					clearTimeout(timer);
					reject(error);
				},
			);
		});
	}

	private checkLocal(key: string, limit: number, windowMs: number, cost: number): KVRateLimitResult {
		const nowMs = this.now();
		const capacity = Math.max(1, Math.floor(limit));
		const leakWindowMs = Math.max(1, Math.floor(windowMs));
		const leakPerMs = capacity / leakWindowMs;
		const stored = this.local.get(key);
		let level = 0;
		if (stored) {
			const elapsed = nowMs - stored.updatedAt;
			level = elapsed > 0 ? Math.max(0, stored.level - elapsed * leakPerMs) : stored.level;
			this.local.delete(key);
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
			this.remember(key, level, nowMs);
			return result(true, Math.max(0, Math.floor(capacity - level)), resetAfter(level), 0);
		}
		if (level + cost > capacity) {
			const retryAfterMs = Math.max(1, Math.ceil((level + cost - capacity) / leakPerMs));
			this.remember(key, level, nowMs);
			return result(false, 0, Math.max(1, resetAfter(level), retryAfterMs), retryAfterMs);
		}
		level += cost;
		this.remember(key, level, nowMs);
		return result(true, Math.max(0, Math.floor(capacity - level)), resetAfter(level), 0);
	}

	private remember(key: string, level: number, nowMs: number): void {
		if (level <= 0) return;
		this.local.set(key, {level, updatedAt: nowMs});
		if (this.local.size > this.maxLocalKeys) {
			const oldest = this.local.keys().next();
			if (!oldest.done) this.local.delete(oldest.value);
		}
	}
}
