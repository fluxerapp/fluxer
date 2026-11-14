// SPDX-License-Identifier: AGPL-3.0-or-later

import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import type {RateLimitConfig, RateLimitResult} from '@pkgs/rate_limit/src/IRateLimitService';
import {RateLimitService} from '@pkgs/rate_limit/src/RateLimitService';
import {describe, expect, it} from 'vitest';

const CONFIG: RateLimitConfig = {identifier: 'report:target:1', maxAttempts: 5, windowMs: 60_000};

function retryAfterMs(result: RateLimitResult): number {
	return Math.round((result.retryAfterDecimal ?? 0) * 1000);
}

describe('RateLimitService.peekLimit', () => {
	it('allows an empty bucket without spending it', async () => {
		const service = new RateLimitService(new MockKVProvider());
		const peek = await service.peekLimit(CONFIG);
		expect(peek).toMatchObject({allowed: true, limit: 5, remaining: 5});
		expect(peek.retryAfter).toBeUndefined();
		expect(peek.retryAfterDecimal).toBeUndefined();
		expect(await service.peekLimit(CONFIG)).toMatchObject({allowed: true, remaining: 5});
	});

	it('allows while capacity remains and reports the remaining count', async () => {
		const service = new RateLimitService(new MockKVProvider());
		await service.checkLimit(CONFIG);
		await service.checkLimit(CONFIG);
		const peek = await service.peekLimit(CONFIG);
		expect(peek).toMatchObject({allowed: true, remaining: 3});
		expect(peek.retryAfter).toBeUndefined();
		expect((await service.peekLimit(CONFIG)).remaining).toBe(3);
	});

	it('rejects a full bucket with the retry-after a consuming check would report', async () => {
		const service = new RateLimitService(new MockKVProvider());
		for (let attempt = 0; attempt < CONFIG.maxAttempts; attempt += 1) {
			expect((await service.checkLimit(CONFIG)).allowed).toBe(true);
		}
		const peek = await service.peekLimit(CONFIG);
		const check = await service.checkLimit(CONFIG);
		expect(check.allowed).toBe(false);
		expect(peek).toMatchObject({allowed: false, remaining: 0, limit: check.limit, retryAfter: 12});
		expect(check.retryAfter).toBe(12);
		expect(retryAfterMs(peek)).toBeGreaterThan(11_900);
		expect(retryAfterMs(peek)).toBeLessThanOrEqual(12_000);
		expect(retryAfterMs(check)).toBeGreaterThan(11_900);
		expect(retryAfterMs(check)).toBeLessThanOrEqual(12_000);
	});
});
