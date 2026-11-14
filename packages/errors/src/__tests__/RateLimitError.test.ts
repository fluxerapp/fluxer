// SPDX-License-Identifier: AGPL-3.0-or-later

import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {describe, expect, it} from 'vitest';

interface RateLimitResponseBody {
	code: string;
	retry_after: number;
	global: boolean;
}

describe('RateLimitError', () => {
	it('returns bucket headers for route limits', async () => {
		const resetTime = new Date(Math.ceil(Date.now() / 1000) * 1000 + 60_000);
		const error = new RateLimitError({
			retryAfter: 5,
			retryAfterDecimal: 4.5,
			limit: 10,
			resetTime,
			resetAfterDecimal: 4.5,
			bucketHash: 'bucket-hash',
			scope: 'user',
		});
		const response = error.getResponse();
		const body = (await response.json()) as RateLimitResponseBody;
		expect(response.status).toBe(429);
		expect(body).toMatchObject({
			code: 'RATE_LIMITED',
			retry_after: 4.5,
			global: false,
		});
		expect(response.headers.get('Retry-After')).toBe('5');
		expect(response.headers.get('X-RateLimit-Limit')).toBe('10');
		expect(response.headers.get('X-RateLimit-Remaining')).toBe('0');
		expect(response.headers.get('X-RateLimit-Reset')).toBe(String(resetTime.getTime() / 1000));
		expect(response.headers.get('X-RateLimit-Reset-After')).toBe('4.5');
		expect(response.headers.get('X-RateLimit-Bucket')).toBe('bucket-hash');
		expect(response.headers.get('X-RateLimit-Scope')).toBe('user');
		expect(response.headers.get('X-RateLimit-Global')).toBeNull();
	});
	it('returns global headers without bucket metadata', async () => {
		const error = new RateLimitError({
			global: true,
			retryAfter: 2,
			retryAfterDecimal: 1.5,
			limit: 50,
			resetTime: new Date('2026-01-27T12:00:02.000Z'),
			scope: 'global',
		});
		const response = error.getResponse();
		const body = (await response.json()) as RateLimitResponseBody;
		expect(body).toMatchObject({
			code: 'RATE_LIMITED',
			retry_after: 1.5,
			global: true,
		});
		expect(response.headers.get('Retry-After')).toBe('2');
		expect(response.headers.get('X-RateLimit-Global')).toBe('true');
		expect(response.headers.get('X-RateLimit-Scope')).toBe('global');
		expect(response.headers.get('X-RateLimit-Limit')).toBeNull();
		expect(response.headers.get('X-RateLimit-Bucket')).toBeNull();
		expect(response.headers.get('X-RateLimit-Reset-After')).toBeNull();
	});
	it('sanitizes invalid retry-after and reset values', () => {
		const error = new RateLimitError({
			retryAfter: NaN,
			retryAfterDecimal: Number.POSITIVE_INFINITY,
			limit: NaN,
			resetTime: new Date(NaN),
		});
		const response = error.getResponse();
		const resetTimestamp = Number(response.headers.get('X-RateLimit-Reset'));
		expect(response.headers.get('Retry-After')).toBe('1');
		expect(response.headers.get('X-RateLimit-Limit')).toBe('1');
		expect(response.headers.get('X-RateLimit-Reset-After')).toBe('1');
		expect(Number.isFinite(resetTimestamp)).toBe(true);
		expect(resetTimestamp).toBeGreaterThan(Math.floor(Date.now() / 1000));
	});
	it('defaults scope to global or user when omitted', () => {
		const globalError = new RateLimitError({
			global: true,
			retryAfter: 1,
			limit: 10,
			resetTime: new Date('2026-01-27T12:00:01.000Z'),
		});
		const userError = new RateLimitError({
			retryAfter: 1,
			limit: 10,
			resetTime: new Date('2026-01-27T12:00:01.000Z'),
		});
		expect(globalError.headers?.['X-RateLimit-Scope']).toBe('global');
		expect(userError.headers?.['X-RateLimit-Scope']).toBe('user');
	});
});
