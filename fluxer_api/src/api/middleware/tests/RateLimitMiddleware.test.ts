// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {
	BucketConfig,
	IRateLimitService,
	RateLimitConfig,
	RateLimitResult,
} from '@pkgs/rate_limit/src/IRateLimitService';
import {type Context, Hono} from 'hono';
import {describe, expect, test} from 'vitest';
import type {HonoEnv} from '../../types/HonoEnv';
import {RateLimitMiddleware, type RouteRateLimitConfig} from '../RateLimitMiddleware';

const CLIENT_IP = '203.0.113.10';
const SWAPPED_CLIENT_IP = '198.51.100.7';

const WEBHOOK_READ: RouteRateLimitConfig = {
	bucket: 'webhook:read::webhook_id',
	config: {limit: 40, windowMs: 10000},
};

const WEBHOOK_UPDATE: RouteRateLimitConfig = {
	bucket: 'webhook:update::webhook_id',
	config: {limit: 20, windowMs: 10000},
};

function createAllowedResult(limit: number): RateLimitResult {
	return {
		allowed: true,
		limit,
		remaining: limit - 1,
		resetTime: new Date(Date.now() + 10000),
		resetAfterDecimal: 10,
	};
}

class RecordingRateLimitService implements IRateLimitService {
	readonly globalIdentifiers: Array<string> = [];
	readonly buckets: Array<string> = [];
	onGlobalCheck: () => void = () => undefined;

	async checkLimit(config: RateLimitConfig): Promise<RateLimitResult> {
		return createAllowedResult(config.maxAttempts);
	}

	async peekLimit(config: RateLimitConfig): Promise<RateLimitResult> {
		return createAllowedResult(config.maxAttempts);
	}

	async checkBucketLimit(bucket: string, config: BucketConfig): Promise<RateLimitResult> {
		this.buckets.push(bucket);
		return createAllowedResult(config.limit);
	}

	async checkGlobalLimit(identifier: string, limit: number): Promise<RateLimitResult> {
		this.globalIdentifiers.push(identifier);
		this.onGlobalCheck();
		return createAllowedResult(limit);
	}

	async resetLimit(_identifier: string): Promise<void> {}

	async clearLimitsByIdentifierPrefix(_identifierPrefix: string): Promise<number> {
		return 0;
	}
}

interface Harness {
	app: Hono<HonoEnv>;
	service: RecordingRateLimitService;
	getContext(): Context<HonoEnv>;
}

function buildHarness(routeConfig: RouteRateLimitConfig): Harness {
	const service = new RecordingRateLimitService();
	let context: Context<HonoEnv> | null = null;
	const app = new Hono<HonoEnv>({strict: true});
	app.use('*', async (ctx, next) => {
		context = ctx;
		ctx.set('rateLimitService', service);
		await next();
	});
	app.get('/webhooks/:webhook_id/:token', RateLimitMiddleware(routeConfig), (ctx) => ctx.text('ok'));
	return {
		app,
		service,
		getContext(): Context<HonoEnv> {
			if (!context) {
				throw new Error('no request has run yet');
			}
			return context;
		},
	};
}

async function callRoute(harness: Harness, webhookId: string, clientIp = CLIENT_IP): Promise<Response> {
	return await harness.app.request(`http://localhost/webhooks/${webhookId}/secret`, {
		headers: {
			'x-forwarded-for': clientIp,
			'x-fluxer-test-enable-rate-limits': 'true',
		},
	});
}

function expectedBucketHash(bucket: string): string {
	return createHash('sha256').update(bucket).digest('hex').slice(0, 16);
}

describe('RateLimitMiddleware', () => {
	test('reports the same bucket hash for every request to a route', async () => {
		const harness = buildHarness(WEBHOOK_READ);

		const first = await callRoute(harness, '111');
		const second = await callRoute(harness, '222');

		expect(first.status).toBe(200);
		expect(second.status).toBe(200);
		expect(first.headers.get('X-RateLimit-Bucket')).toBe(expectedBucketHash(WEBHOOK_READ.bucket));
		expect(second.headers.get('X-RateLimit-Bucket')).toBe(first.headers.get('X-RateLimit-Bucket'));
		expect(harness.service.buckets).toEqual([`ip:${CLIENT_IP}:webhook:read:111`, `ip:${CLIENT_IP}:webhook:read:222`]);
	});

	test('gives routes with different buckets different bucket hashes', async () => {
		const readHarness = buildHarness(WEBHOOK_READ);
		const updateHarness = buildHarness(WEBHOOK_UPDATE);

		const read = await callRoute(readHarness, '111');
		const update = await callRoute(updateHarness, '111');

		expect(read.headers.get('X-RateLimit-Bucket')).toBe(expectedBucketHash(WEBHOOK_READ.bucket));
		expect(update.headers.get('X-RateLimit-Bucket')).toBe(expectedBucketHash(WEBHOOK_UPDATE.bucket));
		expect(read.headers.get('X-RateLimit-Bucket')).not.toBe(update.headers.get('X-RateLimit-Bucket'));
	});

	test('resolves the client identifier once and reuses it for the bucket key', async () => {
		const harness = buildHarness(WEBHOOK_READ);
		harness.service.onGlobalCheck = () => {
			harness.getContext().req.raw.headers.set('x-forwarded-for', SWAPPED_CLIENT_IP);
		};

		const response = await callRoute(harness, '111');

		expect(response.status).toBe(200);
		expect(harness.service.globalIdentifiers).toEqual([`ip:${CLIENT_IP}`]);
		expect(harness.service.buckets).toEqual([`ip:${CLIENT_IP}:webhook:read:111`]);
	});
});
