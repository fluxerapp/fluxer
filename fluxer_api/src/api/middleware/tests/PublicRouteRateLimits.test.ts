// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	BucketConfig,
	IRateLimitService,
	RateLimitConfig,
	RateLimitResult,
} from '@pkgs/rate_limit/src/IRateLimitService';
import {Hono} from 'hono';
import {describe, expect, test} from 'vitest';
import {BlueskyOAuthController} from '../../bluesky/BlueskyOAuthController';
import type {IBlueskyOAuthService} from '../../bluesky/IBlueskyOAuthService';
import {ConnectionRateLimitConfigs} from '../../rate_limit_configs/ConnectionRateLimitConfig';
import type {HonoApp, HonoEnv} from '../../types/HonoEnv';
import {RateLimitMiddleware} from '../RateLimitMiddleware';

const CLIENT_IP = '203.0.113.10';
const HTTP_TOO_MANY_REQUESTS = 429;
const RATE_LIMIT_MIDDLEWARE_SOURCE = String(RateLimitMiddleware(ConnectionRateLimitConfigs.CONNECTION_LIST));

interface BucketCheck {
	bucket: string;
	limit: number;
	windowMs: number;
}

function allowedResult(limit: number): RateLimitResult {
	return {
		allowed: true,
		limit,
		remaining: limit - 1,
		resetTime: new Date(Date.now() + 60000),
		resetAfterDecimal: 60,
	};
}

class RecordingRateLimitService implements IRateLimitService {
	readonly bucketChecks: Array<BucketCheck> = [];
	readonly globalIdentifiers: Array<string> = [];
	denyBuckets = false;

	async checkLimit(config: RateLimitConfig): Promise<RateLimitResult> {
		return allowedResult(config.maxAttempts);
	}

	async peekLimit(config: RateLimitConfig): Promise<RateLimitResult> {
		return allowedResult(config.maxAttempts);
	}

	async checkBucketLimit(bucket: string, config: BucketConfig): Promise<RateLimitResult> {
		this.bucketChecks.push({bucket, limit: config.limit, windowMs: config.windowMs});
		return {...allowedResult(config.limit), allowed: !this.denyBuckets, remaining: this.denyBuckets ? 0 : 1};
	}

	async checkGlobalLimit(identifier: string, limit: number): Promise<RateLimitResult> {
		this.globalIdentifiers.push(identifier);
		return allowedResult(limit);
	}

	async resetLimit(_identifier: string): Promise<void> {}

	async clearLimitsByIdentifierPrefix(_identifierPrefix: string): Promise<number> {
		return 0;
	}
}

const blueskyOAuthService = {
	clientMetadata: {client_id: 'https://fluxer.test/v1/connections/bluesky/client-metadata.json'},
	jwks: {keys: []},
} as unknown as IBlueskyOAuthService;

interface Harness {
	app: HonoApp;
	service: RecordingRateLimitService;
}

function buildHarness(register: (app: HonoApp) => void): Harness {
	const service = new RecordingRateLimitService();
	const app = new Hono<HonoEnv>();
	app.use('*', async (ctx, next) => {
		ctx.set('rateLimitService', service);
		ctx.set('blueskyOAuthService', blueskyOAuthService);
		await next();
	});
	register(app);
	return {app, service};
}

async function callRoute(harness: Harness, path: string): Promise<Response> {
	return await harness.app.request(`http://localhost${path}`, {
		headers: {
			'x-forwarded-for': CLIENT_IP,
			'x-fluxer-test-enable-rate-limits': 'true',
		},
	});
}

function routesWithoutRateLimit(register: (app: HonoApp) => void): Array<string> {
	const app = new Hono<HonoEnv>();
	register(app);
	const registered = new Set<string>();
	const throttled = new Set<string>();
	for (const route of app.routes) {
		const shape = `${route.method} ${route.path}`;
		registered.add(shape);
		if (String(route.handler) === RATE_LIMIT_MIDDLEWARE_SOURCE) {
			throttled.add(shape);
		}
	}
	return [...registered].filter((shape) => !throttled.has(shape)).sort();
}

function bucketHash(bucket: string): string {
	return createHash('sha256').update(bucket).digest('hex').slice(0, 16);
}

describe('public route rate limits', () => {
	test('every Bluesky OAuth route declares a rate limit bucket', () => {
		expect(routesWithoutRateLimit(BlueskyOAuthController)).toEqual([]);
	});

	test('the Bluesky client documents share one bucket keyed by client IP', async () => {
		const harness = buildHarness(BlueskyOAuthController);

		const metadata = await callRoute(harness, '/connections/bluesky/client-metadata.json');
		const jwks = await callRoute(harness, '/connections/bluesky/jwks.json');

		expect(metadata.status).toBe(200);
		expect(jwks.status).toBe(200);
		expect(harness.service.bucketChecks).toEqual([
			{bucket: `ip:${CLIENT_IP}:connection:bluesky:client_document`, limit: 60, windowMs: 60000},
			{bucket: `ip:${CLIENT_IP}:connection:bluesky:client_document`, limit: 60, windowMs: 60000},
		]);
		expect(harness.service.globalIdentifiers).toEqual([`ip:${CLIENT_IP}`, `ip:${CLIENT_IP}`]);
	});

	test('a denied bucket answers a Bluesky client document with 429 and the bucket hash', async () => {
		const harness = buildHarness(BlueskyOAuthController);
		harness.service.denyBuckets = true;

		const response = await callRoute(harness, '/connections/bluesky/client-metadata.json');
		const body = (await response.json()) as {code?: string};

		expect(response.status).toBe(HTTP_TOO_MANY_REQUESTS);
		expect(body.code).toBe(APIErrorCodes.RATE_LIMITED);
		expect(response.headers.get('X-RateLimit-Bucket')).toBe(
			bucketHash(ConnectionRateLimitConfigs.BLUESKY_CLIENT_DOCUMENT.bucket),
		);
		expect(response.headers.get('X-RateLimit-Scope')).toBe('user');
	});
});
