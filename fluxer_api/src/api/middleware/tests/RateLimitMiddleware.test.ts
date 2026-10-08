// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {Config} from '@app/api/Config';
import type {TrustedCallerConfig} from '@app/api/config/APIConfig';
import {RateLimitMiddleware, type RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {DonationRateLimitConfigs} from '@app/api/rate_limit_configs/DonationRateLimitConfig';
import {OAuthRateLimitConfigs} from '@app/api/rate_limit_configs/OAuthRateLimitConfig';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import type {
	BucketConfig,
	IRateLimitService,
	RateLimitConfig,
	RateLimitResult,
} from '@pkgs/rate_limit/src/IRateLimitService';
import {type Context, Hono} from 'hono';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

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

const CALLER_IP = '192.0.2.50';
const FORWARDED_IP = '203.0.113.77';
const OTHER_FORWARDED_IP = '203.0.113.78';
const BUGS_KEY = 'bugs-key-0123456789abcdefghijklmnopqrstuv';
const DONATION_KEY = 'donation-key-0123456789abcdefghijklmnopq';

const TRUSTED_CALLERS: Array<TrustedCallerConfig> = [
	{name: 'bugs', key: BUGS_KEY, buckets: ['oauth:token', 'oauth:revoke']},
	{
		name: 'donation',
		key: DONATION_KEY,
		buckets: ['donation:request_link', 'donation:manage', 'donation:checkout'],
	},
];

function buildTrustedHarness(routeConfig: RouteRateLimitConfig): Harness {
	const service = new RecordingRateLimitService();
	let context: Context<HonoEnv> | null = null;
	const app = new Hono<HonoEnv>({strict: true});
	app.use('*', async (ctx, next) => {
		context = ctx;
		ctx.set('rateLimitService', service);
		await next();
	});
	app.post('/route', RateLimitMiddleware(routeConfig), (ctx) => ctx.text('ok'));
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

async function callTrustedRoute(harness: Harness, headers: Record<string, string>): Promise<Response> {
	return await harness.app.request('http://localhost/route', {
		method: 'POST',
		headers: {
			'x-forwarded-for': CALLER_IP,
			'x-fluxer-test-enable-rate-limits': 'true',
			...headers,
		},
	});
}

describe('RateLimitMiddleware trusted callers', () => {
	let previousTrustedCallers: Array<TrustedCallerConfig>;

	beforeEach(() => {
		previousTrustedCallers = Config.internal.trustedCallers;
		Config.internal.trustedCallers = TRUSTED_CALLERS;
	});

	afterEach(() => {
		Config.internal.trustedCallers = previousTrustedCallers;
	});

	test('keys oauth:token on the address the bugs caller forwards, one bucket per address', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_TOKEN);

		const first = await callTrustedRoute(harness, {
			'x-fluxer-internal-key': BUGS_KEY,
			'x-fluxer-client-ip': FORWARDED_IP,
		});
		const second = await callTrustedRoute(harness, {
			'x-fluxer-internal-key': BUGS_KEY,
			'x-fluxer-client-ip': OTHER_FORWARDED_IP,
		});

		expect(first.status).toBe(200);
		expect(second.status).toBe(200);
		expect(harness.service.globalIdentifiers).toEqual([`ip:${FORWARDED_IP}`, `ip:${OTHER_FORWARDED_IP}`]);
		expect(harness.service.buckets).toEqual([`ip:${FORWARDED_IP}:oauth:token`, `ip:${OTHER_FORWARDED_IP}:oauth:token`]);
	});

	test('keys oauth:revoke on the address the bugs caller forwards', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_REVOKE);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-client-ip': FORWARDED_IP});

		expect(harness.service.buckets).toEqual([`ip:${FORWARDED_IP}:oauth:revoke`]);
	});

	test('ignores the bugs key on a donation route', async () => {
		const harness = buildTrustedHarness(DonationRateLimitConfigs.DONATION_MANAGE);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-client-ip': FORWARDED_IP});
		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-donor-ip': FORWARDED_IP});

		expect(harness.service.globalIdentifiers).toEqual([`ip:${CALLER_IP}`, `ip:${CALLER_IP}`]);
		expect(harness.service.buckets).toEqual([`ip:${CALLER_IP}:donation:manage`, `ip:${CALLER_IP}:donation:manage`]);
	});

	test('ignores the donation key on oauth:token', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_TOKEN);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': DONATION_KEY, 'x-fluxer-client-ip': FORWARDED_IP});

		expect(harness.service.buckets).toEqual([`ip:${CALLER_IP}:oauth:token`]);
	});

	test('accepts the donation key on its routes with the new and the old address header', async () => {
		for (const routeConfig of Object.values(DonationRateLimitConfigs)) {
			const harness = buildTrustedHarness(routeConfig);

			await callTrustedRoute(harness, {'x-fluxer-internal-key': DONATION_KEY, 'x-fluxer-client-ip': FORWARDED_IP});
			await callTrustedRoute(harness, {
				'x-fluxer-internal-key': DONATION_KEY,
				'x-fluxer-donor-ip': OTHER_FORWARDED_IP,
			});

			expect(harness.service.buckets).toEqual([
				`ip:${FORWARDED_IP}:${routeConfig.bucket}`,
				`ip:${OTHER_FORWARDED_IP}:${routeConfig.bucket}`,
			]);
		}
	});

	test('prefers the new address header when both are sent', async () => {
		const harness = buildTrustedHarness(DonationRateLimitConfigs.DONATION_MANAGE);

		await callTrustedRoute(harness, {
			'x-fluxer-internal-key': DONATION_KEY,
			'x-fluxer-client-ip': FORWARDED_IP,
			'x-fluxer-donor-ip': OTHER_FORWARDED_IP,
		});

		expect(harness.service.buckets).toEqual([`ip:${FORWARDED_IP}:donation:manage`]);
	});

	test('ignores a wrong key of the same or a different length, and a missing key', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_TOKEN);
		const sameLengthWrongKey = `${BUGS_KEY.slice(0, -1)}${BUGS_KEY.endsWith('v') ? 'w' : 'v'}`;

		await callTrustedRoute(harness, {'x-fluxer-internal-key': sameLengthWrongKey, 'x-fluxer-client-ip': FORWARDED_IP});
		await callTrustedRoute(harness, {'x-fluxer-internal-key': `${BUGS_KEY}x`, 'x-fluxer-client-ip': FORWARDED_IP});
		await callTrustedRoute(harness, {'x-fluxer-client-ip': FORWARDED_IP});

		expect(harness.service.buckets).toEqual([
			`ip:${CALLER_IP}:oauth:token`,
			`ip:${CALLER_IP}:oauth:token`,
			`ip:${CALLER_IP}:oauth:token`,
		]);
	});

	test('falls back to the caller when a trusted key sends an unparsable address', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_TOKEN);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-client-ip': 'not-an-ip'});

		expect(harness.service.buckets).toEqual([`ip:${CALLER_IP}:oauth:token`]);
	});

	test('ignores a trusted key on a route that does not opt in, even when the bucket is listed', async () => {
		Config.internal.trustedCallers = [{name: 'wide', key: BUGS_KEY, buckets: ['oauth:introspect']}];
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_INTROSPECT);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-client-ip': FORWARDED_IP});

		expect(harness.service.buckets).toEqual([`ip:${CALLER_IP}:oauth:introspect`]);
	});
});
